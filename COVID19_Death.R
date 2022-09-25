library(data.table)
library(xts)
library(argo)
library(forecast)

us_national <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv") #New York Times 
#us_counties <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") #daily updated data from NY Times github
us_states <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
#us_states <-read.csv("~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/us-states-2021-02-17.csv") 

us_national <- data.table(us_national)
us_national$new_cases <- c(0, diff(us_national$cases)) #new cases=lag of totalcases (today-yesterday)
us_national$daily_deaths <- c(0, diff(us_national$deaths))
us_national$date <- as.Date(us_national$date)
plot(us_national$date, us_national$new_cases)
plot(us_national$date, us_national$daily_deaths)

gt.folder <- "~/US-covid19-api_raw_results-2022-08-14"
#gt.folder <- "C:/Users/Steven Ma/Dropbox (GaTech)/Backup/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/US-covid19-api_raw_results-2020-09-20"
files <- list.files(gt.folder) #get all files name as string and combine as list (in order to use lapply)
f <- files[1]

gt.parser.pub.api <- function(gt.folder, f){
  gtdata <- read.csv(file.path(gt.folder, f)) #get search word (common cold) frequency csv file
  f_info <- strsplit(f, "_")[[1]] #get the area of the file ("US" or US-State")
  state <- f_info[1]
  gtdata <- xts(gtdata[,ncol(gtdata),drop=FALSE], as.Date(gtdata$date)) #pick last column (frequency of searched word) and create a time-series object (index is dates)
  if(all(diff(index(gtdata))==7)){ #if each row is one week difference
    index(gtdata) <- index(gtdata) + 6 
    gtfreq <- "week"
  }else if (all(diff(index(gtdata))==1)){ #if each row is one day difference
    gtfreq <- "day"
  }else{ #if each row is one month difference
    index(gtdata) <- as.yearmon(index(gtdata))
    gtfreq <- "month"
  }
  term <- colnames(gtdata) #get the search word (string)
  term <- gsub(".*\\.m\\.", "",term)  #clean up added chars in the search word (usually occur due to space); replace un-need chars with no space
  term <- gsub("^\\X.", "",term)   #replace un-need chars with no space
  if(!grepl(term, gsub(" ",".",f))){  #each freq file's column name(search word) should be the same as filename, if not then output that filename
    cat(f,colnames(gtdata),"\n")  #output that filename if search query doesnt match with file name
  }
  #return the state label (string), a time series object (vector), and a string for frequency
  #later will need "US" or US-state" for example "US-AL"
  return(list(state=state, gtdata=gtdata, gtfreq=gtfreq))  
}

raw_data_parsed <- lapply(files, function(f) gt.parser.pub.api(gt.folder, f)) #apply above function on all elements in the list "files", output each search query file in a list, thus this is a list embedded with a series of lists
all_geo <- sapply(raw_data_parsed, function(x) x$state)   #extract all results' states ("US" or "US-state")
gtdata_all <- lapply(raw_data_parsed, function(x) x$gtdata)   #get frequencies of all words searched, still keep in list (no month or week only day), started since 2019-01-01

# tapply applies some function to the first input argument based on the levels of the second input argument. First and second argument are same size (row and columns). Here, there are different levels in all_geo ("US","US-AK","US-AL",...).
gtdata_daily_state <- tapply(gtdata_all, all_geo, function(gt.eachstate){ #for each all_geo levels, there is a list of search query frequency. For example, first level: "US" has first US_commoncold, US_coronavirus,.... under that level.
  tab <- do.call(merge, gt.eachstate)                                     #tapple recognize which arrays in gtdata_all belongs to which level in all_geo and merge gtdata_all based on these levels, i.e. under "US" list now have all its searched words frequencies, each word as its own list
  tab[,!grepl("\\.1$",colnames(tab))]                                     #return in big list of lists (regions), each list is a matrix: rows are dates, columns are word frequences for each word (column) in that region, i.e list one is matrix of "US", each columns are search freq of words under the level "US"   
})

# Get national level death and cases and State level death and cases
covid_nat_incre <- xts(us_national[,.(new_cases, daily_deaths)], us_national$date) #select national new cases and daily death, make date as index
# Get State level death and cases
population.file <- "~/Population.csv" #gives population of each state and which region it belongs
state_region_info <- fread(population.file)
state_region_info$Population <- as.numeric(gsub(",", "", state_region_info$Population))
state_region_info$Abbre = paste0('US-',state_region_info$Abbre)
region_name_mapping = state_region_info[, c(1, 2)]
colnames(region_name_mapping) = c('sub_region_1','iso_3166_2_code')

us_states = subset(us_states, state%in%region_name_mapping[,sub_region_1]) 
region_code_all = names(gtdata_daily_state)
region_code_all = region_code_all[region_code_all!="US"] #remove "US" as it is not one of the 51 regions
covid_state_level = list()
for (region_code in region_code_all){ #loop through all regions in the order of names(gtdata_daily_state), the list contains mobility and search query
  region_full_name = region_name_mapping[region_name_mapping$iso_3166_2_code %in% region_code]$sub_region_1
  us_states_temp = us_states[us_states$state %in% region_full_name,] #select the given region's cases and deaths
  us_states_temp <- data.table(us_states_temp) #convert to table for easier transite to xts
  us_states_temp$new_cases <- c(0, diff(us_states_temp$cases)) #new cases=lag of totalcases (today-yesterday)
  us_states_temp$daily_deaths <- c(0, diff(us_states_temp$deaths)) #new death=lag of totaldeath (today-yesterday)
  us_states_temp$date <- as.Date(us_states_temp$date) #get date
  covid_state_temp <- xts(us_states_temp[,.(new_cases, daily_deaths)], us_states_temp$date)
  covid_state_level[[region_code]] <- covid_state_temp #put the state level new cases and death in the list in the same order as gtdata_daily_state
}
for (region_code in region_code_all){ #Check if all state's new deaths and cases are index by daily
  if (!all(diff(index(covid_state_level[[region_code]]))==1)){
    print(paste0("ERROR, Daily count not index with difference of 1 in ",region_code," !!"))
  }
}
covid_state_level[["US"]] <- covid_nat_incre #add "US" new deaths and cases into covid_state_level list
covid_state_level = covid_state_level[names(gtdata_daily_state)] #order new cases and death in the list in the same order as gtdata_daily_state
for (region_code in  names(gtdata_daily_state)){ #loop over all state's new death and cases and add some earlier dates and force same number of rows/dates
  #region_code = "US"
  temp = cbind(gtdata_daily_state[[region_code]], covid_state_level[[region_code]])
  covid_state_temp = cbind(temp$new_cases,temp$daily_deaths)
  covid_state_temp = covid_state_temp[-(1:(which(index(covid_state_temp)=="2020-01-21")-1)),]
  covid_state_temp[is.na(covid_state_temp)] = 0
  covid_state_level[[region_code]] = covid_state_temp
  if (region_code == "US-NJ"){ #2020/6/25 New Jersey added additional 1854 Accumulative probable death from COVID-19 to that day. We remove that for now.
    covid_state_level[[region_code]]["2020-06-25",]$daily_deaths = covid_state_level[[region_code]]["2020-06-25",]$daily_deaths - 1854
  }
}


######################################################################################################################################################
# Finalize GT data for national, regional and state (put mobility in)
# Regional GT will be a linear combination of States GT (ones in that region), states GT will be a mix of states GT and regional GT with weight 1/3
# National GT stay the same 
# If predicting weekly instead of daily, then set aggregate_weekly=TRUE
aggregate_weekly = FALSE #whether aggregate daily data into weekly for both new death and GT search query
gt_weekly_state = list()

for (region in names(gtdata_daily_state)){
  gt_nat <- gtdata_daily_state[[region]]
  gt_nat[is.na(gt_nat)] <- 0 #put 0 for missing value
  gt_nat <- gt_nat[index(gtdata_daily_state$US)]
  gtdata_daily_state[[region]] = gt_nat
}

# Construct regional information (linear combination of the states' information in that region)
state_region_info <- fread(population.file)
state_region_info$Population <- as.numeric(gsub(",", "", state_region_info$Population))
state_names = setdiff(names(gtdata_daily_state), c("US")) #get rid of US, that is national level
gtdata_regional <- lapply(1:10, function(region_number) {
  states_id <- which(state_region_info$Region == region_number)
  GT_states_wgted <- lapply(states_id, function(j) {
    gtdata_daily_state[[ paste0("US-",state_region_info$Abbre[j]) ]] * state_region_info$Population[j]/sum(state_region_info$Population[states_id])
  })
  Reduce("+", GT_states_wgted)
})
names(gtdata_regional) <- paste0("Region-", 1:10)

# Construct regional daily death, by simply adding up the states daily deaths thats in the region
covid_regional_level <- lapply(1:10, function(region_number) {
  states_id <- which(state_region_info$Region == region_number)
  covid_states_wgted <- lapply(states_id, function(j) {
    covid_state_level[[ paste0("US-",state_region_info$Abbre[j]) ]] #simply aggregate?Not linear combination with population portion???
  })
  Reduce("+", covid_states_wgted)
})
names(covid_regional_level) <- paste0("Region-", 1:10)

#Here we modify the Google Search query freqency for state level using a linear combination of the statelevel and regional level
#Here we set mix=1/3, where statelevel has weight 2/3 and its specific region has weight 1/3. Assuming that the spacial impact is 1/3. 
gtdata_daily_state_MIXED <- list()
mix <- 1/3 
for (each_state in state_names){ #Note that strsplit if parsing "." then either use "\\." or "[.] or "fixed=TRUE"
  region_id_for_state = state_region_info[Abbre==strsplit(each_state, "-")[[1]][2], Region] #get the region idx (range from 1 to 10) for specific state 
  gtdata_daily_state_MIXED[[each_state]] = (gtdata_daily_state[[each_state]] * (1-mix) + gtdata_regional[[paste0("Region-",region_id_for_state)]] * mix)
}

#national GT is the same 
gtdata_national = gtdata_daily_state[["US"]]
covid_national_level = covid_state_level[["US"]]

# Force negative death to be zero
for (idx in names(covid_state_level)){
  covid_state_level[[idx]][covid_state_level[[idx]]$daily_deaths<0,2]=0
}
for (idx in names(covid_regional_level)){
  covid_regional_level[[idx]][covid_regional_level[[idx]]$daily_deaths<0,2]=0
}
covid_national_level[covid_national_level$daily_deaths<0,2]=0
covid_national_level["2020-06-25",]$daily_deaths = covid_national_level["2020-06-25",]$daily_deaths-1854 #Remove Probable accumulative death of 1854 from New Jersey at 2020-06-25 for now.

######################################################################################################################################################
## DATA FILTERING
######################################################################################################################################################
# Drop the google search terms that have frequency lower than 50,000. Filter out too low and too high values using IQR, while checking its neighbours.
# NOTE THAT all regions (national, regional, states) should have same length of dates (index)
dropped_terms = list()
gtdata_original = gtdata_daily_state #store un-IQR and Media filtered JUST IN CASE
gtdata_mixed_original = gtdata_daily_state_MIXED
gtdata_regional_original = gtdata_regional
# WORKING on national and state level (not mixed) here. FILTERING
for (regions in names(gtdata_daily_state)){
  ratio_term = length(names(gtdata_daily_state[[regions]])) #BE SURE TO LOAD IN THE MEDIA RATIO INTO EACH STATE FIRST
  terms_workon = names(gtdata_daily_state[[regions]])
  terms_workon = terms_workon[-c(ratio_term:length(names(gtdata_daily_state[[regions]])))]
  for (terms in terms_workon){
    gtdata_daily_state[[regions]][is.na(gtdata_daily_state[[regions]][,terms]),terms] = 0
    #if the search term is below 50 search volume OR if there are more than 70% 0s OR if search volume greater than 50 is less than 10% of total days. DELETE
    if (max(gtdata_daily_state[[regions]][,terms])<=50 || length(which(gtdata_daily_state[[regions]][,terms]!=0)) <(0.3*length(gtdata_daily_state[[regions]][,terms])) || length(which(gtdata_daily_state[[regions]][,terms]>50))< (0.1*length(gtdata_daily_state[[regions]][,terms])) ){ 
      dropped_terms[[regions]] = c(dropped_terms[[regions]],as.character(terms))
    }
    else{ #if not, filter out its outliers
      start_filter_date = which(index(gtdata_daily_state[[regions]][,terms]) == "2020-05-01")
      end_filter_date = length(gtdata_daily_state[[regions]][,terms])
      #large valued outlier
      idx_high_outlier = which(gtdata_daily_state[[regions]][c(start_filter_date:end_filter_date),terms] >= quantile(gtdata_daily_state[[regions]][c(start_filter_date:end_filter_date),terms],0.99))
      idx_high_outlier = start_filter_date+idx_high_outlier-1
      if (!length(idx_high_outlier)==0){
        for (idx in 1:length(idx_high_outlier)){
          if (abs(end_filter_date-idx_high_outlier[idx])<=2){
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]-2))
          }
          else{
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]+1),c(idx_high_outlier[idx]+2))
          }
          high_outlier_neighbour_P={}
          for (i in 1:length(idx_high_neighbour)){ #compute the P-values of high outlier's neighbours by assuming Normal Distribution with mean equal to the outliers, covariance equal to the empirical covariance of the google search term series 
            high_outlier_neighbour_P = c(high_outlier_neighbour_P,pnorm(gtdata_daily_state[[regions]][idx_high_neighbour[i],terms],gtdata_daily_state[[regions]][idx_high_outlier[idx],terms],sqrt(cov(gtdata_daily_state[[regions]][,terms]))))
          }
          if (all(high_outlier_neighbour_P<0.05)){ #if there are hight outliers via IQR and their neighbours have p-values less than 5%, replace the outliers via previous two day average
            gtdata_daily_state[[regions]][idx_high_outlier[idx],terms] = (as.matrix(gtdata_daily_state[[regions]][idx_high_outlier[idx]-1,terms]) + as.matrix(gtdata_daily_state[[regions]][idx_high_outlier[idx]-2,terms]) + as.matrix(gtdata_daily_state[[regions]][idx_high_outlier[idx]-3,terms]) )/3
          }
        }
      }
      #Small valued outlier. Less strict than high valued outlier above. No need to use p-value
      idx_low_outlier = which(gtdata_daily_state[[regions]][c(start_filter_date:end_filter_date),terms] <= quantile(gtdata_daily_state[[regions]][c(start_filter_date:end_filter_date),terms],0.01))
      idx_low_outlier = start_filter_date + idx_low_outlier - 1
      if (!length(idx_low_outlier)==0){
        for (idx in 1:length(idx_low_outlier)){
          gtdata_daily_state[[regions]][idx_low_outlier[idx],terms] = (as.matrix(gtdata_daily_state[[regions]][idx_low_outlier[idx]-1,terms]) + as.matrix(gtdata_daily_state[[regions]][idx_low_outlier[idx]-2,terms]) + as.matrix(gtdata_daily_state[[regions]][idx_low_outlier[idx]-3,terms]) )/3
        }
      }
    }
  }
}
names(dropped_terms) = names(gtdata_daily_state) #name the list dropped_terms with region names
gtdataALL_IQR_noMedia = gtdata_daily_state #only IQR filtered

#*************************************************************************************************************************************
# WORKING on Regional here!
dropped_terms = list()
for(regions in names(gtdata_regional)){
  ratio_term = length(names(gtdata_regional[[regions]])) #BE SURE TO LOAD IN THE MEDIA RATIO INTO EACH STATE FIRST
  terms_workon = names(gtdata_regional[[regions]])
  terms_workon = terms_workon[-c(ratio_term:length(names(gtdata_regional[[regions]])))]
  for (terms in terms_workon){
    gtdata_regional[[regions]][is.na(gtdata_regional[[regions]][,terms]),terms] = 0
    #if the search term is below 50 search volume OR if there are more than 70% 0s OR if search volume greater than 50 is less than 2% of total days. DELETE
    if (max(gtdata_regional[[regions]][,terms])<=50 || length(which(gtdata_regional[[regions]][,terms]!=0)) <(0.3*length(gtdata_regional[[regions]][,terms])) || length(which(gtdata_regional[[regions]][,terms]>50))< (0.1*length(gtdata_regional[[regions]][,terms])) ){ 
      dropped_terms[[regions]] = c(dropped_terms[[regions]],as.character(terms))
    }
    else{ #if not, filter out its outliers
      start_filter_date = which(index(gtdata_regional[[regions]][,terms]) == "2020-05-01")
      end_filter_date = length(gtdata_regional[[regions]][,terms])
      #large valued outlier
      idx_high_outlier = which(gtdata_regional[[regions]][c(start_filter_date:end_filter_date),terms] >= quantile(gtdata_regional[[regions]][c(start_filter_date:end_filter_date),terms],0.99))
      idx_high_outlier = start_filter_date+idx_high_outlier-1
      if (!length(idx_high_outlier)==0){
        for (idx in 1:length(idx_high_outlier)){
          if (abs(end_filter_date-idx_high_outlier[idx])<=2){
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]-2))
          }
          else{
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]+1),c(idx_high_outlier[idx]+2))
          }
          high_outlier_neighbour_P={}
          for (i in 1:length(idx_high_neighbour)){ #compute the P-values of high outlier's neighbours by assuming Normal Distribution with mean equal to the outliers, covariance equal to the empirical covariance of the google search term series 
            high_outlier_neighbour_P = c(high_outlier_neighbour_P,pnorm(gtdata_regional[[regions]][idx_high_neighbour[i],terms],gtdata_regional[[regions]][idx_high_outlier[idx],terms],sqrt(cov(gtdata_regional[[regions]][,terms]))))
          }
          if (all(high_outlier_neighbour_P<0.05)){ #if there are hight outliers via IQR and their neighbours have p-values less than 5%, replace the outliers via previous two day average
            gtdata_regional[[regions]][idx_high_outlier[idx],terms] = (as.matrix(gtdata_regional[[regions]][idx_high_outlier[idx]-1,terms]) + as.matrix(gtdata_regional[[regions]][idx_high_outlier[idx]-2,terms]) + as.matrix(gtdata_regional[[regions]][idx_high_outlier[idx]-3,terms]) )/3
          }
        }
      }
      #Small valued outlier. Less strict than high valued outlier above. No need to use p-value
      idx_low_outlier = which(gtdata_regional[[regions]][c(start_filter_date:end_filter_date),terms] <= quantile(gtdata_regional[[regions]][c(start_filter_date:end_filter_date),terms],0.01))
      idx_low_outlier = start_filter_date + idx_low_outlier - 1
      if (!length(idx_low_outlier)==0){
        for (idx in 1:length(idx_low_outlier)){
          gtdata_regional[[regions]][idx_low_outlier[idx],terms] = (as.matrix(gtdata_regional[[regions]][idx_low_outlier[idx]-1,terms]) + as.matrix(gtdata_regional[[regions]][idx_low_outlier[idx]-2,terms]) + as.matrix(gtdata_regional[[regions]][idx_low_outlier[idx]-3,terms]) )/3
        }
      }
    }
  }
}
names(dropped_terms) = names(gtdata_regional) #name the list dropped_terms with region names
for (regions in names(gtdata_regional)){ #DELETE the terms with lower than 50 search volume
  gtdata_regional[[regions]] = gtdata_regional[[regions]][,-which(names(gtdata_regional[[regions]])%in% unlist(dropped_terms[regions]))]
}
gtdata_Region_IQR_noMedia = gtdata_regional #only IQR filtered

#*************************************************************************************************************************************
# WORKING on Mixed State Levels here!
dropped_terms = list()
for (regions in names(gtdata_daily_state_MIXED)){
  ratio_term = length(names(gtdata_daily_state_MIXED[[regions]])) #BE SURE TO LOAD IN THE MEDIA RATIO INTO EACH STATE FIRST
  terms_workon = names(gtdata_daily_state_MIXED[[regions]])
  terms_workon = terms_workon[-c(ratio_term:length(names(gtdata_daily_state_MIXED[[regions]])))]
  for (terms in terms_workon){
    gtdata_daily_state_MIXED[[regions]][is.na(gtdata_daily_state_MIXED[[regions]][,terms]),terms] = 0
    #if the search term is below 50 search volume OR if there are more than 70% 0s OR if search volume greater than 50 is less than 2% of total days. DELETE
    if (max(gtdata_daily_state_MIXED[[regions]][,terms])<=50 || length(which(gtdata_daily_state_MIXED[[regions]][,terms]!=0)) <(0.3*length(gtdata_daily_state_MIXED[[regions]][,terms])) || length(which(gtdata_daily_state_MIXED[[regions]][,terms]>50))< (0.1*length(gtdata_daily_state_MIXED[[regions]][,terms])) ){ 
      dropped_terms[[regions]] = c(dropped_terms[[regions]],as.character(terms))
    }
    else{ #if not, filter out its outliers
      start_filter_date = which(index(gtdata_daily_state_MIXED[[regions]][,terms]) == "2020-05-01")
      end_filter_date = length(gtdata_daily_state_MIXED[[regions]][,terms])
      #large valued outlier
      idx_high_outlier = which(gtdata_daily_state_MIXED[[regions]][c(start_filter_date:end_filter_date),terms] >= quantile(gtdata_daily_state_MIXED[[regions]][c(start_filter_date:end_filter_date),terms],0.99))
      idx_high_outlier = start_filter_date+idx_high_outlier-1
      if (!length(idx_high_outlier)==0){
        for (idx in 1:length(idx_high_outlier)){
          if (abs(end_filter_date-idx_high_outlier[idx])<=2){
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]-2))
          }
          else{
            idx_high_neighbour = c((idx_high_outlier[idx]-1),(idx_high_outlier[idx]+1),c(idx_high_outlier[idx]+2))
          }
          high_outlier_neighbour_P={}
          for (i in 1:length(idx_high_neighbour)){ #compute the P-values of high outlier's neighbours by assuming Normal Distribution with mean equal to the outliers, covariance equal to the empirical covariance of the google search term series 
            high_outlier_neighbour_P = c(high_outlier_neighbour_P,pnorm(gtdata_daily_state_MIXED[[regions]][idx_high_neighbour[i],terms],gtdata_daily_state_MIXED[[regions]][idx_high_outlier[idx],terms],sqrt(cov(gtdata_daily_state_MIXED[[regions]][,terms]))))
          }
          if (all(high_outlier_neighbour_P<0.05)){ #if there are hight outliers via IQR and their neighbours have p-values less than 5%, replace the outliers via previous two day average
            gtdata_daily_state_MIXED[[regions]][idx_high_outlier[idx],terms] = (as.matrix(gtdata_daily_state_MIXED[[regions]][idx_high_outlier[idx]-1,terms]) + as.matrix(gtdata_daily_state_MIXED[[regions]][idx_high_outlier[idx]-2,terms]) + as.matrix(gtdata_daily_state_MIXED[[regions]][idx_high_outlier[idx]-3,terms]) )/3
          }
        }
      }
      #Small valued outlier. Less strict than high valued outlier above. No need to use p-value
      idx_low_outlier = which(gtdata_daily_state_MIXED[[regions]][c(start_filter_date:end_filter_date),terms] <= quantile(gtdata_daily_state_MIXED[[regions]][c(start_filter_date:end_filter_date),terms],0.01))
      idx_low_outlier = start_filter_date + idx_low_outlier - 1
      if (!length(idx_low_outlier)==0){
        for (idx in 1:length(idx_low_outlier)){
          gtdata_daily_state_MIXED[[regions]][idx_low_outlier[idx],terms] = (as.matrix(gtdata_daily_state_MIXED[[regions]][idx_low_outlier[idx]-1,terms]) + as.matrix(gtdata_daily_state_MIXED[[regions]][idx_low_outlier[idx]-2,terms]) + as.matrix(gtdata_daily_state_MIXED[[regions]][idx_low_outlier[idx]-3,terms]) )/3
        }
      }
    }
  }
}
names(dropped_terms) = names(gtdata_daily_state_MIXED) #name the list dropped_terms with region names
for (regions in names(gtdata_daily_state_MIXED)){ #DELETE the terms with lower than 50 search volume
  gtdata_daily_state_MIXED[[regions]] = gtdata_daily_state_MIXED[[regions]][,-which(names(gtdata_daily_state_MIXED[[regions]])%in% unlist(dropped_terms[regions]))]
}
gtdata_stateMixed_IQR_noMedia = gtdata_daily_state_MIXED #only IQR filtered

#*************************************************************************************************************************************
gtdata_national = gtdata_daily_state[["US"]]
ratio_term = length(names(gtdata_daily_state[["US"]]))
terms_workon = names(gtdata_daily_state[["US"]])
terms_workon = terms_workon[-c(ratio_term:length(names(gtdata_daily_state[["US"]])))]
terms_NOT_workon = names(gtdata_daily_state[["US"]])[c(ratio_term:length(names(gtdata_daily_state[["US"]])))] #MOBILITY terms

###########################################################################################################################################################
# ALL STATES JUST ONLY GT!!! (STEP 1 of ARGOX)
###########################################################################################################################################################
# Selected terms that are significant to cases since June. 33 TERMS. Select them and lag them with optimal lag to cases/death
terms_important = c("X.coronavirus.vaccine.", "X.Cough.","X.covid.19.","X.covid.19.vaccine.", "X.Fever.", "X.Nausea.","X.Sore.throat.",
                    "X.Headache.", "bronchitis", "coronavirus.exposure", "coronavirus.cases", "coronavirus.test",
                    "covid.19.cases", "exposed.to.coronavirus",  "how.long.covid.19", 
                    "how.long.contagious",  "loss.of.smell", "loss.of.taste", "pneumonia", "rapid.covid.19", "rapid.coronavirus","robitussin","strep.throat",
                    "sinus", "symptoms.of.the.covid.19", "upper.respiratory")
if (FALSE){ # Fit one linear model use all data find the best lag and see if they are significant w.r.p to the sandwich estimated p-val
  gtdata_tempNAT = gtdataALL_IQR_noMedia[["US"]]
  ratio_term = which(names(gtdata_tempNAT)=="ratio")
  terms_workon = names(gtdata_tempNAT)
  terms_workon = terms_workon[-c(ratio_term:length(names(gtdata_tempNAT)))]
  terms_NOT_workon = names(gtdata_tempNAT)[c(ratio_term:length(names(gtdata_tempNAT)))]
  
  gt_lag = 4:35
  beta_sandPval_MedRaw = matrix(0,length(terms_workon),length(gt_lag))
  rownames(beta_sandPval_MedRaw) = terms_workon
  colnames(beta_sandPval_MedRaw) = paste0("lag-",as.character(gt_lag))
  beta_MedRaw = list()
  resid_MSE_MedRaw = matrix(0,length(terms_workon),length(gt_lag))
  rownames(resid_MSE_MedRaw) = terms_workon
  colnames(resid_MSE_MedRaw) = paste0("lag-",as.character(gt_lag))
  for (terms in terms_important){
    current_term = gtdata_tempNAT[,terms] #get the current working google search term
    #current_term = gtdata_smooth[,terms]
    gt_lag_train = stats::lag(current_term, gt_lag) #get lagged search term for all lags working on, in later loop, work on each column at a time
    gt_train_cur <- na.omit(merge(gt_lag_train)) 
    test_dates_cur <- index(na.omit(merge(gt_train_cur, covid_national_level, all = F))) #get the common training and forecasting for all lags 
    #temp_date = test_dates_cur[c(which(test_dates_cur=="2020-05-01"):length(test_dates_cur))] 
    temp_date =  as.Date(as.Date("2021-07-01"):as.Date("2022-03-01"))
    #temp_date =  as.Date(as.Date("2021-03-28"):as.Date("2021-06-28"))
    beta_MedRaw_mat = matrix(0,2,length(gt_lag))
    colnames(beta_MedRaw_mat) = paste0("lag-",as.character(gt_lag))
    rownames(beta_MedRaw_mat) = c("Intercept", terms)
    for (i in 1:length(gt_lag)){
      temp_lm_medRaw = lm(covid_national_level$daily_deaths[temp_date]~ gt_train_cur[temp_date,i])
      #temp_lm_medRaw = lm(covid_national_level$new_cases[temp_date]~ gt_train_cur[temp_date,i])
      #temp_lm_medRaw = lm(CovidTracking_national_level$daily_deaths[temp_date]~ gt_train_cur[temp_date,i])
      #temp_lm_medRaw = lm(temp_nat_death[temp_date]~ gt_train_cur[temp_date,i])
      beta_MedRaw_mat[,i] = coef(temp_lm_medRaw)
      resid_MSE_MedRaw[terms,i] = sum(temp_lm_medRaw$residuals^2)
      sandwich_se = diag(sandwich::vcovHAC(temp_lm_medRaw))[2]^0.5
      beta_sandPval_MedRaw[terms,i] <- as.numeric(2* (1 - pt(abs(coef(temp_lm_medRaw)[2]/(diag(sandwich::vcovHAC(temp_lm_medRaw))[2]^0.5)) , 88) ) )
    }
    beta_MedRaw[[terms]] = beta_MedRaw_mat
  }
  MedRaw_optLag = apply(resid_MSE_MedRaw , 1, which.min) #Find the optimal lag if looking at all data (fitted lm over all data)
  MedRaw_optLag_MSE = apply(resid_MSE_MedRaw , 1, min)
  index2D <- function(v = MedRaw_optLag, DF = beta_sandPval_MedRaw){sapply(1:length(v), function(X){DF[X,v[X]]})}
  beta_sandPval_MedRaw_opt = data.frame(index2D())
  rownames(beta_sandPval_MedRaw_opt) = terms_workon
  
  MedRaw_optLag = MedRaw_optLag+3
  # MedRaw_optLag_cases = MedRaw_optLag
  MedRaw_optLag_death = MedRaw_optLag
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Daily Indicator (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
Days_of_Week = matrix(0,dim(gtdata_tempNAT)[1],7)
Days_of_Week = xts(Days_of_Week, index(gtdata_tempNAT))
colnames(Days_of_Week) = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
for (i in 1:7){
  Days_of_Week[which(weekdays(index(Days_of_Week)) == colnames(Days_of_Week)[i]),i] = 1
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use Flu ILI data as one of the predictor
# Run "ILI_Data_Clean.R" Script

source("ILI_COVID_Data_Clean.R")


state_info <- state_region_info

# Work with daily or weekly data for Flu
index_daily = TRUE
if_impute_Flu = FALSE
impute_samples = 1
if_masmooth_impute = FALSE

# If Inputing Daily flu data, assuming Flu stores data on Saturdays: 9/11/2021 is a Saturday, stores 9/4+...+9/10. 
# Thus, 9/4, ..., 9/10 will store the week from 9/4+...9/10 data.
if (index_daily){
  # National
  tmp_ili_nat = as.data.table(ili_national, keep.rownames = TRUE)
  # Convert weekly ILI into daily, filling each day in week same value. i.e. fill 5/15, ..., 5/21 the value of 5/15, which is actually week of 5/9, ..., 5/15.
  tmp_ili_nat <- tmp_ili_nat[,.(date=ymd(seq.Date(from = as.Date(index),to = as.Date(index)+6,
                                  by= "day" )), value=ili_national),by="index"]
  ili_national = xts(tmp_ili_nat$value, as.Date(tmp_ili_nat$date))
  # Convert index to 5/15, ..., 5/21 the value of 5/22, which is actually week of 5/16,...,5/22.
  index(ili_national) = index(ili_national) - 7 
  
  # Regional 
  tmp_ili_region = as.data.table(ili_regional, keep.rownames = TRUE)
  tmp_ili_region <- tmp_ili_region[,.(date=ymd(seq.Date(from = as.Date(index),to = as.Date(index)+6,
                                                        by= "day" ))),by="index"]
  tmp_1 = xts(tmp_ili_region, tmp_ili_region$index)
  tmp_1 = cbind(tmp_1, ili_regional[rep(seq_len(nrow(ili_regional)), each = 7), ])
  ili_regional = xts(as.matrix(tmp_1[,-c(1,2)]), tmp_ili_region$date)
  index(ili_regional) = index(ili_regional) - 7 
  
  # States 
  tmp_ili_state = as.data.table(ili_state, keep.rownames = TRUE)
  tmp_ili_state <- tmp_ili_state[,.(date=ymd(seq.Date(from = as.Date(index),to = as.Date(index)+6,
                                                      by= "day" ))),by="index"]
  tmp_1 = xts(tmp_ili_state, tmp_ili_state$index)
  tmp_1 = cbind(tmp_1, ili_state[rep(seq_len(nrow(ili_state)), each = 7), ])
  ili_state = xts(as.matrix(tmp_1[,-c(1,2)]), tmp_ili_state$date)
  index(ili_state) = index(ili_state) - 7 
  
  names(ili_regional) = gsub("Region.", "Region-",names(ili_regional))
  names(ili_state) = gsub("US.", "US-",names(ili_state))
  
  # Clean COVID Cases Data for Data inputation
  if (if_impute_Flu){
    # Input National
    ili_nat_inputList = list()
    # Replace Flu daily data above in each week, with normalized COVID-19 Cases (sum to wILI of that week)
    tmp_ili_nat$date = tmp_ili_nat$date - 7
    flu_covid_idx_mapping = tmp_ili_nat
    flu_covid_idx_mapping = tmp_ili_nat[date%in%index(covid_national_level), ]
    tmp_common_idx = flu_covid_idx_mapping$date
    tmp_common_idx = tmp_common_idx[-c(1:4)]
    flu_covid_idx_mapping = flu_covid_idx_mapping[-c(1:4), ]
    tmp_flu_nat = ili_national[tmp_common_idx, ]
    for (iter in 1:impute_samples){
      ili_nat_inputList[[iter]] = tmp_flu_nat
      tmp_covid_nat = cbind(flu_covid_idx_mapping, as.matrix(covid_national_level[tmp_common_idx,1]))
      tmp_covid_nat[, week_id:=.GRP,by=list(index)]
      rand_shuffle = rep(sample(1:max(tmp_covid_nat$week_id)), each=7)
      tmp_covid_nat = cbind(tmp_covid_nat, rand_shuffle)
      tmp_covid_nat$new_cases = tmp_covid_nat[order(rand_shuffle), new_cases]
      tmp_covid_nat[, Sum:=sum(new_cases), by=list(index)]
      tmp_covid_nat$new_cases = (tmp_covid_nat$new_cases/tmp_covid_nat$Sum)*tmp_flu_nat
      tmp_covid_nat$new_cases[is.na(tmp_covid_nat$new_cases)] = 0
      tmp_covid_nat[, Sum:=sum(new_cases), by=list(index)]
      tmp = tmp_covid_nat[, new_cases]
      tmp[tmp<0] = 0
      if (if_masmooth_impute){
        tmp = ma(tmp,7)
        tmp[1:3] = 0
        tmp = xts(tmp, tmp_covid_nat$date)
        index(tmp) = index(tmp) + 3
      }
      ili_nat_inputList[[iter]][tmp_common_idx,] = tmp
    }
    names(ili_nat_inputList) = paste0("Impute-Iter", 1:length(ili_nat_inputList))
    ili_nat_inputList = do.call(cbind,ili_nat_inputList)
    
    # Regional Inpute 
    ili_reg_imputList = list()
    # Replace Flu daily data above in each week, with normalized COVID-19 Cases (sum to wILI of that week)
    tmp_ili_region$date = tmp_ili_region$date - 7
    flu_covid_idx_mapping = tmp_ili_region
    flu_covid_idx_mapping = tmp_ili_region[date%in%index(covid_state_level[[1]]), ]
    tmp_common_idx = flu_covid_idx_mapping$date
    tmp_common_idx = tmp_common_idx[-c(1:4)]
    flu_covid_idx_mapping = flu_covid_idx_mapping[-c(1:4), ]
    tmp_flu_reg = ili_regional[tmp_common_idx, ]
    for (iter in 1:impute_samples){
      ili_reg_imputList[[iter]] = tmp_flu_reg
      for (i in 1:dim(ili_regional)[2]){
        tmp_covid_reg_i = cbind(flu_covid_idx_mapping, as.matrix(covid_regional_level[[i]][tmp_common_idx,1]))
        tmp_covid_reg_i[, week_id:=.GRP,by=list(index)]
        rand_shuffle = rep(sample(1:max(tmp_covid_reg_i$week_id)), each=7)
        tmp_covid_reg_i = cbind(tmp_covid_reg_i, rand_shuffle)
        tmp_covid_reg_i$new_cases = tmp_covid_reg_i[order(rand_shuffle), new_cases]
        
        tmp_covid_reg_i[, Sum:=sum(new_cases), by=list(index)]
        tmp_covid_reg_i$new_cases = (tmp_covid_reg_i$new_cases/tmp_covid_reg_i$Sum)*tmp_flu_reg[, i]
        tmp_covid_reg_i$new_cases[is.na(tmp_covid_reg_i$new_cases)] = 0
        tmp_covid_reg_i[, Sum:=sum(new_cases), by=list(index)]
        
        tmp = tmp_covid_reg_i[, new_cases]
        tmp[tmp<0] = 0
        if (if_masmooth_impute){
          tmp = ma(tmp,7)
          tmp[1:3] = 0
          tmp = xts(tmp, tmp_covid_reg_i$date)
          index(tmp) = index(tmp) + 3
        }
        ili_reg_imputList[[iter]][tmp_common_idx, i] = tmp
      }
      colnames(ili_reg_imputList[[iter]]) = colnames(ili_regional)
    }
    
    # State Inpute
    ili_state_imputList = list()
    # Replace Flu daily data above in each week, with normalized COVID-19 Cases (sum to wILI of that week)
    tmp_ili_state$date = tmp_ili_state$date - 7
    flu_covid_idx_mapping = tmp_ili_state
    
    flu_covid_idx_mapping = tmp_ili_state[date%in%index(covid_state_level[[1]]), ]
    tmp_common_idx = flu_covid_idx_mapping$date
    tmp_common_idx = tmp_common_idx[-c(1:4)]
    flu_covid_idx_mapping = flu_covid_idx_mapping[-c(1:4), ]
    joint_states = names(covid_state_level)[names(covid_state_level)%in%names(ili_state)] 
    tmp_flu_state = ili_state[tmp_common_idx, ]
    for (iter in 1:impute_samples){
      ili_state_imputList[[iter]] = tmp_flu_state[,joint_states]
      for (i in joint_states){
        tmp_covid_state_i = cbind(flu_covid_idx_mapping, as.matrix(covid_state_level[[i]][tmp_common_idx,1]))
        
        tmp_covid_state_i[, week_id:=.GRP,by=list(index)]
        rand_shuffle = rep(sample(1:max(tmp_covid_state_i$week_id)), each=7)
        tmp_covid_state_i = cbind(tmp_covid_state_i, rand_shuffle)
        tmp_covid_state_i$new_cases = tmp_covid_state_i[order(rand_shuffle), new_cases]
        
        tmp_covid_state_i[, Sum:=sum(new_cases), by=list(index)]
        tmp_covid_state_i$new_cases = (tmp_covid_state_i$new_cases/tmp_covid_state_i$Sum)*tmp_flu_state[, i]
        tmp_covid_state_i$new_cases[is.na(tmp_covid_state_i$new_cases)] = 0
        tmp_covid_state_i[, Sum:=sum(new_cases), by=list(index)]
        
        tmp = tmp_covid_state_i[, new_cases]
        tmp[tmp<0] = 0
        if (if_masmooth_impute){
          tmp = ma(tmp,7)
          tmp[1:3] = 0
          tmp = xts(tmp, tmp_covid_state_i$date)
          index(tmp) = index(tmp) + 3
        }
        ili_state_imputList[[iter]][tmp_common_idx, i] = tmp
      }
    }
  }
  
  # +1: Now let Flu stores data on Saturdays: 9/11/2021 is a Saturday, stores 9/5+...+9/11. 
  # Thus, 9/5, ..., 9/11 will store the week from 9/5+...+9/11 data.
  # Then +7: let 9/5, ..., 9/11 will store the week from 8/29+...+9/4 data. Since when predicting this week COVID, we only know last week Flu.
  index(ili_national) = index(ili_national) + 1 +7
  index(ili_regional) = index(ili_regional) + 1 +7
  index(ili_state) = index(ili_state) + 1 +7
  if (if_impute_Flu){
    for (i in 1:length(ili_state_imputList)){
      index(ili_state_imputList[[i]]) = index(ili_state_imputList[[i]])+1 +7
      index(ili_reg_imputList[[i]]) = index(ili_reg_imputList[[i]])+1 +7
    }
    index(ili_nat_inputList) = index(ili_nat_inputList)+1 +7
  }
}

########################################################################################################################################################
# ARGO STEP 1!!!!
########################################################################################################################################################
if_use_flu = TRUE
if_use_hosp = TRUE
if_use_imputed_flu = FALSE
if (!if_use_imputed_flu){
  impute_samples = 1
}
if (if_use_hosp){
  hosp<-read.csv("~/truth-Incident Hospitalizations.csv")
  national_hosp<-hosp[hosp$location_name=="United States",]
  national_hosp<-xts(national_hosp$value,order.by = as.Date(national_hosp$date))
  colnames(national_hosp)<-"US_incidient_hosp"
}

n_train <- 56  #use previous 90 days to predict today
n_train_minimum <- 42  #use at least previous 40 days to predict today
list_results_step1.Nat <- list()
coef_all_step1.Nat <- list()
coef_smooth_step1.Nat <- list()
# daily training weekly prediction -----------
ts_max_lag <- 1:7  #lag a week
case_lag <- c(21,28,35) #a week and two weeks
hosp_lag = c(1, 7)
flu_lag <- c(7,14)
mobility_lag <- c(0, 7, 14, 21,28,35)
total_forward = 28  #predicting how many days forward
decay <- 0.8
pred_week = 1:7
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# National Level Step 1
# end of day prediction for the future (stand on today want to predict upto two weeks ahead, using data before today, lagged)
RawNAT_32terms = gtdata_tempNAT[,terms_important]
gt_nat <- RawNAT_32terms
gt_lag <- MedRaw_optLag_death[terms_important] #The ALL data optimal Lag to Cases/Death (change here if switching Cases to Death)!!!
pred_target = covid_national_level$daily_deaths #prediction target

list_results_step1.Nat_impute = list()
for (impute_iter in 1:impute_samples){
  print(paste0("Impute Iteration ", impute_iter))
  for(n_forward in 1:total_forward){ 
    print(paste0("n_forward=", n_forward))
    x_ar <- stats::lag(pred_target, ts_max_lag + n_forward - 1) #lag new death from one day to one week (matrix form) loop starting today upto 2 weeks (not using any "new data" as of today, since ts_max_lag+n_forward-1 not ts_max_lag)
    if (!if_use_hosp){
      x_cases <- stats::lag(covid_national_level$new_cases, pmax(case_lag, n_forward)) #(always make sure not use future data). If today is Monday, predict next Tuesday, cannot use tomorrow's data, thus when n_forward>7or14 have to replace 7,14 with n_forward, only have data before and today
    } else{
      x_cases = stats::lag(national_hosp,  pmax(hosp_lag, n_forward))
    }
    x_gt <- lapply(terms_important,function(i){stats::lag(gt_nat[,i],pmax(gt_lag[i],n_forward) )})
    x_gt = do.call(cbind,x_gt)
    x_curr <- na.omit(merge(x_ar, x_cases, x_gt , Days_of_Week)) 
    common_id_curr <- index(na.omit(merge(x_curr, covid_national_level,Days_of_Week, all = F)))  #get the lagged data's date, the dates that we will train and test for (dont have NA)
    
    if (if_use_flu){
      if (!if_use_imputed_flu){
        x_flu = stats::lag(ili_national,  flu_lag+n_forward) # Use Latest Flu info to predict. When predicting 9/16, standing on 9/14 (n_forward=2), use 9/14, 9/7, 8/31, 8/24 Flu info, since they each store last week's Flu ILI.
        x_curr <- na.omit(merge(x_curr, x_flu))
        common_id_curr <- index(na.omit(merge(x_curr, covid_national_level,Days_of_Week, ili_national, all = F)))
      } else{
        x_flu = stats::lag(ili_nat_inputList[,impute_iter],  flu_lag+n_forward)
        x_curr <- na.omit(merge(x_curr, x_flu))
        common_id_curr <- index(na.omit(merge(x_curr, covid_national_level,Days_of_Week, ili_nat_inputList, all = F)))
      }
    }
    
    pred <-  pred_target[common_id_curr]  #predict the dates that have lagged data (omit first two weeks of data)
    pred[] <- NA
    pred_smooth <-pred_target[common_id_curr]  
    pred_smooth[] <- NA
    coef <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
    coef <- xts(coef, order.by = common_id_curr)
    colnames(coef) <- c("intercept",colnames(x_curr))
    coef_smooth <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
    coef_smooth <- xts(coef_smooth, order.by = common_id_curr)
    colnames(coef_smooth) <- c("intercept",colnames(x_curr))
    
    x_gt_new <- lapply(terms_important,function(i){stats::lag(gt_nat[,i],pmax(gt_lag[i]-n_forward,0) )})
    x_gt_new = do.call(cbind,x_gt_new)
    if (!if_use_hosp){
      x_new <- cbind( #started since 2019-01-01 sice word freq started then
        stats::lag(pred_target, ts_max_lag - 1),  # latest available ar lags daily deaths (today, yesterday,...,6 days ago)
        stats::lag(covid_national_level$new_cases, pmax(case_lag - n_forward, 0)), # confirmed cases a week ago new cases (a week ago today and two weeks ago today)
        x_gt_new,
        stats::lag(Days_of_Week,(7-n_forward%%7))
      )  # latest available gt
    } else{
      x_new <- cbind( #started since 2019-01-01 sice word freq started then
        stats::lag(pred_target, ts_max_lag - 1),  # latest available ar lags daily deaths (today, yesterday,...,6 days ago)
        stats::lag(national_hosp, pmax(hosp_lag - n_forward, 0)),
        x_gt_new,
        stats::lag(Days_of_Week,(7-n_forward%%7))
      )  # latest available gt
    }
    
    if (if_use_flu){
      if (!if_use_imputed_flu){
        x_new = cbind(x_new, stats::lag(ili_national, flu_lag)) # Use Latest Flu info to predict. When predicting 9/16, standing on 9/14 (n_forward=2), use 9/14, 9/7, 8/31, 8/24 Flu info, since they each store last week's Flu ILI.
      } else{
        x_new = cbind(x_new, stats::lag(ili_nat_inputList[,impute_iter], flu_lag))
      }
    }
    
    for(date_cur in common_id_curr){
      train_id <- as.Date(date_cur) - ((n_train-1):0) #set training set, use up to 90 days of data to train parameters, when predicting each day use lagged death, cases, wordfreq and mobility
      train_id <- train_id[train_id >= common_id_curr[1]] #keep only dates past the first day of lagged data, 2020-02-04 (since before that no explanatory variables)
      if(length(train_id) >= n_train_minimum){ 
        if (length(which(as.matrix(pred_target[train_id])==0))>(n_train_minimum-5)){ #if too many zeros then perdict death is 0
          coef[as.Date(date_cur),] = 0 
          pred[as.Date(date_cur)] = pred_target[as.Date(date_cur)]
        }else{ #start training only when having at least n_train_minimum days of training data to tain (not considering lagged dates)
          train_weights <- (decay)^(length(train_id):1)
          set.seed(100)
          lasso.fit <- glmnet::cv.glmnet(x = as.matrix(x_curr[train_id]), 
                                         y = as.matrix(pred_target[train_id]), nfolds = 10, grouped = FALSE,  #10 fold, alpha=1->lasso
                                         alpha = 1, weights = train_weights)
          lam.s <- lasso.fit$lambda.1se #select the lambda that gives the simpliest model within one standard error of optimal val of lambda
          coef[as.Date(date_cur),] <- as.numeric(coef(lasso.fit, lambda = lam.s)) #store the coef for the simpliest lambda in the training date's row (if common_id_curr[40] then stored in row 2020-02-04+39=2020-03-14) for all explanatory variables 
          pred[as.Date(date_cur)] <- predict(lasso.fit, newx = as.matrix(x_new[as.Date(date_cur)]), s = lam.s) #store prediction for that date ???predict tomorrow???
        }
        if (as.numeric(pred[as.Date(date_cur)]) < 0){
          pred[as.Date(date_cur)] = 0
        }
        #if (!is.na(as.numeric(coef[as.Date(date_cur),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-1),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-2),])) ){
        #if (!is.na(as.numeric(coef[as.Date(date_cur),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-1),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-2),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-3),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-4),])) ){
        if (!is.na(as.numeric(pred[as.Date(date_cur)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-1)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-2)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-3)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-4)])) ){
          # temp_coef =  (as.numeric(coef[as.Date(date_cur),])+  as.numeric(coef[(as.Date(date_cur)-1),])+  as.numeric(coef[(as.Date(date_cur)-2),]) )/3
          #temp_coef =  (as.numeric(coef[as.Date(date_cur),])+  as.numeric(coef[(as.Date(date_cur)-1),])+  as.numeric(coef[(as.Date(date_cur)-2),]) +  as.numeric(coef[(as.Date(date_cur)-3),]) +  as.numeric(coef[(as.Date(date_cur)-4),]) )/5
          #coef_smooth[as.Date(date_cur),] = temp_coef
          #pred_smooth[as.Date(date_cur)] = sum(temp_coef * c(1,as.numeric(x_new[as.Date(date_cur)])))
          pred_smooth[as.Date(date_cur)] = (as.numeric(pred[as.Date(date_cur)]) + as.numeric(pred[as.Date(date_cur-1)]) + as.numeric(pred[as.Date(date_cur-2)]) + as.numeric(pred[as.Date(date_cur-3)]) + as.numeric(pred[as.Date(date_cur-4)]))/5
          if (as.numeric(pred_smooth[as.Date(date_cur)]) < 0){
            pred_smooth[as.Date(date_cur)] = 0
          }
        }
      }
    }
    ##
    truth <- stats::lag(pred_target, -n_forward) #lag backwards (yesterday has today's data), truth is today predicting tomorrow and upto two weeks head (save to today's row now)
    naive <- pred_target #naive approach simply using the death count today (newest) to predict future (whether it is tomorrow or next week)
    naive_period <- stats::lag(pred_target,(7-n_forward%%7) ) #second naive approach simply using the death count 7 days ago to predict today. 
    set.seed(100)
    argo_pre <- round((pred))
    argo_presmooth <- round((pred_smooth))
    results <- merge(truth, naive, naive_period, argo_pre, argo_presmooth, all = F)
    colnames(results) <- c("truth", "naive", "naive_period", "argo", "argo_smooth")
    
    list_results_step1.Nat[[n_forward]] <- results  
    coef_all_step1.Nat[[n_forward]] <- coef
    coef_smooth_step1.Nat[[n_forward]] <- coef_smooth
  }
  list_results_step1.Nat_impute[[impute_iter]] = list_results_step1.Nat
}
if (if_use_flu){
  list_results_step1.Nat_08142022_withFlu = list()
  for (i in 1:4){
    pred_week = ((i-1)*7+1):(i*7)
    week_ahead = paste0(i, " week ahead")
    list_results_step1.Nat_08142022_withFlu[[week_ahead]] = lapply(pred_week, function(x) list_results_step1.Nat[[x]])
    list_results_step1.Nat_08142022_withFlu[[week_ahead]] = Reduce("+", list_results_step1.Nat_08142022_withFlu[[week_ahead]]) 
  }
} else{
  list_results_step1.Nat_08142022 = list()
  for (i in 1:4){
    pred_week = ((i-1)*7+1):(i*7)
    week_ahead = paste0(i, " week ahead")
    list_results_step1.Nat_08142022[[week_ahead]] = lapply(pred_week, function(x) list_results_step1.Nat[[x]])
    list_results_step1.Nat_08142022[[week_ahead]] = Reduce("+", list_results_step1.Nat_08142022[[week_ahead]]) 
  }
}


#------------------------------------------------------------------------------------------------------------------------------------------------------------
# Regional Level Step 1
if_use_flu = FALSE
if_use_imputed_flu = FALSE
ts_max_lag <- 1:7  #lag a week
gt_lag <- c(7, 14) #a week and two weeks   
mobility_lag <- c(14,21,28)
case_lag <- c(21,28,35) #a week and two weeks
total_forward = 28  #predicting how many days forward
decay <- 0.8

n_train <- 56  
n_train_minimum <- 42 
list_results_step1.Reg <- list()
list_results_step1.Reg_Impute = list()
coef_all_step1.Reg <- list()
coef_smooth_step1.Reg <- list()
for (impute_iter in 1:impute_samples){
  print(paste0("Impute Iteration ", impute_iter))
  for (region in names(gtdata_Region_IQR_noMedia)){
    print(region)
    pred_target = covid_regional_level[[region]]$daily_deaths
    ratio_term_Region = which(names(gtdata_Region_IQR_noMedia[[region]])=="ratio")
    temp_terms = names(gtdata_Region_IQR_noMedia[[region]])[names(gtdata_Region_IQR_noMedia[[region]])%in% terms_important]
    gt_nat <- gtdata_Region_IQR_noMedia[[region]] [,temp_terms]
    gt_lag <- MedRaw_optLag_death[temp_terms]

    # end of day prediction for the future (stand on today want to predict upto two weeks ahead, using data before today, lagged)
    for(n_forward in 1:total_forward){ 
      print(paste0("n_forward=", n_forward))
      x_ar <- stats::lag(pred_target, ts_max_lag + n_forward - 1)
      x_cases <- stats::lag(covid_regional_level[[region]]$new_cases, pmax(case_lag, n_forward))
      x_gt <- lapply(temp_terms,function(i){stats::lag(gt_nat[,i],pmax(gt_lag[i],n_forward) )})
      x_gt = do.call(cbind,x_gt)
      x_curr <- na.omit(merge(x_ar, x_cases, x_gt ,  Days_of_Week)) 
      common_id_curr <- index(na.omit(merge(x_curr, pred_target, all = F)))  #get the lagged data's date, the dates that we will train and test for (dont have NA)
      
      if (if_use_flu && !if_use_imputed_flu){
        x_flu = stats::lag(ili_regional[,region],  flu_lag+n_forward) # Use Latest Flu info to predict. When predicting 9/16, standing on 9/14 (n_forward=2), use 9/14, 9/7, 8/31, 8/24 Flu info, since they each store last week's Flu ILI.
        x_curr <- na.omit(merge(x_curr, x_flu))
        common_id_curr <- index(na.omit(merge(x_curr, pred_target,Days_of_Week, ili_regional, all = F)))
      } else if(if_use_flu && if_use_imputed_flu){
        x_flu = stats::lag(ili_reg_imputList[[impute_iter]][,region],  flu_lag+n_forward)
        x_curr <- na.omit(merge(x_curr, x_flu))
        common_id_curr <- index(na.omit(merge(x_curr, pred_target,Days_of_Week, ili_reg_imputList[[impute_iter]], all = F)))
      }
      
      pred <-  pred_target[common_id_curr] #predict the dates that have lagged data (omit first two weeks of data)
      pred[] <- NA
      pred_smooth <-pred_target[common_id_curr]  
      pred_smooth[] <- NA
      coef <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
      coef <- xts(coef, order.by = common_id_curr)
      colnames(coef) <- c("intercept",colnames(x_curr))
      coef_smooth <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
      coef_smooth <- xts(coef_smooth, order.by = common_id_curr)
      colnames(coef_smooth) <- c("intercept",colnames(x_curr))
      
      x_gt_new <- lapply(temp_terms,function(i){stats::lag(gt_nat[,i],pmax(gt_lag[i]-n_forward,0) )})
      x_gt_new = do.call(cbind,x_gt_new)
      x_new <- cbind(
        stats::lag(pred_target, ts_max_lag - 1),  
        stats::lag(covid_regional_level[[region]]$new_cases, pmax(case_lag - n_forward, 0)), 
        x_gt_new,
        stats::lag(Days_of_Week,(7-n_forward%%7))
      )  
      
      if (if_use_flu && !if_use_imputed_flu){
        x_new = cbind(x_new, stats::lag(ili_regional[,region], flu_lag)) # Use Latest Flu info to predict. When predicting 9/16, standing on 9/14 (n_forward=2), use 9/14, 9/7, 8/31, 8/24 Flu info, since they each store last week's Flu ILI.
      } else if (if_use_flu && if_use_imputed_flu){
        x_new = cbind(x_new, stats::lag(ili_reg_imputList[[impute_iter]][,region], flu_lag))
      }
      
      for(date_cur in common_id_curr){
        train_id <- as.Date(date_cur) - ((n_train-1):0) #set training set, use up to 90 days of data to train parameters, when predicting each day use lagged death, cases, wordfreq and mobility
        train_id <- train_id[train_id >= common_id_curr[1]] #keep only dates past the first day of lagged data, 2020-02-04 (since before that no explanatory variables)
        if(length(train_id) >= n_train_minimum){ 
          if (length(which(as.matrix(pred_target[train_id])==0))>(n_train_minimum-5)){ #if too many zeros then perdict death is 0
            coef[as.Date(date_cur),] = 0 
            pred[as.Date(date_cur)] =pred_target[as.Date(date_cur)]
          }else{ #start training only when having at least n_train_minimum days of training data to tain (not considering lagged dates)
            train_weights <- (decay)^(length(train_id):1)
            set.seed(100)
            lasso.fit <- glmnet::cv.glmnet(x = as.matrix(x_curr[train_id]), 
                                           y = as.matrix(pred_target[train_id]), nfolds = 10, grouped = FALSE,  #10 fold, alpha=1->lasso
                                           alpha = 1, weights = train_weights)
            lam.s <- lasso.fit$lambda.1se #select the lambda that gives the simpliest model within one standard error of optimal val of lambda
            coef[as.Date(date_cur),] <- as.numeric(coef(lasso.fit, lambda = lam.s)) #store the coef for the simpliest lambda in the training date's row (if common_id_curr[40] then stored in row 2020-02-04+39=2020-03-14) for all explanatory variables 
            pred[as.Date(date_cur)] <- predict(lasso.fit, newx = as.matrix(x_new[as.Date(date_cur)]), s = lam.s) #store prediction for that date ???predict tomorrow???
          }
          #if (!is.na(as.numeric(coef[as.Date(date_cur),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-1),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-2),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-3),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-4),])) ){
          #temp_coef =  (as.numeric(coef[as.Date(date_cur),])+  as.numeric(coef[(as.Date(date_cur)-1),])+  as.numeric(coef[(as.Date(date_cur)-2),]) + as.numeric(coef[(as.Date(date_cur)-3),]) + as.numeric(coef[(as.Date(date_cur)-4),]) )/5
          #coef_smooth[as.Date(date_cur),] = temp_coef
          #pred_smooth[as.Date(date_cur)] = sum(temp_coef * c(1,as.numeric(x_new[as.Date(date_cur)])))
          #}
          if (!is.na(as.numeric(pred[as.Date(date_cur)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-1)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-2)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-3)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-4)])) ){
            pred_smooth[as.Date(date_cur)] = (as.numeric(pred[as.Date(date_cur)]) + as.numeric(pred[as.Date(date_cur-1)]) + as.numeric(pred[as.Date(date_cur-2)]) + as.numeric(pred[as.Date(date_cur-3)]) + as.numeric(pred[as.Date(date_cur-4)]))/5
          }
        }
      }
      ##
      truth <- stats::lag(pred_target, -n_forward) #lag backwards (yesterday has today's data), truth is today predicting tomorrow and upto two weeks head (save to today's row now)
      naive <- pred_target#naive approach simply using the death count today (newest) to predict future (whether it is tomorrow or next week)
      naive_period <- stats::lag(pred_target , (7-n_forward%%7) ) #second naive approach simply using the death count 7 days ago to predict today. 
      set.seed(100)
      argo_pre <- round((pred))
      argo_presmooth <- round((pred_smooth))
      results <- merge(truth, naive, naive_period, argo_pre, argo_presmooth, all = F)
      colnames(results) <- c("truth", "naive", "naive_period", "argo", "argo_smooth")
      
      list_results_step1.Reg[[region]][[n_forward]] <- results  
      coef_all_step1.Reg[[region]][[n_forward]] <- coef
      coef_smooth_step1.Reg[[region]][[n_forward]] <- coef_smooth
    }
  }
  list_results_step1.Reg_Impute[[as.character(impute_iter)]] = list_results_step1.Reg
}

list_results_step1.Reg_08142022 = list()
for (i in 1:4){
  pred_week = ((i-1)*7+1):(i*7)
  week_ahead = paste0(i, " week ahead")
  for(i in names(list_results_step1.Reg)){  
    results_week_state = list_results_step1.Reg[[i]]
    results_week.s <- lapply(pred_week, function(x) results_week_state[[x]])  
    results_week.s <- Reduce("+", results_week.s)  #plus the week ahead together, aggregated lists returing matrix
    results_week.s <- na.omit(results_week.s)
    results_week.s[which(results_week.s$argo_smooth<0), 'argo_smooth'] = 0
    #list_results_step1.Reg[[i]] = results_week.s
    list_results_step1.Reg_08142022[[week_ahead]][[i]] = results_week.s
  }
}
  

###########################################################################################################################################################
# 51 STATES 
if_use_flu = FALSE
if_use_imputed_flu = FALSE
n_train <- 56 
n_train_minimum <- 40
list_results_step1.State <- list()
list_results_step1.State_Impute = list()
coef_all_step1.State <- list()
coef_smooth_step1.State <- list()
ts_max_lag <- 1:7  #lag a week
case_lag <- c(21,28,35) #a week and two weeks
#gt_lag = case_lag
mobility_lag <- c(21,28,35)
total_forward = 28  #predicting how many days forward
decay <- 0.8
pred_week = 1:7

for (impute_iter in 1:impute_samples){
  print(paste0("Impute Iteration ", impute_iter))
  for (region in state_names){
    print(region)
    pred_target = covid_state_level[[region]]$daily_deaths
    ratio_term_State = which(names(gtdata_stateMixed_IQR_noMedia[[region]])=="ratio")
    temp_terms = names(gtdata_stateMixed_IQR_noMedia[[region]])[names(gtdata_stateMixed_IQR_noMedia[[region]])%in% terms_important]
    gt_nat <- gtdata_stateMixed_IQR_noMedia[[region]][,temp_terms]
    gt_lag <- MedRaw_optLag_death[temp_terms]
    
    for(n_forward in 1:total_forward){
      print(paste0("n_forward=", n_forward))
      x_ar <- stats::lag(pred_target, ts_max_lag + n_forward - 1)
      x_cases <- stats::lag(covid_state_level[[region]]$new_cases, pmax(case_lag, n_forward))
      x_gt <- lapply(temp_terms,function(i){stats::lag(gt_nat[,i],pmax(gt_lag[i],n_forward) )}) 
      x_gt = do.call(cbind,x_gt)
      x_curr <- na.omit(merge(x_ar, x_cases, x_gt ,  Days_of_Week)) 
      common_id_curr <- index(na.omit(merge(x_curr, pred_target, all = F)))  #get the lagged data's date, the dates that we will train and test for (dont have NA)
      
      if(if_use_flu){
        if (region %in% colnames(ili_state)){
          if (!if_use_imputed_flu){
            x_flu = stats::lag(ili_state[,region],  flu_lag+n_forward) # Use Latest Flu info to predict. When predicting 9/16, standing on 9/14 (n_forward=2), use 9/14, 9/7, 8/31, 8/24 Flu info, since they each store last week's Flu ILI.
            x_curr <- na.omit(merge(x_curr, x_flu))
            common_id_curr <- index(na.omit(merge(x_curr, pred_target,Days_of_Week, ili_state, all = F)))
          } else{
            x_flu = stats::lag(ili_state_imputList[[impute_iter]][,region],  flu_lag+n_forward)
            x_curr <- na.omit(merge(x_curr, x_flu))
            common_id_curr <- index(na.omit(merge(x_curr, pred_target,Days_of_Week, ili_state_imputList[[impute_iter]], all = F)))
          }
        }
      }
      
      pred <-  pred_target[common_id_curr] #predict the dates that have lagged data (omit first two weeks of data)
      pred[] <- NA
      pred_smooth <-pred_target[common_id_curr]  
      pred_smooth[] <- NA
      coef <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
      coef <- xts(coef, order.by = common_id_curr)
      colnames(coef) <- c("intercept",colnames(x_curr))
      coef_smooth <- matrix(nrow=length(common_id_curr), ncol = ncol(x_curr) + 1)
      coef_smooth <- xts(coef_smooth, order.by = common_id_curr)
      colnames(coef_smooth) <- c("intercept",colnames(x_curr))
      
      x_gt_new <- lapply(temp_terms,function(i){stats::lag(gt_nat[,i],pmax(gt_lag[i]-n_forward,0) )})
      x_gt_new = do.call(cbind,x_gt_new)
      x_new <- cbind(
        stats::lag(pred_target, ts_max_lag - 1),  
        stats::lag(covid_state_level[[region]]$new_cases, pmax(case_lag - n_forward, 0)), 
        x_gt_new,
        stats::lag(Days_of_Week,(7-n_forward%%7))
      )  
      
      if(if_use_flu){
        if (region %in% colnames(ili_state)){
          if (!if_use_imputed_flu){
            x_new = cbind(x_new, stats::lag(ili_state[,region], flu_lag)) # Use Latest Flu info to predict. When predicting 9/16, standing on 9/14 (n_forward=2), use 9/14, 9/7, 8/31, 8/24 Flu info, since they each store last week's Flu ILI.
          } else{
            x_new = cbind(x_new, stats::lag(ili_state_imputList[[impute_iter]][,region], flu_lag))
          }
        }
      }
      
      for(date_cur in common_id_curr){
        train_id <- as.Date(date_cur) - ((n_train-1):0) #set training set, use up to 90 days of data to train parameters, when predicting each day use lagged death, cases, wordfreq and mobility
        train_id <- train_id[train_id >= common_id_curr[1]] #keep only dates past the first day of lagged data, 2020-02-04 (since before that no explanatory variables)
        if(length(train_id) >= n_train_minimum){ 
          if (length(which(as.matrix(pred_target[train_id])==0))>(n_train_minimum-5)){ #if too many zeros then perdict death is 0
            coef[as.Date(date_cur),] = 0 
            pred[as.Date(date_cur)] =pred_target[as.Date(date_cur)]
          }else{ #start training only when having at least n_train_minimum days of training data to tain (not considering lagged dates)
            train_weights <- (decay)^(length(train_id):1)
            set.seed(100)
            lasso.fit <- glmnet::cv.glmnet(x = as.matrix(x_curr[train_id]), 
                                           y = as.matrix(pred_target[train_id]), nfolds = 10, grouped = FALSE,  #10 fold, alpha=1->lasso
                                           alpha = 1, weights = train_weights)
            lam.s <- lasso.fit$lambda.1se #select the lambda that gives the simpliest model within one standard error of optimal val of lambda
            coef[as.Date(date_cur),] <- as.numeric(coef(lasso.fit, lambda = lam.s)) #store the coef for the simpliest lambda in the training date's row (if common_id_curr[40] then stored in row 2020-02-04+39=2020-03-14) for all explanatory variables 
            pred[as.Date(date_cur)] <- predict(lasso.fit, newx = as.matrix(x_new[as.Date(date_cur)]), s = lam.s) #store prediction for that date ???predict tomorrow???
          }
          #if (!is.na(as.numeric(coef[as.Date(date_cur),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-1),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-2),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-3),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-4),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-5),])) && !is.na(as.numeric(coef[(as.Date(date_cur)-6),])) ){
          #temp_coef =  (as.numeric(coef[as.Date(date_cur),])+  as.numeric(coef[(as.Date(date_cur)-1),])+  as.numeric(coef[(as.Date(date_cur)-2),]) + as.numeric(coef[(as.Date(date_cur)-3),]) + as.numeric(coef[(as.Date(date_cur)-4),]) + as.numeric(coef[(as.Date(date_cur)-5),]) + as.numeric(coef[(as.Date(date_cur)-6),]) )/7
          #coef_smooth[as.Date(date_cur),] = temp_coef
          #pred_smooth[as.Date(date_cur)] = sum(temp_coef * c(1,as.numeric(x_new[as.Date(date_cur)])))
          #}
          if (!is.na(as.numeric(pred[as.Date(date_cur)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-1)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-2)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-3)])) && !is.na(as.numeric(pred[(as.Date(date_cur)-4)])) ){
            pred_smooth[as.Date(date_cur)] = (as.numeric(pred[as.Date(date_cur)]) + as.numeric(pred[as.Date(date_cur-1)]) + as.numeric(pred[as.Date(date_cur-2)]) + as.numeric(pred[as.Date(date_cur-3)]) + as.numeric(pred[as.Date(date_cur-4)]))/5
          }
        }
      }
      ##
      truth <- stats::lag(pred_target, -n_forward) #lag backwards (yesterday has today's data), truth is today predicting tomorrow and upto two weeks head (save to today's row now)
      naive <- pred_target #naive approach simply using the death count today (newest) to predict future (whether it is tomorrow or next week)
      naive_period <- stats::lag(pred_target, (7-n_forward%%7)) #second naive approach simply using the death count 7 days ago to predict today. 
      set.seed(100)
      argo_pre <- round((pred))
      argo_presmooth <- round((pred_smooth))
      results <- merge(truth, naive, naive_period, argo_pre, argo_presmooth, all = F)
      colnames(results) <- c("truth", "naive", "naive_period", "argo", "argo_smooth")
      list_results_step1.State[[region]][[n_forward]] <- results  
      coef_all_step1.State[[region]][[n_forward]] <- coef
      coef_smooth_step1.State[[region]][[n_forward]] <- coef_smooth
    }
  }
  list_results_step1.State_Impute[[impute_iter]] = list_results_step1.State
}


list_results_step1.State_08142022 = list()
for (i in 1:4){
  pred_week = ((i-1)*7+1):(i*7)
  week_ahead = paste0(i, " week ahead")
  for(i in names(list_results_step1.State)){  
    results_week_state = list_results_step1.State[[i]]
    results_week.s <- lapply(pred_week, function(x) results_week_state[[x]])  
    results_week.s <- Reduce("+", results_week.s)  #plus the week ahead together, aggregated lists returing matrix
    results_week.s <- na.omit(results_week.s)
    results_week.s[which(results_week.s$argo_smooth<0), 'argo_smooth'] = 0
    #list_results_step1.State[[i]] = results_week.s
    list_results_step1.State_08142022[[week_ahead]][[i]] = results_week.s
  }
}


###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################
weeks_forward = 1
list_results_step1.Reg = list_results_step1.Reg_08142022[[weeks_forward]]
list_results_step1.State = list_results_step1.State_08142022[[weeks_forward]]
list_results_step1.Nat = list_results_step1.Nat_08142022[[weeks_forward]]
temp_region = temp_region_08142022[[weeks_forward]]
temp_state = temp_state_08142022[[weeks_forward]]
state_names = names(list_results_step1.State)

argo.reg.p = {}
for (i in names(list_results_step1.Reg)){
  if (temp_region[5,i]<=temp_region[4,i]){
    argo.reg.p = cbind(argo.reg.p, list_results_step1.Reg[[i]][,5])
    print(paste0(i,"-ARGOSmooth"))
  }
  else{
    argo.reg.p = cbind(argo.reg.p, list_results_step1.Reg[[i]][,4])
    print(paste0(i,"-ARGO"))
  }
}
colnames(argo.reg.p) <- names(list_results_step1.Reg) #set column names to "Region-x"
index(argo.reg.p) <- as.Date(index(argo.reg.p)) #convert to daily index (apply.weekly will change index to seconds UTC, weird)

argo.state.p = {}
for (i in names(list_results_step1.State)){
  if (temp_state[5,i]<=temp_state[4,i]){
    argo.state.p = cbind(argo.state.p, list_results_step1.State[[i]][,5])
    print(paste0(i,"-ARGOSmooth"))
  }
  else{
    argo.state.p = cbind(argo.state.p, list_results_step1.State[[i]][,4])
    print(paste0(i,"-ARGO"))
  }
}
colnames(argo.state.p) <- names(list_results_step1.State)
index(argo.state.p) <- as.Date(index(argo.state.p))

# Note that on 3/8, truth is total death for 3/9+3/10+...+3/15. Naive later is on 3/8, stored 3/2+3/3+...+3/8. True diff is on 3/8, stored this week-last week. Lagged 1 week diff is on 3/8, stored last week - lastlast week
# Thus, predicted stored on 3/8 is the forecast for 3/9+...+3/15
argo.state.true <- lapply(list_results_step1.State, function(x) x[,"truth"]) #state level true Covid-19 weekly death
argo.state.true <- do.call(merge, argo.state.true)
colnames(argo.state.true) <- names(list_results_step1.State)
index(argo.state.true) <- as.Date(index(argo.state.true))

argo.nat.p <- list_results_step1.Nat$argo_smooth #national level predicted Covid-19 weekly death
index(argo.nat.p) <- as.Date(index(argo.nat.p))


#########################################################################################################################
# ARGOX Joint States
HI_VT = c("US-HI", "US-VT")
joint_states = names(argo.state.p)
joint_states = joint_states[!joint_states%in%HI_VT]

LAG_PREDICT = weeks_forward-1 #n_forward weeks
truth_JOINT = argo.state.true[,joint_states]
use_yt2=TRUE

argo.state.p_JOINT = argo.state.p[,joint_states]
state_names_JOINT = joint_states

training_period = 70  #COVID-19 use 70 days, 10 weeks training period
Ensumble_training_period = 120

naive.p <- truth_JOINT  
index(naive.p) <- index(truth_JOINT ) + 7*(1+LAG_PREDICT) #construct naive.p which simply uses this week's %ILI to predict next week's (7 days forward or n_forward=7*1, can change in Covid where basis is 1:1day instead of 7 days:1week)

#Note that Y is a matrix where each columns contain true %ILI for each state
Y <- truth_JOINT  - naive.p #obtain Z_t, the difference of p_t-p_{t-1}, here one difference in time steps is a week
Yt2 <- Y 
index(Yt2) <- index(Yt2) + 7*(1+LAG_PREDICT) #Yt2 is like Z_{t}=p_{t-1}-p_{t-2}, which is same as Z_{t-1}=p_{t-1}-p_{t-2}. i.e. at time t, Y[t] is Z_t while Yt2 is Z_{t-1}. Used for correlation calculation
common_idx <- index(na.omit(merge(naive.p, argo.state.p_JOINT , argo.nat.p, Yt2))) 

argo.nat.p <- argo.nat.p[common_idx]
argo.reg.p <- argo.reg.p[common_idx]
naive.p <- naive.p[common_idx]
truth_JOINT  <- truth_JOINT [common_idx]
argo.state.p_JOINT  <- argo.state.p_JOINT [common_idx]
Yt2 <- Yt2[common_idx]

X <- argo.state.p_JOINT  - naive.p #obtain p^{GT}_t-p_{t-1}, state increment prediction for t
X.nat <- as.numeric(argo.nat.p) - naive.p #obtain p^{nat}_t-p_{t-1}, national increment prediction for t. 
X.nat <- X.nat[common_idx]#Note that everything here are same dimensions now, all having 51 columns (number of states), for region its the same value for all states in that region then minus truth_JOINT  (state level), which is stable 
argo.reg.p.dup <- lapply(state_names_JOINT, function(each_state){
  region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
  argo.reg.p[,region_id_for_state]
})
argo.reg.p.dup <- do.call(merge, argo.reg.p.dup)
names(argo.reg.p.dup) <- state_names_JOINT #Here, insteaad of having 10 columns in region p, we bump to 51 where simply duplicate the regions p for all states in that region. STORE IN argo.reg.p.dup
X.reg <- argo.reg.p.dup - naive.p #obtain p^{reg}_t-p_{t-1}, here each column is the region that state is in's p (51 columns instead of 10), regional increment prediction for t

projection.mat <- list()
mean.mat <- list()

Y.pred <- X
Y.pred[] <- NA
Y.CI <- X
Y.CI[] <- NA

zw_used <- list()
fitting_error <- list()
sigma_ww.structured <- sigma_ww.empirical <-
  sigma_zw.structured <- sigma_zw.empirical <-
  heat.vec.structured <-
  sigma_zwzw.structured <- sigma_zwzw.empirical <- list() #construct lists for variance matrix elements

for(it in (training_period+7*(1+LAG_PREDICT)):length(common_idx) ){ #Note n_training in ARGO is 104
  training_idx <- common_idx[(it-training_period-7*(1+LAG_PREDICT)+1):(it-7*(1+LAG_PREDICT))] #get training dates/period (total of fixed 104 days), same idea as first step (have a training period for prediction at t, which is why a "-1'at the end of range)
  t.now <- common_idx[it] #The time step t that we predict (testing upon training)
  y <- Y[training_idx,]  #get Z_t=p_t-p_{t-1}
  x <- X[training_idx,]  #get p^{GT}_t-p_{t-1}, state_level
  x.nat <- X.nat[training_idx,] #get p^{nat}_t-p_{t-1}
  x.reg <- X.reg[training_idx,] #get p^{reg}_t-p_{t-1}
  yt2 <- Yt2[training_idx,] #get Z_{t-1}
  
  sigma_yy <- var(y) #compute sigma_zz
  
  m1 <- cor(y, yt2) #??First, we are assuming each t (rows) follow a multivariate distribution, but here is computing according to column?
  m2 <- cor(y)  #Also, This way of cumpting ACF for one lag doesn't seem to be correct? Isn't it just computing sample correlation and scale by variance?
  rho.l2 <- sum(m1*m2)/sum(m2^2) #rho, where cor(Z_t,Z_{t-1})=rho*sigma_zz????  
  
  autocov.y.yt2 <- rho.l2*sigma_yy #get \rho*sigma_ZZ, one step ACF
  
  vcov.x_xreg_xnat <- cbind(rbind(sigma_yy+var((argo.state.p_JOINT  - truth_JOINT )[training_idx,]),sigma_yy, sigma_yy),
                            rbind(sigma_yy,sigma_yy+var((argo.reg.p.dup - truth_JOINT )[training_idx,]), sigma_yy),
                            rbind(sigma_yy,sigma_yy,sigma_yy+var((as.numeric(argo.nat.p) - truth_JOINT )[training_idx,]) ) )
  
  sigma_zw <- cbind(sigma_yy,sigma_yy,sigma_yy) 
  
  if(use_yt2){ #whether using naive increment estimation in W "vector" also? if yes, then add-in its term in sigma_ww and sigma_zw
    vcov.x_xreg_xnat <- cbind(vcov.x_xreg_xnat, rbind(autocov.y.yt2, autocov.y.yt2, autocov.y.yt2) ) 
    vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),sigma_yy))
    sigma_zw <- cbind(sigma_zw, autocov.y.yt2)
  }
  
  # shrinked  
  if(use_yt2){
    y.pred <- colMeans(y) + sigma_zw %*% solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,Yt2)[training_idx,]))), 
                                               c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(Yt2[t.now,]-colMeans(yt2))) )
    
    y.fitted <- colMeans(y) + sigma_zw %*% solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat, Yt2)[training_idx,]))), 
                                                 rbind((t(x)-colMeans(x)), (t(x.reg)-colMeans(x.reg)), (t(x.nat)-colMeans(x.nat)), (t(yt2)-colMeans(yt2))) )
    residual_epsilon <- y - t(y.fitted) #get fitted residual: true_training-predicted_training
  }else{
    y.pred <- colMeans(y) +
      sigma_zw %*% 
      solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,X_1.5)[training_idx,]))), 
            c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(X_1.5[t.now,])-colMeans(x_1.5)))
  }
  
  Y.pred[t.now, ] <- t(y.pred) #store the prediction from equation (6)
  
  projection.mat[[as.character(t.now)]] <- sigma_zw %*% solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,Yt2)[training_idx,]))))
  mean.mat[[as.character(t.now)]] <- c(colMeans(y), colMeans(x), colMeans(x.reg), colMeans(x.nat),colMeans(yt2))
  
  Y.CI[t.now,] = sqrt(diag(sigma_yy - 0.5*(projection.mat[[as.character(t.now)]] %*% t(sigma_zw))))
  
  #Store everything in a list where each element contains all stuff for the forecast of time step t and its training time steps
  sigma_ww <- vcov.x_xreg_xnat 
  sigma_zz <- sigma_yy
  sigma_ww.structured[[as.character(t.now)]] <- sigma_ww #structured is using independence assumption and derived 
  sigma_ww.empirical[[as.character(t.now)]] <- var(cbind(X,X.reg,X.nat, Yt2)[training_idx,])
  sigma_zw.structured[[as.character(t.now)]] <- sigma_zw
  sigma_zw.empirical[[as.character(t.now)]] <- cov(Y[training_idx,], cbind(X,X.reg,X.nat,Yt2)[training_idx,])
  
  sigma_zwzw.structured[[as.character(t.now)]] <- rbind( #The huge covariance matrix of Z (1 "element") and W (4 "elemtns")
    cbind(sigma_zz, sigma_zw),
    cbind(t(sigma_zw), sigma_ww)
  )
  sigma_zwzw.empirical[[as.character(t.now)]] <- var(cbind(Y,X,X.reg,X.nat,Yt2)[training_idx,])
  zw_used[[as.character(t.now)]] <- cbind(Y,X,X.reg,X.nat,Yt2)[training_idx,] 
  fitting_error[[as.character(t.now)]] <- residual_epsilon 
}

projection.mat <- sapply(projection.mat, identity, simplify = "array") #convert a list into 3D matrix (dimensions agree)
mean.mat <- sapply(mean.mat, identity, simplify = "array") #convert to a 2D matrix, each column is a mean vector for forecast of time step t. (5*51 columns)

argo2.p <- Y.pred + naive.p #the actual forecast of %ILI for time t (p_t=p_{t-1}+(p_t-p_{t-1})) is p_{t-1}+\hat{Z}_t
argo2.p[argo2.p<0] = 0
argo2.p_CI = list()
argo2.p_CI[['upper']] = argo2.p + 1.96 * Y.CI
argo2.p_CI[['lower']] = argo2.p - 1.96 * Y.CI

err.twostep <- argo2.p - truth_JOINT  #compute the forecasting error. Here 2-step means using 2-step ARGOX, 1-step means just using Google search step 1 only.

#---------------------------------------------------------------------------------------------------
# ARGOX Alone States
argo.state.p_ALONE = argo.state.p[,HI_VT]
truth_ALONE = argo.state.true[,HI_VT]
training_period_ALONE = training_period
naive.p_Alone <- truth_ALONE
index(naive.p_Alone) <- index(truth_ALONE) + 7*(1+LAG_PREDICT)

Y_ALONE <- truth_ALONE - naive.p_Alone
Yt2_ALONE <- Y_ALONE
index(Yt2_ALONE) <- index(Yt2_ALONE) + 7*(1+LAG_PREDICT)

common_idx <- index(na.omit(merge(naive.p_Alone, argo.state.p_ALONE, argo.nat.p, Yt2_ALONE)))

argo.nat.p <- argo.nat.p[common_idx]
naive.p_Alone <- naive.p_Alone[common_idx]
truth_ALONE <- truth_ALONE[common_idx]
argo.state.p_ALONE <- argo.state.p_ALONE[common_idx]
Yt2_ALONE <- Yt2_ALONE[common_idx]

X_ALONE <- argo.state.p_ALONE - naive.p_Alone

X.nat_ALONE <- as.numeric(argo.nat.p) - naive.p_Alone
X.nat_ALONE <- X.nat_ALONE[common_idx]

Y.pred_ALONE <- X_ALONE
Y.pred_ALONE[] <- NA
Y.CI_ALONE <- X_ALONE
Y.CI_ALONE[] <- NA

fitting_error <- list()

for(it in (training_period_ALONE+7*(1+LAG_PREDICT)):length(common_idx) ){
  training_idx <- common_idx[(it-training_period_ALONE-7*(1+LAG_PREDICT)+1):(it-7*(1+LAG_PREDICT))]
  residual_epsilon <- Y_ALONE[training_idx,]
  residual_epsilon[,] <- NA
  
  for(state_id in 1:ncol(argo.state.p_ALONE)){
    t.now <- common_idx[it]
    y_ALONE <- Y_ALONE[training_idx,state_id] #p_t - p_{t-1}
    x_ALONE <- X_ALONE[training_idx,state_id] #p^{GT}_t - p_{t-1}
    x.nat_ALONE <- X.nat_ALONE[training_idx,state_id] #p^{nat}_t - p_{t-1}
    yt2_ALONE <- Yt2_ALONE[training_idx,state_id] #p_{t-1} - p_{t-2}
    
    sigma_zw_ALONE <- cov(y_ALONE, cbind(x_ALONE, x.nat_ALONE, yt2_ALONE))
    vcov.x_xreg_xnats_ALONE <- var(cbind(x_ALONE, x.nat_ALONE, yt2_ALONE))
    
    y.pred_ALONE <- colMeans(y_ALONE) +
      sigma_zw_ALONE %*% 
      solve(vcov.x_xreg_xnats_ALONE + diag(diag(vcov.x_xreg_xnats_ALONE)+1), 
            c(X_ALONE[t.now,state_id]-mean(x_ALONE), X.nat_ALONE[t.now,state_id]-mean(x.nat_ALONE), Yt2_ALONE[t.now,state_id]-mean(yt2)))
    
    y.fitted_ALONE <- colMeans(y_ALONE) +
      sigma_zw_ALONE %*% 
      solve(vcov.x_xreg_xnats_ALONE + diag(diag(vcov.x_xreg_xnats_ALONE)+1), 
            t(cbind(x_ALONE-mean(x_ALONE), x.nat_ALONE-mean(x.nat_ALONE), yt2_ALONE-mean(yt2_ALONE))))
    
    residual_epsilon[, state_id] <- y_ALONE - t(y.fitted_ALONE)
    
    Y.pred_ALONE[t.now, state_id] <- y.pred_ALONE
    Y.CI_ALONE[t.now, state_id] = sqrt(var(y_ALONE) - 0.5*sigma_zw_ALONE%*%solve(vcov.x_xreg_xnats_ALONE + diag(diag(vcov.x_xreg_xnats_ALONE)+1))%*%t(sigma_zw_ALONE))
  }
  fitting_error[[as.character(t.now)]] <- residual_epsilon
}

argo2.p_ALONE <- Y.pred_ALONE + naive.p_Alone
argo2.p_ALONE[argo2.p_ALONE<0] = 0
err.twostep_ALONE <- argo2.p_ALONE - truth_ALONE
argo2.p_ALONE_CI = list()
argo2.p_ALONE_CI[['upper']] = argo2.p_ALONE + 1.96 * Y.CI_ALONE
argo2.p_ALONE_CI[['lower']] = argo2.p_ALONE - 1.96 * Y.CI_ALONE

argo2.p_2Step = cbind(argo2.p, argo2.p_ALONE)
argo2.p_2Step = argo2.p_2Step[,match(names(list_results_step1.State),gsub("\\.","-",names(argo2.p_2Step)))]
colnames(argo2.p_2Step) = names(list_results_step1.State)

argo.state.p_ALL = argo.state.p
truth_ALL = argo.state.true
err.twostep = na.omit(argo2.p_2Step - truth_ALL)

#-----------------------------------------------------------------------------------------------
# ARGOX-Nat_Constraint
joint_states
truth_JOINT = argo.state.true[,joint_states]
argo.state.p_JOINT = argo.state.p_ALL[,joint_states]

training_period 

naive.p_Joint <- truth_JOINT 
index(naive.p_Joint) <- index(truth_JOINT) + 7*(1+LAG_PREDICT) #construct naive.p which simply uses this week's %ILI to predict next week's (7 days forward or n_forward=7*1, can change in Covid where basis is 1:1day instead of 7 days:1week)

#Note that Y is a matrix where each columns contain true %ILI for each state
Y <- truth_JOINT - naive.p_Joint #obtain Z_t, the difference of p_t-p_{t-1}, here one difference in time steps is a week
Yt2 <- Y 
index(Yt2) <- index(Yt2) + 7*(1+LAG_PREDICT) #Yt2 is like Z_{t}=p_{t-1}-p_{t-2}, which is same as Z_{t-1}=p_{t-1}-p_{t-2}. i.e. at time t, Y[t] is Z_t while Yt2 is Z_{t-1}. Used for correlation calculation

common_idx <- index(na.omit(merge(naive.p_Joint, argo.state.p_JOINT, argo.nat.p, Yt2))) 

argo.nat.p <- argo.nat.p[common_idx]
argo.reg.p <- argo.reg.p[common_idx]
naive.p_Joint <- naive.p_Joint[common_idx]
truth_JOINT <- truth_JOINT[common_idx]
argo.state.p_JOINT <- argo.state.p_JOINT[common_idx]
Yt2 <- Yt2[common_idx]

X <- argo.state.p_JOINT - naive.p_Joint #obtain p^{GT}_t-p_{t-1}, state increment prediction for t
X.nat <- as.numeric(argo.nat.p) - naive.p_Joint #obtain p^{nat}_t-p_{t-1}, national increment prediction for t. 
X.nat <- X.nat[common_idx]#Note that everything here are same dimensions now, all having 51 columns (number of states), for region its the same value for all states in that region then minus truth (state level), which is stable 
argo.reg.p.dup <- lapply(state_names_JOINT, function(each_state){
  region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
  argo.reg.p[,region_id_for_state]
})
argo.reg.p.dup <- do.call(merge, argo.reg.p.dup)
names(argo.reg.p.dup) <- state_names_JOINT #Here, insteaad of having 10 columns in region p, we bump to 51 where simply duplicate the regions p for all states in that region. STORE IN argo.reg.p.dup
X.reg <- argo.reg.p.dup - naive.p_Joint #obtain p^{reg}_t-p_{t-1}, here each column is the region that state is in's p (51 columns instead of 10), regional increment prediction for t

projection.mat <- list()
mean.mat <- list()

Y.pred_ALL <- X
Y.pred_ALL[] <- NA
Y_NatConstraint.pred <- X
Y_NatConstraint.pred[] <- NA
NatConstraint_constant <-X[,1]
colnames(NatConstraint_constant) <- "NatConstraint_constant"
NatConstraint_constant[] <- NA
zw_used <- list()
fitting_error <- list()
sigma_ww.structured <- sigma_ww.empirical <-
  sigma_zw.structured <- sigma_zw.empirical <-
  heat.vec.structured <-
  sigma_zwzw.structured <- sigma_zwzw.empirical <- list() #construct lists for variance matrix elements

for(it in (training_period+7*(1+LAG_PREDICT)):length(common_idx) ){ #Note n_training in ARGO is 104
  training_idx <- common_idx[(it-training_period-7*(1+LAG_PREDICT)+1):(it-7*(1+LAG_PREDICT))] #get training dates/period (total of fixed 104 days), same idea as first step (have a training period for prediction at t, which is why a "-1'at the end of range)
  t.now <- common_idx[it] #The time step t that we predict (testing upon training)
  y <- Y[training_idx,]  #get Z_t=p_t-p_{t-1}
  x <- X[training_idx,]  #get p^{GT}_t-p_{t-1}, state_level
  #x_1.5 <- X_1.5[training_idx,]  #get p^{AR}_t-p_{t-1}, state_level
  x.nat <- X.nat[training_idx,] #get p^{nat}_t-p_{t-1}
  x.reg <- X.reg[training_idx,] #get p^{reg}_t-p_{t-1}
  yt2 <- Yt2[training_idx,] #get Z_{t-1}
  
  sigma_yy <- var(y) #compute sigma_zz
  
  m1 <- cor(y, yt2) #??First, we are assuming each t (rows) follow a multivariate distribution, but here is computing according to column?
  m2 <- cor(y)  #Also, This way of cumpting ACF for one lag doesn't seem to be correct? Isn't it just computing sample correlation and scale by variance?
  m1[is.na(m1)] = 0
  m2[is.na(m2)] = 0
  rho.l2 <- sum(m1*m2)/sum(m2^2) #rho, where cor(Z_t,Z_{t-1})=rho*sigma_zz????  
  
  autocov.y.yt2 <- rho.l2*sigma_yy #get \rho*sigma_ZZ, one step ACF
  
  vcov.x_xreg_xnat <- cbind(rbind(sigma_yy+var((argo.state.p_JOINT - truth_JOINT)[training_idx,]),sigma_yy, sigma_yy),
                            rbind(sigma_yy,sigma_yy+var((argo.reg.p.dup - truth_JOINT)[training_idx,]), sigma_yy),
                            rbind(sigma_yy,sigma_yy,sigma_yy+var((as.numeric(argo.nat.p) - truth_JOINT)[training_idx,]) ) )
  
  sigma_zw <- cbind(sigma_yy,sigma_yy,sigma_yy) 
  
  if(use_yt2){ #whether using naive increment estimation in W "vector" also? if yes, then add-in its term in sigma_ww and sigma_zw
    vcov.x_xreg_xnat <- cbind(vcov.x_xreg_xnat, rbind(autocov.y.yt2, autocov.y.yt2, autocov.y.yt2) ) 
    vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),sigma_yy))
    sigma_zw <- cbind(sigma_zw, autocov.y.yt2)
    
    ridge_sig = vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,Yt2)[training_idx,]))) #get the ridge inspired sigma matrix
    
    w_nat = argo.nat.p[t.now] - sum(colMeans(y)) - sum(naive.p_Joint[t.now])  - sum(argo.state.p_ALONE[t.now,HI_VT]) #w_nat in the equation is national truth at t minus sum of state truth at t-1 (since predicted z is diff)
    w_t = as.matrix( c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(Yt2[t.now,]-colMeans(yt2))) ) # w vector is mean 0
    one_vec = matrix(1,length(state_names_JOINT),1)
    quad_frac = as.numeric( (w_nat - t(one_vec)%*%sigma_zw%*%solve(ridge_sig,w_t)) / (sum(one_vec) * t(w_t) %*% solve(ridge_sig, w_t)) ) #compute the constaint created constant
    A_hat = sigma_zw + quad_frac * (one_vec %*% t(w_t)) #compute the A_hat matrix, without the ridge inspired sigma matrix inverse multiplied yet
    y_natconstraint.pred <- A_hat %*% solve(ridge_sig, w_t)
    Y_NatConstraint.pred[t.now, ] <- t(y_natconstraint.pred)
    NatConstraint_constant[t.now, ] <- quad_frac
  }
  # shrinked  
  if(use_yt2){
    y.pred_ALL <- colMeans(y) + sigma_zw %*% solve(ridge_sig, 
                                                   c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(Yt2[t.now,]-colMeans(yt2))) )
    y.fitted <- colMeans(y) + sigma_zw %*% solve(ridge_sig, 
                                                 rbind((t(x)-colMeans(x)), (t(x.reg)-colMeans(x.reg)), (t(x.nat)-colMeans(x.nat)), (t(yt2)-colMeans(yt2))) )
    residual_epsilon <- y - t(y.fitted) #get fitted residual: true_training-predicted_training
  }else{
    y.pred_ALL <- colMeans(y) +
      sigma_zw %*% 
      solve(vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,X_1.5)[training_idx,]))), 
            c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(X_1.5[t.now,])-colMeans(x_1.5)))
  }
  
  Y.pred_ALL[t.now, ] <- t(y.pred_ALL) #store the prediction from equation (6)
  
  ridge_sig = vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,Yt2)[training_idx,]))) #get the ridge inspired sigma matrix
  ridge_sig[which(diag(ridge_sig) == 0),which(diag(ridge_sig) == 0)] = 1
  projection.mat[[as.character(t.now)]] <- sigma_zw %*% solve(ridge_sig)
  mean.mat[[as.character(t.now)]] <- c(colMeans(y), colMeans(x), colMeans(x.reg), colMeans(x.nat),colMeans(yt2))
  
  #Store everything in a list where each element contains all stuff for the forecast of time step t and its training time steps
  sigma_ww <- vcov.x_xreg_xnat 
  sigma_zz <- sigma_yy
  sigma_ww.structured[[as.character(t.now)]] <- sigma_ww #structured is using independence assumption and derived 
  sigma_ww.empirical[[as.character(t.now)]] <- var(cbind(X,X.reg,X.nat, Yt2)[training_idx,])
  sigma_zw.structured[[as.character(t.now)]] <- sigma_zw
  sigma_zw.empirical[[as.character(t.now)]] <- cov(Y[training_idx,], cbind(X,X.reg,X.nat,Yt2)[training_idx,])
  
  sigma_zwzw.structured[[as.character(t.now)]] <- rbind( #The huge covariance matrix of Z (1 "element") and W (4 "elemtns")
    cbind(sigma_zz, sigma_zw),
    cbind(t(sigma_zw), sigma_ww)
  )
  sigma_zwzw.empirical[[as.character(t.now)]] <- var(cbind(Y,X,X.reg,X.nat,Yt2)[training_idx,])
  zw_used[[as.character(t.now)]] <- cbind(Y,X,X.reg,X.nat,Yt2)[training_idx,] 
  fitting_error[[as.character(t.now)]] <- residual_epsilon 
}

projection.mat <- sapply(projection.mat, identity, simplify = "array") #convert a list into 3D matrix (dimensions agree)
mean.mat <- sapply(mean.mat, identity, simplify = "array") #convert to a 2D matrix, each column is a mean vector for forecast of time step t. (5*51 columns)

argo2.p_ALL <- Y.pred_ALL + naive.p_Joint #the actual forecast of %ILI for time t (p_t=p_{t-1}+(p_t-p_{t-1})) is p_{t-1}+\hat{Z}_t
argo2_NatConstraint.p <- Y_NatConstraint.pred + naive.p_Joint 
argo2.p_ALL[argo2.p_ALL<0] = 0
argo2_NatConstraint.p[argo2_NatConstraint.p<0] = 0

err.twostep_ALL <- argo2.p_ALL - truth_JOINT #compute the forecasting error. Here 2-step means using 2-step ARGOX, 1-step means just using Google search step 1 only.
err_NatConstraint.twostep <- argo2_NatConstraint.p - truth_JOINT

# ARGOX all 51 states data
argo2.p_ALL = cbind(argo2.p_ALL, argo.state.p_ALL[,HI_VT])
colnames(argo2.p_ALL) = gsub("US.", "US-", colnames(argo2.p_ALL))
argo2.p_ALL = argo2.p_ALL[,names(list_results_step1.State)]
# ARGOX Nat constraint 51 states data 
argo2_NatConstraint.p = cbind(argo2_NatConstraint.p, argo.state.p_ALL[,HI_VT])
colnames(argo2_NatConstraint.p) = gsub("US.", "US-", colnames(argo2_NatConstraint.p))#cbind and merge of xts will change dash to dot
argo2_NatConstraint.p = argo2_NatConstraint.p[,names(list_results_step1.State)]

#------------------------------------------------------------------------------------------------------------------------------------
naive.p_ALL = truth_ALL
index(naive.p_ALL) = index(truth_ALL) + 7*(1+LAG_PREDICT)
# ARGO
ARGO_ALL_MSE = colMeans(na.omit(argo.state.p_ALL-truth_ALL)[index(err.twostep_ALL)]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(err.twostep_ALL)]^2)
# ARGOX all 51 2 step
ARGOX_2Step_MSE = colMeans(na.omit(err.twostep)[index(err.twostep)]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(err.twostep)]^2)
# ARGOX all 51 no constraint
ARGOX_ALL_MSE = colMeans(na.omit(argo2.p_ALL-truth_ALL)[index(err.twostep_ALL)]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(err.twostep_ALL)]^2)
# ARGOX nat constraint
ARGOX_NatConstraint_MSE = colMeans(na.omit(argo2_NatConstraint.p-truth_ALL)[index(na.omit(err.twostep_ALL))]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(na.omit(err.twostep_ALL))]^2)
# ARGOX all 51 states data
dim(argo2.p_ALL)
# ARGOX Nat constraint 51 states data 
dim(argo2_NatConstraint.p) 

Ensumble_order = apply(rbind(ARGO_ALL_MSE,ARGOX_2Step_MSE,ARGOX_ALL_MSE, ARGOX_NatConstraint_MSE),2,which.min)

argo2_Ensumble.p = argo.state.p_ALL
for (t.now in index(na.omit(err.twostep_ALL))){
  date_idx = as.Date(t.now)
  argo_pred_now = rbind( argo.state.p_ALL[date_idx,],argo2.p_2Step[date_idx, ], argo2.p_ALL[date_idx, ], argo2_NatConstraint.p[date_idx, ]) 
  argo2_Ensumble.p[date_idx,] = apply(abs(argo_pred_now[date_idx-7*weeks_ahead,] - matrix(rep(t(truth_ALL[date_idx-7*weeks_ahead,]),each =4),nrow=4))^2, 2, min) + truth_ALL[date_idx, ]
 
}
argo2_Ensumble.p[argo2_Ensumble.p<0] = 0
err_Ensumble.twostep <- argo2_Ensumble.p - truth_ALL
ARGOX_Ensumble_MSE = colMeans(na.omit(err_Ensumble.twostep)[index(na.omit(err.twostep_ALL))]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(na.omit(err.twostep_ALL))]^2)

ARGOX_Ensumble_MSE_old3 = ARGOX_Ensumble_MSE
argo2_Ensumble.p_old3 = argo2_Ensumble.p


####################################################################################################################################################################################
# ARGOX Idv with Flu
####################################################################################################################################################################################
joint_states
joint_states = names(argo.state.p)[names(argo.state.p)%in%names(ili_state)] # Florida is not in ILI, HI and VT have too many zeros
joint_states = joint_states[!joint_states%in%HI_VT]
state_In_Both_ILI_COVID = joint_states[joint_states%in%names(ili_state)]

LAG_PREDICT = weeks_forward-1 #n_forward weeks
truth_JOINT = argo.state.true[,joint_states]

argo.state.p_JOINT = argo.state.p[,joint_states]
state_names_JOINT = joint_states
# Take in truth (the true p at state level, matrix of number of states columns), predicted states p, regional p, national p, state names, which state to which region

naive.p <- truth_JOINT
index(naive.p) <- index(truth_JOINT ) + 7*(1+LAG_PREDICT) #construct naive.p which simply uses this week's %ILI to predict next week's (7 days forward or n_forward=7*1, can change in Covid where basis is 1:1day instead of 7 days:1week)

#Note that Y is a matrix where each columns contain true %ILI for each state
Y <- truth_JOINT  - naive.p #obtain Z_t, the difference of p_t-p_{t-1}, here one difference in time steps is a week
names(Y) = gsub("US.", "US-",names(Y))
Yt2 <- Y 
index(Yt2) <- index(Yt2) + 7*(1+LAG_PREDICT) #Yt2 is like Z_{t}=p_{t-1}-p_{t-2}, which is same as Z_{t-1}=p_{t-1}-p_{t-2}. i.e. at time t, Y[t] is Z_t while Yt2 is Z_{t-1}. Used for correlation calculation

common_idx <- index(na.omit(merge(naive.p, argo.state.p_JOINT , argo.nat.p, Yt2, ili_national, ili_regional, ili_state))) 

# Convert COVID Data to near ILI percentages
state_populations = rep(state_info[Abbre%in%gsub("US-", "", joint_states), Population], each=length(common_idx))
state_populations = matrix(state_populations, nrow=length(common_idx), ncol=length(joint_states))
state_populations = xts(state_populations, common_idx)
colnames(state_populations) = joint_states
US_Total_pop = sum(state_info[Abbre%in%gsub("US-", "", joint_states), Population])
region_populations=state_info[, .(Sum=sum(Population)), by=list(Region)]
region_populations = matrix(rep(region_populations[order(Region), Sum], each=length(common_idx)), nrow=length(common_idx), ncol=length(colnames(argo.reg.p)))
region_populations = xts(region_populations, common_idx)
colnames(region_populations) = colnames(argo.reg.p)

#---------------------------------------------------------------------------------------------------------------------------------
pct_scale = 100
if_scale_to_ILI = TRUE
if (if_scale_to_ILI){
  argo.nat.p <- (argo.nat.p[common_idx]/US_Total_pop)*100*pct_scale
  argo.reg.p <- (argo.reg.p[common_idx]/region_populations)*100*pct_scale
  naive.p <- (naive.p[common_idx]/state_populations)*100*pct_scale
  truth_JOINT  <- (truth_JOINT[common_idx]/state_populations)*100*pct_scale
  argo.state.p_JOINT  <- (argo.state.p_JOINT[common_idx]/state_populations)*100*pct_scale
  Yt2 <- (Yt2[common_idx]/state_populations)*100*pct_scale
  Y <-  (Y[common_idx]/state_populations)*100*pct_scale
} else{
  argo.nat.p <- argo.nat.p[common_idx]
  argo.reg.p <- argo.reg.p[common_idx]
  naive.p <- naive.p[common_idx]
  truth_JOINT  <- truth_JOINT[common_idx]
  argo.state.p_JOINT  <- argo.state.p_JOINT[common_idx]
  Yt2 <- Yt2[common_idx]
  Y <-  Y[common_idx]
}

#----------------------------------------------------------------------------------------------------------------------------------
X <- argo.state.p_JOINT  - naive.p #obtain p^{GT}_t-p_{t-1}, state increment prediction for t
X.nat <- as.numeric(argo.nat.p) - naive.p #obtain p^{nat}_t-p_{t-1}, national increment prediction for t. 
X.nat <- X.nat[common_idx]#Note that everything here are same dimensions now, all having 51 columns (number of states), for region its the same value for all states in that region then minus truth_JOINT  (state level), which is stable 

#for national its the same value accross all states, then when minus truth_JOINT  and get p^{nat}_t-p_{t} its also stable since p_{t} is different across states
argo.reg.p.dup <- lapply(state_names_JOINT, function(each_state){
  region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
  argo.reg.p[,region_id_for_state]
})
argo.reg.p.dup <- do.call(merge, argo.reg.p.dup)
names(argo.reg.p.dup) <- state_names_JOINT #Here, insteaad of having 10 columns in region p, we bump to 51 where simply duplicate the regions p for all states in that region. STORE IN argo.reg.p.dup
X.reg <- argo.reg.p.dup - naive.p #obtain p^{reg}_t-p_{t-1}, here each column is the region that state is in's p (51 columns instead of 10), regional increment prediction for t

# Get Flu needed informations
Flu.ili_state_WeekAgg = Flu.ili_reg_WeekAgg = list()
if (if_impute_Flu){
  for (i in 1:impute_samples){
    Flu.ili_state_WeekAgg[[i]] = ili_state_imputList_WeekAgg[[i]][[(LAG_PREDICT)+1]]
    tmp_reg = lapply(state_names_JOINT, function(each_state){
      region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
      ili_reg_imputList_WeekAgg[[i]][[(LAG_PREDICT+1)]][,region_id_for_state]
    })
    Flu.ili_reg_WeekAgg[[i]] = do.call(merge, tmp_reg)
    names(Flu.ili_reg_WeekAgg[[i]]) <- state_names_JOINT
  }
} else{
  impute_samples = 1
  Flu.ili_state_WeekAgg[[1]] = ili_state_noImpute[[(LAG_PREDICT+1)]]
  tmp_reg = lapply(state_names_JOINT, function(each_state){
    region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
    ili_reg_noImpute[[(LAG_PREDICT+1)]][,region_id_for_state]
  })
  Flu.ili_reg_WeekAgg[[1]] = do.call(merge, tmp_reg)
  names(Flu.ili_reg_WeekAgg[[1]]) <- state_names_JOINT
}

weight_diag = 0.5 # Weight on the Variance and Diagonal Matrix Linear Combo

# Initialize Results Storing
Y.pred <- X
Y.pred[] <- NA
Y.pred_list = list()

projection.mat <- mean.mat <- list()
zw_used <- fitting_error <- sigma_ww.structured <- sigma_ww.empirical <- sigma_zw.structured <- 
  sigma_zw.empirical <- heat.vec.structured <- sigma_zwzw.structured <- sigma_zwzw.empirical <- list() #construct lists for variance matrix elements
projection.mat_ALL = list()


#--------------------------------------------------------------------------------------------------
# IF Predicting Each State Individually
training_period = 70  #COVID-19 use 70 days, 10 weeks training period


if_use_neighbour_COVID = TRUE
if_use_flu_region = FALSE
if_use_flu_nat = FALSE
if_linearcombo_rho = FALSE
for (impute_iter in 1:impute_samples){
  Y.pred_list[[impute_iter]] = Y.pred
  print(paste0("Impute Iteration ", impute_iter))
  Flu.state_naive <- Flu.ili_state_WeekAgg[[impute_iter]]
  index(Flu.state_naive) <- index(Flu.state_naive) + 7*(1+LAG_PREDICT)
  Flu.state = Flu.ili_state_WeekAgg[[impute_iter]] - Flu.state_naive
  Flu.state_curr = Flu.state
  Flu.state = Flu.state[common_idx]
  colnames(Flu.state) = colnames(Flu.ili_state_WeekAgg[[impute_iter]])
  
  index(Flu.state_curr) = index(Flu.state_curr) - 7*(1+LAG_PREDICT)
  Flu.state_curr = Flu.state_curr[common_idx]
  colnames(Flu.state_curr) = colnames(Flu.state)
  
  Flu.reg_naive = Flu.ili_reg_WeekAgg[[impute_iter]]
  index(Flu.reg_naive) <- index(Flu.reg_naive) + 7*(1+LAG_PREDICT)
  Flu.reg <- Flu.ili_reg_WeekAgg[[impute_iter]] - Flu.reg_naive
  Flu.reg = Flu.reg[common_idx]
  colnames(Flu.reg) = colnames(Flu.ili_reg_WeekAgg[[impute_iter]])
  
  for (state_workon in joint_states){
    print(state_workon)
    region_id_for_state = state_info[Abbre==strsplit(state_workon, "-")[[1]][2], Region]
    neighbour_states = paste0("US-", state_info[Region==region_id_for_state]$Abbre)
    neighbour_states = neighbour_states[neighbour_states%in%state_In_Both_ILI_COVID]
    if (if_use_flu_region && if_use_neighbour_COVID){
      projection.mat_ALL[[state_workon]] = matrix(0, 1, length(neighbour_states)*2+4)
    }else if (if_use_flu_region && !if_use_neighbour_COVID) {
      projection.mat_ALL[[state_workon]] = matrix(0, 1,  4+length(neighbour_states)+1)
    }else if (!if_use_flu_region && if_use_neighbour_COVID) {
      projection.mat_ALL[[state_workon]] = matrix(0, 1,  length(neighbour_states)*2+3)
    } else{
      projection.mat_ALL[[state_workon]] = matrix(0, 1,  4+length(neighbour_states))
    }
    
    for(it in (training_period+7*(1+LAG_PREDICT)):length(common_idx) ){ #Note n_training in ARGO is 104
      training_idx <- common_idx[(it-training_period-7*(1+LAG_PREDICT)+1):(it-7*(1+LAG_PREDICT))] #get training dates/period (total of fixed 104 days), same idea as first step (have a training period for prediction at t, which is why a "-1'at the end of range)
      t.now <- common_idx[it] #The time step t that we predict (testing upon training)
      y <- Y[training_idx, state_workon]  #get Z_t=p_t-p_{t-1}
      x <- X[training_idx, state_workon]
      x.nat <- X.nat[training_idx, state_workon] #get p^{nat}_t-p_{t-1}
      x.reg <- X.reg[training_idx, state_workon] #get p^{reg}_t-p_{t-1}
      yt2 <- Yt2[training_idx, state_workon] #get Z_{t-1}
      
      x.flu.neighbour_state = Flu.state[training_idx, neighbour_states]
      x.flu.state_curr = Flu.state_curr[training_idx, neighbour_states]
      if (if_use_flu_region){
        x.flu.neighbour_state = cbind(x.flu.neighbour_state, Flu.reg[training_idx, state_workon])
        tmp_reg = Flu.reg[, state_workon]
        index(tmp_reg) = index(tmp_reg) - 7*(1+LAG_PREDICT)
        x.flu.state_curr = cbind(x.flu.state_curr, tmp_reg[training_idx,])
        colnames(x.flu.neighbour_state) = c(neighbour_states, paste0(state_workon, ".1"))
        colnames(x.flu.state_curr) = c(neighbour_states, paste0(state_workon, ".1"))
      }
      
      sigma_yy <- var(y) #compute sigma_zz
      sigma_yyF = cov(y, x.flu.state_curr)
      sigma_yFyF = var(x.flu.neighbour_state)
      
      m1 <- cor(y, yt2) #??First, we are assuming each t (rows) follow a multivariate distribution, but here is computing according to column?
      m2 <- cor(y)  #Also, This way of cumpting ACF for one lag doesn't seem to be correct? Isn't it just computing sample correlation and scale by variance?
      rho.l2 <- sum(m1*m2)/sum(m2^2) #rho, where cor(Z_t,Z_{t-1})=rho*sigma_zz????  
      autocov.y.yt2 <- rho.l2*sigma_yy #get \rho*sigma_ZZ, one step ACF
      
      m1_F = cor(y, x.flu.neighbour_state)
      m2_F = cor(y, x.flu.state_curr)
      rho_F <- sum(m1_F*m2_F)/sum(m2_F^2)
      autocov.y.yF = rho_F*sigma_yyF
      
      if (if_linearcombo_rho){
        m1_F = matrix(m1_F, dim(m1_F)[1]*dim(m1_F)[2], 1)
        m2_F = matrix(m2_F, dim(m2_F)[1]*dim(m2_F)[2], 1)
        m3_F = matrix(cor(x.flu.neighbour_state)[state_workon,], dim(x.flu.neighbour_state)[2], 1)
        m_23_F = cbind(m2_F, m3_F)
        rho_1_2 = solve(t(m_23_F)%*%m_23_F, t(m_23_F)%*%m1_F)
        autocov.y.yF = rho_1_2[1]*sigma_yyF + rho_1_2[2]*sigma_yFyF[state_workon,]
      }
      
      if (if_use_neighbour_COVID){
        x <- X[training_idx, neighbour_states]  #get p^{GT}_t-p_{t-1}, state_level
        y_neigh <- Y[training_idx, neighbour_states]
        yt2_neigh <- Yt2[training_idx, neighbour_states]
        sigma_yy_neigh <- var(y_neigh) 
        rho.l2_neigh <- sum( cor(y, yt2_neigh) *cor(y_neigh)[state_workon,])/sum( cor(y_neigh)[state_workon,] ^2) 
        autocov.y.yt2_neigh <- rho.l2_neigh*sigma_yy_neigh[state_workon,]
        
        sigma_yyF_neigh = cov(y_neigh, x.flu.state_curr)
        autocov.y.yF_neigh = sum(cor(y_neigh, x.flu.neighbour_state)*cor(y_neigh, x.flu.state_curr))/sum(cor(y_neigh, x.flu.state_curr)^2)*sigma_yyF_neigh
        
        if (if_linearcombo_rho){
          m1_F = matrix(cor(y_neigh, x.flu.neighbour_state), dim(y_neigh)[2]*dim(x.flu.neighbour_state)[2], 1)
          m2_F = matrix(cor(y_neigh, x.flu.state_curr), dim(y_neigh)[2]*dim(x.flu.state_curr)[2], 1)
          m3_F = matrix(cor(x.flu.neighbour_state), dim(sigma_yFyF)[1]*dim(sigma_yFyF)[2], 1)
          if (if_use_flu_region){
            m1_F = matrix(cor(cbind(y_neigh, x.reg[training_idx, state_workon]), x.flu.neighbour_state), dim(x.flu.neighbour_state)[2]*dim(x.flu.neighbour_state)[2], 1)
            m2_F = matrix(cor(cbind(y_neigh, x.reg[training_idx, state_workon]), x.flu.state_curr), dim(x.flu.neighbour_state)[2]*dim(x.flu.neighbour_state)[2], 1)
          }
          m_23_F = cbind(m2_F, m3_F)
          rho.l2_neigh = solve(t(m_23_F)%*%m_23_F, t(m_23_F)%*%m1_F)
          autocov.y.yF_neigh = rho_1_2[1]*sigma_yyF_neigh + rho_1_2[2]*sigma_yFyF[1:length(neighbour_states),]
        }
        
        vcov.x_xreg_xnat <- cbind(rbind(sigma_yy_neigh+var((argo.state.p_JOINT - truth_JOINT)[training_idx,neighbour_states]),sigma_yy_neigh[state_workon,], sigma_yy_neigh[state_workon,]),
                                  rbind(t(t(sigma_yy_neigh[,state_workon])),sigma_yy+var((argo.reg.p.dup - truth_JOINT)[training_idx,state_workon]), sigma_yy),
                                  rbind(t(t(sigma_yy_neigh[,state_workon])),sigma_yy,sigma_yy+var((as.numeric(argo.nat.p) - truth_JOINT)[training_idx,state_workon]) ) )
        vcov.x_xreg_xnat <- cbind(vcov.x_xreg_xnat, rbind(t(t(autocov.y.yt2_neigh)), autocov.y.yt2_neigh[state_workon],  autocov.y.yt2_neigh[state_workon]) )
        vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, c(autocov.y.yt2_neigh, autocov.y.yt2_neigh[state_workon], autocov.y.yt2_neigh[state_workon],sigma_yy))
        
        vcov.x_xreg_xnat = cbind(vcov.x_xreg_xnat, rbind(autocov.y.yF_neigh, t(autocov.y.yF_neigh[state_workon,]), t(autocov.y.yF_neigh[state_workon,]), sigma_yyF) )
        vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yF_neigh),t(t(autocov.y.yF_neigh[state_workon,])),t(t(autocov.y.yF_neigh[state_workon,])),t(sigma_yyF), sigma_yFyF))
        
        sigma_zw <- cbind(t(sigma_yy_neigh[state_workon,]),sigma_yy,sigma_yy, autocov.y.yt2_neigh[state_workon]) 
        sigma_zw = cbind(sigma_zw, t(autocov.y.yF_neigh[state_workon,]) )
      } else{
        vcov.x_xreg_xnat <- cbind(rbind(sigma_yy+var((argo.state.p_JOINT - truth_JOINT)[training_idx,state_workon]),sigma_yy, sigma_yy),
                                  rbind(sigma_yy,sigma_yy+var((argo.reg.p.dup - truth_JOINT)[training_idx,state_workon]), sigma_yy),
                                  rbind(sigma_yy,sigma_yy,sigma_yy+var((as.numeric(argo.nat.p) - truth_JOINT)[training_idx,state_workon]) ) )
        vcov.x_xreg_xnat <- cbind(vcov.x_xreg_xnat, rbind(autocov.y.yt2, autocov.y.yt2, autocov.y.yt2) ) 
        vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),sigma_yy))
        vcov.x_xreg_xnat = cbind(vcov.x_xreg_xnat, rbind(autocov.y.yF, autocov.y.yF, autocov.y.yF, sigma_yyF) )
        vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yF),t(autocov.y.yF),t(autocov.y.yF),t(sigma_yyF), sigma_yFyF))
        
        sigma_zw <- cbind(sigma_yy,sigma_yy,sigma_yy, autocov.y.yt2) 
        sigma_zw = cbind(sigma_zw, autocov.y.yF) 
      }
      
      if (if_use_flu_region && if_use_neighbour_COVID){
        w_pred = c(t(X[t.now,neighbour_states])-colMeans(x), t(X.reg[t.now,state_workon])-colMeans(x.reg), 
                   t(X.nat[t.now,state_workon])-colMeans(x.nat), t(Yt2[t.now,state_workon]-colMeans(yt2)), 
                   t(cbind(Flu.state[t.now, neighbour_states], Flu.reg[t.now, state_workon])) - colMeans(x.flu.neighbour_state))
      } else if (if_use_flu_region && !if_use_neighbour_COVID){
        w_pred = c(t(X[t.now,state_workon])-colMeans(x), t(X.reg[t.now,state_workon])-colMeans(x.reg), 
                   t(X.nat[t.now,state_workon])-colMeans(x.nat), t(Yt2[t.now,state_workon]-colMeans(yt2)), 
                   t(cbind(Flu.state[t.now, neighbour_states], Flu.reg[t.now, state_workon])) - colMeans(x.flu.neighbour_state))
      } else if (!if_use_flu_region && if_use_neighbour_COVID){
        w_pred = c(t(X[t.now,neighbour_states])-colMeans(x), t(X.reg[t.now,state_workon])-colMeans(x.reg), 
                   t(X.nat[t.now,state_workon])-colMeans(x.nat), t(Yt2[t.now,state_workon]-colMeans(yt2)), 
                   t(Flu.state[t.now, neighbour_states]) - colMeans(x.flu.neighbour_state))
      } else{
        w_pred = c(t(X[t.now,state_workon])-colMeans(x), t(X.reg[t.now,state_workon])-colMeans(x.reg), 
                   t(X.nat[t.now,state_workon])-colMeans(x.nat), t(Yt2[t.now,state_workon]-colMeans(yt2)), 
                   t(Flu.state[t.now, neighbour_states]) - colMeans(x.flu.neighbour_state))
      }
      
      w_vec =  t(rbind((t(x)-colMeans(x)), (t(x.reg)-colMeans(x.reg)), (t(x.nat)-colMeans(x.nat)), (t(yt2)-colMeans(yt2)), 
                       t(x.flu.neighbour_state) - colMeans(x.flu.neighbour_state)))
      
      y.pred <- colMeans(y) + sigma_zw %*% solve(weight_diag * vcov.x_xreg_xnat + (1-weight_diag) * diag(diag(vcov.x_xreg_xnat)), w_pred)
      y.fitted <- colMeans(y) + sigma_zw %*% solve(weight_diag * vcov.x_xreg_xnat + (1-weight_diag) * diag(diag(vcov.x_xreg_xnat)), t(w_vec) )
      Y.pred[t.now, state_workon] <- y.pred
      
      projection.mat[[as.character(t.now)]] <- sigma_zw %*% solve(weight_diag * vcov.x_xreg_xnat + (1-weight_diag) * diag(diag(vcov.x_xreg_xnat)) )
      projection.mat_ALL[[state_workon]] = projection.mat_ALL[[state_workon]] + projection.mat[[as.character(t.now)]]
      mean.mat[[as.character(t.now)]] <- c(colMeans(y), colMeans(x), colMeans(x.reg), colMeans(x.nat), colMeans(yt2), colMeans(x.flu.neighbour_state))
      
      #Store everything in a list where each element contains all stuff for the forecast of time step t and its training time steps
      sigma_ww <- vcov.x_xreg_xnat 
      sigma_zz <- sigma_yy
      sigma_ww.structured[[state_workon]][[as.character(t.now)]] <- sigma_ww #structured is using independence assumption and derived 
      sigma_ww.empirical[[state_workon]][[as.character(t.now)]] <- var(cbind(X[training_idx, state_workon], X.reg[training_idx, state_workon], 
                                                                             X.nat[training_idx, state_workon], Yt2[training_idx, state_workon], 
                                                                             Flu.state[training_idx, neighbour_states]))
      sigma_zw.structured[[state_workon]][[as.character(t.now)]] <- sigma_zw
      sigma_zw.empirical[[state_workon]][[as.character(t.now)]] <- cov(Y[training_idx,], 
                                                                       cbind(X[training_idx, state_workon], X.reg[training_idx, state_workon], 
                                                                             X.nat[training_idx, state_workon], Yt2[training_idx, state_workon], 
                                                                             Flu.state[training_idx, neighbour_states]))
      
      sigma_zwzw.structured[[state_workon]][[as.character(t.now)]] <- rbind( #The huge covariance matrix of Z (1 "element") and W (4 "elemtns")
        cbind(sigma_zz, sigma_zw),
        cbind(t(sigma_zw), sigma_ww)
      )
      sigma_zwzw.empirical[[state_workon]][[as.character(t.now)]] <- var(cbind(Y[training_idx, state_workon], X[training_idx, state_workon], X.reg[training_idx, state_workon], 
                                                                               X.nat[training_idx, state_workon], Yt2[training_idx, state_workon], 
                                                                               Flu.state[training_idx, neighbour_states]))
      zw_used[[state_workon]][[as.character(t.now)]] <- cbind(Y[training_idx, state_workon], X[training_idx, state_workon], X.reg[training_idx, state_workon], 
                                                              X.nat[training_idx, state_workon], Yt2[training_idx, state_workon], 
                                                              Flu.state[training_idx, neighbour_states])
      fitting_error[[state_workon]][[as.character(t.now)]] <- y - t(y.fitted) 
    }
    projection.mat_ALL[[state_workon]] = projection.mat_ALL[[state_workon]]/length((training_period+7*(1+LAG_PREDICT)):length(common_idx))
  }
  Y.pred_list[[impute_iter]] = Y.pred
}
projection.mat <- sapply(projection.mat, identity, simplify = "array") #convert a list into 3D matrix (dimensions agree)
mean.mat <- sapply(mean.mat, identity, simplify = "array") #convert to a 2D matrix, each column is a mean vector for forecast of time step t. (5*51 columns)

# Get Errors
argo2.p_useFlu = list()
err.argo2_useFlu = list()
if (if_scale_to_ILI){
  truth_JOINT_ActualScale = (truth_JOINT/(100*pct_scale))*state_populations
} else{
  truth_JOINT_ActualScale = truth_JOINT
}
argo2_useFlu_JOINT_MSE = matrix(NA, impute_samples, length(joint_states))
colnames(argo2_useFlu_JOINT_MSE) = joint_states
for (impute_iter in 1:impute_samples){
  tmp_argo2.p_useFlu = Y.pred_list[[impute_iter]] + naive.p #the actual forecast of %ILI for time t (p_t=p_{t-1}+(p_t-p_{t-1})) is p_{t-1}+\hat{Z}_t
  tmp_argo2.p_useFlu[tmp_argo2.p_useFlu<0] = 0
  argo2.p_useFlu[[impute_iter]] = tmp_argo2.p_useFlu
  if(if_scale_to_ILI){
    argo2.p_useFlu[[impute_iter]] = round((argo2.p_useFlu[[impute_iter]]/(100*pct_scale))*state_populations) # Convert Back to Actual Deaths
  }
  err.argo2_useFlu[[impute_iter]] = argo2.p_useFlu[[impute_iter]] - truth_JOINT_ActualScale
  
  naive.p_ActualScale <- truth_JOINT_ActualScale
  index(naive.p_ActualScale) <- index(truth_JOINT_ActualScale ) + 7*(1+LAG_PREDICT)
  argo2_useFlu_JOINT_MSE[impute_iter, ] = colMeans(na.omit(argo2.p_useFlu[[impute_iter]] -truth_JOINT_ActualScale)[index(na.omit(err.argo2_useFlu[[impute_iter]]))]^2) / colMeans(na.omit(naive.p_ActualScale - truth_JOINT_ActualScale )[index(na.omit(err.argo2_useFlu[[impute_iter]]))]^2)
}

argo2_Ind_useFlu = argo2.p_useFlu[[1]]
dim(truth_ALL)

for (i in names(argo2.p_useFlu[[1]])){
  na_selected = is.na(argo2_Ind_useFlu[,i])
  if (length(argo2_Ind_useFlu[na_selected,i]) > 0){
    argo2_Ind_useFlu[na_selected,i] = truth_ALL[na_selected,i] + xts(rnorm(length(truth_ALL[na_selected,i]), 0, 10),index(truth_ALL[na_selected,i]))
    print(i)
  }
}

argo2_Ind_useFlu = cbind(argo2_Ind_useFlu, argo.state.p[,c(HI_VT,'US-FL')])
colnames(argo2_Ind_useFlu) = gsub("US.", "US-", colnames(argo2_Ind_useFlu))
argo2_Ind_useFlu = argo2_Ind_useFlu[,colnames(argo.state.p)]

###################################################################################################
# Get new 5 method ensemble
truth_ALL = argo.state.true
naive.p_ALL <- truth_ALL 
index(naive.p_ALL) <- index(truth_ALL) + 7*(1+LAG_PREDICT)
ARGOX_Idv_withFlu_MSE = colMeans(na.omit(argo2_Ind_useFlu - truth_ALL)[index(na.omit(argo2_Ind_useFlu))]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL )[index(na.omit(argo2_Ind_useFlu))]^2)

Ensumble_order = apply(rbind(ARGO_ALL_MSE,ARGOX_2Step_MSE,ARGOX_ALL_MSE, ARGOX_NatConstraint_MSE, ARGOX_Idv_withFlu_MSE),2,which.min)

argo2_Ensumble.p = argo.state.p_ALL
for (t.now in index(na.omit(err.twostep_ALL))){
  date_idx = as.Date(t.now)
  argo_pred_now = rbind( argo.state.p_ALL[date_idx,],argo2.p_2Step[date_idx, ], argo2.p_ALL[date_idx, ], argo2_NatConstraint.p[date_idx, ], argo2_Ind_useFlu[date_idx,]) 
  argo2_Ensumble.p[date_idx,] = apply(abs(argo_pred_now[date_idx-7*weeks_ahead,] - matrix(rep(t(truth_ALL[date_idx-7*weeks_ahead,]),each =5),nrow=5))^2, 2, min) + truth_ALL[date_idx, ]
    
}

argo2_Ensumble.p[argo2_Ensumble.p<0] = 0
err_Ensumble.twostep <- argo2_Ensumble.p - truth_ALL
ARGOX_Ensumble_MSE = colMeans(na.omit(err_Ensumble.twostep)[index(na.omit(err.twostep_ALL))]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(na.omit(err.twostep_ALL))]^2)

ARGOX_Ensumble_MSE_new = ARGOX_Ensumble_MSE
argo2_Ensumble.p_new = argo2_Ensumble.p
