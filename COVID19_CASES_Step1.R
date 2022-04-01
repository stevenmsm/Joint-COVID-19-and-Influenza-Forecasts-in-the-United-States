library(data.table)
library(xts)
library(argo)
library(forecast)

setwd("~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/FLU+COVID19")
us_national <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv") #New York Times 
#us_counties <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") #daily updated data from NY Times github
us_states <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
#us_states <-read.csv("~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/us-states-2021-02-17.csv") 

#google_mobility <- fread("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv")
# relative volume of directions requests per country/region, sub-region or city compared to a baseline volume (Jan13 2020)
#apple_mobility <- read.csv("~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/applemobilitytrends-2021-10-12.csv") #The website above doesn't work, so I download first and then read in.
#apple_mobility = setDT(apple_mobility)

us_national <- data.table(us_national)
us_national$new_cases <- c(0, diff(us_national$cases)) #new cases=lag of totalcases (today-yesterday)
us_national$daily_deaths <- c(0, diff(us_national$deaths))
us_national$date <- as.Date(us_national$date)
plot(us_national$date, us_national$new_cases)
plot(us_national$date, us_national$daily_deaths)

gt.folder <- "~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/US-covid19-api_raw_results-2022-02-27"
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
population.file <- "~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/ARGOX/Population.csv" #gives population of each state and which region it belongs
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
#No matter if running Media Coverage reduction. Append RATIO to the gtdata list ALWAYS!
Media_ratio <- read.csv("~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/coronavirus-stories-over-time-20220308024010.csv")
Media_ratio <- xts(Media_ratio[,ncol(Media_ratio),drop=FALSE], as.Date(Media_ratio$date))
for (region in names(gtdata_daily_state)){
  gt_state = gtdata_daily_state[[region]]
  gt_state = cbind(gt_state,Media_ratio)
  gt_state = na.omit(gt_state)
  gtdata_daily_state[[region]] = gt_state
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
population.file <- "~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/ARGOX/Population.csv" #gives population of each state and which region it belongs
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
  ratio_term = which(names(gtdata_daily_state[[regions]])=="ratio") #BE SURE TO LOAD IN THE MEDIA RATIO INTO EACH STATE FIRST
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
  ratio_term = which(names(gtdata_regional[[regions]])=="ratio") #BE SURE TO LOAD IN THE MEDIA RATIO INTO EACH STATE FIRST
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
  ratio_term = which(names(gtdata_daily_state_MIXED[[regions]])=="ratio") #BE SURE TO LOAD IN THE MEDIA RATIO INTO EACH STATE FIRST
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
ratio_term = which(names(gtdata_daily_state[["US"]])=="ratio")
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
    gt_lag_train = stats::lag(current_term, gt_lag) #get lagged search term for all lags working on, in later loop, work on each column at a time
    gt_train_cur <- na.omit(merge(gt_lag_train)) 
    test_dates_cur <- index(na.omit(merge(gt_train_cur, covid_national_level, all = F))) #get the common training and forecasting for all lags 
    # temp_date =  as.Date(as.Date("2020-05-01"):as.Date("2020-09-01"))
    temp_date =  as.Date(as.Date("2021-04-01"):as.Date("2021-12-01"))
    beta_MedRaw_mat = matrix(0,2,length(gt_lag))
    colnames(beta_MedRaw_mat) = paste0("lag-",as.character(gt_lag))
    rownames(beta_MedRaw_mat) = c("Intercept", terms)
    for (i in 1:length(gt_lag)){
      temp_lm_medRaw = lm(covid_national_level$new_cases[temp_date]~ gt_train_cur[temp_date,i])
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
  MedRaw_optLag_cases = MedRaw_optLag
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Daily Indicator (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday)
Days_of_Week = matrix(0,dim(gtdata_tempNAT)[1],7)
Days_of_Week = xts(Days_of_Week, index(gtdata_tempNAT))
colnames(Days_of_Week) = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
for (i in 1:7){
  Days_of_Week[which(weekdays(index(Days_of_Week)) == colnames(Days_of_Week)[i]),i] = 1
}

########################################################################################################################################################
# ARGO STEP 1!!!!
########################################################################################################################################################
if_use_flu = FALSE
if_use_imputed_flu = FALSE
if (!if_use_imputed_flu){
  impute_samples = 1
}

n_train <- 56  #use previous 90 days to predict today
n_train_minimum <- 42  #use at least previous 40 days to predict today
list_results_step1.Nat <- list()
coef_all_step1.Nat <- list()
coef_smooth_step1.Nat <- list()
# daily training weekly prediction -----------
ts_max_lag <- 1:7  #lag a week
# case_lag <- c(7,14,21) #a week and two weeks
case_lag <- c(14)
# flu_lag <- c(0, 7)
flu_lag <- c(0,7,14)
total_forward = 28  #predicting how many days forward
decay <- 0.8
pred_week = 1:7
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
# National Level Step 1
# end of day prediction for the future (stand on today want to predict upto two weeks ahead, using data before today, lagged)
RawNAT_32terms = gtdata_tempNAT[,terms_important]
gt_nat <- RawNAT_32terms
gt_lag <- MedRaw_optLag_cases[terms_important] #The ALL data optimal Lag to Cases/Death (change here if switching Cases to Death)!!!
pred_target = covid_national_level$new_cases #prediction target

list_results_step1.Nat_impute = list()
for (impute_iter in 1:impute_samples){
  print(paste0("Impute Iteration ", impute_iter))
  for(n_forward in 1:total_forward){ 
    print(paste0("n_forward=", n_forward))
    #x_ar <- stats::lag(pred_target, ts_max_lag + n_forward - 1) #lag new death from one day to one week (matrix form) loop starting today upto 2 weeks (not using any "new data" as of today, since ts_max_lag+n_forward-1 not ts_max_lag)
    x_ar <- stats::lag(pred_target, (n_forward%%7) + n_forward - 1)
    #x_cases <- stats::lag(covid_national_level$new_cases, pmax(case_lag, n_forward)) #(always make sure not use future data). If today is Monday, predict next Tuesday, cannot use tomorrow's data, thus when n_forward>7or14 have to replace 7,14 with n_forward, only have data before and today
    x_gt <- lapply(terms_important,function(i){stats::lag(gt_nat[,i],pmax(gt_lag[i],n_forward) )})
    x_gt = do.call(cbind,x_gt)
    # x_curr <- na.omit(merge(x_ar, x_cases, x_gt , Days_of_Week)) 
    x_curr <- na.omit(merge(x_ar , x_gt, Days_of_Week)) 
    # x_curr <- na.omit(merge(x_ar, x_cases , Days_of_Week)) 
    # x_curr <- na.omit(merge(x_gt , Days_of_Week)) 
    common_id_curr <- index(na.omit(merge(x_curr, covid_national_level,Days_of_Week, all = F)))  #get the lagged data's date, the dates that we will train and test for (dont have NA)
    
    if (if_use_flu && !if_use_imputed_flu){
      x_flu = stats::lag(ili_national,  flu_lag+n_forward) # Use Latest Flu info to predict. When predicting 9/16, standing on 9/14 (n_forward=2), use 9/14, 9/7, 8/31, 8/24 Flu info, since they each store last week's Flu ILI.
      x_curr <- na.omit(merge(x_curr, x_flu))
      common_id_curr <- index(na.omit(merge(x_curr, covid_national_level,Days_of_Week, ili_national, all = F)))
    } else if(if_use_flu && if_use_imputed_flu){
      x_flu = stats::lag(ili_nat_inputList[,impute_iter],  flu_lag+n_forward)
      x_curr <- na.omit(merge(x_curr, x_flu))
      common_id_curr <- index(na.omit(merge(x_curr, covid_national_level,Days_of_Week, ili_nat_inputList, all = F)))
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
    x_new <- cbind( #started since 2019-01-01 sice word freq started then
      # stats::lag(pred_target, ts_max_lag - 1),  # latest available ar lags daily deaths (today, yesterday,...,6 days ago)
      stats::lag(pred_target, (n_forward%%7) - 1),
      # stats::lag(covid_national_level$new_cases, pmax(case_lag - n_forward, 0)), # confirmed cases a week ago new cases (a week ago today and two weeks ago today)
      # stats::lag(covid_national_level$new_cases, (7-n_forward%%7)), 
      x_gt_new,
      stats::lag(Days_of_Week,(7-n_forward%%7))
    )  # latest available gt
    
    if (if_use_flu && !if_use_imputed_flu){
      x_new = cbind(x_new, stats::lag(ili_national, flu_lag)) # Use Latest Flu info to predict. When predicting 9/16, standing on 9/14 (n_forward=2), use 9/14, 9/7, 8/31, 8/24 Flu info, since they each store last week's Flu ILI.
    } else if (if_use_flu && if_use_imputed_flu){
      x_new = cbind(x_new, stats::lag(ili_nat_inputList[,impute_iter], flu_lag))
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
if (FALSE){
  cases_nat_step1_withFlu_10102021 = list()
  pred_week = 22:28
  week_ahead = paste0(4, " week ahead")
  cases_nat_step1_withFlu_10102021[[week_ahead]] = lapply(pred_week, function(x) list_results_step1.Nat[[x]])
  cases_nat_step1_withFlu_10102021[[week_ahead]] = Reduce("+", cases_nat_step1_withFlu_10102021[[week_ahead]]) 
  
  cases_nat_step1_noFlu_10102021 = list()
  pred_week = 22:28
  week_ahead = paste0(4, " week ahead")
  cases_nat_step1_noFlu_10102021[[week_ahead]] = lapply(pred_week, function(x) list_results_step1.Nat[[x]])
  cases_nat_step1_noFlu_10102021[[week_ahead]] = Reduce("+", cases_nat_step1_noFlu_10102021[[week_ahead]]) 
}
if (FALSE){
  temp_nat <- lapply(pred_week, function(x) list_results_step1.Nat[[x]])
  temp_nat <- Reduce("+", temp_nat) 
  plot(temp_nat[,-2],legend.loc = "topleft")
  temp_nat = na.omit(temp_nat)
  plot(temp_nat["2020-05/",-2],lwd=c(1,1,1,2),legend.loc = "topleft")
  temp_nat = temp_nat[-c(1:(which(index(temp_nat)=="2020-05-01")-1)),]
  #temp_nat = temp_nat[-c(which(index(temp_nat)=="2021-06-27"):dim(temp_nat)[1]),]
  temp_nat = apply((sweep(temp_nat, 1, temp_nat$truth, "-"))^2, 2, mean)
  temp_nat[5]/temp_nat[3]
}
plot(coef_all_step1.Nat[[1]]$daily_deaths.1, col=2)
lines(coef_all_step1.Nat[[2]]$daily_deaths.1,col=3)
lines(coef_all_step1.Nat[[3]]$daily_deaths.1,col=4)
lines(coef_all_step1.Nat[[4]]$daily_deaths.1,col=5)
lines(coef_all_step1.Nat[[5]]$daily_deaths.1,col=6)
lines(coef_all_step1.Nat[[6]]$daily_deaths.1,col=7)
lines(coef_all_step1.Nat[[7]]$daily_deaths.1,col=8)


#------------------------------------------------------------------------------------------------------------------------------------------------------------
# Regional Level Step 1
if_use_flu = FALSE
if_use_imputed_flu = FALSE
ts_max_lag <- 1:7  #lag a week
gt_lag <- c(7, 14) #a week and two weeks   
case_lag <- c(7,14,21,28) #a week and two weeks
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
    pred_target = covid_regional_level[[region]]$new_cases
    ratio_term_Region = which(names(gtdata_Region_IQR_noMedia[[region]])=="ratio")
    temp_terms = names(gtdata_Region_IQR_noMedia[[region]])[names(gtdata_Region_IQR_noMedia[[region]])%in% terms_important]
    gt_nat <- gtdata_Region_IQR_noMedia[[region]] [,temp_terms]
    gt_lag <- MedRaw_optLag_cases[temp_terms]

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

if (FALSE){
  list_results_Reg_0328 = list()
  pred_week = 1:7
  for (i in names(list_results_step1.Reg)){
    list_results_Reg_0328[["1 week ahead"]][[i]] = lapply(pred_week, function(x) list_results_step1.Reg[[i]][[x]])
    list_results_Reg_0328[["1 week ahead"]][[i]]  = Reduce("+", list_results_Reg_0328[["1 week ahead"]][[i]]) 
    #list_results_Reg_0328[["1 week ahead"]][[i]] = list_results_Reg_0328[["1 week ahead"]][[i]]["2021-01/",]
  }
  
  pdf("ARGO_region_death_2Weeks.pdf") #Print Plot dotcharts for each gt term, their insample optimal lags over time
  for(i in names(list_results_Reg_0328[[2]])){  
    print(plot(list_results_Reg_0328[[2]][[i]][,-2],lwd=c(1,1,1,2),main = i, legend.loc = "topleft"))
  }
  dev.off()
  
  temp_region_10102021 = list()
  week_ahead = paste0(4, " week ahead")
  temp_region = list()
  for(i in names(cases_reg_step1_10102021[[week_ahead]])){  
    temp_region[[i]] = cases_reg_step1_10102021[[week_ahead]][[i]]
  }
  temp_region<- lapply(names(cases_reg_step1_10102021[[week_ahead]]), function(x){
    apply((sweep(temp_region[[x]], 1,temp_region[[x]]$truth, "-"))^2, 2, mean)
  })
  temp_region = do.call(cbind,temp_region)
  colnames(temp_region) = names(cases_reg_step1_10102021[[week_ahead]])
  temp_region_10102021[[week_ahead]] = temp_region
}
if (FALSE){
  cases_reg_step1_10102021 = list()
  pred_week = 22:28
  week_ahead = paste0(4, " week ahead")
  for(i in names(list_results_step1.Reg)){  
    results_week_state = list_results_step1.Reg[[i]]
    results_week.s <- lapply(pred_week, function(x) results_week_state[[x]])  
    results_week.s <- Reduce("+", results_week.s)  #plus the week ahead together, aggregated lists returing matrix
    results_week.s <- na.omit(results_week.s)
    results_week.s[which(results_week.s$argo_smooth<0), 'argo_smooth'] = 0
    # list_results_step1.Reg[[i]] = results_week.s
    cases_reg_step1_10102021[[week_ahead]][[i]] = results_week.s
  }
  pdf("ARGO_region_death_2Weeks.pdf") #Print Plot dotcharts for each gt term, their insample optimal lags over time
  for(i in names(list_results_step1.Reg)){  
    print(plot(list_results_step1.Reg[[i]][,-2],lwd=c(1,1,1,2),main = i, legend.loc = "topleft"))
  }
  dev.off()
  temp_region = list()
  for(i in names(list_results_step1.Reg)){  
    temp_region[[i]] = list_results_step1.Reg[[i]][-c(1:(which(index(list_results_step1.Reg[[i]])=="2020-07-01")-1)),]
  }
  temp_region<- lapply(names(list_results_step1.Reg), function(x){
    apply((sweep(temp_region[[x]], 1,temp_region[[x]]$truth, "-"))^2, 2, mean)
  })
  temp_region = do.call(cbind,temp_region)
  colnames(temp_region) = names(list_results_step1.Reg)
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
case_lag <- c(7,14,21,28) #a week and two weeks
total_forward = 28  #predicting how many days forward
decay <- 0.8
pred_week = 1:7

for (impute_iter in 1:impute_samples){
  print(paste0("Impute Iteration ", impute_iter))
  for (region in names(gtdata_stateMixed_IQR_noMedia)){
    print(region)
    pred_target = covid_state_level[[region]]$new_cases
    ratio_term_State = which(names(gtdata_stateMixed_IQR_noMedia[[region]])=="ratio")
    temp_terms = names(gtdata_stateMixed_IQR_noMedia[[region]])[names(gtdata_stateMixed_IQR_noMedia[[region]])%in% terms_important]
    gt_nat <- gtdata_stateMixed_IQR_noMedia[[region]][,temp_terms]
    gt_lag <- MedRaw_optLag_cases[temp_terms]
    
    for(n_forward in 1:total_forward){
      print(paste0("n_forward=", n_forward))
      x_ar <- stats::lag(pred_target, ts_max_lag + n_forward - 1)
      x_cases <- stats::lag(covid_state_level[[region]]$new_cases, pmax(case_lag, n_forward))
      x_gt <- lapply(temp_terms,function(i){stats::lag(gt_nat[,i],pmax(gt_lag[i],n_forward) )}) 
      x_gt = do.call(cbind,x_gt)
      x_curr <- na.omit(merge(x_ar, x_cases, x_gt ,  Days_of_Week)) 
      common_id_curr <- index(na.omit(merge(x_curr, pred_target, all = F)))  #get the lagged data's date, the dates that we will train and test for (dont have NA)
      
      if (region %in% colnames(ili_state)){
        if (if_use_flu && !if_use_imputed_flu){
          x_flu = stats::lag(ili_state[,region],  flu_lag+n_forward) # Use Latest Flu info to predict. When predicting 9/16, standing on 9/14 (n_forward=2), use 9/14, 9/7, 8/31, 8/24 Flu info, since they each store last week's Flu ILI.
          x_curr <- na.omit(merge(x_curr, x_flu))
          common_id_curr <- index(na.omit(merge(x_curr, pred_target,Days_of_Week, ili_state, all = F)))
        } else if(if_use_flu && if_use_imputed_flu){
          x_flu = stats::lag(ili_state_imputList[[impute_iter]][,region],  flu_lag+n_forward)
          x_curr <- na.omit(merge(x_curr, x_flu))
          common_id_curr <- index(na.omit(merge(x_curr, pred_target,Days_of_Week, ili_state_imputList[[impute_iter]], all = F)))
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
      
      if (region %in% colnames(ili_state)){
        if (if_use_flu && !if_use_imputed_flu){
          x_new = cbind(x_new, stats::lag(ili_state[,region], flu_lag)) # Use Latest Flu info to predict. When predicting 9/16, standing on 9/14 (n_forward=2), use 9/14, 9/7, 8/31, 8/24 Flu info, since they each store last week's Flu ILI.
        } else if (if_use_flu && if_use_imputed_flu){
          x_new = cbind(x_new, stats::lag(ili_state_imputList[[impute_iter]][,region], flu_lag))
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


if (FALSE){
  list_results_State_0328 = list()
  pred_week = 1:7
  for (i in names(list_results_step1.State)){
    list_results_State_0328[["1 week ahead"]][[i]] = lapply(pred_week, function(x) list_results_step1.State[[i]][[x]])
    list_results_State_0328[["1 week ahead"]][[i]]  = Reduce("+", list_results_State_0328[["1 week ahead"]][[i]]) 
    #list_results_State_0328_2021Pred[["1 week ahead"]][[i]] = list_results_State_0328_2021Pred[["1 week ahead"]][[i]]["2021-01/",]
  }
 
  pdf("ARGO_State_death_1Week.pdf") #Print Plot dotcharts for each gt term, their insample optimal lags over time
  for(i in names(list_results_State_0328[[1]])){  
    print(plot(list_results_State_0328[[1]][[i]][,-2],lwd=c(1,1,1,2),main = i, legend.loc = "topleft"))
  }
  dev.off()
  
  temp_state_10102021 = list()
  week_ahead = paste0(4, " week ahead")
  temp_state = list()
  for(i in names(list_results_step1.State)){  
    temp_state[[i]] = cases_state_step1_10102021[[week_ahead]][[i]]
  }
  temp_state<- lapply(names(list_results_step1.State), function(x){
    apply((sweep(temp_state[[x]], 1,temp_state[[x]]$truth, "-"))^2, 2, mean)
  })
  temp_state = do.call(cbind,temp_state)
  colnames(temp_state) = names(list_results_step1.State)
  temp_state_10102021[[week_ahead]] = temp_state
}
if (FALSE){
  cases_state_step1_10102021 = list()
  week_ahead = paste0(4, " week ahead")
  pred_week = 22:28
  for(i in names(list_results_step1.State)){  
    #i = "US-NJ"
    results_week_state = list_results_step1.State[[i]]
    results_week.s <- lapply(pred_week, function(x) results_week_state[[x]])  
    results_week.s <- Reduce("+", results_week.s)  #plus the week ahead together, aggregated lists returing matrix
    results_week.s <- na.omit(results_week.s)
    results_week.s[which(results_week.s$argo_smooth<0), 'argo_smooth'] = 0
    # list_results_step1.State[[i]] = results_week.s
    cases_state_step1_10102021[[week_ahead]][[i]] = results_week.s
  }
  pdf("ARGO_state_death_2Weeks.pdf") #Print Plot dotcharts for each gt term, their insample optimal lags over time
  for(i in names(list_results_step1.State)){  
    print(plot(list_results_step1.State[[i]][,-2],lwd=c(1,1,1,2),main = i, legend.loc = "topleft"))
  }
  dev.off()
  temp_state = list()
  for(i in names(list_results_step1.State)){  
    temp_state[[i]] = list_results_step1.State[[i]][-c(1:(which(index(list_results_step1.State[[i]])=="2020-05-01")-1)),]
  }
  temp_state<- lapply(names(list_results_step1.State), function(x){
    apply((sweep(temp_state[[x]], 1,temp_state[[x]]$truth, "-"))^2, 2, mean)
  })
  temp_state = do.call(cbind,temp_state)
  colnames(temp_state) = names(list_results_step1.State)
}

