options(echo=TRUE)
library(xts)
library(glmnet)
library(argo)
library(parallel)
library(boot)
library(abind)
library(data.table)
library(lubridate)
library(cdcfluview)

ili_national = ilinet('national')
ili_regional = ilinet('hhs')
ili_state = ilinet('state')

head(ili_national)
national_end_dates = ili_national$week_start + 6
ili_national = xts(ili_national$weighted_ili, national_end_dates)
colnames(ili_national) = c("ili_national")

unique_regions = unique(ili_regional$region)
tmp_region = data.table(ili_regional)
region_i = list()
for (i in 1:length(unique_regions)){
  region_i[[i]] = xts(tmp_region[region == unique_regions[i]]$weighted_ili, tmp_region[region == unique_regions[i]]$week_start+6)
}
ili_regional = do.call(cbind, region_i)
colnames(ili_regional) = paste0("Region-", 1:length(unique_regions))

unique_states = unique(ili_state$region)
tmp_state = data.table(ili_state)
state_i = list()
for (i in 1:length(unique_states)){
  state_i[[i]] = xts(tmp_state[region == unique_states[i]]$unweighted_ili, tmp_state[region == unique_states[i]]$week_start+6)
}
ili_state = do.call(cbind, state_i)
colnames(ili_state) = unique_states

population.file <- "~/Population.csv" 
state_region_info <- fread(population.file)
state_region_info$Population <- as.numeric(gsub(",", "", state_region_info$Population))

for (i in 1:length(unique_states)){
  full_name = colnames(ili_state)[i]
  if (full_name %in% state_region_info$State){
    colnames(ili_state)[i] = paste0("US-", state_region_info[State==full_name, Abbre])
  }
}
colnames(ili_state)[52] = "US-NYC"
ili_state = ili_state[, -c(10, 53:55)] # Exclude FL, and islands


############################################################################################################
us_national <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv") #New York Times 
us_states <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

us_national <- data.table(us_national)
us_national$new_cases <- c(0, diff(us_national$cases)) #new cases=lag of totalcases (today-yesterday)
us_national$daily_deaths <- c(0, diff(us_national$deaths))
us_national$date <- as.Date(us_national$date)
covid_nat_incre <- xts(us_national[,.(new_cases, daily_deaths)], us_national$date) #select national new cases and daily death, make date as index

state_region_info <- fread(population.file)
state_region_info$Population <- as.numeric(gsub(",", "", state_region_info$Population))
state_region_info$Abbre = paste0('US-',state_region_info$Abbre)
region_name_mapping = state_region_info[, c(1, 2)]
colnames(region_name_mapping) = c('sub_region_1','iso_3166_2_code')

us_states = subset(us_states, state%in%region_name_mapping[,sub_region_1]) 
covid_state_level = list()
for (region_code in region_name_mapping$iso_3166_2_code){ #loop through all regions in the order of names(gtdata_daily_state), the list contains mobility and search query
  region_full_name = region_name_mapping[region_name_mapping$iso_3166_2_code %in% region_code]$sub_region_1
  us_states_temp = us_states[us_states$state %in% region_full_name,] #select the given region's cases and deaths
  us_states_temp <- data.table(us_states_temp) #convert to table for easier transite to xts
  us_states_temp$new_cases <- c(0, diff(us_states_temp$cases)) #new cases=lag of totalcases (today-yesterday)
  us_states_temp$daily_deaths <- c(0, diff(us_states_temp$deaths)) #new death=lag of totaldeath (today-yesterday)
  us_states_temp$date <- as.Date(us_states_temp$date) #get date
  covid_state_temp <- xts(us_states_temp[,.(new_cases, daily_deaths)], us_states_temp$date)
  covid_state_level[[region_code]] <- covid_state_temp #put the state level new cases and death in the list in the same order as gtdata_daily_state
}
common_idx = index(covid_state_level[["US-WV"]])
for (region_code in region_name_mapping$iso_3166_2_code){ #Check if all state's new deaths and cases are index by daily
  if (!all(diff(index(covid_state_level[[region_code]]))==1)){
    print(paste0("ERROR, Daily count not index with difference of 1 in ",region_code," !!"))
  }
  covid_state_level[[region_code]] = covid_state_level[[region_code]][common_idx,]
}
covid_state_level[["US-NJ"]]["2020-06-25",]$daily_deaths = covid_state_level[["US-NJ"]]["2020-06-25",]$daily_deaths - 1854
covid_national_level = covid_nat_incre
covid_national_level["2020-06-25",]$daily_deaths = covid_national_level["2020-06-25",]$daily_deaths - 1854

covid_regional_level <- lapply(1:10, function(region_number) {
  states_id <- which(state_region_info$Region == region_number)
  covid_states_wgted <- lapply(states_id, function(j) {
    covid_state_level[[state_region_info$Abbre[j] ]] #simply aggregate?Not linear combination with population portion???
  })
  Reduce("+", covid_states_wgted)
})
names(covid_regional_level) <- paste0("Region-", 1:10)

for (idx in names(covid_state_level)){
  covid_state_level[[idx]][covid_state_level[[idx]]$daily_deaths<0,2]=0
}
for (idx in names(covid_regional_level)){
  covid_regional_level[[idx]][covid_regional_level[[idx]]$daily_deaths<0,2]=0
}

