setwd("~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/FLU+COVID19")
options(echo=TRUE)
library(xts)
library(glmnet)
library(argo)
library(parallel)
library(boot)
library(abind)
library(data.table)
library(lubridate)

# Organize data from ARGO Step 1
population.file <- "~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/FLU+COVID19/Population.csv" #gives population of each state and which region it belongs
state_info <- fread(population.file)
state_info$Population <- as.numeric(gsub(",", "", state_info$Population))

# Work with daily or weekly data for Flu
index_daily = TRUE
if_impute_Flu = FALSE
impute_samples = 1

source("ILI_COVID_Data_Clean.R")
total_days_forward = 28
# If Inputing Daily flu data, assuming Flu stores data on Saturdays: 9/11/2021 is a Saturday, stores 9/4+...+9/10. 
# Thus, 9/4, ..., 9/10 will store the week from 9/4+...9/10 data.
if (index_daily){
  colnames(ili_national) = c('ili_national')
  
  # National
  tmp = as.data.table(ili_national, keep.rownames = TRUE)
  # Convert weekly ILI into daily, filling each day in week same value. i.e. fill 5/15, ..., 5/21 the value of 5/15, which is actually week of 5/9, ..., 5/15.
  tmp <- tmp[,.(date=ymd(seq.Date(from = as.Date(index),to = as.Date(index)+6,
                                  by= "day" )), value=ili_national),by="index"]
  ili_national = xts(tmp$value, as.Date(tmp$date))
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
      print(paste0("Impute Sample ", iter))
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
        ili_reg_imputList[[iter]][tmp_common_idx, i] = tmp
      }
      colnames(ili_reg_imputList[[iter]]) = colnames(ili_regional)
    }
    
    # Impute State
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
        ili_state_imputList[[iter]][tmp_common_idx, i] = tmp
      }
    }
  }
  
  # Now let Flu stores data on Saturdays: 9/11/2021 is a Saturday, stores 9/5+...+9/11. 
  # Thus, 9/5, ..., 9/11 will store the week from 9/5+...+9/11 data.
  index(ili_national) = index(ili_national) + 1
  index(ili_regional) = index(ili_regional) + 1
  index(ili_state) = index(ili_state) + 1
  ili_state_noImpute = ili_reg_noImpute = list()
  if (if_impute_Flu){
    for (i in 1:length(ili_state_imputList)){
      index(ili_state_imputList[[i]]) = index(ili_state_imputList[[i]])+1
      index(ili_reg_imputList[[i]]) = index(ili_reg_imputList[[i]]) + 1
    }
    # Since ARGOX Step 2 Below will work with weekly aggregated daily data. Convert Inputed Daily Flu data to Weekly Aggregate again. If inpute Flu from COVID Case
    tmp_ili_aggregate = tmp_ili_aggregate_reg = list()
    ili_state_imputList_WeekAgg = ili_reg_imputList_WeekAgg = list()
    for (iter in 1:length(ili_state_imputList)){
      ili_state_imputList_WeekAgg[[iter]] = list()
      ili_reg_imputList_WeekAgg[[iter]] = list()
      for (tmp_lags in 1:total_days_forward){
        tmp_ili_aggregate[[tmp_lags]] = stats::lag(ili_state_imputList[[iter]], -tmp_lags)
        tmp_ili_aggregate_reg[[tmp_lags]] = stats::lag(ili_reg_imputList[[iter]], -tmp_lags)
      }
      ili_state_1WeekAgg = lapply(1:7, function(x) tmp_ili_aggregate[[x]])
      ili_state_1WeekAgg = Reduce("+", ili_state_1WeekAgg) 
      index(ili_state_1WeekAgg) = index(ili_state_1WeekAgg) +7
      ili_state_imputList_WeekAgg[[iter]][["1 week ahead"]] = na.omit(ili_state_1WeekAgg)
      
      ili_reg_1WeekAgg = lapply(1:7, function(x) tmp_ili_aggregate_reg[[x]])
      ili_reg_1WeekAgg = Reduce("+", ili_reg_1WeekAgg) 
      index(ili_reg_1WeekAgg) = index(ili_reg_1WeekAgg) +7
      ili_reg_imputList_WeekAgg[[iter]][["1 week ahead"]] = na.omit(ili_reg_1WeekAgg)
      
      ili_state_2WeeksAgg = lapply(8:14, function(x) tmp_ili_aggregate[[x]])
      ili_state_2WeeksAgg = Reduce("+", ili_state_2WeeksAgg) 
      index(ili_state_2WeeksAgg) = index(ili_state_2WeeksAgg) +14
      ili_state_imputList_WeekAgg[[iter]][["2 week ahead"]] = na.omit(ili_state_2WeeksAgg)
      
      ili_reg_2WeeksAgg = lapply(8:14, function(x) tmp_ili_aggregate_reg[[x]])
      ili_reg_2WeeksAgg = Reduce("+", ili_reg_2WeeksAgg) 
      index(ili_reg_2WeeksAgg) = index(ili_reg_2WeeksAgg) +14
      ili_reg_imputList_WeekAgg[[iter]][["2 week ahead"]] = na.omit(ili_reg_2WeeksAgg)
      
      ili_state_3WeeksAgg = lapply(15:21, function(x) tmp_ili_aggregate[[x]])
      ili_state_3WeeksAgg = Reduce("+", ili_state_3WeeksAgg) 
      index(ili_state_3WeeksAgg) = index(ili_state_3WeeksAgg) +21
      ili_state_imputList_WeekAgg[[iter]][["3 week ahead"]] = na.omit(ili_state_3WeeksAgg)
      
      ili_reg_3WeeksAgg = lapply(15:21, function(x) tmp_ili_aggregate_reg[[x]])
      ili_reg_3WeeksAgg = Reduce("+", ili_reg_3WeeksAgg) 
      index(ili_reg_3WeeksAgg) = index(ili_reg_3WeeksAgg) +21
      ili_reg_imputList_WeekAgg[[iter]][["3 week ahead"]] = na.omit(ili_reg_3WeeksAgg)
      
      ili_state_4WeeksAgg = lapply(22:28, function(x) tmp_ili_aggregate[[x]])
      ili_state_4WeeksAgg = Reduce("+", ili_state_4WeeksAgg) 
      index(ili_state_4WeeksAgg) = index(ili_state_4WeeksAgg) +28
      ili_state_imputList_WeekAgg[[iter]][["4 week ahead"]] = na.omit(ili_state_4WeeksAgg)
      
      ili_reg_4WeeksAgg = lapply(22:28, function(x) tmp_ili_aggregate_reg[[x]])
      ili_reg_4WeeksAgg = Reduce("+", ili_reg_4WeeksAgg) 
      index(ili_reg_4WeeksAgg) = index(ili_reg_4WeeksAgg) +28
      ili_reg_imputList_WeekAgg[[iter]][["4 week ahead"]] = na.omit(ili_reg_4WeeksAgg)
    }
  } else{
    ili_state_noImpute[["1 week ahead"]] = ili_state
    index(ili_state_noImpute[["1 week ahead"]]) = index(ili_state_noImpute[["1 week ahead"]]) +7
    ili_state_noImpute[["2 week ahead"]] = ili_state
    index(ili_state_noImpute[["2 week ahead"]]) = index(ili_state_noImpute[["2 week ahead"]]) +14
    ili_state_noImpute[["3 week ahead"]] = ili_state
    index(ili_state_noImpute[["3 week ahead"]]) = index(ili_state_noImpute[["3 week ahead"]]) +21
    ili_state_noImpute[["4 week ahead"]] = ili_state
    index(ili_state_noImpute[["4 week ahead"]]) = index(ili_state_noImpute[["4 week ahead"]]) +28
    
    ili_reg_noImpute[["1 week ahead"]] = ili_regional
    index(ili_reg_noImpute[["1 week ahead"]]) = index(ili_reg_noImpute[["1 week ahead"]]) +7
    ili_reg_noImpute[["2 week ahead"]] = ili_regional
    index(ili_reg_noImpute[["2 week ahead"]]) = index(ili_reg_noImpute[["2 week ahead"]]) +14
    ili_reg_noImpute[["3 week ahead"]] = ili_regional
    index(ili_reg_noImpute[["3 week ahead"]]) = index(ili_reg_noImpute[["3 week ahead"]]) +14
    ili_reg_noImpute[["4 week ahead"]] = ili_regional
    index(ili_reg_noImpute[["4 week ahead"]]) = index(ili_reg_noImpute[["4 week ahead"]]) +14
  }
}

##################################################################################################################################################################################################################################################################################################################
# ARGOX step 2, predict diff 1, Spacial Correlated JOINT States only, ARGOX no Constraint. Daily Predict (daily stored moving average 7 days)
####################################################################################################################################################################################################################################################################################################################
# Though daily stored weekly, Y_{t} - Y_{t-1}, the (t-1) is still a week ago, i.e. t-7
load("~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/ARGOX/COVID19Death_10102021_Step1.rda")
load("Death_Step1_withFlu.rda")

weeks_forward = 4
list_results_step1.Reg = list_results_step1.Reg_10102021[[weeks_forward]]
list_results_step1.State = list_results_step1.State_10102021[[weeks_forward]]
list_results_step1.Nat = list_results_step1.Nat_10102021_withFlu[[weeks_forward]]
temp_region = temp_region_10102021[[weeks_forward]]
temp_state = temp_state_10102021[[weeks_forward]]
state_names = names(list_results_step1.State)
state_info = state_region_info

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

####################################################################################################################################################################################
HI_VT = c("US-HI", "US-VT")
joint_states = names(argo.state.p)
joint_states = names(argo.state.p)[names(argo.state.p)%in%names(ili_state)] # Florida is not in ILI, HI and VT have too many zeros
joint_states = joint_states[!joint_states%in%HI_VT]
state_In_Both_ILI_COVID = joint_states[joint_states%in%names(ili_state)]

LAG_PREDICT = weeks_forward-1 #n_forward weeks
state_info = state_region_info
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


####################################################################################################################################################################################
# IF Predicting Each State Individually
####################################################################################################################################################################################
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

if (FALSE){
  argo2_Ind_useFlu = argo2.p_useFlu[[1]]
  tmp_selection = apply(argo2_useFlu_JOINT_MSE, 2, which.min)
  for (i in names(tmp_selection)){
    argo2_Ind_useFlu[,i] = argo2.p_useFlu[[as.numeric(tmp_selection[i])]][,i]
  }
  colMeans(na.omit(argo2_Ind_useFlu - truth_JOINT_ActualScale)[index(na.omit(argo2_Ind_useFlu))]^2) / colMeans(na.omit(naive.p_ActualScale - truth_JOINT_ActualScale )[index(na.omit(argo2_Ind_useFlu))]^2)
}

ensemble_training = 20
argo2_Ind_useFlu = argo2.p_useFlu[[1]]
argo2_Ind_useFlu[] = NA
for (i in names(argo2.p_useFlu[[1]])){
  print(i)
  tmp = argo2.p_useFlu[[1]][,i]
  for (iter in 2:impute_iter){
    tmp = cbind(tmp, argo2.p_useFlu[[iter]][,i])
  }
  tmp = cbind(tmp, naive.p_ActualScale[,i]) 
  tmp = na.omit(tmp)
  for (it in (ensemble_training:(length(index(tmp))-1) )){ 
    training_idx <- index(tmp)[(it-ensemble_training+1):it]
    err_tmp = (tmp[training_idx, ]- matrix(rep(t(truth_JOINT_ActualScale[training_idx,i]), (impute_iter+1)), nrow=length(training_idx)))^2
    err_tmp = apply(err_tmp, 2, mean)
    selected_method = which.min(err_tmp)
    argo2_Ind_useFlu[index(tmp)[it+1],i] = tmp[index(tmp)[it+1],as.numeric(selected_method)]
  }
}


#######################################################################################################################
# New Ensemble#
#######################################################################################################################
load("Death_Step1_withFlu.rda")
load("~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/ARGOX/COVID19Death_10102021_Step1.rda")

argo2_NewEnsemble_withFlu_10102021 = list()

weeks_forward = 4
list_results_step1.State = list_results_step1.State_10102021[[weeks_forward]]

argo.state.true <- lapply(list_results_step1.State, function(x) x[,"truth"]) #state level true Covid-19 weekly death
argo.state.true <- do.call(merge, argo.state.true)
colnames(argo.state.true) <- names(list_results_step1.State)
index(argo.state.true) <- as.Date(index(argo.state.true))
truth_ALL = argo.state.true

naive.p = truth_ALL
index(naive.p) = index(truth_ALL) + 7*weeks_forward

ensemble_training = 10
argo2_NewEnsemble_withFlu = argo2_Ensumble.p_10102021[[weeks_forward]]
argo2_NewEnsemble_withFlu[] = NA
for (i in names(argo2_Ensumble.p_10102021[[weeks_forward]])){
  print(i)
  tmp = cbind(argo.state.p_ALL_10102021[[weeks_forward]][,i], 
              argo2.p_ALL_10102021[[weeks_forward]][,i],
              argo2.p_2Step_10102021[[weeks_forward]][,i],
              argo2_NatConstraint.p_10102021[[weeks_forward]][,i],
              argo2_Ind_useFlu_10102021[[weeks_forward]][,i])
  for (it in (ensemble_training:(length(index(tmp))-1) )){ 
    training_idx <- index(tmp)[(it-ensemble_training+1):it]
    err_tmp = (tmp[training_idx, ]- matrix(rep(t(truth_ALL[training_idx,i]), 5), nrow=length(training_idx)))^2
    err_tmp = apply(err_tmp, 2, mean)
    selected_method = which.min(err_tmp)
    argo2_NewEnsemble_withFlu[index(tmp)[it+1],i] = tmp[index(tmp)[it+1],as.numeric(selected_method)]
  }
}
argo2_NewEnsemble_withFlu[,c("US-FL","US-HI", "US-VT")] = argo2_Ensumble.p_10102021[[weeks_forward]][,c("US-FL","US-HI", "US-VT")]
compare_idx = index(na.omit(argo2_NewEnsemble_withFlu))
compare_idx = compare_idx[compare_idx >= as.Date('2020-07-01')]
NewEnsemble_MSE = colMeans(na.omit(argo2_NewEnsemble_withFlu - truth_ALL)[compare_idx]^2) / colMeans(na.omit(naive.p - truth_ALL)[compare_idx]^2)
OldEnsemble_MSE = colMeans(na.omit(argo2_Ensumble.p_10102021[[weeks_forward]] - truth_ALL)[compare_idx]^2) / colMeans(na.omit(naive.p - truth_ALL)[compare_idx]^2)

argo2_NewEnsemble_withFlu_10102021[[paste0(weeks_forward, ' weeks ahead')]] = argo2_NewEnsemble_withFlu

