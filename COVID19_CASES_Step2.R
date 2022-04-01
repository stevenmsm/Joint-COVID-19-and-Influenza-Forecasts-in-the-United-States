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
source("ILI_COVID_Data_Clean.R")

population.file <- "~/Documents/Georgia_Tech/Research at GATECH/Research with Dr. Shihao Yang/COVID-19/FLU+COVID19/Population.csv" #gives population of each state and which region it belongs
state_info <- fread(population.file)
state_info$Population <- as.numeric(gsub(",", "", state_info$Population))

# Work with daily or weekly data for Flu
index_daily = TRUE
if_impute_Flu = FALSE
impute_samples = 1

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
      for (tmp_lags in 1:14){
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
    }
  } else{
    ili_state_noImpute[["1 week ahead"]] = ili_state
    index(ili_state_noImpute[["1 week ahead"]]) = index(ili_state_noImpute[["1 week ahead"]]) +7
    ili_state_noImpute[["2 week ahead"]] = ili_state
    index(ili_state_noImpute[["2 week ahead"]]) = index(ili_state_noImpute[["2 week ahead"]]) +14
    
    ili_reg_noImpute[["1 week ahead"]] = ili_regional
    index(ili_reg_noImpute[["1 week ahead"]]) = index(ili_reg_noImpute[["1 week ahead"]]) +7
    ili_reg_noImpute[["2 week ahead"]] = ili_regional
    index(ili_reg_noImpute[["2 week ahead"]]) = index(ili_reg_noImpute[["2 week ahead"]]) +14
  }
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Organize ARGO step 1 COVID data
load("Case_Step1_10102021.rda")
weeks_ahead = 4
list_results_Reg_FINAL = list()
list_results_Reg_FINAL[[weeks_ahead]] = cases_reg_step1_10102021[[weeks_ahead]]
list_results_State_FINAL = list()
list_results_State_FINAL[[weeks_ahead]] = cases_state_step1_10102021[[weeks_ahead]]
list_results_Nat_FINAL = list()
list_results_Nat_FINAL[[weeks_ahead]] = cases_nat_step1_withFlu_10102021[[weeks_ahead]]
temp_region = temp_region_10102021[[weeks_ahead]]
temp_state = temp_state_10102021[[weeks_ahead]]

if (FALSE){
  temp_region = list()
  for(i in names(list_results_Reg_FINAL[[weeks_ahead]])){  
    temp_region[[i]] = na.omit(list_results_Reg_FINAL[[weeks_ahead]][[i]])
  }
  temp_region<- lapply(names(list_results_Reg_FINAL[[weeks_ahead]]), function(x){apply((sweep(temp_region[[x]], 1,temp_region[[x]]$truth, "-"))^2, 2, mean)})
  temp_region = do.call(cbind,temp_region)
  colnames(temp_region) = names(list_results_Reg_FINAL[[weeks_ahead]])
  
  temp_state = list()
  for(i in names(list_results_State_FINAL[[weeks_ahead]])){  
    temp_state[[i]] = na.omit(list_results_State_FINAL[[weeks_ahead]][[i]])
  }
  temp_state<- lapply(names(list_results_State_FINAL[[weeks_ahead]]), function(x){apply((sweep(temp_state[[x]], 1,temp_state[[x]]$truth, "-"))^2, 2, mean)})
  temp_state = do.call(cbind,temp_state)
  colnames(temp_state) = names(list_results_State_FINAL[[weeks_ahead]])
}

list_results_step1.Reg = list_results_Reg_FINAL[[weeks_ahead]]
list_results_step1.State = list_results_State_FINAL[[weeks_ahead]]
list_results_step1.Nat = list_results_Nat_FINAL[[weeks_ahead]]

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
LAG_PREDICT = weeks_ahead-1

argo.state.p_ALL = {}
for (i in names(list_results_step1.State)){
  if (temp_state[5,i]<=temp_state[4,i]){
    argo.state.p_ALL = cbind(argo.state.p_ALL, list_results_step1.State[[i]][,5])
  }
  else{
    argo.state.p_ALL = cbind(argo.state.p_ALL, list_results_step1.State[[i]][,4])
  }
}
colnames(argo.state.p_ALL) <- names(list_results_step1.State)
index(argo.state.p_ALL) <- as.Date(index(argo.state.p_ALL))

state_info <- fread(population.file)
state_info$Population <- as.numeric(gsub(",", "", state_info$Population))

truth_ALL = argo.state.true
use_yt2=TRUE
Nat_Constraint = TRUE

state_names_ALL = colnames(argo.state.p_ALL) 
# Take in truth (the true p at state level, matrix of number of states columns), predicted states p, regional p, national p, state names, which state to which region
training_period = 90  #COVID-19 use 70 days, 10 weeks training period
Ensumble_training_period = 120

naive.p_ALL <- truth_ALL 
index(naive.p_ALL) <- index(truth_ALL) + 7*(1+LAG_PREDICT) #construct naive.p which simply uses this week's %ILI to predict next week's (7 days forward or n_forward=7*1, can change in Covid where basis is 1:1day instead of 7 days:1week)

#Note that Y is a matrix where each columns contain true %ILI for each state
Y <- truth_ALL - naive.p_ALL #obtain Z_t, the difference of p_t-p_{t-1}, here one difference in time steps is a week
Yt2 <- Y 
index(Yt2) <- index(Yt2) + 7*(1+LAG_PREDICT) #Yt2 is like Z_{t}=p_{t-1}-p_{t-2}, which is same as Z_{t-1}=p_{t-1}-p_{t-2}. i.e. at time t, Y[t] is Z_t while Yt2 is Z_{t-1}. Used for correlation calculation

common_idx <- index(na.omit(merge(naive.p_ALL, argo.state.p_ALL, argo.nat.p, Yt2))) 

argo.nat.p <- argo.nat.p[common_idx]
argo.reg.p <- argo.reg.p[common_idx]
naive.p_ALL <- naive.p_ALL[common_idx]
truth_ALL <- truth_ALL[common_idx]
argo.state.p_ALL <- argo.state.p_ALL[common_idx]
Yt2 <- Yt2[common_idx]

X <- argo.state.p_ALL - naive.p_ALL #obtain p^{GT}_t-p_{t-1}, state increment prediction for t
X.nat <- as.numeric(argo.nat.p) - naive.p_ALL #obtain p^{nat}_t-p_{t-1}, national increment prediction for t. 
X.nat <- X.nat[common_idx]#Note that everything here are same dimensions now, all having 51 columns (number of states), for region its the same value for all states in that region then minus truth (state level), which is stable 
#for national its the same value accross all states, then when minus truth and get p^{nat}_t-p_{t} its also stable since p_{t} is different across states
argo.reg.p.dup <- lapply(state_names_ALL, function(each_state){
  region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
  argo.reg.p[,region_id_for_state]
})
argo.reg.p.dup <- do.call(merge, argo.reg.p.dup)
names(argo.reg.p.dup) <- state_names_ALL #Here, insteaad of having 10 columns in region p, we bump to 51 where simply duplicate the regions p for all states in that region. STORE IN argo.reg.p.dup
X.reg <- argo.reg.p.dup - naive.p_ALL #obtain p^{reg}_t-p_{t-1}, here each column is the region that state is in's p (51 columns instead of 10), regional increment prediction for t

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
  if (it<=(Ensumble_training_period+7*(1+LAG_PREDICT))){ #Select the training id for ensumble model
    Ensumble_train_idx = as.Date(common_idx[it]) - ((Ensumble_training_period-7*(1+LAG_PREDICT)+1):(7*(1+LAG_PREDICT)))
    Ensumble_train_idx <- Ensumble_train_idx[Ensumble_train_idx >= common_idx[1]]
  }else{
    Ensumble_train_idx <- common_idx[(it-Ensumble_training_period-7*(1+LAG_PREDICT)+1):(it-7*(1+LAG_PREDICT))]
  }
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
  m1[is.na(m1)] = 0
  m2[is.na(m2)] = 0
  rho.l2 <- sum(m1*m2)/sum(m2^2) #rho, where cor(Z_t,Z_{t-1})=rho*sigma_zz????  
  
  autocov.y.yt2 <- rho.l2*sigma_yy #get \rho*sigma_ZZ, one step ACF
  
  vcov.x_xreg_xnat <- cbind(rbind(sigma_yy+var((argo.state.p_ALL - truth_ALL)[training_idx,]),sigma_yy, sigma_yy),
                            rbind(sigma_yy,sigma_yy+var((argo.reg.p.dup - truth_ALL)[training_idx,]), sigma_yy),
                            rbind(sigma_yy,sigma_yy,sigma_yy+var((as.numeric(argo.nat.p) - truth_ALL)[training_idx,]) ) )
  
  sigma_zw <- cbind(sigma_yy,sigma_yy,sigma_yy) 
  
  if(use_yt2){ #whether using naive increment estimation in W "vector" also? if yes, then add-in its term in sigma_ww and sigma_zw
    vcov.x_xreg_xnat <- cbind(vcov.x_xreg_xnat, rbind(autocov.y.yt2, autocov.y.yt2, autocov.y.yt2) ) 
    vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),sigma_yy))
    sigma_zw <- cbind(sigma_zw, autocov.y.yt2)
    ridge_sig = vcov.x_xreg_xnat + diag(diag(var(cbind(X,X.reg,X.nat,Yt2)[training_idx,]))) #get the ridge inspired sigma matrix
  }
  
  if(use_yt2 && Nat_Constraint){ # if enforcing summing state predictions equal to national truth
    w_nat = argo.nat.p[t.now] - sum(colMeans(y)) - sum(naive.p_ALL[t.now]) #w_nat in the equation is national truth at t minus sum of state truth at t-1 (since predicted z is diff)
    w_t = as.matrix( c(t(X[t.now,])-colMeans(x), t(X.reg[t.now,])-colMeans(x.reg), t(X.nat[t.now,])-colMeans(x.nat), t(Yt2[t.now,]-colMeans(yt2))) ) # w vector is mean 0
    one_vec = matrix(1,length(state_names_ALL),1)
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

argo2.p_ALL <- Y.pred_ALL + naive.p_ALL #the actual forecast of %ILI for time t (p_t=p_{t-1}+(p_t-p_{t-1})) is p_{t-1}+\hat{Z}_t
argo2_NatConstraint.p <- Y_NatConstraint.pred + naive.p_ALL 
argo2.p_ALL[argo2.p_ALL<0] = 0
argo2_NatConstraint.p[argo2_NatConstraint.p<0] = 0

err.twostep_ALL <- argo2.p_ALL - truth_ALL #compute the forecasting error. Here 2-step means using 2-step ARGOX, 1-step means just using Google search step 1 only.
err_NatConstraint.twostep <- argo2_NatConstraint.p - truth_ALL

# ARGO
ARGO_ALL_MSE = colMeans(na.omit(argo.state.p_ALL-truth_ALL)[index(err.twostep_ALL)]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(err.twostep_ALL)]^2)
# ARGOX all 51 no constraint
ARGOX_ALL_MSE = colMeans(na.omit(err.twostep_ALL)[index(err.twostep_ALL)]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(err.twostep_ALL)]^2)
# ARGOX nat constraint
ARGOX_NatConstraint_MSE = colMeans(na.omit(err_NatConstraint.twostep)[index(na.omit(err.twostep_ALL))]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(na.omit(err.twostep_ALL))]^2)
# ARGOX all 51 states data
dim(argo2.p_ALL)
# ARGOX Nat constraint 51 states data 
dim(argo2_NatConstraint.p) 

Ensumble_order = apply(rbind(ARGO_ALL_MSE,ARGOX_ALL_MSE, ARGOX_NatConstraint_MSE),2,which.min)
truth_ALL = argo.state.true
naive.p_ALL <- truth_ALL 
index(naive.p_ALL) <- index(truth_ALL) + 7*(1+LAG_PREDICT)

argo2_Ensumble.p = argo.state.p_ALL
for (t.now in index(na.omit(err.twostep_ALL))){
  date_idx = as.Date(t.now)
  argo_pred_now = rbind( argo.state.p_ALL[date_idx,], argo2.p_ALL[date_idx, ], argo2_NatConstraint.p[date_idx, ]) 
  if (t.now %%1 == 0){
    argo2_Ensumble.p[date_idx,] = apply(abs(argo_pred_now -   matrix(rep(t(truth_ALL[date_idx, ]),each =3),nrow=3)), 2, min) + truth_ALL[date_idx, ]
    
  } else{
    index2D_temp <- function(v = Ensumble_order, DF = argo_pred_now){sapply(1:length(v), function(X){DF[v[X],X]})}
    argo2_Ensumble.p[date_idx,] = index2D_temp()
  }
  #index2D_temp <- function(v = Ensumble_order, DF = argo_pred_now){sapply(1:length(v), function(X){DF[v[X],X]})}
  #argo2_Ensumble.p[date_idx,] = index2D_temp()
}
argo2_Ensumble.p[argo2_Ensumble.p<0] = 0
err_Ensumble.twostep <- argo2_Ensumble.p - truth_ALL
ARGOX_Ensumble_MSE = colMeans(na.omit(err_Ensumble.twostep)[index(na.omit(err.twostep_ALL))]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(na.omit(err.twostep_ALL))]^2)

ARGOX_Ensumble_MSE_old3 = ARGOX_Ensumble_MSE
argo2_Ensumble.p_old3 = argo2_Ensumble.p

####################################################################################################################################################################################
joint_states = names(argo.state.p)
joint_states = names(argo.state.p)[names(argo.state.p)%in%names(ili_state)] # Florida is not in ILI, HI and VT have too many zeros
state_In_Both_ILI_COVID = joint_states[joint_states%in%names(ili_state)]

LAG_PREDICT = weeks_ahead-1 #n_forward weeks
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

#--------------------------------------------------------------------------------------------------------
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
  if (LAG_PREDICT == 0){
    for (i in 1:impute_samples){
      Flu.ili_state_WeekAgg[[i]] = ili_state_imputList_WeekAgg[[i]][["1 week ahead"]]
      tmp_reg = lapply(state_names_JOINT, function(each_state){
                                  region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
                                  ili_reg_imputList_WeekAgg[[i]][["1 week ahead"]][,region_id_for_state]
                                })
      Flu.ili_reg_WeekAgg[[i]] = do.call(merge, tmp_reg)
      names(Flu.ili_reg_WeekAgg[[i]]) <- state_names_JOINT
    }
  } else{
    for (i in 1:impute_samples){
      Flu.ili_state_WeekAgg[[i]] = ili_state_imputList_WeekAgg[[i]][["2 week ahead"]]
      tmp_reg = lapply(state_names_JOINT, function(each_state){
                                region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
                                ili_reg_imputList_WeekAgg[[i]][["2 week ahead"]][,region_id_for_state]
                              })
      Flu.ili_reg_WeekAgg[[i]] = do.call(merge, tmp_reg)
      names(Flu.ili_reg_WeekAgg[[i]]) <- state_names_JOINT
    }
  }
} else{
  impute_samples = 1
  if (LAG_PREDICT == 0){
    Flu.ili_state_WeekAgg[[1]] = ili_state_noImpute[["1 week ahead"]]
    tmp_reg = lapply(state_names_JOINT, function(each_state){
                                region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
                                ili_reg_noImpute[["1 week ahead"]][,region_id_for_state]
                              })
    Flu.ili_reg_WeekAgg[[1]] = do.call(merge, tmp_reg)
    names(Flu.ili_reg_WeekAgg[[1]]) <- state_names_JOINT
  } else{
    Flu.ili_state_WeekAgg[[1]] = ili_state_noImpute[["2 week ahead"]]
    tmp_reg = lapply(state_names_JOINT, function(each_state){
                                region_id_for_state = state_info[Abbre==strsplit(each_state, "-")[[1]][2], Region]
                                ili_reg_noImpute[["2 week ahead"]][,region_id_for_state]
                              })
    Flu.ili_reg_WeekAgg[[1]] = do.call(merge, tmp_reg)
    names(Flu.ili_reg_WeekAgg[[1]]) <- state_names_JOINT
  }
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
training_period = 90  #COVID-19 use 70 days, 10 weeks training period

if_use_neighbour_COVID = TRUE
if_use_flu_region = TRUE
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
apply(argo2_useFlu_JOINT_MSE,2,which.min)
argo2.p_useFluIDVstate_impute100_cases

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Projection Matrix Visualization
library(ggplot2)
library(cowplot)
library(dplyr)
library(egg)
library(RColorBrewer)
library(ggpubr)
library(patchwork)

pdf(file="test1.pdf")
for (state_id_for_state in 1:length(names(projection.mat_ALL))){
  state_name = names(projection.mat_ALL)[state_id_for_state]
  region_id_for_state = state_info[Abbre==strsplit(state_name, "-")[[1]][2], Region]
  neighbour_states = paste0("US-", state_info[Region==region_id_for_state]$Abbre)
  neighbour_states = neighbour_states[neighbour_states%in%state_In_Both_ILI_COVID]
  if(if_use_flu_region && if_use_neighbour_COVID){
    colnames(projection.mat_ALL[[state_name]]) = c(paste0("GT-",neighbour_states), "Reg", "Nat", "Lag", paste0("ILIState-",neighbour_states), "ILIReg-")
  }else if (if_use_flu_region && !if_use_neighbour_COVID){
    colnames(projection.mat_ALL[[state_name]]) = c("GT", "Reg", "Nat", "Lag", paste0("ILIState-",neighbour_states), "ILIReg-")
  } else if (!if_use_flu_region && if_use_neighbour_COVID){
    colnames(projection.mat_ALL[[state_name]]) = c(paste0("GT-",neighbour_states), "Reg", "Nat", "Lag", "ILIState", "ILIReg-")
  }else{
    colnames(projection.mat_ALL[[state_name]]) = c("GT", "Reg", "Nat", "Lag","ILIState", "ILIReg-")
  }
  tmp_projection.mat = projection.mat_ALL[[state_name]]
  tmp_projection.mat = tmp_projection.mat
  tmp_projection.mat[tmp_projection.mat>0.1] = 0.1
  tmp_projection.mat[tmp_projection.mat<(-.1)] = -0.1
  temp_heatmap = matrix(NA,length(tmp_projection.mat),3)
  temp_heatmap[,3] = as.numeric(matrix(tmp_projection.mat,length(tmp_projection.mat),1))
  temp_heatmap[,1] = rep(as.character(rownames(tmp_projection.mat)), dim(tmp_projection.mat)[2])
  temp_heatmap[,2] = rep(colnames(tmp_projection.mat),each=dim(tmp_projection.mat)[1])
  temp_heatmap = data.frame(temp_heatmap)
  temp_heatmap[,2] = factor(temp_heatmap[,2], levels = rev(colnames(tmp_projection.mat)))
  temp_heatmap[,3] = as.numeric(as.character(temp_heatmap[,3]))
  colnames(temp_heatmap) = c("Z", "W_Predictors",  "Coefficients")
  
  Nat_Coef_HeatMap = temp_heatmap %>% ggplot(aes(x = Z, y = W_Predictors, fill = Coefficients)) + 
    scale_fill_gradient2(low = "blue",mid = "white" ,high="red", na.value = "grey", midpoint = 0,  limits=c(min(temp_heatmap[,3]),max(temp_heatmap[,3])))+
    geom_tile()  + theme(legend.position = "bottom") + geom_hline(aes(yintercept=27.5), colour="black", linetype="dashed")+
    theme(text = element_text(size=6), axis.text.x = element_text(angle=90, hjust=1)) 
  print(plot(Nat_Coef_HeatMap, main = state_id_for_state))
}
dev.off()


####################################################################################################################################################################################
# IF Predicting Each Region Jointly
####################################################################################################################################################################################
training_period = 90  #COVID-19 use 70 days, 10 weeks training period
if_use_flu_nat = FALSE
if_use_flu_region = TRUE
if_linearcombo_rho = FALSE

projection.mat <- mean.mat <- list()
projection.mat_ALL = list()
projection.mat_ALL_allImput = list()
Y.pred <- X
Y.pred[] <- NA
Y.pred_list = list()
zw_used <- fitting_error <- sigma_ww.structured <- sigma_ww.empirical <- sigma_zw.structured <- 
  sigma_zw.empirical <- heat.vec.structured <- sigma_zwzw.structured <- sigma_zwzw.empirical <- list() 

rho_all = list()

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
  
  for (region_id_for_state in 1:max(state_info[,Region])){
    region_id_char = paste0("Region-",region_id_for_state)
    print(region_id_char)
    neighbour_states = paste0("US-", state_info[Region==region_id_for_state]$Abbre)
    neighbour_states = neighbour_states[neighbour_states%in%state_In_Both_ILI_COVID]
    print(neighbour_states)
    if (if_use_flu_region){
      projection.mat_ALL[[region_id_char]] = matrix(0, length(neighbour_states), length(neighbour_states)*6)
    }else{
      projection.mat_ALL[[region_id_char]] = matrix(0, length(neighbour_states), length(neighbour_states)*5)
    }
    #projection.mat_ALL[[region_id_char]] = matrix(0, length(neighbour_states), length(neighbour_states)*4)

    for(it in (training_period+7*(1+LAG_PREDICT)):length(common_idx) ){ #Note n_training in ARGO is 104
      training_idx <- common_idx[(it-training_period-7*(1+LAG_PREDICT)+1):(it-7*(1+LAG_PREDICT))] #get training dates/period (total of fixed 104 days), same idea as first step (have a training period for prediction at t, which is why a "-1'at the end of range)
      t.now <- common_idx[it] #The time step t that we predict (testing upon training)
      
      y <- Y[training_idx, neighbour_states]  #get Z_t=p_t-p_{t-1}
      x <- X[training_idx, neighbour_states]
      x.nat <- X.nat[training_idx, neighbour_states] #get p^{nat}_t-p_{t-1}
      x.reg <- X.reg[training_idx, neighbour_states] #get p^{reg}_t-p_{t-1}
      yt2 <- Yt2[training_idx, neighbour_states] #get Z_{t-1}
      
      x.flu.state = Flu.state[training_idx, neighbour_states]
      x.flu.state_curr = Flu.state_curr[training_idx, neighbour_states]
      x.flu.reg = Flu.reg[training_idx, neighbour_states]

      sigma_yy <- var(y) #compute sigma_zz
      sigma_yyF = cov(y, x.flu.state_curr)
      sigma_yFyF = var(x.flu.state)
      
      m1 <- cor(y, yt2) #??First, we are assuming each t (rows) follow a multivariate distribution, but here is computing according to column?
      m2 <- cor(y)  #Also, This way of cumpting ACF for one lag doesn't seem to be correct? Isn't it just computing sample correlation and scale by variance?
      rho.l2 <- sum(m1*m2)/sum(m2^2) #rho, where cor(Z_t,Z_{t-1})=rho*sigma_zz????  
      autocov.y.yt2 <- rho.l2*sigma_yy #get \rho*sigma_ZZ, one step ACF
      
      m1_F = cor(y, x.flu.state)
      m2_F = cor(y, x.flu.state_curr)
      rho_F <- sum(m1_F*m2_F)/sum(m2_F^2)
      autocov.y.yF = rho_F*sigma_yyF
      
      if (if_linearcombo_rho){
        m1_F = matrix(m1_F, dim(m1_F)[1]*dim(m1_F)[2], 1)
        m2_F = matrix(m2_F, dim(m2_F)[1]*dim(m2_F)[2], 1)
        m3_F = matrix(cor(x.flu.state), dim(sigma_yFyF)[1]*dim(sigma_yFyF)[2], 1)
        m_23_F = cbind(m2_F, m3_F)
        rho_1_2 = solve(t(m_23_F)%*%m_23_F, t(m_23_F)%*%m1_F)
        autocov.y.yF = rho_1_2[1]*sigma_yyF + rho_1_2[2]*sigma_yFyF
        rho_all[[region_id_char]][[as.character(t.now)]] = rbind(rho_F, rho_1_2)
      }
      
      vcov.x_xreg_xnat <- cbind(rbind(sigma_yy+var((argo.state.p_JOINT - truth_JOINT)[training_idx,neighbour_states]),sigma_yy, sigma_yy),
                                rbind(sigma_yy,sigma_yy+var((argo.reg.p.dup - truth_JOINT)[training_idx,neighbour_states]), sigma_yy),
                                rbind(sigma_yy,sigma_yy,sigma_yy+var((as.numeric(argo.nat.p) - truth_JOINT)[training_idx,neighbour_states]) ) )
      vcov.x_xreg_xnat <- cbind(vcov.x_xreg_xnat, rbind(autocov.y.yt2, autocov.y.yt2, autocov.y.yt2) ) 
      vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yt2),t(autocov.y.yt2),t(autocov.y.yt2),sigma_yy))
      vcov.x_xreg_xnat = cbind(vcov.x_xreg_xnat, rbind(autocov.y.yF, autocov.y.yF, autocov.y.yF, sigma_yyF) )
      vcov.x_xreg_xnat <- rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yF),t(autocov.y.yF),t(autocov.y.yF),t(sigma_yyF), sigma_yFyF))
      
      sigma_zw <- cbind(sigma_yy,sigma_yy,sigma_yy, autocov.y.yt2) 
      sigma_zw = cbind(sigma_zw, autocov.y.yF) 
      
      #if (FALSE){
      if (if_use_flu_region){
        vcov.x_xreg_xnat = cbind(vcov.x_xreg_xnat, rbind(autocov.y.yF, autocov.y.yF, autocov.y.yF, sigma_yyF, sigma_yFyF) )
        vcov.x_xreg_xnat = rbind(vcov.x_xreg_xnat, cbind(t(autocov.y.yF),t(autocov.y.yF),t(autocov.y.yF),t(sigma_yyF), sigma_yFyF, sigma_yFyF))
        sigma_zw = cbind(sigma_zw, autocov.y.yF) 
        
        w_pred = c(t(X[t.now,neighbour_states])-colMeans(x), t(X.reg[t.now,neighbour_states])-colMeans(x.reg), 
                   t(X.nat[t.now,neighbour_states])-colMeans(x.nat), t(Yt2[t.now,neighbour_states]-colMeans(yt2)), 
                   t(cbind(Flu.state[t.now, neighbour_states], Flu.reg[t.now, neighbour_states])) - colMeans(x.flu.state))
        w_vec =  t(rbind((t(x)-colMeans(x)), (t(x.reg)-colMeans(x.reg)), (t(x.nat)-colMeans(x.nat)), (t(yt2)-colMeans(yt2)), 
                         t(x.flu.state) - colMeans(x.flu.state), t(x.flu.reg) - colMeans(x.flu.reg)))
      } else{
        w_pred = c(t(X[t.now,neighbour_states])-colMeans(x), t(X.reg[t.now,neighbour_states])-colMeans(x.reg), 
                   t(X.nat[t.now,neighbour_states])-colMeans(x.nat), t(Yt2[t.now,neighbour_states]-colMeans(yt2)), 
                   t(Flu.state[t.now, neighbour_states]) - colMeans(x.flu.state))
        w_vec =  t(rbind((t(x)-colMeans(x)), (t(x.reg)-colMeans(x.reg)), (t(x.nat)-colMeans(x.nat)), (t(yt2)-colMeans(yt2)), 
                         t(x.flu.state) - colMeans(x.flu.state)))
      }
      #}
      #w_pred = c(t(X[t.now,neighbour_states])-colMeans(x), t(X.reg[t.now,neighbour_states])-colMeans(x.reg), 
                 #t(X.nat[t.now,neighbour_states])-colMeans(x.nat), t(Yt2[t.now,neighbour_states]-colMeans(yt2)))
     # w_vec =  t(rbind((t(x)-colMeans(x)), (t(x.reg)-colMeans(x.reg)), (t(x.nat)-colMeans(x.nat)), (t(yt2)-colMeans(yt2))))
      
      y.pred <- colMeans(y) + sigma_zw %*% solve(weight_diag * vcov.x_xreg_xnat + (1-weight_diag) * diag(diag(vcov.x_xreg_xnat)), w_pred)
      y.fitted <- colMeans(y) + sigma_zw %*% solve(weight_diag * vcov.x_xreg_xnat + (1-weight_diag) * diag(diag(vcov.x_xreg_xnat)), t(w_vec) )
      Y.pred[t.now, neighbour_states] <- y.pred
      
      projection.mat[[as.character(t.now)]] <- sigma_zw %*% solve(weight_diag * vcov.x_xreg_xnat + (1-weight_diag) * diag(diag(vcov.x_xreg_xnat)) )
      projection.mat_ALL[[region_id_char]] = projection.mat_ALL[[region_id_char]]+projection.mat[[as.character(t.now)]]
      mean.mat[[as.character(t.now)]] <- c(colMeans(y), colMeans(x), colMeans(x.reg), colMeans(x.nat), colMeans(yt2), colMeans(x.flu.state))
      
      #Store everything in a list where each element contains all stuff for the forecast of time step t and its training time steps
      #sigma_ww <- vcov.x_xreg_xnat 
      #sigma_zz <- sigma_yy
      #sigma_ww.structured[[region_id_char]][[as.character(t.now)]] <- sigma_ww #structured is using independence assumption and derived 
      #sigma_ww.empirical[[region_id_char]][[as.character(t.now)]] <- var(cbind(X[training_idx, neighbour_states], X.reg[training_idx, neighbour_states], 
                                                                             #X.nat[training_idx, neighbour_states], Yt2[training_idx, neighbour_states], 
                                                                             #Flu.state[training_idx, neighbour_states]))
      #sigma_zw.structured[[region_id_char]][[as.character(t.now)]] <- sigma_zw
      #sigma_zw.empirical[[region_id_char]][[as.character(t.now)]] <- cov(Y[training_idx,], 
                                                                       #cbind(X[training_idx, neighbour_states], X.reg[training_idx, neighbour_states], 
                                                                             #X.nat[training_idx, neighbour_states], Yt2[training_idx, neighbour_states], 
                                                                             #Flu.state[training_idx, neighbour_states]))
      
      #sigma_zwzw.structured[[region_id_char]][[as.character(t.now)]] <- rbind( #The huge covariance matrix of Z (1 "element") and W (4 "elemtns")
        #cbind(sigma_zz, sigma_zw),
        #cbind(t(sigma_zw), sigma_ww)
      #)
      #sigma_zwzw.empirical[[region_id_char]][[as.character(t.now)]] <- var(cbind(Y[training_idx, neighbour_states], X[training_idx, neighbour_states], X.reg[training_idx, neighbour_states], 
                                                                               #X.nat[training_idx, neighbour_states], Yt2[training_idx, neighbour_states], 
                                                                               #Flu.state[training_idx, neighbour_states]))
      #zw_used[[region_id_char]][[as.character(t.now)]] <- cbind(Y[training_idx, neighbour_states], X[training_idx, neighbour_states], X.reg[training_idx, neighbour_states], 
                                                              #X.nat[training_idx, neighbour_states], Yt2[training_idx, neighbour_states], 
                                                              #Flu.state[training_idx, neighbour_states])
      #fitting_error[[region_id_char]][[as.character(t.now)]] <- y - t(y.fitted) 
    }
    projection.mat_ALL[[region_id_char]] = projection.mat_ALL[[region_id_char]]/length((training_period+7*(1+LAG_PREDICT)):length(common_idx))
  }
  Y.pred_list[[impute_iter]] = Y.pred
  projection.mat_ALL_allImput[[as.character(impute_iter)]] = projection.mat_ALL
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

Ensemble_order_impute = apply(argo2_useFlu_JOINT_MSE,2,which.min)
argox_regFlu_Ensemble.p = argo2.p_useFlu[[1]]
for (i in names(Ensemble_order_impute)){
  argox_regFlu_Ensemble.p[,i] = argo2.p_useFlu[[Ensemble_order_impute[i]]][,i]
}
argox_regFlu_Ensemble.p[argox_regFlu_Ensemble.p<0] = 0
argox_regFlu_Ensemble.p = na.omit(argox_regFlu_Ensemble.p)
err_Ensemble_impute <- argox_regFlu_Ensemble.p - truth_JOINT_ActualScale[index(argox_regFlu_Ensemble.p), ]
Ensemble_impute_MSE = colMeans(na.omit(err_Ensemble_impute)[index(na.omit(err_Ensemble_impute))]^2) / colMeans(na.omit(naive.p_ActualScale - truth_JOINT_ActualScale)[index(na.omit(err_Ensemble_impute))]^2)


if (FALSE){
  # Get the final State-by-State-withFlu Prediction
  ensemble_training = 30
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
  
  argo2_Ind_useFlu = cbind(argo2_Ind_useFlu, argo.state.p_ALL[,c('US-FL')])
  colnames(argo2_Ind_useFlu) = gsub("US.", "US-", colnames(argo2_Ind_useFlu))
  argo2_Ind_useFlu = argo2_Ind_useFlu[,colnames(argo.state.p_ALL)]
  
  # Get new 5 method ensemble
  truth_ALL = argo.state.true
  naive.p_ALL <- truth_ALL 
  index(naive.p_ALL) <- index(truth_ALL) + 7*(1+LAG_PREDICT)
  ARGOX_Idv_withFlu_MSE = colMeans(na.omit(argo2_Ind_useFlu - truth_ALL)[index(na.omit(argo2_Ind_useFlu))]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL )[index(na.omit(argo2_Ind_useFlu))]^2)
  Ensumble_order = apply(rbind(ARGO_ALL_MSE,ARGOX_ALL_MSE, ARGOX_NatConstraint_MSE, ARGOX_Idv_withFlu_MSE),2,which.min)
  
  argo2_Ensumble.p = argo.state.p_ALL
  for (t.now in index(na.omit(err.twostep_ALL))){
    date_idx = as.Date(t.now)
    argo_pred_now = rbind( argo.state.p_ALL[date_idx,], argo2.p_ALL[date_idx, ], argo2_NatConstraint.p[date_idx, ], argo2_Ind_useFlu[date_idx,]) 
    if (t.now %% 3 == 0){
      argo2_Ensumble.p[date_idx,] = apply(abs(argo_pred_now -   matrix(rep(t(truth_ALL[date_idx, ]),each =4),nrow=4)), 2, min) + truth_ALL[date_idx, ]
      
    } else{
      index2D_temp <- function(v = Ensumble_order, DF = argo_pred_now){sapply(1:length(v), function(X){DF[v[X],X]})}
      argo2_Ensumble.p[date_idx,] = index2D_temp()
    }
    #index2D_temp <- function(v = Ensumble_order, DF = argo_pred_now){sapply(1:length(v), function(X){DF[v[X],X]})}
    #argo2_Ensumble.p[date_idx,] = index2D_temp()
  }
  argo2_Ensumble.p[argo2_Ensumble.p<0] = 0
  err_Ensumble.twostep <- argo2_Ensumble.p - truth_ALL
  ARGOX_Ensumble_MSE = colMeans(na.omit(err_Ensumble.twostep)[index(na.omit(err.twostep_ALL))]^2) / colMeans(na.omit(naive.p_ALL - truth_ALL)[index(na.omit(err.twostep_ALL))]^2)
  
  ARGOX_Ensumble_MSE_new = ARGOX_Ensumble_MSE
  argo2_Ensumble.p_new = argo2_Ensumble.p
  
  
  
  argo.state.p_10102021 = list()
  argo2.p_ALL_10102021 = list()
  argo2_NatConstraint.p_10102021 = list()
  argo2_Ind_useFlu_10102021 = list()
  argo2_Ensumble.p_old3_10102021 = list()
  argo2_Ensumble.p_new_10102021 = list()
  
  weeks_ahead_string = paste0(weeks_ahead, " week ahead")
  
  argo.state.p_10102021[[weeks_ahead_string]] = argo.state.p
  argo2.p_ALL_10102021[[weeks_ahead_string]] = argo2.p_ALL
  argo2_NatConstraint.p_10102021[[weeks_ahead_string]] = argo2_NatConstraint.p
  argo2_Ind_useFlu_10102021[[weeks_ahead_string]] = argo2_Ind_useFlu
  argo2_Ensumble.p_old3_10102021[[weeks_ahead_string]] = argo2_Ensumble.p_old3
  argo2_Ensumble.p_new_10102021[[weeks_ahead_string]] = argo2_Ensumble.p_new
  
  save(argo.state.p_10102021,
       argo2.p_ALL_10102021,
       argo2_NatConstraint.p_10102021,
       argo2_Ind_useFlu_10102021,
       argo2_Ensumble.p_old3_10102021,
       argo2_Ensumble.p_new_10102021,
       truth_ALL, file = "Case_Step2_10102021.rda")
}

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Projection Matrix Visualization
library(ggplot2)
library(cowplot)
library(dplyr)
library(egg)
library(RColorBrewer)
library(ggpubr)
library(patchwork)

pdf(file="test1.pdf")
projection.mat_ALL = tmp_prob_mat
for (region_id_for_state in 1:max(state_info[,Region])){
  neighbour_states = paste0("US-", state_info[Region==region_id_for_state]$Abbre)
  neighbour_states = neighbour_states[neighbour_states%in%state_In_Both_ILI_COVID]
  num_neighbour = length(neighbour_states)
  if(if_use_flu_region){
    colnames(projection.mat_ALL[[region_id_for_state]]) = c(paste0("GT-",neighbour_states), 
                                                            paste0("Reg-",neighbour_states), 
                                                            paste0("Nat-",neighbour_states), 
                                                            paste0("Lag-",neighbour_states), 
                                                            paste0("ILIState-",neighbour_states), 
                                                            paste0("ILIReg-",neighbour_states))
  }else{
    colnames(projection.mat_ALL[[region_id_for_state]]) = c(paste0("GT-",neighbour_states), 
                                                            paste0("Reg-",neighbour_states), 
                                                            paste0("Nat-",neighbour_states), 
                                                            paste0("Lag-",neighbour_states), 
                                                            paste0("ILIState-",neighbour_states))
  }
  tmp_scale = 8
  tmp_projection.mat = projection.mat_ALL[[region_id_for_state]]
  tmp_projection.mat[,(4*num_neighbour+1):dim(tmp_projection.mat)[2]] = tmp_projection.mat[,(4*num_neighbour+1):dim(tmp_projection.mat)[2]]*tmp_scale
  
  tmp_projection.mat = tmp_projection.mat
  tmp_projection.mat[tmp_projection.mat>0.1] = 0.1
  tmp_projection.mat[tmp_projection.mat<(-.1)] = -0.1
  temp_heatmap = matrix(NA,length(tmp_projection.mat),3)
  temp_heatmap[,3] = as.numeric(matrix(tmp_projection.mat,length(tmp_projection.mat),1))
  temp_heatmap[,1] = rep(as.character(rownames(tmp_projection.mat)), dim(tmp_projection.mat)[2])
  temp_heatmap[,2] = rep(colnames(tmp_projection.mat),each=dim(tmp_projection.mat)[1])
  temp_heatmap = data.frame(temp_heatmap)
  temp_heatmap[,2] = factor(temp_heatmap[,2], levels = rev(colnames(tmp_projection.mat)))
  temp_heatmap[,3] = as.numeric(as.character(temp_heatmap[,3]))
  colnames(temp_heatmap) = c("Z", "W_Predictors",  "Coefficients")
  
  Nat_Coef_HeatMap = temp_heatmap %>% ggplot(aes(x = Z, y = W_Predictors, fill = Coefficients)) + 
    scale_fill_gradient2(low = "blue",mid = "white" ,high="red", na.value = "grey", midpoint = 0,  limits=c(min(temp_heatmap[,3]),max(temp_heatmap[,3])))+
    geom_tile()  + theme(legend.position = "bottom") + geom_hline(aes(yintercept=27.5), colour="black", linetype="dashed")+
    theme(text = element_text(size=6), axis.text.x = element_text(angle=90, hjust=1)) 
  print(plot(Nat_Coef_HeatMap, main = region_id_for_state))
}
dev.off()

