## 20210726
## ARGO step 2, individual state, with covid data
## 20210810
## ARGO with covid cases
options(echo=TRUE)
library(xts)
library(glmnet)
library(argo)
library(parallel)
library(boot)
library(abind)

current_dic = "~/"
GTpub_all <- "api_raw_results-2022-09-11"
population.file <- paste0(current_dic, "Population.csv")
GTpub_version <- paste0(GTpub_all, "/")
gt.folder <- paste0(current_dic, GTpub_all, "/")

save.folder <- paste0("results/RegionalX_4x4_GTpub/", ili_version, GTpub_all)
dir.create(save.folder, showWarnings = FALSE, recursive = TRUE)

out.folder <- paste0(ili.folder, GTpub_version)
if(!exists(out.folder)){
  dir.create(out.folder, showWarnings = FALSE, recursive = TRUE)
}  

gt.parser.cur <- argo:::gt.parser.pub.api

source("ILI_COVID_Data_Clean.R")
state_data <- load_reg_data(gt.folder=gt.folder,
                            ili.folder=ili.folder,
                            population.file=population.file,
                            gft.file="GFT.txt",
                            gt.parser = gt.parser.cur)
print("Data input done.")

ili_national <- state_data$ili_national
ili_state <- state_data$ili_state
ili_regional <- state_data$ili_regional

states.abb.all <- colnames(ili_state)
states.rm <- c("US.FL", "US.Virgin Islands",
               "US.Puerto Rico", "US.Commonwealth of the Northern Mariana Islands")
states.abb.all <- states.abb.all[-match(c(states.rm),states.abb.all)]
ili_state <- ili_state[, states.abb.all]

GT_national <- state_data$GT_national
GT_state <- state_data$GT_state
GT_regional <- state_data$GT_regional

GT_state[["US"]] <- NULL
GT_state[["US.FL"]] <- NULL

terms = names(GT_national)

##########################################################################################################################################################
# National
##########################################################################################################################################################
temp = na.omit(covid_national_level)
temp <- as.xts(temp)
index(temp) = index(temp) +1
temp = apply.weekly(temp,colSums) 
temp = xts(temp, as.Date(index(temp)))  
index(temp) = index(temp) - 8 #Now making index Saturdays. i.e. 5/2 store 5/3+...+5/9
covid_weekly_nat = temp[-dim(temp)[1],]
covid_weekly_nat_cases = na.omit(stats::lag(covid_weekly_nat[,1], c(0,7)))
colnames(covid_weekly_nat_cases) = c('case_lag0', 'case_lag7')

common_idx <- index(merge(ili_national, GT_national, all=FALSE))
common_idx <- common_idx[common_idx > as.Date("2010-05-22")]
set.seed(1000)

# national 
idx.nat1 <- c(index(GT_national)[1]-(52:1)*7, index(GT_national))
GT_national1 <- xts(matrix(nrow=length(idx.nat1), ncol=ncol(GT_national)), order.by = as.Date(idx.nat1))
colnames(GT_national1) <- colnames(GT_national)
GT_national1[index(GT_national), ] <- GT_national
common_idx_nat <- c(common_idx[1]-(52:1)*7, common_idx)

argo_nat_noCovid <- argo(data = transY(ili_national[common_idx_nat]),
                         exogen = log(GT_national1[common_idx_nat, terms]+1), N_lag=1:52, mc.cores = 8, N_training = 52)
GT_nat_withCOVIDcase = cbind(log(GT_national1[common_idx_nat, terms]+1), covid_weekly_nat_cases)
tmp = as.data.frame(GT_nat_withCOVIDcase)
tmp[is.na(tmp)] = 0
GT_nat_withCOVIDcase = xts(tmp, index(GT_nat_withCOVIDcase))
argo_nat_withCovid <- argo(data = transY(ili_national[common_idx_nat]),
                           exogen = GT_nat_withCOVIDcase[common_idx_nat,], N_lag=1:52, mc.cores = 8, N_training = 52)

argo_nat_noCovid_pred = inv_transY(argo_nat_noCovid$pred)
argo_nat_withCovid_pred = inv_transY(argo_nat_withCovid$pred)

naive = ili_national
index(naive) = index(naive) +21
rMSE_noCovid = mean(abs(argo_nat_noCovid_pred-ili_national)['2020-07/']^2)/mean(abs(naive-ili_national)['2020-07/']^2)
rMSE_withCovid = mean(abs(argo_nat_withCovid_pred-ili_national)['2020-07/']^2)/mean(abs(naive-ili_national)['2020-07/']^2)

ar_3 = argo(data = transY(ili_national), exogen = NULL, N_lag = 1:3, N_training = 104, alpha = NA, mc.cores = 8)
ar_3 = inv_transY(ar_3$pred)


##########################################################################################################################################################
## state level
##########################################################################################################################################################
nyt_states <- unique(tab_covid_case_state$state)
covid_case_state_daily_incre = matrix(NA, ncol= sum(sum(nyt_states%in%tab.states$State)), nrow=length(unique(unique(tab_covid_case_state$date))))
colnames(covid_case_state_daily_incre) <- names(covid_nowcast)
covid_case_state_daily_incre <- xts(covid_case_state_daily_incre, order.by = as.Date(unique(tab_covid_case_state$date)))

for(i in 1:dim(covid_case_state_daily_incre)[2]){
  state_curr <- as.character(tab.states$State[tab.states$Abbre_names == names(covid_nowcast)[i]])
  cases_curr <- tab_covid_case_state[tab_covid_case_state$state==state_curr, c("date", "cases")]
  covid_case_state_daily_incre[as.Date(cases_curr$date), names(covid_nowcast)[i]] <- cases_curr$cases
}

covid_case_state_daily_incre <- apply(covid_case_state_daily_incre, 2, diff)

temp = na.omit(covid_case_state_daily_incre)
temp <- as.xts(temp)
index(temp) = index(temp) +1
temp = apply.weekly(temp,colSums) 
temp = xts(temp, as.Date(index(temp)))  
index(temp) = index(temp) - 8 #Now making index Saturdays. i.e. 5/2 store 5/3+...+5/9
covid_case_state_weekly_incre = temp


#save(covid_case_state_weekly_incre, covid_case_nat_weekly_incre, file=file.path(covid_folder, "covid_weekly_cases_NYTimes.Rdata"))

### regional level covid cases
covid_case_reg_weekly_incre <- sapply(1:10, function(x){apply(covid_case_state_weekly_incre[, tab.states$Abbre_names[(tab.states$Region==x)&(tab.states$Abbre_names%in%colnames(covid_case_state_weekly_incre))]], 1, sum)})
colnames(covid_case_reg_weekly_incre) <- paste0("Region", 1:10)
covid_case_reg_weekly_incre <- xts(covid_case_reg_weekly_incre, order.by = as.Date(rownames(covid_case_reg_weekly_incre)))


##############
## preparing ARGOX flu predictions (first-step) for 2nd step aggregation
truth <-  ili_state
naive.p <- lag(truth, 1)
argo1.p <- argo.state.pred
common_idx_flu <- zoo::index(na.omit(merge(truth, naive.p, argo1.p, 
                                       argo.nat.p,argo.reg.pred)))
## national level
if (ncol(argo.nat.p) == 1) {
  argo.nat.p <- do.call(cbind, lapply(1:ncol(argo1.p), function(i) argo.nat.p))
}
colnames(argo.nat.p) <- colnames(truth)
## regional level (first step)
argo.reg.p <- do.call(cbind, lapply(colnames(truth), function(x){argo.reg.pred[,tab.states$Region[tab.states$Abbre_names_flu==x]]}))
colnames(argo.reg.p) <- colnames(truth)

## common dates with covid data
if(idx_death){
  common_idx_covid <- as.Date(intersect(index(na.omit(covid_nowcast[[1]])), common_idx_flu-7))
} else{
  common_idx_covid <- as.Date(intersect(index(covid_case_state_weekly_incre), common_idx_flu-7))
}
length(common_idx_covid)


## creating output variables to save predicitons
## covid pred output
## covid 1 week ahead sat of previous week
argo2_pred_covid <- matrix(nrow = length(common_idx_covid), ncol=length(covid_nowcast))
argo2_pred_covid <- xts(argo2_pred_covid, order.by = common_idx_covid-7)
colnames(argo2_pred_covid) <- names(covid_nowcast)
## removing FL
argo2_pred_covid <- argo2_pred_covid[, -which(names(covid_nowcast)=="US-FL")]
var_est_ind_covid <- argo2_pred_covid
var_est_ind_covid[] <- NA

## flu pred output
## flu ending sat
argo2_pred_flu <- xts(argo2_pred_covid, order.by = common_idx_covid)
colnames(argo2_pred_flu) <- tab.states$Abbre_names_flu[match(colnames(argo2_pred_covid), tab.states$Abbre_names)]
argo2_pred_flu[] <- NA
var_est_ind_flu <- argo2_pred_flu 
var_est_ind_flu[] <- NA


##### ARGOX 2nd step
#### individual states
## training length, in weeks
n_training <- 12

idx_percent <- T ## whether transform counts to percents 
pct_scale <- 1 ## scaling factor for percnets

w <- 0.15 ## adjust weights between diagonal and structured cov in 2nd step


idx_flu_only <- T ## only predicting flu?
idx_argo_covid_pred <- F ## predictors including argo covid predictions?
idx_covid_current <- T ## predictors including current week covid cases?
idx_covid_ts <- F ## hitorical time series of covid cases? t-1,t-2
idx_ts_covid_pred <- F ## predictors include covid death ts?
idx_ts_covid_cases <- T   ## predictors include covid cases ts?
idx_nat_ts <- F ## national covid
idx_inreg_ts <- T ## other states within region, covid
idx_outreg_ts <- F ## other 9 outside regions, covid
idx_reg_ts <- F ## regional covid
idx_reg_flu_ts <- T  ## regional flu
idx_inreg_flu_ts <- F ## other states within region, flu

for(idx.state.select in 1:nrow(tab.states)){
  ## get state names
  name_state_covid <- tab.states$Abbre_names[idx.state.select]
  name_state_flu <- tab.states$Abbre_names_flu[idx.state.select]
  ## get region name
  idx_region <- tab.states$Region[idx.state.select]
  ## get other states in the region
  name_states_reg_covid <- tab.states$Abbre_names[tab.states$Region == idx_region]
  name_states_reg_covid <- name_states_reg_covid[(name_states_reg_covid!=name_state_covid) & (name_states_reg_covid%in%colnames(argo2_pred_covid))]
  name_states_reg_flu <- tab.states$Abbre_names_flu[tab.states$Region == idx_region]
  name_states_reg_flu <- name_states_reg_flu[(name_states_reg_flu!=name_state_flu) & (name_states_reg_flu%in%colnames(ili_state))]
  
  ## organize state-level data; response + predictors
  state_truth_covid_all <- lapply(names(covid_nowcast), function(xx){covid_nowcast[[xx]][,"truth"]})
  state_truth_covid_all <- do.call(cbind, state_truth_covid_all)
  colnames(state_truth_covid_all) <- names(covid_nowcast)
  
  state_truth_naive_all <- lapply(names(covid_nowcast), function(xx){covid_nowcast[[xx]][,"naive_period"]})
  state_truth_naive_all <- do.call(cbind, state_truth_naive_all)
  colnames(state_truth_naive_all) <- names(covid_nowcast)
  
  state_truth_argo_all <- lapply(names(covid_nowcast), function(xx){covid_nowcast[[xx]][,"naive_period"]})
  state_truth_argo_all <- do.call(cbind, state_truth_argo_all)
  colnames(state_truth_argo_all) <- names(covid_nowcast)
  
  ## organize regional data
  reg_truth_covid <- sapply(1:10, function(x){apply(state_truth_covid_all[, tab.states$Abbre_names[(tab.states$Region==x)&(tab.states$Abbre_names%in%colnames(state_truth_covid_all))]], 1, sum)})
  colnames(reg_truth_covid) <- paste0("Region", 1:10)
  reg_truth_covid <- xts(reg_truth_covid, order.by = as.Date(rownames(reg_truth_covid)))
  
  ## only consider states with covid predictions (exclude NYC, NYC just using ARGOX)  
  if(name_state_covid%in%colnames(argo2_pred_covid)){
    ## organizing data for targeting state
    state_truth_covid <- covid_nowcast[[name_state_covid]][,"truth"]
    state_naive_covid <- covid_nowcast[[name_state_covid]][,"naive_period"]
    state_argo_covid <- covid_nowcast[[name_state_covid]][,"argo"]
 
  
    state_truth_covid_reg <- state_truth_covid_all[, name_states_reg_covid]
    state_naive_covid_reg <- state_truth_naive_all[, name_states_reg_covid]
    state_argo_covid_reg <- state_truth_argo_all[, name_states_reg_covid]
    
    national_truth_covid <- covid_nowcast_nat[,"truth"]
    national_naive_covid <- covid_nowcast_nat[,"naive_period"]
    national_argo_covid <- covid_nowcast_nat[,"argo"]

    ## organize case counts
    if(idx_cases){
      state_truth_covid_case <- covid_case_state_weekly_incre[, name_state_covid]
      national_truth_covid_case <- covid_case_nat_weekly_incre
      state_truth_covid_reg_case <- covid_case_state_weekly_incre[, name_states_reg_covid]
      reg_truth_covid_case <- covid_case_reg_weekly_incre
    }
    
    ## transform counts to percents
    if(idx_percent){
      ## changing to *1000, similar scale
      state_truth_covid <- state_truth_covid / tab.states$Population[tab.states$Abbre_names_flu==name_state_flu] * 100 * pct_scale
      #print(name_state_covid)
      #print(summary(state_truth_covid))
      state_naive_covid <- state_naive_covid / tab.states$Population[tab.states$Abbre_names_flu==name_state_flu] * 100 * pct_scale
      state_argo_covid <- state_argo_covid / tab.states$Population[tab.states$Abbre_names_flu==name_state_flu] * 100 * pct_scale
      national_truth_covid <- national_truth_covid / sum(tab.states$Population[1:51]) * 100 * pct_scale
      national_argo_covid <- national_argo_covid / sum(tab.states$Population[1:51]) * 100 * pct_scale
      national_naive_covid <- national_naive_covid / sum(tab.states$Population[1:51]) * 100 * pct_scale
      state_truth_covid_reg <- sweep(state_truth_covid_reg, 2, tab.states$Population[match(name_states_reg_covid, tab.states$Abbre_names)], "/") * 100 * pct_scale
      state_naive_covid_reg <- sweep(state_naive_covid_reg, 2, tab.states$Population[match(name_states_reg_covid, tab.states$Abbre_names)], "/") * 100 * pct_scale
      state_argo_covid_reg <- sweep(state_argo_covid_reg, 2, tab.states$Population[match(name_states_reg_covid, tab.states$Abbre_names)], "/") * 100 * pct_scale
      reg_truth_covid <- sweep(reg_truth_covid, 2,  aggregate(Population ~ Region, data = tab.states, sum)$Population, "/") * 100 * pct_scale
      
      if(idx_cases){
        state_truth_covid_case <-state_truth_covid_case / tab.states$Population[tab.states$Abbre_names_flu==name_state_flu] * 100 * pct_scale
        national_truth_covid_case <- national_truth_covid_case / sum(tab.states$Population[1:51]) * 100 * pct_scale
        state_truth_covid_reg_case <- sweep(state_truth_covid_reg_case, 2, tab.states$Population[match(name_states_reg_covid, tab.states$Abbre_names)], "/") * 100 * pct_scale
        reg_truth_covid_case <- sweep(reg_truth_covid_case, 2, aggregate(Population ~ Region, data = tab.states, sum)$Population, "/") * 100 * pct_scale
      }
    }
    
    ## covid response var, various combo
    Z_covid <- (state_truth_covid - state_naive_covid)[common_idx_flu-7,drop=F]
    ## covid predictors "W"
    
    if(idx_argo_covid_pred){ ## include argo estimate of current week covid?
      W_covid <- merge(lag(Z_covid, 1), 
                       (state_argo_covid - state_naive_covid)[common_idx_flu-7, drop=F], 
                       (national_argo_covid - state_naive_covid)[common_idx_flu-7,  drop=F])
      if(idx_covid_current){
        W_covid <- merge(W_covid, Z_covid) 
      }
      if(idx_ts_covid_pred){
        W_covid <- merge(W_covid, state_naive_covid - lag(state_naive_covid, 1))[common_idx_flu-7, drop=F]
        if (idx_nat_ts){
          W_covid <- merge(W_covid, national_naive_covid - lag(national_naive_covid, 1))[common_idx_flu-7, drop=F]
        }
        if(idx_inreg_ts){
          W_covid <- merge(W_covid, state_naive_covid_reg - lag(state_naive_covid_reg, 1))[common_idx_flu-7, drop=F]
        }
      }
      
      if(idx_ts_covid_cases){
        W_covid <- merge(W_covid, lag(state_truth_covid_case, 1) - lag(state_truth_covid_case, 2))[common_idx_flu-7, drop=F]
        if(idx_covid_current){
          W_covid <- merge(W_covid, state_truth_covid_case-lag(state_truth_covid_case, 1))[common_idx_flu-7, drop=F]
        }
        if (idx_nat_ts){
          W_covid <- merge(W_covid, lag(national_truth_covid_case, 1) - lag(national_truth_covid_case, 2))[common_idx_flu-7, drop=F]
        }
        if (idx_inreg_ts){
          W_covid <- merge(W_covid, lag(state_truth_covid_reg_case, 1) - lag(state_truth_covid_reg_case, 2))[common_idx_flu-7, drop=F]
        }
      }
      

    } else {
      if(idx_ts_covid_pred){
        if(idx_covid_ts){
          W_covid <- merge(state_naive_covid - lag(state_naive_covid, 1))[common_idx_flu-7, drop=F]
          if (idx_nat_ts){
            W_covid <- merge(W_covid, national_naive_covid - lag(national_naive_covid, 1))[common_idx_flu-7, drop=F]
          }
          if(idx_inreg_ts){
            W_covid <- merge(W_covid, state_naive_covid_reg - lag(state_naive_covid_reg, 1))[common_idx_flu-7, drop=F]
          }
          if(idx_outreg_ts){
            W_covid <- merge(W_covid, lag(reg_truth_covid[,-idx_region], 1) - lag(reg_truth_covid[,-idx_region], 2))[common_idx_flu-7, drop=F]
          }
          if(idx_reg_ts){
            W_covid <- merge(W_covid, lag(reg_truth_covid[,idx_region], 1) - lag(reg_truth_covid[,idx_region], 2))[common_idx_flu-7, drop=F]
          }
        }
        if(idx_covid_current){
          W_covid <- merge(Z_covid)[common_idx_flu-7, drop=F]
          if (idx_nat_ts){
            W_covid <- merge(W_covid, national_truth_covid - lag(national_truth_covid, 1))[common_idx_flu-7, drop=F]
          }
          if(idx_inreg_ts){
            W_covid <- merge(W_covid, state_truth_covid_reg - lag(state_truth_covid_reg, 1))[common_idx_flu-7, drop=F]
          }
          if(idx_outreg_ts){
            W_covid <- merge(W_covid, reg_truth_covid[,-idx_region] - lag(reg_truth_covid[,-idx_region], 1))[common_idx_flu-7, drop=F]
          }
          if(idx_reg_ts){
            W_covid <- merge(W_covid, reg_truth_covid[,idx_region] - lag(reg_truth_covid[,idx_region], 1))[common_idx_flu-7, drop=F]
          }
        }
        if(idx_covid_current & idx_covid_ts){
          W_covid <- merge(Z_covid, state_naive_covid - lag(state_naive_covid, 1))[common_idx_flu-7, drop=F]
          if (idx_nat_ts){
            W_covid <- merge(W_covid, national_truth_covid - lag(national_truth_covid, 1), national_naive_covid - lag(national_naive_covid, 1))[common_idx_flu-7, drop=F]
          }
          if(idx_inreg_ts){
            W_covid <- merge(W_covid, state_truth_covid_reg - lag(state_truth_covid_reg, 1), state_naive_covid_reg - lag(state_naive_covid_reg, 1))[common_idx_flu-7, drop=F]
          }
          if(idx_outreg_ts){
            W_covid <- merge(W_covid, reg_truth_covid[,-idx_region] - lag(reg_truth_covid[,-idx_region], 1), lag(reg_truth_covid[,-idx_region], 1) - lag(reg_truth_covid[,-idx_region], 2))[common_idx_flu-7, drop=F]
          }
          if(idx_reg_ts){
            W_covid <- merge(W_covid, reg_truth_covid[,idx_region] - lag(reg_truth_covid[,idx_region], 1), lag(reg_truth_covid[,-idx_region], 1) - lag(reg_truth_covid[,-idx_region], 2))[common_idx_flu-7, drop=F]
          }
        }

      }
      
      if(idx_ts_covid_cases){
        if(idx_covid_current){
          W_covid <- merge(state_truth_covid_case - lag(state_truth_covid_case, 1))[common_idx_flu-7, drop=F]
          if (idx_nat_ts){
            W_covid <- merge(W_covid, national_truth_covid_case - lag(national_truth_covid_case, 1))[common_idx_flu-7, drop=F]
          }
          if (idx_inreg_ts){
            W_covid <- merge(W_covid, state_truth_covid_reg_case - lag(state_truth_covid_reg_case, 1))[common_idx_flu-7, drop=F]
          }
          if (idx_outreg_ts){
            W_covid <- merge(W_covid, reg_truth_covid_case[,-idx_region] - lag(reg_truth_covid_case[,-idx_region], 1))[common_idx_flu-7, drop=F]
          }
          if (idx_reg_ts){
            W_covid <- merge(W_covid, reg_truth_covid_case[,idx_region] - lag(reg_truth_covid_case[,idx_region], 1))[common_idx_flu-7, drop=F]
          }
          
        }
        if(idx_covid_ts){
          W_covid <- merge(lag(state_truth_covid_case, 1) - lag(state_truth_covid_case, 2))[common_idx_flu-7, drop=F]
          if (idx_nat_ts){
            W_covid <- merge(W_covid, lag(national_truth_covid_case, 1) - lag(national_truth_covid_case, 2))[common_idx_flu-7, drop=F]
          }
          if (idx_inreg_ts){
            W_covid <- merge(W_covid, lag(state_truth_covid_reg_case, 1) - lag(state_truth_covid_reg_case, 2))[common_idx_flu-7, drop=F]
          }
          if (idx_outreg_ts){
            W_covid <- merge(W_covid, lag(reg_truth_covid_case[,-idx_region],1) - lag(reg_truth_covid_case[,-idx_region], 2))[common_idx_flu-7, drop=F]
          }
          if (idx_reg_ts){
            W_covid <- merge(W_covid, lag(reg_truth_covid_case[,idx_region],1) - lag(reg_truth_covid_case[,idx_region], 2))[common_idx_flu-7, drop=F]
          }
        }
        if(idx_covid_ts & idx_covid_current){
          W_covid <- merge(state_truth_covid_case - lag(state_truth_covid_case, 1), lag(state_truth_covid_case, 1) - lag(state_truth_covid_case, 2))[common_idx_flu-7, drop=F]
          if (idx_nat_ts){
            W_covid <- merge(W_covid,  national_naive_covid - lag(national_naive_covid, 1), lag(national_truth_covid_case, 1) - lag(national_truth_covid_case, 2))[common_idx_flu-7, drop=F]
          }
          if (idx_inreg_ts){
            W_covid <- merge(W_covid,  state_naive_covid_reg - lag(state_naive_covid_reg, 1), lag(state_truth_covid_reg_case, 1) - lag(state_truth_covid_reg_case, 2))[common_idx_flu-7, drop=F]
          }
          if (idx_outreg_ts){
            W_covid <- merge(W_covid, reg_truth_covid_case[,-idx_region] - lag(reg_truth_covid_case[,-idx_region], 1), lag(reg_truth_covid_case[,-idx_region],1) - lag(reg_truth_covid_case[,-idx_region], 2))[common_idx_flu-7, drop=F]
          }
          if (idx_reg_ts){
            W_covid <- merge(W_covid, reg_truth_covid_case[,idx_region] - lag(reg_truth_covid_case[,idx_region], 1), lag(reg_truth_covid_case[,-idx_region],1) - lag(reg_truth_covid_case[,-idx_region], 2))[common_idx_flu-7, drop=F]
          }
        }

      }
      
      if(idx_ts_covid_pred & idx_ts_covid_cases){
        W_covid <- merge(state_naive_covid - lag(state_naive_covid, 1),
                         lag(state_truth_covid_case, 1) - lag(state_truth_covid_case, 2))[common_idx_flu-7, drop=F]
        if (idx_nat_ts){
          W_covid <- merge(W_covid, national_naive_covid - lag(national_naive_covid, 1),
                           national_naive_covid - lag(national_naive_covid, 1))[common_idx_flu-7, drop=F]
        }
        if (idx_inreg_ts){
          W_covid <- merge(W_covid, state_naive_covid_reg - lag(state_naive_covid_reg, 1))[common_idx_flu-7, drop=F]
          W_covid <- merge(W_covid, lag(state_truth_covid_reg_case, 1) - lag(state_truth_covid_reg_case, 2))[common_idx_flu-7, drop=F]
        }
        
      }
    }
    W_covid <- W_covid[index(Z_covid),]
    
    ZW_covid <- as.matrix((merge(Z_covid,  W_covid)))
    

    ## flu: Z, W 
    Z_flu <- (truth - naive.p)[index(Z_covid)+7, name_state_flu, drop=F]
    W_flu <- merge(lag(Z_flu, 1), 
                   (argo1.p - naive.p)[index(Z_covid)+7, paste0("predict.", name_state_flu), drop=F], 
                   (argo.nat.p - naive.p)[index(Z_covid)+7, name_state_flu, drop=F])
    if(idx_reg_flu_ts){
      W_flu <- merge(W_flu, (argo.reg.p - naive.p)[index(Z_covid)+7, paste0(name_state_flu), drop=F])
    }
    if(idx_inreg_flu_ts){
      W_flu <- merge(W_flu, (argo1.p - naive.p)[index(Z_covid)+7,  paste0("predict.", name_states_reg_flu), drop=F])
    }
    ZW_flu <- as.matrix((merge(Z_flu, W_flu)))
    
    ## combine flu + covid 
    Z <- cbind(as.matrix(Z_flu), as.matrix(Z_covid))
    W <- cbind(as.matrix(W_flu), as.matrix(W_covid))
    ZW <-  cbind(as.matrix(Z), as.matrix(W))
    
    if(idx_flu_only){
      Z <- Z_flu
      W <- cbind(as.matrix(W_flu), as.matrix(W_covid))
      ZW <- cbind(as.matrix(Z), as.matrix(W))
    }

    ## ARGOX 2nd step: predict by each week
    for(it in (n_training+1):nrow(argo2_pred_flu)){
      idx.train <- it-(1:n_training)
      t.now <- rownames(as.matrix(Z_flu))[it]
      t.now.covid <- rownames(as.matrix(Z_covid))[it]
      
      Sigma.zz <- cov(ZW[idx.train, (1:ncol(Z)), drop=F], ZW[idx.train, (1:ncol(Z)), drop=F], use="complete.obs")
      mu.hat.z <- colMeans(ZW[idx.train, (1:ncol(Z)), drop=F], na.rm = T)
      mu.hat.w <- colMeans(ZW[idx.train, -(1:ncol(Z)), drop=F], na.rm = T)
      Sigma.ww.str <- cov(ZW[idx.train, -(1:ncol(Z)), drop=F], ZW[idx.train, -(1:ncol(Z)), drop=F], use="complete.obs") 
      Sigma.zw.str <- cov(ZW[idx.train, (1:ncol(Z)), drop=F], ZW[idx.train, -(1:ncol(Z)), drop=F], use="complete.obs")
      
      D.ww <- diag(diag(var(ZW[idx.train, -(1:ncol(Z)), drop=F], na.rm = T)))
      
      ## exception for some degenerative cases
      if(det(w*Sigma.ww.str + (1-w)*D.ww)!=0){
        Z.hat <- (w * Sigma.zw.str) %*% solve(w*Sigma.ww.str + (1-w)*D.ww) %*% t(ZW[it,-(1:ncol(Z)), drop=F] - mu.hat.w) + mu.hat.z
        var.est <- (Sigma.zz - (w*Sigma.zw.str) %*% solve(w*Sigma.ww.str + (1-w)*D.ww) %*% t((w*Sigma.zw.str)))
      } else if(det(D.ww)!=0){
        Z.hat <- (w * Sigma.zw.str) %*% solve(D.ww) %*% t(ZW[it,-(1:ncol(Z)), drop=F] - mu.hat.w) + mu.hat.z
        print(paste0("singular:", name_state_flu, t.now))
        var.est <- (Sigma.zz - (w*Sigma.zw.str) %*% solve(D.ww) %*% t((w*Sigma.zw.str)))
      } else{
        Z.hat <- Z[it-1,]
        var.est <- Sigma.zz
        print(paste0("singular**2:", name_state_flu, t.now))
      }
      
      argo2_pred_flu[t.now, name_state_flu] <- as.numeric(Z.hat[1]) + naive.p[t.now, name_state_flu]
      var_est_ind_flu[t.now, name_state_flu]  <- var.est[1,1]
      if(!idx_flu_only){
        argo2_pred_covid[t.now.covid, name_state_covid] <- Z.hat[2] + state_naive_covid[t.now.covid,]
        var_est_ind_covid[t.now.covid, name_state_covid]  <- var.est[2,2]
      }
    }
    if(!idx_flu_only){
      if(idx_percent){
        var_est_ind_covid[, name_state_covid] <- var_est_ind_covid[, name_state_covid] * (tab.states$Population[tab.states$Abbre_names_flu==name_state_flu] / 10000)^2
        argo2_pred_covid[, name_state_covid] <- argo2_pred_covid[, name_state_covid] * (tab.states$Population[tab.states$Abbre_names_flu==name_state_flu] / 10000)
      }
    }

  
  }
  print(name_state_covid)
}


out_file_name <- paste0("GTstate_ind_covid_", 
                        ifelse(idx_flu_only, "flu_only_", ""),
                        ifelse(idx_argo_covid_pred, "argo_pred_",""),
                        ifelse(idx_ts_covid_pred, "ts_death_",""),
                        ifelse(idx_ts_covid_cases, "cases_",""),
                        ifelse(idx_nat_ts, "nat_",""),
                        ifelse(idx_inreg_ts, "inreg_",""),
                        ifelse(idx_outreg_ts, "outreg_", ""),
                        ifelse(idx_reg_ts, "reg_",""),
                        ifelse(idx_reg_flu_ts, "inregflu_",""),
                        ifelse(idx_reg_flu_ts, "regflu_",""),
                        ifelse(idx_ts_covid_cases, "covidcurr_", ""),
                        ifelse(w==0.5, "", paste0("w", w, "_")),
                        ifelse(idx_percent, paste0("percent_", pct_scale),""), "_train", n_training, ".Rdata")
save(argo2_pred_flu, argo2_pred_covid, var_est_ind_flu, var_est_ind_covid, file= file.path(out.folder, out_file_name))


## change out name in summary
############summarize MSE
####summary
load(file.path(ili.folder, "results_44b_210707_gft_GTapi_raw.Rdata"))
states.abb.all <- colnames(ili_state)
idx.states <- 1:length(states.abb.all)

reg.method.p1 <- sapply(idx.states, function(region.id){
  pred.region <- 
    merge(xts(reg.method.p.all[,,region.id], as.Date(rownames(reg.method.p.all[,,region.id]))),
          argo2.pred.final.all[,region.id])
  colnames(pred.region) <- c("CDC.data", paste0("argo1"),
                             "var1",
                             "naive", "GFT", "argo2")
  pred.region <- pred.region[is.finite(pred.region$argo2),]
  data.matrix(pred.region)
}, simplify = "array")

dim(reg.method.p1)
dimnames(reg.method.p1)[[3]] <- (states.abb.all)[idx.states]

file.name.step2 <- paste0("data_states_all_logit01_", gt.out, "a_step2_44b.Rdata")
load(file.path(out.folder, file.name.step2))

reg.method.p1 <- abind(reg.method.p1,as.matrix(argo2.pred.ind[dimnames(reg.method.p1)[[1]],,]), along=2)
dimnames(reg.method.p1)[[2]][length(dimnames(reg.method.p1)[[2]])] <- paste0("argox_ind")

argo2_pred_ind_covid <- argo2.pred.ind
#### NYC using individual??
#if(idx_inreg_ts){
#  argo2_pred_ind_covid <- argo2.pred.final.all
#}
out_file_name <- paste0("GTstate_ind_covid_", 
                        ifelse(idx_flu_only, "flu_only_", ""),
                        ifelse(idx_argo_covid_pred, "argo_pred_",""),
                        ifelse(idx_ts_covid_pred, "ts_death_",""),
                        ifelse(idx_ts_covid_cases, "cases_",""),
                        ifelse(idx_nat_ts, "nat_",""),
                        ifelse(idx_inreg_ts, "inreg_",""),
                        ifelse(idx_outreg_ts, "outreg_", ""),
                        ifelse(idx_reg_ts, "reg_",""),
                        ifelse(idx_reg_flu_ts, "inregflu_",""),
                        ifelse(idx_reg_flu_ts, "regflu_",""),
                        ifelse(idx_ts_covid_cases, "covidcurr_", ""),
                        ifelse(w==0.5, "", paste0("w", w, "_")),
                        ifelse(idx_percent, paste0("percent_", pct_scale),""), "_train", n_training, ".Rdata")
out_file <- file.path(out.folder, out_file_name)
load(out_file)

## !!! switch in new pred; remaining states/dates using ind prediction
argo2_pred_flu_all <- na.omit(argo2_pred_flu)
argo2_pred_ind_covid[index(argo2_pred_flu_all), colnames(argo2_pred_flu_all)] <- argo2_pred_flu_all
reg.method.p1 <- abind(reg.method.p1,as.matrix(argo2_pred_ind_covid[dimnames(reg.method.p1)[[1]],,]), along=2)
dimnames(reg.method.p1)[[2]][length(dimnames(reg.method.p1)[[2]])] <- paste0("argox_ind_covid")

zoom_periods <- c("2014-10-11/2020-03-21",
                  "2014-10-11/2015-05-24",
                  "2015-10-10/2016-05-22",
                  "2016-10-08/2017-05-21",
                  "2017-10-07/2018-05-26", 
                  "2018-10-06/2019-05-18",
                  "2019-10-11/2020-05-16",
                  "2020-10-03/2021-05-22",
                  "2019-10-11/2020-03-21",
                  "2020-03-07/2021-06-19",
                  "2014-10-11/2015-08-15",
                  "2014-10-11/2017-05-14")

zoom_periods <- c("2021-01-01/2021-06-19", "2020-03-07/2021-06-19")
eval.period <- "2014-10-11/2021-06-19"

eval.period1 <- "2020-03-07/2021-06-19" ## 18 states worse than naive 87%
#eval.period1 <- c("2021-01-01/2021-06-19")

tab.allregion <- mclapply(1:length(idx.states), function(region.id){
  tab <- summary_argo(xts(reg.method.p1[,,region.id], as.Date(dimnames(reg.method.p1)[[1]])),
                      dimnames(reg.method.p1)[[2]], dimnames(reg.method.p1)[[2]],
                      zoom_periods, eval.period)
}, mc.cores = 2)

tab.allregion <- sapply(c("rmse","abse","rmspe","mape","corr", "corr_diff"), function(type){
  sapply(1:length(idx.states), function(region.id){
    if(type=="rmse"){
      tab.allregion[[region.id]][[type]]^2
    }else{
      tab.allregion[[region.id]][[type]]  
    }
  }, simplify = "array")
}, simplify = "array")
dimnames(tab.allregion)[[3]] <- (states.abb.all)[idx.states]
dim(tab.allregion)

tt <- rbind(t(tab.allregion[,eval.period1,,"rmse"]), 
            colMeans(t(tab.allregion[,eval.period1,,"rmse"]),na.rm=T))

print((tt[,6]/tt[,4])[dim(tt)[1]])
print((tt[,dim(tt)[2]]/tt[,4])[dim(tt)[1]])
print(sum((tt[,dim(tt)[2]]/tt[,4])>1, na.rm = T))
tt1 <- sweep(tt, 1, tt[,4], "/")
apply(tt1[-nrow(tt1), ]>1, 2, sum)

View(tt1)

