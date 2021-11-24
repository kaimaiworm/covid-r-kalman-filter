library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)
library(KFAS)
library(dlm)
library(MLmetrics)
library(KFAS)
library(tseries)
library(timeSeries)
library(zoo)
library(quantmod)
library(car)


#set working directory


input_path = "input_path"
output_path = "output_path"


data = read_csv(paste0(input_path, "Deutschland.csv"), locale = locale(encoding = "ISO-8859-1"))



##delete data for which there are not enough observations
districts = unique(data$district)
for (i in districts) {

  y = data$district[which(data$district == i)]

  if(length(y) < 20){
    data = data[-c(which(data$district== i)),]
  }
}



##kalman filter function
estimate_R = function(y, gamma){

    model = SSModel(y ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))
    modelfit = fitSSM(model, c(log(var(y)), log(var(y))), method = "BFGS")$model
    out = KFS(modelfit, filtering = "state", smoothing = "state")

    R_sm = as.numeric(1 + ((1 / gamma) * out$alphahat))
    se_R_sm = (1 / gamma * (c(out$V)** 0.5))

    R_fl = as.numeric(1 + ((1 / gamma) * out$att))
    se_R_fl = (1 / gamma * (c(out$Ptt)** 0.5))
    
    s2_irr = modelfit$H
    s2_lev = modelfit$Q
    s_t_n = s2_lev/s2_irr
    
    sigma2 = data.frame("s2_irregular" = s2_irr, "s2_level" = s2_lev, "signal_to_noise" = s_t_n)
    df_est = data.frame("R_sm" = R_sm, "se_R_sm" = se_R_sm, "R_fl" = R_fl, "se_R_fl" = se_R_fl )
    
    results = list("estimates"=df_est, "sigma2"=sigma2)
    return(results)

}


##estimate R for each district

  gamma = 1 / 7.0
  R_estimations = data.frame()
  sigma2_res = data.frame()
  for (i in districts) {

    y = data$gr_infection_7[which(data$district == i)]
    date = data$date[which(data$district == i)]

    rslt = estimate_R(y, gamma = gamma)
    
    estimates = rslt$estimates
    sigma2 = rslt$sigma2

    df_district = data.frame("district" = rep(i, times = length(y)))
    df_district = cbind(date, df_district, estimates)
    
     vec_sigma = data.frame("district" = i)
     vec_sigma = cbind(vec_sigma, sigma2)
    
    R_estimations = rbind(R_estimations, df_district)
    sigma2_res = rbind(sigma2_res, vec_sigma)

  }

##confidence intervals

crit_val_95 = qnorm(0.975)
crit_val_65 = qnorm(0.825)
R_estimations$sm_ci_95_l = with(R_estimations, R_sm - crit_val_95 * se_R_sm)
R_estimations$sm_ci_95_u = with(R_estimations, R_sm + crit_val_95 * se_R_sm)

R_estimations$sm_ci_65_l = with(R_estimations, R_sm - crit_val_65 * se_R_sm)
R_estimations$sm_ci_65_u = with(R_estimations, R_sm + crit_val_65 * se_R_sm)

R_estimations$fl_ci_95_l = with(R_estimations, R_fl - crit_val_95 * se_R_fl)
R_estimations$fl_ci_95_u = with(R_estimations, R_fl + crit_val_95 * se_R_fl)

R_estimations$fl_ci_65_l = with(R_estimations, R_fl - crit_val_65 * se_R_fl)
R_estimations$fl_ci_65_u = with(R_estimations, R_fl + crit_val_65 * se_R_fl)


##Sort out unreliable results
min_s_t_n = 0.001
max_s_t_n = 10

del_districts = with(sigma2_res, district[which(signal_to_noise < min_s_t_n | signal_to_noise > max_s_t_n)]) 
R_estimations = R_estimations[-c(which(R_estimations$district %in% del_districts)),]
sigma2_res = sigma2_res[-c(which(sigma2_res$district %in% del_districts)),]

write.csv(R_estimations, file = paste0(output_path, "R_estimations.csv"), row.names = FALSE)
write.csv(sigma2_res, file = paste0(output_path, "sigma2_res.csv"), row.names = FALSE)



###Estimate R for Germany based on JHU data####
data_jhu = read_csv(paste0(input_path, "Deutschland_jhu.csv"), locale = locale(encoding = "ISO-8859-1"))

#delete countries with few observations

districts = unique(data_jhu$district)
for (i in districts) {
  
  y = data_jhu$district[which(data_jhu$district == i)]
  
  if(length(y) < 20){
    data_jhu = data_jhu[-c(which(data_jhu$district== i)),]
  }
}

y = data_jhu$gr_infection_7
ger_jhu = estimate_R(y, gamma =1/7)

ger_estimates = ger_jhu$estimates
ger_sigma2 = ger_jhu$sigma2

date = data_jhu$Date[which(data_jhu$Country == "Germany")]

df_germany = data.frame("district" = rep("Germany", times = length(y)))
df_germany = cbind(date, df_germany, ger_estimates)


crit_val_95 = qnorm(0.975)
crit_val_65 = qnorm(0.825)

df_germany$sm_ci_95_l = with(df_germany, R_sm - crit_val_95 * se_R_sm)
df_germany$sm_ci_95_u = with(df_germany, R_sm + crit_val_95 * se_R_sm)

df_germany$sm_ci_65_l = with(df_germany, R_sm - crit_val_65 * se_R_sm)
df_germany$sm_ci_65_u = with(df_germany, R_sm + crit_val_65 * se_R_sm)

df_germany$fl_ci_95_l = with(df_germany, R_fl - crit_val_95 * se_R_fl)
df_germany$fl_ci_95_u = with(df_germany, R_fl + crit_val_95 * se_R_fl)

df_germany$fl_ci_65_l = with(df_germany, R_fl - crit_val_65 * se_R_fl)
df_germany$fl_ci_65_u = with(df_germany, R_fl + crit_val_65 * se_R_fl)

write.csv(df_germany, file = paste0(output_path, "R_estimations_JHU.csv"), row.names = FALSE)
write.csv(ger_sigma2, file = paste0(output_path, "sigma2_res_JHU.csv"), row.names = FALSE)



