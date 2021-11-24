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
library(ggplot2)
library(ggpubr)

#set working directory


input_path = "input_path"
output_path = "output_path"


data_growth = read_csv(paste0(input_path, "Deutschland.csv"), locale = locale(encoding = "ISO-8859-1"))
data_rki = read_csv(paste0(input_path, "R_estimations.csv"), locale = locale(encoding = "ISO-8859-1"))
data_jhu = read_csv(paste0(input_path, "R_estimations_JHU.csv"), locale = locale(encoding = "ISO-8859-1"))
sigma2_rki = read_csv(paste0(input_path, "sigma2_res.csv"), locale = locale(encoding = "ISO-8859-1"))
sigma2_jhu =read_csv(paste0(input_path, "sigma2_res_JHU.csv"), locale = locale(encoding = "ISO-8859-1"))

nowcast_rki = read_csv(paste0(input_path, "r_nowcast.csv"), locale = locale(encoding = "ISO-8859-1"))

dates = as.Date("2020-03-01") + 0:471



###estimate growth rates for düsseldorf
dus = data_growth$gr_infection_7[which(data_growth$district == "SK Düsseldorf" & data_growth$date %in% dates)]
model = SSModel(dus ~ SSMtrend(1, Q = list(matrix(NA))), H = matrix(NA))
modelfit = fitSSM(model, c(log(var(dus)), log(var(dus))), method = "BFGS")$model
out = KFS(modelfit, filtering = "state", smoothing = "state")

#düsseldorf first date 17-03-2020
düsseldorf_growth_fl = c(rep(NA, times=16),out$att)
düsseldorf_growth_sm = c(rep(NA, times=16),out$alphahat)
düsseldorf_growth = c(rep(NA, times=16), dus)


#köln first date: 09-03-2020
köln_sm = c(rep(NA, times=8), data_rki$R_sm[which(data_rki$district == "SK Köln" & data_rki$date %in% dates)])
köln_fl = c(rep(NA, times=8),data_rki$R_fl[which(data_rki$district == "SK Köln" & data_rki$date %in% dates)])  
köln_sm_65_l = c(rep(NA, times=8),data_rki$sm_ci_65_l[which(data_rki$district == "SK Köln" & data_rki$date %in% dates)])  
köln_sm_65_u = c(rep(NA, times=8),data_rki$sm_ci_65_u[which(data_rki$district == "SK Köln" & data_rki$date %in% dates)]) 

#düsseldorf first date 17-03-2020
düsseldorf_sm = c(rep(NA, times=16),data_rki$R_sm[which(data_rki$district == "SK Düsseldorf" & data_rki$date %in% dates)])
düsseldorf_fl = c(rep(NA, times=16),data_rki$R_fl[which(data_rki$district == "SK Düsseldorf" & data_rki$date %in% dates)])
düsseldorf_sm_65_l = c(rep(NA, times=16),data_rki$sm_ci_65_l[which(data_rki$district == "SK Düsseldorf" & data_rki$date %in% dates)])  
düsseldorf_sm_65_u = c(rep(NA, times=16),data_rki$sm_ci_65_u[which(data_rki$district == "SK Düsseldorf" & data_rki$date %in% dates)])
düsseldorf_fl_65_l = c(rep(NA, times=16),data_rki$fl_ci_65_l[which(data_rki$district == "SK Düsseldorf" & data_rki$date %in% dates)])  
düsseldorf_fl_65_u = c(rep(NA, times=16),data_rki$fl_ci_65_u[which(data_rki$district == "SK Düsseldorf" & data_rki$date %in% dates)]) 

#germany first date_ 02-03-2020 
deutschland_sm = c(rep(NA, times=1),data_jhu$R_sm[which(data_jhu$district == "Germany" & data_jhu$date %in% dates)])
deutschland_fl = c(rep(NA, times=1),data_jhu$R_fl[which(data_jhu$district == "Germany" & data_jhu$date %in% dates)] )
deutschland_sm_95_l = c(rep(NA, times=1),data_jhu$sm_ci_95_l[which(data_jhu$district == "Germany" & data_jhu$date %in% dates)] ) 
deutschland_sm_95_u = c(rep(NA, times=1),data_jhu$sm_ci_95_u[which(data_jhu$district == "Germany" & data_jhu$date %in% dates)] )
deutschland_fl_95_l = c(rep(NA, times=1),data_jhu$fl_ci_95_u[which(data_jhu$district == "Germany" & data_jhu$date %in% dates)] )
deutschland_fl_95_u = c(rep(NA, times=1),data_jhu$fl_ci_95_u[which(data_jhu$district == "Germany" & data_jhu$date %in% dates)] )

#first date: 06-03-2020
r_rki_4 = c(rep(NA, times=1), nowcast_rki$PS_4_Tage_R_Wert[which(nowcast_rki$Datum %in% dates)])
r_rki_4_95_l = c(rep(NA, times=1), nowcast_rki$UG_PI_4_Tage_R_Wert[which(nowcast_rki$Datum %in% dates)])
r_rki_4_95_u = c(rep(NA, times=1), nowcast_rki$OG_PI_4_Tage_R_Wert[which(nowcast_rki$Datum %in% dates)])

r_rki_7 = c(rep(NA, times=1), nowcast_rki$PS_7_Tage_R_Wert[which(nowcast_rki$Datum %in% dates)])
r_rki_7_95_l = c(rep(NA, times=1), nowcast_rki$UG_PI_7_Tage_R_Wert[which(nowcast_rki$Datum %in% dates)])
r_rki_7_95_u = c(rep(NA, times=1), nowcast_rki$OG_PI_7_Tage_R_Wert[which(nowcast_rki$Datum %in% dates)])



df_results = data.frame(date = dates, köln_sm, köln_fl, köln_sm_65_l, köln_sm_65_u, düsseldorf_sm, düsseldorf_fl, düsseldorf_sm_65_l, 
                        düsseldorf_sm_65_u, düsseldorf_fl_65_l, düsseldorf_fl_65_u, düsseldorf_growth, düsseldorf_growth_sm, düsseldorf_growth_fl, deutschland_sm, deutschland_fl, 
                        deutschland_sm_95_l, deutschland_sm_95_u, deutschland_fl_95_l, deutschland_fl_95_u, r_rki_4, r_rki_4_95_l, r_rki_4_95_u, r_rki_7, r_rki_7_95_l, r_rki_7_95_u )

transparent = rgb(0, 0, 255, max = 255, alpha = 125, names = "blue50")

#köln vs düsseldorf
plot_köln_düsseldorf = ggplot(df_results, aes(dates)) + 
                          geom_ribbon(
                            aes(ymin = köln_sm_65_l, ymax = köln_sm_65_u), fill = "red", alpha=0.2) + 
                          geom_line(aes(y = köln_sm, color = "Cologne"), size = 1) + 
                          geom_ribbon(
                            aes(ymin = düsseldorf_sm_65_l, ymax = düsseldorf_sm_65_u), fill = "blue", alpha=0.2) + 
                          geom_line(aes(y = düsseldorf_sm, color = "Düsseldorf"), size=1) + 
                          scale_color_manual(name="",values = c('Cologne' = 'red','Düsseldorf' = 'blue'))+
                          labs(y="Effective R", x = "Date", size=10) + theme(
                          text = element_text( size = 30),
                          legend.position=c(0.7,0.9),
                          legend.justification = "left",
                          legend.direction = "vertical",
                          legend.background = element_rect("transparent"),
                          legend.key =element_rect("transparent"))+
                          scale_x_date(date_breaks = "2 month", date_labels = "%b")


png(file= paste0(output_path, "köln_düsseldorf.png"),width=950, height=600)
plot_köln_düsseldorf
dev.off()


##düsseldorf smoother vs filter
plot_düsseldorf_sm_fl = ggplot(df_results, aes(dates)) + 
                          geom_line(aes(y = düsseldorf_growth_fl, color = "Filter"), size = 1.2) + 
                            geom_line(aes(y = düsseldorf_growth_sm, color = "Smoother"), size=1.2) + 
                         geom_line(aes(y = düsseldorf_growth, color = "Raw"), size=0.5) +
                          scale_color_manual(name="",values = c('Filter' = 'red','Smoother' = 'green', "Raw" = "black"))+
                          labs(y="Infection Growth Rate", x = "Date", size=10) + theme(
                            text = element_text( size = 30),
                            legend.position=c(0.7,0.9),
                            legend.justification = "left",
                            legend.direction = "vertical",
                            legend.background = element_rect("transparent"),
                            legend.key =element_rect("transparent"))+
                          scale_x_date(date_breaks = "2 month", date_labels = "%b")

png(file= paste0(output_path, "düsseldorf_sm_fl.png"),width=950, height=600)
plot_düsseldorf_sm_fl
dev.off()



##kalman filter vs rki 4-day nowcast
plot_kalman_fl_nowcast_4 = ggplot(df_results, aes(x = date)) + 
                          geom_line(aes(y = deutschland_fl, color = "Kalman Filter"), size = 1) + 
                          geom_line(aes(y = r_rki_4, color = "RKI 4-Day Nowcast"), size=1) + 
                          scale_color_manual(name="",values = c('Kalman Filter' = 'red','RKI 4-Day Nowcast' = 'blue'))+
                          labs(y="Effective R", x = "Date", size=10) + theme(
                            text = element_text( size = 30),
                            legend.position=c(0.65,0.9),
                            legend.justification = "left",
                            legend.direction = "vertical",
                            legend.background = element_rect("transparent"),
                            legend.key =element_rect("transparent"))+
                          scale_x_date(date_breaks = "2 month", date_labels = "%b")

png(file= paste0(output_path, "kalman_fl_nowcast_4.png"),width=800, height=600)
plot_kalman_fl_nowcast_4
dev.off()


##kalman smoother vs rki 7-day nowcast
plot_kalman_sm_nowcast_7 = ggplot(df_results, aes(x = date)) + 
                            geom_line(aes(y = deutschland_sm, color = "Kalman Smoother"), size = 1) +
                            geom_line(aes(y = r_rki_7, color = "RKI 7-Day Nowcast"), size=1) + 
                            scale_color_manual(name="",values = c('Kalman Smoother' = 'red','RKI 7-Day Nowcast' = 'blue'))+
                            labs(y="Effective R", x = "Date", size=10) + theme(
                              text = element_text( size = 30),
                              legend.position=c(0.65,0.9),
                              legend.justification = "left",
                              legend.direction = "vertical",
                              legend.background = element_rect("transparent"),
                              legend.key =element_rect("transparent"))+
                            scale_x_date(date_breaks = "2 month", date_labels = "%b")

png(file= paste0(output_path, "kalman_sm_nowcast_7.png"),width=800, height=600)
plot_kalman_sm_nowcast_7
dev.off()


###Basic reproduction number

basic_r_köln = mean(na.omit(köln_sm)[1:7])
basic_r_düsseldorf = mean(na.omit(düsseldorf_sm)[1:7])
basic_r_deutschland = mean(na.omit(deutschland_sm)[1:7])

