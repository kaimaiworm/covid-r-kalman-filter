library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(data.table)
library(stringr)
library(stringi)


#set working directory


input_path = "input_path"
output_path = "output_path"

#######RKI datasets#####

##function to extract relevant data from raw datasets

construct_dataset_rki = function(bland_name){

  data_temp = read_csv(paste0(input_path, "RKI/RKI_Covid19_", bland_name, ".csv"))

  #initiate dataframe
  bland = data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("date", "district", "new_cases", "new_deaths", "new_recovered"))))

  data_temp$Refdatum = as.Date(data_temp$Refdatum)
  n = 1
  date = c()

  #sum cases, deaths and recovered of each age group for each day and for each district
  for(d in sort(unique(data_temp$Refdatum))){
    date= append(date, format(as.Date(d,origin="1970-01-01")))
  }

  for (i in sort(unique(data_temp$Landkreis))) {

    for (j in date) {

      newrow = c()
      newrow[1] =  j
      newrow[2] = i
      newrow[3] = (with(data_temp, sum(AnzahlFall[Refdatum == j & Landkreis == i])))
      newrow[4] = (with(data_temp, sum(AnzahlTodesfall[Refdatum == j & Landkreis == i])))
      newrow[5] = (with(data_temp, sum(AnzahlGenesen[Refdatum == j & Landkreis == i])))
      bland[n, ] = newrow

      n = n + 1


    }

  }

  #sum daily cases for each distirct
  bland = bland %>% group_by(district) %>% mutate(total_cases = cumsum(new_cases))
  bland = bland %>% group_by(district) %>% mutate(total_deaths = cumsum(new_deaths))
  bland = bland %>% group_by(district) %>% mutate(total_recovered = cumsum(new_recovered))

  bland = data.frame(bland)
  bland$date = as.Date(bland$date)

  #make vectors numeric
  for (i in 3:5) {bland[,i] = as.numeric(bland[,i]) }


  #create aggregate for corresponding state

  if(bland_name == "Berlin" | bland_name =="Hamburg"){

    bland = arrange(bland, district, date)
    return(bland)

  } else if(bland_name == "Bremen"){

  #differentiate between state and city of bremen
   bland$district[which(bland$district == "Bremen")] = "Stadt Bremen"
   agg = with(bland, aggregate(cbind(new_cases, new_deaths, new_recovered, total_cases, total_deaths, total_recovered), by=list(Category= bland$date), FUN=sum))
   colnames(agg)[1] = "date"
   agg$district = bland_name

   bland = rbind(bland, agg)

   bland = arrange(bland, district, date)
   return(bland)


  } else {

    agg = with(bland, aggregate(cbind(new_cases, new_deaths, new_recovered, total_cases, total_deaths, total_recovered), by=list(Category= bland$date), FUN=sum))
    colnames(agg)[1] = "date"
    agg$district = bland_name

    bland = rbind(bland, agg)

    bland = arrange(bland, district, date)
    return(bland)

  }



}


laender = c("Baden_Wuerttemberg", "Bayern", "Brandenburg", "Berlin", "Bremen", "Hamburg", "Hessen",
           "Mecklenburg_Vorpommern", "Niedersachsen", "Nordrhein_Westfalen", "Rheinland_Pfalz", "Saarland",
           "Sachsen", "Sachsen_Anhalt", "Schleswig_Holstein", "Thueringen")



##construct dataset for each german state
for (i in laender) {

  assign(paste0("df_", i), construct_dataset(i))

}



##merge datasets
data = rbind(df_Baden_Wuerttemberg, df_Bayern, df_Brandenburg, df_Berlin, df_Bremen, df_Hamburg, df_Hessen,
              df_Mecklenburg_Vorpommern, df_Niedersachsen, df_Nordrhein_Westfalen, df_Rheinland_Pfalz, df_Saarland,
              df_Sachsen, df_Sachsen_Anhalt, df_Schleswig_Holstein, df_Thueringen)


##aggregate for germany

data_temp = data[which(data$district %in% laender), ]
ger = with(data_temp, aggregate(cbind(new_cases, new_deaths, new_recovered, total_cases, total_deaths, total_recovered), by=list(Category= data_temp$date), FUN=sum))
colnames(ger)[1] = "date"
ger$district = "Deutschland"

data= rbind(data, ger)

##delete cases with small amount of observations
data = data[-c(which(data$total_cases <= 100)),]


###infection gamma

counter = 1
infection = c(5, 6, 7, 8, 9, 10)

for(i in infection){

  data[[paste0("infection_",i)]] = NA

  for (region in unique(data$district)) {

    region_temp= data[which(data$district == region),]
    T = nrow(region_temp)

    infected = rep(NA, times = T)
    infected[1] = region_temp$total_cases[1]


    for(tt in 2:T){


      gamma = 1/i
      infected[tt] = ((1 - gamma) * infected[tt - 1] + as.numeric(max(region_temp$new_cases[tt], 0)))

    }
    data[which(data$district== region), 8 + counter] = infected

  }
  counter = counter + 1
}



###growth rate of infections

data = data.table(data)
setkey(data, district)
data[, gr_infection_5 := c(0, diff(infection_5))/c(NA, infection_5)[-length(infection_5)], by = district]
data[, gr_infection_6 := c(0, diff(infection_6))/c(NA, infection_6)[-length(infection_6)], by = district]
data[, gr_infection_7 := c(0, diff(infection_7))/c(NA, infection_7)[-length(infection_7)], by = district]
data[, gr_infection_8 := c(0, diff(infection_8))/c(NA, infection_8)[-length(infection_8)], by = district]
data[, gr_infection_9 := c(0, diff(infection_9))/c(NA, infection_9)[-length(infection_9)], by = district]
data[, gr_infection_10 := c(0, diff(infection_10))/c(NA, infection_10)[-length(infection_10)], by = district]

data = data.frame(data)

#omit first NAN observation of each district
data = na.omit(data)


data = arrange(data, district, date)



##save data frames



laender_list = list(df_Baden_Wuerttemberg, df_Bayern, df_Brandenburg, df_Berlin, df_Bremen, df_Hamburg, df_Hessen,
                     df_Mecklenburg_Vorpommern, df_Niedersachsen, df_Nordrhein_Westfalen, df_Rheinland_Pfalz, df_Saarland,
                     df_Sachsen, df_Sachsen_Anhalt, df_Schleswig_Holstein, df_Thueringen)


write.csv(data, paste0(output_path, "Deutschland.csv"), row.names = FALSE)

for (i in 1:16) {

  write.csv(laender_list[i], paste0(output_path, laender[i],".csv"), row.names = FALSE)

  }







########Johns Hopkins Datasets#######


cases_raw = read_csv(paste0(input_path, "JHU/time_series_covid19_confirmed_global.csv"))
deaths_raw = read_csv(paste0(input_path, "JHU/time_series_covid19_deaths_global.csv"))
recovered_raw = read_csv(paste0(input_path, "JHU/time_series_covid19_recovered_global.csv"))

#define function that transforms data frame from wide to long format
construct_df_jhu = function(df){

  df = df[-c(3,4)]
  colnames(df)[2] = "Country"

  multi = c("Australia", "Canada", "China", "Denmark", "France", "Netherlands", "United Kingdom")

  for (i in multi) {

    sumcol = colSums(df[which(df$Country== i), -c(1,2)])
    df = df[-c(which(df$Country == i)), ]
    df = rbind.data.frame(df, c(NA, i, sumcol))
    df[ , -c(1, 2)] = apply(df[ , -c(1, 2)], 2,
                            function(x) as.numeric(as.character(x)))
  }
  
  ###Germany only##
  
  df = df[which(df$Country == "Germany"), ]
  ##Delete this later##
  
  df = df[-c(1)]

  df = df %>%
    gather(Date, var, -c(Country))

  df = df[order(df$Country),]
  #countries = unique(df$Country)
  dates = rep(seq(as.Date("2020/01/22"), as.Date("2021/07/09"), by = "days"))

  df$Date = dates
  df$var = as.numeric(df$var)


  return(df)
}



cases_data = construct_df_jhu(cases_raw)
deaths_data = construct_df_jhu(deaths_raw)
recovered_data = construct_df_jhu(recovered_raw)

#merge data frames
colnames(cases_data)[3] = "total_cases"
colnames(deaths_data)[3] = "total_deaths"
colnames(recovered_data)[3] = "total_recovered"

data_jhu = cbind.data.frame(cases_data, deaths_data$total_deaths, recovered_data$total_recovered)

data_jhu$Date = as.Date(data_jhu$Date)

colnames(data_jhu)[4] = "total_deaths"
colnames(data_jhu)[5] = "total_recovered"


#create variables for new cases/deaths/recoveries

data_jhu = data.table(data_jhu)
setkey(data_jhu, Country)

data_jhu[, new_cases := c(0, diff(total_cases)), by = Country]
data_jhu[, new_deaths := c(0, diff(total_deaths)), by = Country]
data_jhu[, new_recovered := c(0, diff(total_recovered)), by = Country]
data_jhu = data.frame(data_jhu)

data_jhu = data_jhu[-c(which(data_jhu$total_cases <= 100)),]

#infection gamma


counter = 1
infection = c(5, 6, 7, 8, 9, 10)

for(i in infection){

  data_jhu[[paste0("infection_",i)]] = NA

  for (region in unique(data_jhu$Country)) {

    region_temp= data_jhu[which(data_jhu$Country == region),]
    T = nrow(region_temp)

    infected = rep(NA, times = T)
    infected[1] = region_temp$total_cases[1]


    for(tt in 2:T){


      gamma = 1/i
      infected[tt] = ((1 - gamma) * infected[tt - 1] + as.numeric(max(region_temp$new_cases[tt], 0)))

    }
    data_jhu[which(data_jhu$Country == region), 8 + counter] = infected

  }
  counter = counter + 1
}

#growth rate of infections

data_jhu = data.table(data_jhu)
setkey(data_jhu, Country)
data_jhu[, gr_infection_5 := c(0, diff(infection_5))/c(NA, infection_5)[-length(infection_5)], by = Country]
data_jhu[, gr_infection_6 := c(0, diff(infection_6))/c(NA, infection_6)[-length(infection_6)], by = Country]
data_jhu[, gr_infection_7 := c(0, diff(infection_7))/c(NA, infection_7)[-length(infection_7)], by = Country]
data_jhu[, gr_infection_8 := c(0, diff(infection_8))/c(NA, infection_8)[-length(infection_8)], by = Country]
data_jhu[, gr_infection_9 := c(0, diff(infection_9))/c(NA, infection_9)[-length(infection_9)], by = Country]
data_jhu[, gr_infection_10 := c(0, diff(infection_10))/c(NA, infection_10)[-length(infection_10)], by = Country]

data_jhu = data.frame(data_jhu)

#remove diamond princess and sweden
data_jhu = data_jhu[which(data_jhu$Country != "Diamond Princess" & data_jhu$Country != "Sweden"), ]

#remove first observation of each country (because growth rate of infection is NA)
data_jhu = na.omit(data_jhu)

#Save data frame
write.csv(data_jhu, paste0(output_path, "Deutschland_JHU.csv"), row.names = FALSE)


y1_me = data$total_cases[which(data$Country == "Germany")]
y2_me = data$infection_7[which(data$Country == "Germany")]
y3_me = data$gr_infection_7[which(data$Country == "Germany")]

y1_at = data.autoren$total_cases[which(data.autoren$`Country/Region` == "Germany")]
y2_at = data.autoren$infected_7[which(data.autoren$`Country/Region` == "Germany")]
y3_at = data.autoren$gr_infected_7[which(data.autoren$`Country/Region` == "Germany")]

