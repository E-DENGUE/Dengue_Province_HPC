library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(tidyverse)
library(zoo)
library(lubridate)
library(pbapply)
library(INLA)
library(MASS)
library(scoringutils)


source('./R/predict.rlm.R')
source('./R/deseasonalize_climate.R')
source('./R/all_province_fwd1.R')
source('./R/scoring_func.R')

######## Load data
excel_files <- list.files(path = "./Data/Province_cleaned_Nguyen/",pattern = "\\.xlsx$")
excel_path <- paste0( "./Data/Province_cleaned_Nguyen/", excel_files)

read.xl_files <- lapply(excel_path, function(file) {
  read_excel(file)
})




d1 <-  bind_rows(read.xl_files) %>%
  rename(date=year_month) 

rain1 <- deseasonalize_climate("Total_Rainfall") %>% rename(total_rainfall_ab = climate_aberration)
rain2 <- deseasonalize_climate("Max_Daily_Rainfall")  %>% rename(daily_rainfall_ab = climate_aberration)
temp1 <- deseasonalize_climate("Average_temperature")  %>% rename( ave_temp_ab = climate_aberration)
temp2 <- deseasonalize_climate("Max_Average_Temperature")  %>% rename( max_temp_ab = climate_aberration)
temp3 <- deseasonalize_climate("Min_Average_Temperature")  %>% rename( min_ave_temp_ab = climate_aberration)
temp4 <- deseasonalize_climate("Max_Asolute_Temperature")  %>% rename( max_abs_temp_abb = climate_aberration)
temp5 <- deseasonalize_climate("Min_Asolute_Temperature")  %>% rename( min_abs_temp_abb= climate_aberration)
humid1 <- deseasonalize_climate("Min_Humudity")  %>% rename(min_humid_abb = climate_aberration)

d1 <- d1 %>%
  left_join(rain1, by=c('province', 'date')) %>%
  left_join(rain2, by=c('province', 'date')) %>%
  left_join(temp1, by=c('province', 'date')) %>%
  left_join(temp2, by=c('province', 'date')) %>%
  left_join(temp3, by=c('province', 'date')) %>%
  left_join(temp4, by=c('province', 'date')) %>%
  left_join(temp5, by=c('province', 'date')) %>%
  left_join(humid1, by=c('province', 'date')) 

d1.agg <- d1 %>%
  group_by(date) %>%
  summarize(mean_dengue = mean(Dengue_fever_rates)) %>%
  mutate(lag1_mean_dengue = lag(mean_dengue,1),
         log_lag1_mean_dengue = log(lag1_mean_dengue +1 ))

d1 <- d1 %>%
  left_join(d1.agg, by='date')


d1 <-   d1 %>% arrange(province,date) %>%
  group_by(province) %>%
  mutate(  
    date=as.Date(date),
    first_date=min(date),
    last_date =max(date),
    #redefine the lag variables
    Average_Humidity = as.vector(scale(Average_Humidity)),
    lag1_Average_Humidity = dplyr::lag(Average_Humidity,1,default=NA),
    lag2_Average_Humidity = dplyr::lag(Average_Humidity,2,default=NA),
    lag3_Average_Humidity = dplyr::lag(Average_Humidity,3),
    lag4_Average_Humidity = dplyr::lag(Average_Humidity,4),
    lag5_Average_Humidity = dplyr::lag(Average_Humidity,5),
    lag6_Average_Humidity = dplyr::lag(Average_Humidity,6),
    
    Average_temperature= as.vector(scale(Average_temperature)),
    lag1_Average_temperature= dplyr::lag(Average_temperature,1),
    lag2_Average_temperature= dplyr::lag(Average_temperature,2),
    lag3_Average_temperature= dplyr::lag(Average_temperature,3),
    lag4_Average_temperature= dplyr::lag(Average_temperature,4),
    lag5_Average_temperature= dplyr::lag(Average_temperature,5),
    lag6_Average_temperature= dplyr::lag(Average_temperature,6),
    
    Total_Rainfall=as.vector(scale(Total_Rainfall)),
    lag1_Total_Rainfall= dplyr::lag(Total_Rainfall,1),
    lag2_Total_Rainfall= dplyr::lag(Total_Rainfall,2),
    lag3_Total_Rainfall= dplyr::lag(Total_Rainfall,3),
    lag4_Total_Rainfall= dplyr::lag(Total_Rainfall,4),
    lag5_Total_Rainfall= dplyr::lag(Total_Rainfall,5),
    lag6_Total_Rainfall= dplyr::lag(Total_Rainfall,6),
    
    Min_Average_Temperature=as.vector(scale(Min_Average_Temperature)),
    lag1_Min_Average_Temperature= dplyr::lag(Min_Average_Temperature,1),
    lag2_Min_Average_Temperature= dplyr::lag(Min_Average_Temperature,2),
    lag3_Min_Average_Temperature= dplyr::lag(Min_Average_Temperature,3),
    lag4_Min_Average_Temperature= dplyr::lag(Min_Average_Temperature,4),
    lag5_Min_Average_Temperature= dplyr::lag(Min_Average_Temperature,5),
    lag6_Min_Average_Temperature= dplyr::lag(Min_Average_Temperature,6),
    
    Max_Average_Temperature= as.vector(scale(Max_Average_Temperature)),
    lag1_Max_Average_Temperature= dplyr::lag(Max_Average_Temperature,1),
    lag2_Max_Average_Temperature= dplyr::lag(Max_Average_Temperature,2),
    lag3_Max_Average_Temperature= dplyr::lag(Max_Average_Temperature,3),
    lag4_Max_Average_Temperature= dplyr::lag(Max_Average_Temperature,4),
    lag5_Max_Average_Temperature= dplyr::lag(Max_Average_Temperature,5),
    lag6_Max_Average_Temperature= dplyr::lag(Max_Average_Temperature,6),
    
    # rainfall, ,im temp, max temp1
    lag1_total_rainfall_ab = dplyr::lag(total_rainfall_ab,1),
    lag2_total_rainfall_ab = dplyr::lag(total_rainfall_ab,2),
    
    lag1_ave_temp_ab = dplyr::lag(ave_temp_ab,1),
    lag2_ave_temp_ab = dplyr::lag(ave_temp_ab,2),
    
    lag1_max_temp_ab= dplyr::lag(max_temp_ab,1),
    lag2_max_temp_ab = dplyr::lag(max_temp_ab,2),
    
  )   %>%
  filter(!is.na(lag6_Total_Rainfall) )  %>% #filter out regions with partial time series
  ungroup()

### Run the models






#just test a few years
date.test2 <- seq.Date(from=as.Date('2008-01-01') ,to=as.Date('2016-12-01') , by='month')


#All models


###############best model based on CRPS (full posterior) ############
mod1 <- 'Dengue_fever_rates_hold~   lag_y +
                        f(provinceID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '

#same as mod 9 without AR1
mod2 <- 'Dengue_fever_rates_hold~   lag_y +
                        f(provinceID,model = "iid")+
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))
                         '

#building off of model 9, adds rainfall
mod3 <- 'Dengue_fever_rates_hold~   lag_y +
                        f(provinceID,model = "iid")+
                        lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '

#building off of model 9, allow AR1 to vary by district
mod4 <- 'Dengue_fever_rates_hold~   lag_y +
                        f(provinceID,model = "iid")+
                        f(t, group=provinceID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '

#building off of model 9, allow AR1 to vary by district AND add rainfall
mod5 <- 'Dengue_fever_rates_hold~   lag_y +
                        f(provinceID,model = "iid")+
                        lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, group=provinceID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '

## try adding different combos of climate vars
mod6 <- 'Dengue_fever_rates_hold ~   lag_y + lag1_Total_Rainfall +lag2_Total_Rainfall +
                        f(provinceID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'


mod7 <- 'Dengue_fever_rates_hold ~   lag_y + lag1_Min_Average_Temperature +lag2_Min_Average_Temperature +
                        f(provinceID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'


mod8 <- 'Dengue_fever_rates_hold ~   lag_y + lag1_Max_Average_Temperature +lag2_Max_Average_Temperature +
                        f(provinceID,model = "iid")+
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'

mod9 <- 'Dengue_fever_rates_hold~   lag_y + log_lag1_mean_dengue +
                        f(provinceID,model = "iid")+
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '

mod10 <- 'Dengue_fever_rates_hold~   lag_y + log_lag1_mean_dengue +
                        f(provinceID,model = "iid")+
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
            '

## province AR(1) without  lagged cases           
mod11 <- 'Dengue_fever_rates_hold~    
                        f(provinceID,model = "iid")+
                        f(t, group=provinceID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '
                         
 ## province AR(1) without  lagged cases    +rain       
mod12 <- 'Dengue_fever_rates_hold~    
                        f(provinceID,model = "iid")+ lag1_Total_Rainfall +lag2_Total_Rainfall +
                        f(t, group=provinceID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '
                         
 ## province AR(1) without  lagged cases    +temp       
mod13 <- 'Dengue_fever_rates_hold~    
                        f(provinceID,model = "iid")+ lag1_Min_Average_Temperature +lag2_Min_Average_Temperature +
                        f(t, group=provinceID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         '     
                         
 ## province AR(1) without  lagged cases    +temp   +rain    
mod14 <- 'Dengue_fever_rates_hold~    
                        f(provinceID,model = "iid")+ lag1_Min_Average_Temperature +lag2_Min_Average_Temperature + lag1_Total_Rainfall +lag2_Total_Rainfall +
                        f(t, group=provinceID3, model="ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         ' 
                         
 ## province AR(1) without  lagged cases    +all 
mod15 <- 'Dengue_fever_rates_hold~    lag_y + 
                        f(provinceID,model = "iid")+ lag1_Min_Average_Temperature +lag2_Min_Average_Temperature + lag1_Total_Rainfall +lag2_Total_Rainfall +
                        lag1_Max_Average_Temperature +lag2_Max_Average_Temperature + lag1_Min_Average_Temperature +lag2_Min_Average_Temperature +
                        lag1_total_rainfall_ab + lag2_total_rainfall_ab +
                        f(t, model="ar1") + #shared AR(1) across districts
                        f(monthN, group=provinceID2,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2)))) 
                         ' 

all.mods <- list(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10, mod11, mod12, mod13, mod14, mod15)
