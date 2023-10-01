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

source('./R/WaveletPkg.R')

######## Load data
excel_files <- list.files(path = "./Data/Province_cleaned_Nguyen/",pattern = "\\.xlsx$")
excel_path <- paste0( "./Data/Province_cleaned_Nguyen/", excel_files)

read.xl_files <- lapply(excel_path, function(file) {
  read_excel(file)
})


d1 <-  bind_rows(read.xl_files) %>%
  rename(date=year_month) 

all.provinces <- unique(d1$province)

runwaves <- function(X) {
  wave1<- d1 %>% 
    arrange(province, date) %>%
    filter(province== X) %>% 
    mutate(x=sqrt(Dengue_fever_rates), t=row_number()) %>%
    dplyr::select(x, t)
  
  cwt1 <- complete.cwt(wave1, dj=1/6, dt=1/12)
  
  phase1<-phase(cwt1, s=c(0.8, 1.2))
  
  return(list('phase_angle'=phase1, 'cwt'=cwt1))
}

all.wave <- lapply(all.provinces, runwaves)

all.phases.mat <- sapply(all.wave, '[[', 'phase_angle')
matplot(all.phases.mat, type='l')
abline(v=c(seq(from=1, to=300, by=12)))

all.phases <- sapply(all.wave, '[[', 'phase_angle') %>%
  as.data.frame() %>%
  mutate(date = as.Date(unique(d1$date)),
         year=year(date),
         month=month(date),
         epiyr = if_else(month>=4,year,(year-1) )
         ) %>%
  dplyr::select(starts_with('V'), date, year, month) %>%
  reshape2::melt(., id.vars=c('date', 'year','month')) %>%
  group_by(variable, year) %>%
  mutate(min_phase_abs = min(abs(value)),
         min_phase = min(value),
          peak_month = if_else(min_phase_abs==abs(value), month, NA_real_),
         start_month = if_else(min_phase== value, month, NA_real_)
                  ) %>%
  filter(!is.na(start_month)) %>%
  ungroup() %>%
  arrange(year, start_month) %>%
  group_by(year) %>%
  mutate(delay = start_month - min(start_month)) %>%
  reshape2::dcast(., year ~ variable, value.var='delay') 

# Mean delay from earliest peak  
apply(all.phases,2,mean)

#Earliest provinces on average:
all.provinces[c(2,3,11,14,15,19)]

#Bạc Liêu": Mekong Delta
#"Bình Phước" : SE
#	Phú Yên:  South Central Coast
# Quảng Ninh": Northeast
# Trà Vinh: Mekong Delta

plot(phase1, type='l')


phase.diff <- all.phases[,11]-all.phases[,3]

phasediff2.1_correct <- ((phase.diff + 3*pi) %% (2*pi)) - (pi)

plot(phasediff2.1_correct, type='l')

#phasediff2.1_correct <- ((phase.diff2.1 + 3*pi) %% (2*pi)) - (pi)
#shows corrected phase diff in modulo for the 12 month phase

#plot(dates,phasediff2.1_correct,type='l',col="blue", lty=1,ylim=c(-pi,pi))

