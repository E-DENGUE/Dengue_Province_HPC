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
library(epiCleanr)

source('./R/WaveletPkg.R')

######## Load data
# excel_files <- list.files(path = "./Data/Province_cleaned_Nguyen/",pattern = "\\.xlsx$")
# excel_path <- paste0( "./Data/Province_cleaned_Nguyen/", excel_files)
# 
# read.xl_files <- lapply(excel_path, function(file) {
#   read_excel(file)
# })
# 
# 
# d1 <-  bind_rows(read.xl_files) %>%
#   rename(date=year_month) 

admin_names <- get_admin_names(country_name_or_code = "Vietnam")

d1 <- read.csv('./Data/historical_input_data_osf_io4zgrx.csv') %>%
  filter(country=='vietnam') %>%
  rename(province=admin1) %>%
  mutate(date = as.Date(date,'%d/%m/%Y'),
         more_than_10 =if_else(total>10,1,0)) %>%
  group_by(province) %>%
  mutate(N_months_above_10 =sum(more_than_10, na.rm=T)) %>%
  filter(date>='2001-01-01' & date<='2017-12-01' & N_months_above_10>10) %>%
  ungroup()

all.provinces <- unique(d1$province)
#all.provinces <- all.provinces[-grep('tuyen quang',all.provinces)]

runwaves <- function(X) {
  wave1<- d1 %>% 
    arrange(province, date) %>%
    mutate(var_cases=var(total, na.rm=T),
           t=row_number(),
           x=sqrt(total)) %>%
    filter(province== X & !is.na(total) & var_cases>0) %>% 
    dplyr::select(x, t)
  
  cwt1 <- complete.cwt(wave1, dj=1/6, dt=1/12)
  
  phase1<-phase(cwt1, s=c(0.8, 1.2))
  
  return(list('phase_angle'=phase1, 'cwt'=cwt1, 'ds'=wave1, 'province'=X))
}

### OPTION 1: look at phase difference for whole series
## drawback: seems to be very jumpy even after modulo fix
all.wave <- pblapply(all.provinces, runwaves)

all.phases.mat <- sapply(all.wave, '[[', 'phase_angle')

all.ds <- lapply(all.wave, '[[', 'ds')


matplot(all.phases.mat, type='l')
abline(v=c(seq(from=1, to=300, by=12)))

phase.diff.raw.mat <- apply(all.phases.mat,2, function(X){
  Y = X - all.phases.mat[,1]
  return(Y)
} )

phase.diff.mat <- apply(phase.diff.raw.mat,2, function(Y){
  Z <- ((Y + 3*pi) %% (2*pi))- (pi)
  return(Z)
} )

matplot(phase.diff.mat, type='l')

provinces.run <-  sapply(all.wave, '[[', 'province')

mean.phase.diff <- apply(phase.diff.mat,2,mean)

phase.diff <- cbind.data.frame('phase.diff.1x'=mean.phase.diff,'province'=provinces.run) %>%
arrange(-phase.diff.1x)

#Leaing provinces note red river delta region and northeast are adjacent:
#bac giang (northeast); 
#thai binh (red river delta), 
#dak nong (central highlands), 
#binh phuoc (southeast)
#phu yen (south central coast)
#soc trang (MDR)





## OPTION 2: look at start or mid point
### Drawbacks--need to be able to define epidemiological year...not clear where boundary is
dates <- d1 %>%
  filter(!is.na(total)) %>%
    group_by(date) %>%
  summarize(N=n())

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
  filter(!is.na(peak_month)) %>%
  ungroup() %>%
  arrange(year, peak_month) %>%
  group_by(year) %>%
  mutate(delay = peak_month - min(peak_month)) %>%
  reshape2::dcast(., year ~ variable, value.var='delay')

# Mean delay from earliest peak  
peak_delays <- apply(all.phases,2,mean) 
  peak_delays <- cbind.data.frame(peak_delays[-1], all.provinces)

  ##This tells a similar story to the approach1:
  #leading provinces ar ein Northeast and red river delta (and maybe northwest); also some in MDR
#an giang is in MDR
  #ba ria vung tau is in southeast
  #bac lieu is in MDR
  
  d1.early <- d1 %>%
    filter(province %in% c('bac giang',   'thai binh',    'dak nong',  'binh phuoc', 'phu yen','soc trang' , ' ba ria vung tau')) %>%
    dplyr::select(province, date, total)
  # ggplot(d1.early, aes(x=date, y=total, group=province, color=province))+geom_line() +facet_wrap(~province, scales='free')
  
  
  #test Gibb data
  # d1 <- read.csv('https://raw.githubusercontent.com/rorygibb/dengue_vietnam_ms/main/data/dengue/dengue_districts_19982020.csv') %>%
  #   mutate(province=tolower(province), date=as.Date(date)) %>%
  #            group_by(province, date) %>%
  #   summarize( cases=sum(cases, na.rm=T)) %>%
  #   ungroup() %>%
  #   filter(date>='2001-01-01' & date<='2017-12-01')
  # 
  # ggplot(d1, aes(x=date, y=cases, group=province, color=province))+geom_line() +facet_wrap(~province, scales='free')
  # 