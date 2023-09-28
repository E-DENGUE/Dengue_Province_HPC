all_province_fwd1 <- function(date.test.in, modN, formula1='y ~ -1 +  X +   f(t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'){

mean.log.df <- mean(log(d1$Dengue_fever_rates[1:60]+1)) #mean dengue rates during first 5 years
sd.log.df <- sd(log(d1$Dengue_fever_rates[1:60]+1)) #mean dengue rates during first 5 years

  c1 <- d1 %>%
    # filter(province %in% select.provinces) %>%
    arrange(province, date) %>%
    group_by(province) %>%
    mutate(province2=province,
           log_df = log(Dengue_fever_rates+1) , 
           
           log_df_scale = (log_df - mean.log.df)/sd.log.df,
           
           year = year(date) ,
           Dengue_fever_rates_hold= if_else( date>= (date.test.in[1]), NA_real_,
                                             log_df_scale),
           y=Dengue_fever_rates_hold,
           lag_y = lag(y, 1),
           t=row_number(),
           t2=t,
           month=as.factor(month(date)),
           monthN=month(date),
           #log_offset=log(pop/100000)
    ) %>%
    filter(date<= (date.test.in[1]) & !is.na(lag_y)) %>%
    ungroup() %>%
    mutate(provinceID = as.numeric(as.factor(province)),
           provinceID2 = provinceID,
           provinceID3 = provinceID)
  
  form2 <- as.formula (formula1)
  
  # form2 <- as.formula(y ~ f(t, group = provinceID2, model = "ar1", 
  # hyper = list(theta1 = list(prior = "loggamma", param = c(3, 
  #    2)))))    
  
  
  
  mod1 <- inla(form2, data = c1,  family = "gaussian",
               control.compute = list(dic = FALSE, 
                                      waic = FALSE, 
                                      config = T
               ),
            control.inla = list(strategy='adaptive', # adaptive gaussian
                   cmin=0),
            control.fixed = list(mean.intercept=0, 
                   prec.intercept=1, # precision 1
                   mean=0, 
                   prec=1), # weakly regularising on fixed effects (sd of 1)
            inla.mode = "experimental", # new version of INLA algorithm (requires R 4.1 and INLA testing version)
            num.threads=8
               )    
  
  
  
  
  c1 <- c1 %>%
    ungroup() %>%
    mutate(forecast= as.factor(if_else(is.na(Dengue_fever_rates_hold),1,0)))
  
  score.list =list ('ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed)

  scores <- scoring_func(score.list)
  
  c1.out <- c1 %>%
    dplyr::select(date, province, Dengue_fever_rates, log_df_scale,forecast ) %>%
    mutate(preds = exp(mod1$summary.linear.predictor * sd.log.df + mean.log.df))
  
  out.list =  list ('ds'=c1.out, 'scores'=scores,  'fixed.eff'=mod1$summary.fixed)
  saveRDS(out.list,paste0('./Results/', 'mod',modN,'_',date.test.in  ,'.rds' )   )
  return(out.list)
}
