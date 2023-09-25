all_province_fwd1 <- function(date.test.in, modN, formula1='y ~ -1 +  X +   f(t,model = "ar1", hyper = list(theta1 = list(prior = "loggamma", param = c(3, 2))))'){
  c1 <- d1 %>%
    # filter(province %in% select.provinces) %>%
    arrange(province, date) %>%
    group_by(province) %>%
    mutate(province2=province,
           log_df = log(Dengue_fever_rates+1) , 
           year = year(date) ,
           Dengue_fever_rates_hold= if_else( date>= (date.test.in[1]), NA_real_,
                                             log_df),
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
               ))    
  
  
  
  
  c1 <- c1 %>%
    ungroup() %>%
    mutate(forecast= as.factor(if_else(is.na(Dengue_fever_rates_hold),1,0)))
  
  score.list =list ('obs.exp'=NA, 'ds'=c1, mod=mod1, 'fixed.eff'=mod1$summary.fixed)

  scores <- scoring_func(score.list)
  
  out.list =  list ('obs.exp'=NA, 'ds'=c1, 'scores'=scores, obs=c1$Dengue_fever_rates,  'preds'=mod1$summary.linear.predictor, 'fixed.eff'=mod1$summary.fixed)
  saveRDS(out.list,paste0('./Results/', 'mod',modN,'_',date.test.in  ,'.rds' )   )
  return(out.list)
}



