deseasonalize_climate <- function(climate_var){
  
  seas.mod <-d1 %>% 
    arrange(province, date) %>%
    group_by(province) %>%
    mutate( Climate_Train = if_else(date<as.Date('2005-01-01'), .data[[climate_var]], NA_real_),
            t=row_number(),
            sin12=sin(2*pi*t/12),
            cos12=cos(2*pi*t/12),
            sin6=sin(2*pi*t/6),
            cos6=cos(2*pi*t/6),
    )  %>%
    ungroup()
  
  form1 <-as.formula(paste0('Climate_Train', '~ sin12 + cos12+ sin6 +cos6'))
  
  fitted_models = seas.mod %>% 
    group_by(province) %>% 
    do(mod1 = rlm(form1, data=.))
  
  
  fun1 <- function(X, Y){
    seas.mod %>% filter(province==X) %>%
      cbind.data.frame(., predict.rlm( Y, newdata = seas.mod[seas.mod$province==X,], interval = "prediction"))
  }
  
  all_preds <-  mapply( fun1, fitted_models$province, fitted_models$mod1, SIMPLIFY=F) %>%
    bind_rows()
  
  all_mods <- all_preds %>%
    mutate(climate_diff = (.data[[climate_var]] - upr),
           climate_aberration = if_else(.data[[climate_var]] > upr,climate_diff , 0 ) 
    ) %>% 
    dplyr::select(province, date,climate_aberration)
  
  return(all_mods)
}  