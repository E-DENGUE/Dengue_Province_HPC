scoring_func <- function(Y){
  
  forecast.index <- which((Y$ds$forecast==1))
  
  test1 <-inla.posterior.sample(1000, Y$mod, selection = list('Predictor'=forecast.index), num.threads=NULL,seed=123)
  
  log.scale.pred.samples <- sapply(test1, function(X) X$latent , simplify='array')
  
  #pred.samples <- exp(log.pred.samples) - 1   
  
  #obs <- Y$ds$Dengue_fever_rates[forecast.index]
   obs <- Y$ds$log_df_scale[forecast.index]

  miss.obs <- which(is.na(obs))
  if(length(miss.obs>0)){
    obs <- obs[-miss.obs]
    log.scale.pred.samples  <- log.scale.pred.samples[-miss.obs,1,]
  }
  
  crps1 <- crps_sample(obs, log.scale.pred.samples[,1,])
  
  return(crps1)
}
  
  return(crps1)
}
