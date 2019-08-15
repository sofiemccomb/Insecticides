#This function performs the lm panel regression analyses. 
#Different versions can be run by changing the variable inputs, including data, predictor varaibles, index, model, and effect.

library(stats)
run_lm<-function(predictors,data=finaldata){
  Formula=stats::as.formula(paste("insect_planted ~ ", paste(predictors, collapse=" + "),"+factor(FIPS)+factor(Year)-1"))
  lm_model<-stats::lm(Formula, 
                      data = data)
  return(lm_model)
}
