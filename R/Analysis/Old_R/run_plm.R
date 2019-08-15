#This function performs the plm regression analyses. 
  #Different versions can be run by changing the variable inputs, including data, predictor varaibles, index, model, and effect.

library(plm)
library(stats)
run_plm<-function(predictors,
                  data=finaldata,index = c("FIPS","Year"), model = "within",effect = "twoways"){
  Formula=stats::as.formula(paste("insect_planted ~ ", paste(predictors, collapse=" + ")))
  plm_model<-plm::plm(Formula, 
                 data = data,
                 index = index,
                 model = model,
                 effect = effect)
  return(plm_model)
}
