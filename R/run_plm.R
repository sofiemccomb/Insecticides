#Create plm function to apply
run_plm<-function(predictors,
                  data=fulldata,index = c("FIPS","Year"), model = "within",effect = "twoways"){
  Formula=as.formula(paste("insect_planted ~ ", paste(predictors, collapse=" + ")))
  plm_model<-plm(Formula, 
                 data = data,
                 index = index,
                 model = model,
                 effect = effect)
  return(plm_model)
}
