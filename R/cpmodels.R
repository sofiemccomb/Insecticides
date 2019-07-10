#Perform all regressions for model="within" (for coefficients) and model="pooling" (for running vif) using run_plm function/script

#Source the run_plm function, created in the run_plm script in order to perform the panel linear model regressions
source("R/run_plm.R")

perform_cpmodels<-function(modeltype){

  #For each option perform: twoways, individual, and time fixed effects
    
    #A) All 7 variables
    cp_twoways_var7<-run_plm(predictors=var7, model=modeltype)
    cp_individual_var7<-run_plm(predictors=var7, effect = "individual", model=modeltype)
    cp_time_var7<-run_plm(predictors=var7, effect = "time", model=modeltype)
    
    #B) 6 variables (no CDL)
    cp_twoways_var6<-run_plm(predictors=var6, model=modeltype)
    cp_individual_var6<-run_plm(predictors=var6, effect = "individual", model=modeltype)
    cp_time_var6<-run_plm(predictors=var6, effect = "time", model=modeltype)
    
    #C) 6 variables (no CDL) but only 2007, 2012, and 2017 to match 3 years with CDL
    cp_twoways_var6_3yrs<-run_plm(predictors=var6, data=fulldata_3yrs, model=modeltype)
    cp_individual_var6_3yrs<-run_plm(predictors=var6, effect = "individual", data=fulldata_3yrs, model=modeltype)
    cp_time_var6_3yrs<-run_plm(predictors=var6, effect = "time", data=fulldata_3yrs, model=modeltype)
    
    #D) All crops covariates, repeating A-C with these inclusions
    #var7crops
    cp_twoways_var7crops<-run_plm(predictors=var7crops, model=modeltype)
    cp_individual_var7crops<-run_plm(predictors=var7crops, effect = "individual", model=modeltype)
    cp_time_var7crops<-run_plm(predictors=var7crops, effect = "time", model=modeltype)
    
    #var6crops
    cp_twoways_var6crops<-run_plm(predictors=var6crops, model=modeltype)
    cp_individual_var6crops<-run_plm(predictors=var6crops, effect = "individual", model=modeltype)
    cp_time_var6crops<-run_plm(predictors=var6crops, effect = "time", model=modeltype)
    
    #var6crops with fulldata_3yrs
    cp_twoways_var6crops_3yrs<-run_plm(predictors=var6crops, data=fulldata_3yrs, model=modeltype)
    cp_individual_var6crops_3yrs<-run_plm(predictors=var6crops, effect = "individual", data=fulldata_3yrs, model=modeltype)
    cp_time_var6crops_3yrs<-run_plm(predictors=var6crops, effect = "time", data=fulldata_3yrs, model=modeltype)
    
    
    #Creating two lists as otherwise files are too large for storage and want to keep everything together
    cpmodels<-list(cp_twoways_var7,cp_individual_var7,cp_time_var7,
                   cp_twoways_var6,cp_individual_var6,cp_time_var6,
                   cp_twoways_var6_3yrs,cp_individual_var6_3yrs,cp_time_var6_3yrs)
    
    names(cpmodels)<-c("cp_twoways_var7","cp_individual_var7","cp_time_var7",
                       "cp_twoways_var6","cp_individual_var6","cp_time_var6",
                       "cp_twoways_var6_3yrs","cp_individual_var6_3yrs","cp_time_var6_3yrs")
    
    cpmodels_crops<-list(cp_twoways_var7,cp_individual_var7,cp_time_var7,
                   cp_twoways_var6,cp_individual_var6,cp_time_var6,
                   cp_twoways_var6_3yrs,cp_individual_var6_3yrs,cp_time_var6_3yrs,
                   cp_twoways_var7crops,cp_individual_var7crops,cp_time_var7crops,
                   cp_twoways_var6crops,cp_individual_var6crops,cp_time_var6crops,
                   cp_twoways_var6crops_3yrs,cp_individual_var6crops_3yrs,cp_time_var6crops_3yrs)
    
    names(cpmodels_crops)<-c("cp_twoways_var7","cp_individual_var7","cp_time_var7",
                       "cp_twoways_var6","cp_individual_var6","cp_time_var6",
                       "cp_twoways_var6_3yrs","cp_individual_var6_3yrs","cp_time_var6_3yrs",
                       "cp_twoways_var7crops","cp_individual_var7crops","cp_time_var7crops",
                       "cp_twoways_var6crops","cp_individual_var6crops","cp_time_var6crops",
                       "cp_twoways_var6crops_3yrs","cp_individual_var6crops_3yrs","cp_time_var6crops_3yrs")
    
    #Assign cpmodels to within or pooled name
    assign(paste0("cpmodels_", modeltype), cpmodels)
    
    #Save the correct cpmodels file
    if(modeltype=="within"){
      save(cpmodels_within, file = paste0("Data/models/cpmodels_within.Rda"))
    }else if (modeltype=="pooling"){
      save(cpmodels_pooling, file = paste0("Data/models/cpmodels_pooling.Rda"))
    }else
      print("Failed to Save")
    
} #End perform_cpmodels function

#Apply function to both model within and pooling
  model_types<-c("within", "pooling")
  sapply(model_types, function(n){perform_cpmodels(modeltype = n)})

  