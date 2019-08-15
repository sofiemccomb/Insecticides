#Perform all regressions for model="within" (for coefficients) and model="pooling" (for running vif) using run_plm function/script

#Load finaldata in case not loaded
load("Data/DataProcessing/df/finaldata.rda")

#Source the run_plm function, created in the run_plm script in order to perform the panel linear model regressions
source("R/Analysis/run_plm.R")

#Create Variable Combinations
var7<-c("pland_crops","cropintensity", "msidi","mna_crops","largefarm_planted","natural_crops_ed","SDI")
var6<-c("pland_crops","cropintensity", "msidi","mna_crops","largefarm_planted","natural_crops_ed")
var7crops<-c("pland_crops","cropintensity", "msidi","mna_crops","largefarm_planted","natural_crops_ed","SDI",
             "soysmallgrain_planted", "corn_planted", "fruitveg_planted")
var6crops<-c("pland_crops","cropintensity", "msidi","mna_crops","largefarm_planted","natural_crops_ed",
             "soysmallgrain_planted", "corn_planted", "fruitveg_planted")
var4crops<-c("pland_crops","cropintensity", "largefarm_planted", "SDI",
             "soysmallgrain_planted", "corn_planted", "fruitveg_planted")
var3crops<-c("pland_crops","cropintensity", "largefarm_planted",
             "soysmallgrain_planted", "corn_planted", "fruitveg_planted")

#Data subset that has 1997 and 2002 removed, since there are so many NAs for those years across the datasets
finaldata_3yrs<-finaldata[finaldata$Year!=1997&finaldata$Year!=2002,]

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
    cp_twoways_var6_3yrs<-run_plm(predictors=var6, data=finaldata_3yrs, model=modeltype)
    cp_individual_var6_3yrs<-run_plm(predictors=var6, effect = "individual", data=finaldata_3yrs, model=modeltype)
    cp_time_var6_3yrs<-run_plm(predictors=var6, effect = "time", data=finaldata_3yrs, model=modeltype)
    
    #D) All crops covariates, repeating A-C with these inclusions
    #var7crops
    cp_twoways_var7crops<-run_plm(predictors=var7crops, model=modeltype)
    cp_individual_var7crops<-run_plm(predictors=var7crops, effect = "individual", model=modeltype)
    cp_time_var7crops<-run_plm(predictors=var7crops, effect = "time", model=modeltype)
    
    #var6crops
    cp_twoways_var6crops<-run_plm(predictors=var6crops, model=modeltype)
    cp_individual_var6crops<-run_plm(predictors=var6crops, effect = "individual", model=modeltype)
    cp_time_var6crops<-run_plm(predictors=var6crops, effect = "time", model=modeltype)
    
    #var6crops with finaldata_3yrs
    cp_twoways_var6crops_3yrs<-run_plm(predictors=var6crops, data=finaldata_3yrs, model=modeltype)
    cp_individual_var6crops_3yrs<-run_plm(predictors=var6crops, effect = "individual", data=finaldata_3yrs, model=modeltype)
    cp_time_var6crops_3yrs<-run_plm(predictors=var6crops, effect = "time", data=finaldata_3yrs, model=modeltype)
    
    #E) All crops covariates and no NLCD variables, repeating A-C with these inclusions and exclusions
    #var4crops
    cp_twoways_var4crops<-run_plm(predictors=var4crops, model=modeltype)
    cp_individual_var4crops<-run_plm(predictors=var4crops, effect = "individual", model=modeltype)
    cp_time_var4crops<-run_plm(predictors=var4crops, effect = "time", model=modeltype)
    
    #var3crops
    cp_twoways_var3crops<-run_plm(predictors=var3crops, model=modeltype)
    cp_individual_var3crops<-run_plm(predictors=var3crops, effect = "individual", model=modeltype)
    cp_time_var3crops<-run_plm(predictors=var3crops, effect = "time", model=modeltype)
    
    #var3crops with finaldata_3yrs
    cp_twoways_var3crops_3yrs<-run_plm(predictors=var3crops, data=finaldata_3yrs, model=modeltype)
    cp_individual_var3crops_3yrs<-run_plm(predictors=var3crops, effect = "individual", data=finaldata_3yrs, model=modeltype)
    cp_time_var3crops_3yrs<-run_plm(predictors=var3crops, effect = "time", data=finaldata_3yrs, model=modeltype)
    
    
    #Creating three lists as otherwise files are too large for storage and want to keep everything together
    cpmodels<-list(cp_twoways_var7,cp_individual_var7,cp_time_var7,
                   cp_twoways_var6,cp_individual_var6,cp_time_var6,
                   cp_twoways_var6_3yrs,cp_individual_var6_3yrs,cp_time_var6_3yrs)
    
    names(cpmodels)<-c("cp_twoways_var7","cp_individual_var7","cp_time_var7",
                       "cp_twoways_var6","cp_individual_var6","cp_time_var6",
                       "cp_twoways_var6_3yrs","cp_individual_var6_3yrs","cp_time_var6_3yrs")
    
    cpmodels_crops<-list(cp_twoways_var7crops,cp_individual_var7crops,cp_time_var7crops,
                   cp_twoways_var6crops,cp_individual_var6crops,cp_time_var6crops,
                   cp_twoways_var6crops_3yrs,cp_individual_var6crops_3yrs,cp_time_var6crops_3yrs)
    
    names(cpmodels_crops)<-c("cp_twoways_var7crops","cp_individual_var7crops","cp_time_var7crops",
                       "cp_twoways_var6crops","cp_individual_var6crops","cp_time_var6crops",
                       "cp_twoways_var6crops_3yrs","cp_individual_var6crops_3yrs","cp_time_var6crops_3yrs")
   
    cpmodels_crops_noNLCD<-list(cp_twoways_var4crops,cp_individual_var4crops,cp_time_var4crops,
                         cp_twoways_var3crops,cp_individual_var3crops,cp_time_var3crops,
                         cp_twoways_var3crops_3yrs,cp_individual_var3crops_3yrs,cp_time_var3crops_3yrs)
    
    names(cpmodels_crops_noNLCD)<-c("cp_twoways_var4crops","cp_individual_var4crops","cp_time_var4crops",
                             "cp_twoways_var3crops","cp_individual_var3crops","cp_time_var3crops",
                             "cp_twoways_var3crops_3yrs","cp_individual_var3crops_3yrs","cp_time_var3crops_3yrs")
    
    #Assign cpmodels to within or pooled name
    assign(paste0("cpmodels_", modeltype), cpmodels)
    #Assign cpmodels_crops to within or pooled name
    assign(paste0("cpmodels_crops_", modeltype), cpmodels_crops)
    #Assign cpmodels_crops to within or pooled name
    assign(paste0("cpmodels_crops_noNLCD_", modeltype), cpmodels_crops_noNLCD)
    
    #Save the correct cpmodels file
    if(modeltype=="within"){
      save(cpmodels_within, file = paste0("Data/Analysis/models/CP/cpmodels_within.Rda"))
      save(cpmodels_crops_within, file = paste0("Data/Analysis/models/CP/cpmodels_crops_within.Rda"))
      save(cpmodels_crops_noNLCD_within, file = paste0("Data/Analysis/models/CP/cpmodels_crops_noNLCD_within.Rda"))
    }else if (modeltype=="pooling"){
      save(cpmodels_pooling, file = paste0("Data/Analysis/models/CP/cpmodels_pooling.Rda"))
      save(cpmodels_crops_pooling, file = paste0("Data/Analysis/models/CP/cpmodels_crops_pooling.Rda"))
      save(cpmodels_crops_noNLCD_pooling, file = paste0("Data/Analysis/models/CP/cpmodels_crops_noNLCD_pooling.Rda"))
    }else
      print("Failed to Save")
    
} #End perform_cpmodels function

#Apply function to both model within and pooling
  model_types<-c("within", "pooling")
  sapply(model_types, function(n){perform_cpmodels(modeltype = n)})

  