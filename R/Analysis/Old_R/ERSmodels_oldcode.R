#Perform all regressions for model="within" (for coefficients) and model="pooling" (for running vif) using run_plm function/script

#Load and subset all necessary data and inputs
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
    
    #Data subset that has 1997 and 2002 removed, since there are so many NAs for those years across the datasets
    finaldata_3yrs<-finaldata[finaldata$Year!=1997&finaldata$Year!=2002,]

#Create models for all ERS
  #Create for loop that iterates through the 9 ERS groups (ignores ERS NA rows), and creates the 4 model RDA files per ERS
  ERS<-1:9 #9 ERS Regions (defined in Markdown)
  for (e in ERS){
    data_by_ERS<-finaldata[finaldata$ERSCode==e,]
    data_by_ERS_3yrs<-finaldata_3yrs[finaldata_3yrs$ERSCode==e,]

    
    perform_ERSmodels<-function(modeltype){
      
      #For each option perform: twoways, individual, and time fixed effects
      
      #A) All 7 variables
      ERS_twoways_var7<-run_plm(predictors=var7, model=modeltype, data=data_by_ERS)
      ERS_individual_var7<-run_plm(predictors=var7, effect = "individual", model=modeltype, data=data_by_ERS)
      ERS_time_var7<-run_plm(predictors=var7, effect = "time", model=modeltype, data=data_by_ERS)
      
      #B) 6 variables (no CDL)
      ERS_twoways_var6<-run_plm(predictors=var6, model=modeltype, data=data_by_ERS)
      ERS_individual_var6<-run_plm(predictors=var6, effect = "individual", model=modeltype, data=data_by_ERS)
      ERS_time_var6<-run_plm(predictors=var6, effect = "time", model=modeltype, data=data_by_ERS)
      
      #C) 6 variables (no CDL) but only 2007, 2012, and 2017 to match 3 years with CDL
      ERS_twoways_var6_3yrs<-run_plm(predictors=var6, data=data_by_ERS_3yrs, model=modeltype)
      ERS_individual_var6_3yrs<-run_plm(predictors=var6, effect = "individual", data=data_by_ERS_3yrs, model=modeltype)
      ERS_time_var6_3yrs<-run_plm(predictors=var6, effect = "time", data=data_by_ERS_3yrs, model=modeltype)
      
      #D) All crops covariates, repeating A-C with these inclusions
      #var7crops
      ERS_twoways_var7crops<-run_plm(predictors=var7crops, model=modeltype, data=data_by_ERS)
      ERS_individual_var7crops<-run_plm(predictors=var7crops, effect = "individual", model=modeltype, data=data_by_ERS)
      ERS_time_var7crops<-run_plm(predictors=var7crops, effect = "time", model=modeltype, data=data_by_ERS)
      
      #var6crops
      ERS_twoways_var6crops<-run_plm(predictors=var6crops, model=modeltype, data=data_by_ERS)
      ERS_individual_var6crops<-run_plm(predictors=var6crops, effect = "individual", model=modeltype, data=data_by_ERS)
      ERS_time_var6crops<-run_plm(predictors=var6crops, effect = "time", model=modeltype, data=data_by_ERS)
      
      #var6crops with finaldata_3yrs
      ERS_twoways_var6crops_3yrs<-run_plm(predictors=var6crops, data=data_by_ERS_3yrs, model=modeltype)
      ERS_individual_var6crops_3yrs<-run_plm(predictors=var6crops, effect = "individual", data=data_by_ERS_3yrs, model=modeltype)
      ERS_time_var6crops_3yrs<-run_plm(predictors=var6crops, effect = "time", data=data_by_ERS_3yrs, model=modeltype)
      
      
      #Creating two lists as otherwise files are too large for storage and want to keep everything together
      ERSmodels<-list(ERS_twoways_var7,ERS_individual_var7,ERS_time_var7,
                     ERS_twoways_var6,ERS_individual_var6,ERS_time_var6,
                     ERS_twoways_var6_3yrs,ERS_individual_var6_3yrs,ERS_time_var6_3yrs)
      
      names(ERSmodels)<-c("ERS_twoways_var7","ERS_individual_var7","ERS_time_var7",
                         "ERS_twoways_var6","ERS_individual_var6","ERS_time_var6",
                         "ERS_twoways_var6_3yrs","ERS_individual_var6_3yrs","ERS_time_var6_3yrs")
      
      ERSmodels_crops<-list(ERS_twoways_var7crops,ERS_individual_var7crops,ERS_time_var7crops,
                           ERS_twoways_var6crops,ERS_individual_var6crops,ERS_time_var6crops,
                           ERS_twoways_var6crops_3yrs,ERS_individual_var6crops_3yrs,ERS_time_var6crops_3yrs)
      
      names(ERSmodels_crops)<-c("ERS_twoways_var7crops","ERS_individual_var7crops","ERS_time_var7crops",
                               "ERS_twoways_var6crops","ERS_individual_var6crops","ERS_time_var6crops",
                               "ERS_twoways_var6crops_3yrs","ERS_individual_var6crops_3yrs","ERS_time_var6crops_3yrs")
      
      #Assign ERSmodels to within or pooled name
      assign(paste0("ERSmodels_", modeltype), ERSmodels)
      #Assign ERSmodels_crops to within or pooled name
      assign(paste0("ERSmodels_crops_", modeltype), ERSmodels_crops)
      
      #Save the correct ERSmodels file
      if(modeltype=="within"){
        save(ERSmodels_within, file = paste0("Data/Analysis/models/ERS/", e,"/ERSmodels_within.Rda"))
        save(ERSmodels_crops_within, file = paste0("Data/Analysis/models/ERS/", e,"/ERSmodels_crops_within.Rda"))
      }else if (modeltype=="pooling"){
        save(ERSmodels_pooling, file = paste0("Data/Analysis/models/ERS/", e,"/ERSmodels_pooling.Rda"))
        save(ERSmodels_crops_pooling, file = paste0("Data/Analysis/models/ERS/", e,"/ERSmodels_crops_pooling.Rda"))
      }else
        print("Failed to Save")
      
    } #End perform_ERSmodels function
    
    #Apply function to both model within and pooling
    model_types<-c("within", "pooling")
    sapply(model_types, function(n){perform_ERSmodels(modeltype = n)})
    
  }#End ERS for loop
    
