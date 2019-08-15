#Perform all regressions using run_lm function/script

#Load finaldata in case not loaded
load("Data/DataProcessing/df/finaldata.rda")

#Source the run_lm function, created in the run_lm script in order to perform the panel linear model regressions
source("R/Analysis/run_lm.R")

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

    
    #A) All 7 variables
    cp_twoways_var7<-run_lm(predictors=var7)
    
    #B) 6 variables (no CDL)
    cp_twoways_var6<-run_lm(predictors=var6)
    
    #C) 6 variables (no CDL) but only 2007, 2012, and 2017 to match 3 years with CDL
    cp_twoways_var6_3yrs<-run_lm(predictors=var6, data=finaldata_3yrs)
    
    #D) All crops covariates, repeating A-C with these inclusions
    #var7crops
    cp_twoways_var7crops<-run_lm(predictors=var7crops)
    #var6crops
    cp_twoways_var6crops<-run_lm(predictors=var6crops)
    #var6crops with finaldata_3yrs
    cp_twoways_var6crops_3yrs<-run_lm(predictors=var6crops, data=finaldata_3yrs)
    
    #E) All crops covariates and no NLCD variables, repeating A-C with these inclusions and exclusions
    #var4crops
    cp_twoways_var4crops<-run_lm(predictors=var4crops)
    #var3crops
    cp_twoways_var3crops<-run_lm(predictors=var3crops)
    #var3crops with finaldata_3yrs
    cp_twoways_var3crops_3yrs<-run_lm(predictors=var3crops, data=finaldata_3yrs)
    
    #Creating three lists as otherwise files are too large for storage and want to keep everything together
    cpmodels<-list(cp_twoways_var7,cp_twoways_var6,cp_twoways_var6_3yrs)
    names(cpmodels)<-c("Var7","Var6","Var6_3yrs")
    
    cpmodels_crops<-list(cp_twoways_var7crops,cp_twoways_var6crops,cp_twoways_var6crops_3yrs)
    names(cpmodels_crops)<-c("Var7crops","Var6crops","Var6crops_3yrs")
   
    cpmodels_crops_noNLCD<-list(cp_twoways_var4crops,cp_twoways_var3crops,cp_twoways_var3crops_3yrs)
    names(cpmodels_crops_noNLCD)<-c("Var4crops","Var3crops","Var3crops_3yrs")
    
    #Save
    save(cpmodels, file = paste0("Data/Analysis/models/CP/cpmodels.Rda"))
    save(cpmodels_crops, file = paste0("Data/Analysis/models/CP/cpmodels_crops.Rda"))
    save(cpmodels_crops_noNLCD, file = paste0("Data/Analysis/models/CP/cpmodels_crops_noNLCD.Rda"))
    