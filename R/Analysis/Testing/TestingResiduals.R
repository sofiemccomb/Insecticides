#Testing if fixed effects test on residuals produces same results as fixed effects tests untransformed
  #Using county to prove since issue with duplicate records for ASDCode

#Load finaldata in case not loaded
load("Data/DataProcessing/df/finaldata.rda")

#Packages
library(tidyverse) #Datatable manipulation
library(broom) #Convert statistical analysis into df
library(clubSandwich) #for coeftest (cluster robust standard errors)
library(car) # For calculating variance inflation factors (vif)
library(stats)


#All 6 variables and 5 variables but only 3 years (same dataframe as if all 6 variables as CropDiversity removes all year 2002)
finaldf<-finaldata %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, natural_crops_ed, SDI)
final<-na.omit(finaldf)#8984 observations


############
#With Residuals and County
resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(FIPS)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(FIPS)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(FIPS)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(FIPS)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(FIPS)+factor(Year), data=final)),
         NaturalCropEdge=residuals(lm(natural_crops_ed~factor(FIPS)+factor(Year), data=final)),
         CropDiversity=residuals(lm(SDI~factor(FIPS)+factor(Year), data=final)))

Var6<-lm(Insecticide~PercentCrop+LandDiversity+MeanCropArea+
           LargeFarms+NaturalCropEdge+CropDiversity, data=resid_df)
Var6_df<-broom::tidy(Var6) %>% 
  mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))


######################
#Without residuals, but for county

df<-final %>%
  mutate(Insecticide=insect_planted,
         PercentCrop=pland_crops,
         LandDiversity=msidi,
         MeanCropArea=mna_crops,
         LargeFarms=largefarm_planted,
         NaturalCropEdge=natural_crops_ed,
         CropDiversity=SDI)

Var6_noresid<-lm(Insecticide~PercentCrop+LandDiversity+MeanCropArea+
           LargeFarms+NaturalCropEdge+CropDiversity+ factor(FIPS) + factor(Year), data=df)
Var6_df_noresid<-broom::tidy(Var6_noresid) %>% 
  mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))


readr::write_csv(Var6_df, "Data/Analysis/Testing/residuals_var6df.csv")
readr::write_csv(Var6_df_noresid, "Data/Analysis/Testing/noresiduals_var6df.csv")

