#Perform all regressions for counties pooled model 

#Load finaldata in case not loaded
load("Data/DataProcessing/df/finaldata.rda")

#Packages
library(tidyverse) #Datatable manipulation
library(broom) #Convert statistical analysis into df
library(clubSandwich) #for coeftest (cluster robust standard errors)
library(car) # For calculating variance inflation factors (vif)
library(stats)



#All 7 variables and 6 variables but only 3 years (same dataframe as if all 7 variables as CropDiversity removes all year 2002)
finaldf<-finaldata %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, cropintensity, msidi, mna_crops, largefarm_planted, natural_crops_ed, SDI)
final<-na.omit(finaldf)#8905 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         CropIntensity=residuals(lm(cropintensity~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         NaturalCropEdge=residuals(lm(natural_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         CropDiversity=residuals(lm(SDI~factor(ASDCode)+factor(Year), data=final)))


Var7<-lm(Insecticide~PercentCrop+CropIntensity+LandDiversity+MeanCropArea+
           LargeFarms+NaturalCropEdge+CropDiversity, data=resid_df)
Var7_df<-broom::tidy(Var7) %>% 
  mutate(model="Var7") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var7_df_coef<- Var7 %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var7") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var7_vif<-rownames_to_column(as.data.frame(car::vif(Var7)), "term") %>% 
  setNames(c("term", "Var7"))


Var6_3yrs<-lm(Insecticide~PercentCrop+CropIntensity+LandDiversity+MeanCropArea+
                LargeFarms+NaturalCropEdge, data=resid_df)
Var6_3yrs_df<-broom::tidy(Var6_3yrs) %>% 
  mutate(model="Var6_3yrs") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_3yrs_df_coef<- Var6_3yrs %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6_3yrs") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_3yrs_vif<-rownames_to_column(as.data.frame(car::vif(Var6_3yrs)), "term") %>% 
  setNames(c("term", "Var6_3yrs"))




#6 variables (no Crop Diversity)
finaldf<-finaldata %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, cropintensity, msidi, mna_crops, largefarm_planted, natural_crops_ed)
final<-na.omit(finaldf)#11881 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         CropIntensity=residuals(lm(cropintensity~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         NaturalCropEdge=residuals(lm(natural_crops_ed~factor(ASDCode)+factor(Year), data=final)))

Var6<-lm(Insecticide~PercentCrop+CropIntensity+LandDiversity+MeanCropArea+
           LargeFarms+NaturalCropEdge, data=resid_df)
Var6_df<-broom::tidy(Var6) %>% 
  mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_df_coef<- Var6 %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_vif<-rownames_to_column(as.data.frame(car::vif(Var6)), "term") %>% 
  setNames(c("term", "Var6"))



#Add in Crop Covarates (7var and 6var_3yrs)
finaldf<-finaldata %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, cropintensity, msidi, mna_crops, largefarm_planted, natural_crops_ed, SDI,
         soysmallgrain_planted, corn_planted, fruitveg_planted)
final<-na.omit(finaldf)#8860 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         CropIntensity=residuals(lm(cropintensity~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         NaturalCropEdge=residuals(lm(natural_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         CropDiversity=residuals(lm(SDI~factor(ASDCode)+factor(Year), data=final)),
         SoyGrains=residuals(lm(soysmallgrain_planted~factor(ASDCode)+factor(Year), data=final)),
         Corn=residuals(lm(corn_planted~factor(ASDCode)+factor(Year), data=final)),
         FruitVeg=residuals(lm(fruitveg_planted~factor(ASDCode)+factor(Year), data=final)))


Var7_crops<-lm(Insecticide~PercentCrop+CropIntensity+LandDiversity+MeanCropArea+
                 LargeFarms+NaturalCropEdge+CropDiversity+SoyGrains+Corn+FruitVeg, data=resid_df)
Var7_crops_df<-broom::tidy(Var7_crops) %>% 
  mutate(model="Var7_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var7_crops_df_coef<- Var7_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var7_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var7_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var7_crops)), "term") %>% 
  setNames(c("term", "Var7_crops"))


Var6_3yrs_crops<-lm(Insecticide~PercentCrop+CropIntensity+LandDiversity+MeanCropArea+
                      LargeFarms+NaturalCropEdge+SoyGrains+Corn+FruitVeg, data=resid_df) 
Var6_3yrs_crops_df<-broom::tidy(Var6_3yrs_crops) %>% 
  mutate(model="Var6_3yrs_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_3yrs_crops_df_coef<- Var6_3yrs_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6_3yrs_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_3yrs_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var6_3yrs_crops)), "term") %>% 
  setNames(c("term", "Var6_3yrs_crops"))



#Add in Crop Covarates (6var)
finaldf<-finaldata %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, cropintensity, msidi, mna_crops, largefarm_planted, natural_crops_ed,
         soysmallgrain_planted, corn_planted, fruitveg_planted)
final<-na.omit(finaldf)#11816 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         CropIntensity=residuals(lm(cropintensity~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         NaturalCropEdge=residuals(lm(natural_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         SoyGrains=residuals(lm(soysmallgrain_planted~factor(ASDCode)+factor(Year), data=final)),
         Corn=residuals(lm(corn_planted~factor(ASDCode)+factor(Year), data=final)),
         FruitVeg=residuals(lm(fruitveg_planted~factor(ASDCode)+factor(Year), data=final)))

Var6_crops<-lm(Insecticide~PercentCrop+CropIntensity+LandDiversity+MeanCropArea+
                 LargeFarms+NaturalCropEdge+SoyGrains+Corn+FruitVeg, data=resid_df)
Var6_crops_df<-broom::tidy(Var6_crops) %>% 
  mutate(model="Var6_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_crops_df_coef<- Var6_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var6_crops)), "term") %>% 
  setNames(c("term", "Var6_crops"))


#Combine dataframes
cp_df<-rbind(Var7_df, Var6_3yrs_df, Var6_df, Var7_crops_df ,Var6_3yrs_crops_df, Var6_crops_df)
cp_df_coef<-rbind(Var7_df_coef, Var6_3yrs_df_coef, Var6_df_coef, Var7_crops_df_coef, Var6_3yrs_crops_df_coef, Var6_crops_df_coef)
cp_vif<-list(Var7_vif, Var6_3yrs_vif, Var6_vif, Var7_crops_vif, Var6_3yrs_crops_vif, Var6_crops_vif) %>% 
  purrr::reduce(full_join, by = "term") %>% 
  reshape2::melt(id.vars="term")
colnames(cp_vif)<-c("term", "model", "vif")

#Write csv for each of the created dataframes and rda for the correlation matrix list (save under Data/Analysis/df/CP)
write_csv(cp_df, "Data/Analysis/df/CP/cp_df.csv")
write_csv(cp_df_coef, "Data/Analysis/df/CP/cp_df_coeftest.csv") # No metadata yet
write_csv(cp_vif, "Data/Analysis/df/CP/cp_vif.csv")
