#Perform all regressions for ERS model 

#Load finaldata_plandcrops in case not loaded
load("Data/DataProcessing/df/finaldata_plandcrops.rda")

#Packages
library(tidyverse) #Datatable manipulation
library(broom) #Convert statistical analysis into df
library(clubSandwich) #for coeftest (cluster robust standard errors)
library(car) # For calculating variance inflation factors (vif)
library(stats)



#All 6 variables and 5 variables but only 3 years (same dataframe as if all 6 variables as CropDiversity removes all year 2002)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, natural_crops_ed, SDI) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#6620 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         NaturalCropEdge=residuals(lm(natural_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         CropDiversity=residuals(lm(SDI~factor(ASDCode)+factor(Year), data=final)))


Var6<-lm(Insecticide~PercentCrop*ERSCode + LandDiversity*ERSCode +
           MeanCropArea*ERSCode + LargeFarms*ERSCode + NaturalCropEdge*ERSCode +
           CropDiversity*ERSCode, data=resid_df)
Var6_df<-broom::tidy(Var6) %>% 
  mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_df_coef<- Var6 %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var6_vif<-rownames_to_column(as.data.frame(car::vif(Var6)), "term") %>% 
#   setNames(c("term", "Var6"))


Var5_3yrs<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                MeanCropArea*ERSCode+LargeFarms*ERSCode+NaturalCropEdge*ERSCode, data=resid_df)
Var5_3yrs_df<-broom::tidy(Var5_3yrs) %>% 
  mutate(model="Var5_3yrs") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_3yrs_df_coef<- Var5_3yrs %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5_3yrs") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_3yrs_vif<-rownames_to_column(as.data.frame(car::vif(Var5_3yrs)), "term") %>% 
#   setNames(c("term", "Var5_3yrs"))




#5 variables (no Crop Diversity)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, natural_crops_ed) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#8797 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         NaturalCropEdge=residuals(lm(natural_crops_ed~factor(ASDCode)+factor(Year), data=final)))

Var5<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
           MeanCropArea*ERSCode+LargeFarms*ERSCode+NaturalCropEdge*ERSCode, data=resid_df)
Var5_df<-broom::tidy(Var5) %>% 
  mutate(model="Var5") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_df_coef<- Var5 %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_vif<-rownames_to_column(as.data.frame(car::vif(Var5)), "term") %>% 
#   setNames(c("term", "Var5"))



#Add in Crop Covarates (6var and 5var_3yrs)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, natural_crops_ed, SDI,
         soysmallgrain_planted, corn_planted, fruitveg_planted) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#6590 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         NaturalCropEdge=residuals(lm(natural_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         CropDiversity=residuals(lm(SDI~factor(ASDCode)+factor(Year), data=final)),
         SoyGrains=residuals(lm(soysmallgrain_planted~factor(ASDCode)+factor(Year), data=final)),
         Corn=residuals(lm(corn_planted~factor(ASDCode)+factor(Year), data=final)),
         FruitVeg=residuals(lm(fruitveg_planted~factor(ASDCode)+factor(Year), data=final)))


Var6_crops<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                 MeanCropArea*ERSCode+LargeFarms*ERSCode+NaturalCropEdge*ERSCode+
                 CropDiversity*ERSCode+SoyGrains*ERSCode+Corn*ERSCode+FruitVeg*ERSCode,
               data=resid_df)
Var6_crops_df<-broom::tidy(Var6_crops) %>% 
  mutate(model="Var6_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_crops_df_coef<- Var6_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var6_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var6_crops)), "term") %>% 
#   setNames(c("term", "Var6_crops"))


Var5_3yrs_crops<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                      MeanCropArea*ERSCode+LargeFarms*ERSCode+NaturalCropEdge*ERSCode+
                      SoyGrains*ERSCode+Corn*ERSCode+FruitVeg*ERSCode, 
                    data=resid_df) 
Var5_3yrs_crops_df<-broom::tidy(Var5_3yrs_crops) %>% 
  mutate(model="Var5_3yrs_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_3yrs_crops_df_coef<- Var5_3yrs_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5_3yrs_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_3yrs_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var5_3yrs_crops)), "term") %>% 
#   setNames(c("term", "Var5_3yrs_crops"))



#Add in Crop Covarates (5var)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, natural_crops_ed,
         soysmallgrain_planted, corn_planted, fruitveg_planted) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#8752 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         NaturalCropEdge=residuals(lm(natural_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         SoyGrains=residuals(lm(soysmallgrain_planted~factor(ASDCode)+factor(Year), data=final)),
         Corn=residuals(lm(corn_planted~factor(ASDCode)+factor(Year), data=final)),
         FruitVeg=residuals(lm(fruitveg_planted~factor(ASDCode)+factor(Year), data=final)))

Var5_crops<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                 MeanCropArea*ERSCode+LargeFarms*ERSCode+NaturalCropEdge*ERSCode+
                 SoyGrains*ERSCode+Corn*ERSCode+FruitVeg*ERSCode, data=resid_df)
Var5_crops_df<-broom::tidy(Var5_crops) %>% 
  mutate(model="Var5_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_crops_df_coef<- Var5_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var5_crops)), "term") %>% 
#   setNames(c("term", "Var5_crops"))


#Combine dataframes
ERS_df<-rbind(Var6_df, Var5_3yrs_df, Var5_df, Var6_crops_df ,Var5_3yrs_crops_df, Var5_crops_df) %>% 
  dplyr::filter(!term=="ERSCode2"&!term=="ERSCode3"&!term=="ERSCode4"&
                  !term=="ERSCode5"&!term=="ERSCode6"&!term=="ERSCode7"&
                  !term=="ERSCode8"&!term=="ERSCode9") %>%
  separate(term, c("ERS", "term"), ":") %>% 
  mutate(term = gsub("ERSCode", "", term),
         ERS = gsub("ERSCode", "", ERS),
         term = replace_na(term, 1),
         reference=term) %>% 
  transform(term = ifelse(ERS == 'PercentCrop', ERS, reference),
            ERS = ifelse(ERS == 'PercentCrop', reference, ERS)) %>% 
  transform(term=ifelse(term=="1", ERS, term),
            ERS=ifelse(reference=="1", reference, ERS)) %>% 
  select(-reference) %>% 
  select(term, everything()) %>% 
  arrange(model, term,ERS)

ERS_df_coef<-rbind(Var6_df_coef, Var5_3yrs_df_coef, Var5_df_coef, Var6_crops_df_coef, Var5_3yrs_crops_df_coef, Var5_crops_df_coef) %>% 
  dplyr::filter(!term=="ERSCode2"&!term=="ERSCode3"&!term=="ERSCode4"&
                  !term=="ERSCode5"&!term=="ERSCode6"&!term=="ERSCode7"&
                  !term=="ERSCode8"&!term=="ERSCode9") %>% 
  separate(term, c("ERS", "term"), ":") %>% 
  mutate(term = gsub("ERSCode", "", term),
         ERS = gsub("ERSCode", "", ERS),
         term = replace_na(term, 1),
         reference=term) %>% 
  transform(term = ifelse(ERS == 'PercentCrop', ERS, reference),
            ERS = ifelse(ERS == 'PercentCrop', reference, ERS)) %>% 
  transform(term=ifelse(term=="1", ERS, term),
            ERS=ifelse(reference=="1", reference, ERS)) %>% 
  select(-reference) %>% 
  select(term, everything()) %>% 
  arrange(model, term,ERS)

# ERS_vif<-list(Var6_vif, Var5_3yrs_vif, Var5_vif, Var6_crops_vif, Var5_3yrs_crops_vif, Var5_crops_vif) %>% 
#   purrr::reduce(full_join, by = "term") %>% 
#   reshape2::melt(id.vars="term")
# colnames(ERS_vif)<-c("term", "model", "vif")

#Write csv for each of the created dataframes
write_csv(ERS_df, "Data/Analysis/df/ERS/ERS_df_plandcrops.csv")
write_csv(ERS_df_coef, "Data/Analysis/df/ERS/ERS_df_coeftest_plandcrops.csv") # No metadata yet
#write_csv(cp_vif, "Data/Analysis/df/ERS/ERS_vif_plandcrops.csv")


#Replace natural with forest and shrubgrass crop edges with repeated code below
###################################

#Forest
#######
#Perform all regressions for ERS model 

#Load finaldata_plandcrops in case not loaded
load("Data/DataProcessing/df/finaldata_plandcrops.rda")

#Packages
library(tidyverse) #Datatable manipulation
library(broom) #Convert statistical analysis into df
library(clubSandwich) #for coeftest (cluster robust standard errors)
library(car) # For calculating variance inflation factors (vif)
library(stats)



#All 6 variables and 5 variables but only 3 years (same dataframe as if all 6 variables as CropDiversity removes all year 2002)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, forest_crops_ed, SDI) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#6620 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         ForestCropEdge=residuals(lm(forest_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         CropDiversity=residuals(lm(SDI~factor(ASDCode)+factor(Year), data=final)))


Var6<-lm(Insecticide~PercentCrop*ERSCode + LandDiversity*ERSCode +
           MeanCropArea*ERSCode + LargeFarms*ERSCode + ForestCropEdge*ERSCode +
           CropDiversity*ERSCode, data=resid_df)
Var6_df<-broom::tidy(Var6) %>% 
  mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_df_coef<- Var6 %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var6_vif<-rownames_to_column(as.data.frame(car::vif(Var6)), "term") %>% 
#   setNames(c("term", "Var6"))


Var5_3yrs<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                MeanCropArea*ERSCode+LargeFarms*ERSCode+ForestCropEdge*ERSCode, data=resid_df)
Var5_3yrs_df<-broom::tidy(Var5_3yrs) %>% 
  mutate(model="Var5_3yrs") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_3yrs_df_coef<- Var5_3yrs %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5_3yrs") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_3yrs_vif<-rownames_to_column(as.data.frame(car::vif(Var5_3yrs)), "term") %>% 
#   setNames(c("term", "Var5_3yrs"))




#5 variables (no Crop Diversity)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, forest_crops_ed) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#8797 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         ForestCropEdge=residuals(lm(forest_crops_ed~factor(ASDCode)+factor(Year), data=final)))

Var5<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
           MeanCropArea*ERSCode+LargeFarms*ERSCode+ForestCropEdge*ERSCode, data=resid_df)
Var5_df<-broom::tidy(Var5) %>% 
  mutate(model="Var5") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_df_coef<- Var5 %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_vif<-rownames_to_column(as.data.frame(car::vif(Var5)), "term") %>% 
#   setNames(c("term", "Var5"))



#Add in Crop Covarates (6var and 5var_3yrs)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, forest_crops_ed, SDI,
         soysmallgrain_planted, corn_planted, fruitveg_planted) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#6590 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         ForestCropEdge=residuals(lm(forest_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         CropDiversity=residuals(lm(SDI~factor(ASDCode)+factor(Year), data=final)),
         SoyGrains=residuals(lm(soysmallgrain_planted~factor(ASDCode)+factor(Year), data=final)),
         Corn=residuals(lm(corn_planted~factor(ASDCode)+factor(Year), data=final)),
         FruitVeg=residuals(lm(fruitveg_planted~factor(ASDCode)+factor(Year), data=final)))


Var6_crops<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                 MeanCropArea*ERSCode+LargeFarms*ERSCode+ForestCropEdge*ERSCode+
                 CropDiversity*ERSCode+SoyGrains*ERSCode+Corn*ERSCode+FruitVeg*ERSCode,
               data=resid_df)
Var6_crops_df<-broom::tidy(Var6_crops) %>% 
  mutate(model="Var6_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_crops_df_coef<- Var6_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var6_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var6_crops)), "term") %>% 
#   setNames(c("term", "Var6_crops"))


Var5_3yrs_crops<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                      MeanCropArea*ERSCode+LargeFarms*ERSCode+ForestCropEdge*ERSCode+
                      SoyGrains*ERSCode+Corn*ERSCode+FruitVeg*ERSCode, 
                    data=resid_df) 
Var5_3yrs_crops_df<-broom::tidy(Var5_3yrs_crops) %>% 
  mutate(model="Var5_3yrs_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_3yrs_crops_df_coef<- Var5_3yrs_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5_3yrs_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_3yrs_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var5_3yrs_crops)), "term") %>% 
#   setNames(c("term", "Var5_3yrs_crops"))



#Add in Crop Covarates (5var)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, forest_crops_ed,
         soysmallgrain_planted, corn_planted, fruitveg_planted) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#8752 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         ForestCropEdge=residuals(lm(forest_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         SoyGrains=residuals(lm(soysmallgrain_planted~factor(ASDCode)+factor(Year), data=final)),
         Corn=residuals(lm(corn_planted~factor(ASDCode)+factor(Year), data=final)),
         FruitVeg=residuals(lm(fruitveg_planted~factor(ASDCode)+factor(Year), data=final)))

Var5_crops<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                 MeanCropArea*ERSCode+LargeFarms*ERSCode+ForestCropEdge*ERSCode+
                 SoyGrains*ERSCode+Corn*ERSCode+FruitVeg*ERSCode, data=resid_df)
Var5_crops_df<-broom::tidy(Var5_crops) %>% 
  mutate(model="Var5_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_crops_df_coef<- Var5_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var5_crops)), "term") %>% 
#   setNames(c("term", "Var5_crops"))


#Combine dataframes
ERS_df<-rbind(Var6_df, Var5_3yrs_df, Var5_df, Var6_crops_df ,Var5_3yrs_crops_df, Var5_crops_df) %>% 
  dplyr::filter(!term=="ERSCode2"&!term=="ERSCode3"&!term=="ERSCode4"&
                  !term=="ERSCode5"&!term=="ERSCode6"&!term=="ERSCode7"&
                  !term=="ERSCode8"&!term=="ERSCode9") %>%
  separate(term, c("ERS", "term"), ":") %>% 
  mutate(term = gsub("ERSCode", "", term),
         ERS = gsub("ERSCode", "", ERS),
         term = replace_na(term, 1),
         reference=term) %>% 
  transform(term = ifelse(ERS == 'PercentCrop', ERS, reference),
            ERS = ifelse(ERS == 'PercentCrop', reference, ERS)) %>% 
  transform(term=ifelse(term=="1", ERS, term),
            ERS=ifelse(reference=="1", reference, ERS)) %>% 
  select(-reference) %>% 
  select(term, everything()) %>% 
  arrange(model, term,ERS)

ERS_df_coef<-rbind(Var6_df_coef, Var5_3yrs_df_coef, Var5_df_coef, Var6_crops_df_coef, Var5_3yrs_crops_df_coef, Var5_crops_df_coef) %>% 
  dplyr::filter(!term=="ERSCode2"&!term=="ERSCode3"&!term=="ERSCode4"&
                  !term=="ERSCode5"&!term=="ERSCode6"&!term=="ERSCode7"&
                  !term=="ERSCode8"&!term=="ERSCode9") %>% 
  separate(term, c("ERS", "term"), ":") %>% 
  mutate(term = gsub("ERSCode", "", term),
         ERS = gsub("ERSCode", "", ERS),
         term = replace_na(term, 1),
         reference=term) %>% 
  transform(term = ifelse(ERS == 'PercentCrop', ERS, reference),
            ERS = ifelse(ERS == 'PercentCrop', reference, ERS)) %>% 
  transform(term=ifelse(term=="1", ERS, term),
            ERS=ifelse(reference=="1", reference, ERS)) %>% 
  select(-reference) %>% 
  select(term, everything()) %>% 
  arrange(model, term,ERS)

# ERS_vif<-list(Var6_vif, Var5_3yrs_vif, Var5_vif, Var6_crops_vif, Var5_3yrs_crops_vif, Var5_crops_vif) %>% 
#   purrr::reduce(full_join, by = "term") %>% 
#   reshape2::melt(id.vars="term")
# colnames(ERS_vif)<-c("term", "model", "vif")

#Write csv for each of the created dataframes
write_csv(ERS_df, "Data/Analysis/df/ERS/ERS_df_forest_plandcrops.csv")
write_csv(ERS_df_coef, "Data/Analysis/df/ERS/ERS_df_coeftest_forest_plandcrops.csv") # No metadata yet
#write_csv(cp_vif, "Data/Analysis/df/ERS/ERS_vif_forest_plandcrops.csv")



#Shrubgrass
#######
#Perform all regressions for ERS model 

#Load finaldata_plandcrops in case not loaded
load("Data/DataProcessing/df/finaldata_plandcrops.rda")

#Packages
library(tidyverse) #Datatable manipulation
library(broom) #Convert statistical analysis into df
library(clubSandwich) #for coeftest (cluster robust standard errors)
library(car) # For calculating variance inflation factors (vif)
library(stats)



#All 6 variables and 5 variables but only 3 years (same dataframe as if all 6 variables as CropDiversity removes all year 2002)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, shrubgrass_crops_ed, SDI) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#6620 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         ShrubGrassCropEdge=residuals(lm(shrubgrass_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         CropDiversity=residuals(lm(SDI~factor(ASDCode)+factor(Year), data=final)))


Var6<-lm(Insecticide~PercentCrop*ERSCode + LandDiversity*ERSCode +
           MeanCropArea*ERSCode + LargeFarms*ERSCode + ShrubGrassCropEdge*ERSCode +
           CropDiversity*ERSCode, data=resid_df)
Var6_df<-broom::tidy(Var6) %>% 
  mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_df_coef<- Var6 %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var6_vif<-rownames_to_column(as.data.frame(car::vif(Var6)), "term") %>% 
#   setNames(c("term", "Var6"))


Var5_3yrs<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                MeanCropArea*ERSCode+LargeFarms*ERSCode+ShrubGrassCropEdge*ERSCode, data=resid_df)
Var5_3yrs_df<-broom::tidy(Var5_3yrs) %>% 
  mutate(model="Var5_3yrs") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_3yrs_df_coef<- Var5_3yrs %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5_3yrs") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_3yrs_vif<-rownames_to_column(as.data.frame(car::vif(Var5_3yrs)), "term") %>% 
#   setNames(c("term", "Var5_3yrs"))




#5 variables (no Crop Diversity)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, shrubgrass_crops_ed) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#8797 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         ShrubGrassCropEdge=residuals(lm(shrubgrass_crops_ed~factor(ASDCode)+factor(Year), data=final)))

Var5<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
           MeanCropArea*ERSCode+LargeFarms*ERSCode+ShrubGrassCropEdge*ERSCode, data=resid_df)
Var5_df<-broom::tidy(Var5) %>% 
  mutate(model="Var5") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_df_coef<- Var5 %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_vif<-rownames_to_column(as.data.frame(car::vif(Var5)), "term") %>% 
#   setNames(c("term", "Var5"))



#Add in Crop Covarates (6var and 5var_3yrs)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, shrubgrass_crops_ed, SDI,
         soysmallgrain_planted, corn_planted, fruitveg_planted) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#6590 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         ShrubGrassCropEdge=residuals(lm(shrubgrass_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         CropDiversity=residuals(lm(SDI~factor(ASDCode)+factor(Year), data=final)),
         SoyGrains=residuals(lm(soysmallgrain_planted~factor(ASDCode)+factor(Year), data=final)),
         Corn=residuals(lm(corn_planted~factor(ASDCode)+factor(Year), data=final)),
         FruitVeg=residuals(lm(fruitveg_planted~factor(ASDCode)+factor(Year), data=final)))


Var6_crops<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                 MeanCropArea*ERSCode+LargeFarms*ERSCode+ShrubGrassCropEdge*ERSCode+
                 CropDiversity*ERSCode+SoyGrains*ERSCode+Corn*ERSCode+FruitVeg*ERSCode,
               data=resid_df)
Var6_crops_df<-broom::tidy(Var6_crops) %>% 
  mutate(model="Var6_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_crops_df_coef<- Var6_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var6_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var6_crops)), "term") %>% 
#   setNames(c("term", "Var6_crops"))


Var5_3yrs_crops<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                      MeanCropArea*ERSCode+LargeFarms*ERSCode+ShrubGrassCropEdge*ERSCode+
                      SoyGrains*ERSCode+Corn*ERSCode+FruitVeg*ERSCode, 
                    data=resid_df) 
Var5_3yrs_crops_df<-broom::tidy(Var5_3yrs_crops) %>% 
  mutate(model="Var5_3yrs_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_3yrs_crops_df_coef<- Var5_3yrs_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5_3yrs_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_3yrs_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var5_3yrs_crops)), "term") %>% 
#   setNames(c("term", "Var5_3yrs_crops"))



#Add in Crop Covarates (5var)
finaldf<-finaldata_plandcrops %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, shrubgrass_crops_ed,
         soysmallgrain_planted, corn_planted, fruitveg_planted) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#8752 observations

resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         ShrubGrassCropEdge=residuals(lm(shrubgrass_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         SoyGrains=residuals(lm(soysmallgrain_planted~factor(ASDCode)+factor(Year), data=final)),
         Corn=residuals(lm(corn_planted~factor(ASDCode)+factor(Year), data=final)),
         FruitVeg=residuals(lm(fruitveg_planted~factor(ASDCode)+factor(Year), data=final)))

Var5_crops<-lm(Insecticide~PercentCrop*ERSCode+LandDiversity*ERSCode+
                 MeanCropArea*ERSCode+LargeFarms*ERSCode+ShrubGrassCropEdge*ERSCode+
                 SoyGrains*ERSCode+Corn*ERSCode+FruitVeg*ERSCode, data=resid_df)
Var5_crops_df<-broom::tidy(Var5_crops) %>% 
  mutate(model="Var5_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var5_crops_df_coef<- Var5_crops %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = resid_df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var5_crops") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
# Var5_crops_vif<-rownames_to_column(as.data.frame(car::vif(Var5_crops)), "term") %>% 
#   setNames(c("term", "Var5_crops"))


#Combine dataframes
ERS_df<-rbind(Var6_df, Var5_3yrs_df, Var5_df, Var6_crops_df ,Var5_3yrs_crops_df, Var5_crops_df) %>% 
  dplyr::filter(!term=="ERSCode2"&!term=="ERSCode3"&!term=="ERSCode4"&
                  !term=="ERSCode5"&!term=="ERSCode6"&!term=="ERSCode7"&
                  !term=="ERSCode8"&!term=="ERSCode9") %>%
  separate(term, c("ERS", "term"), ":") %>% 
  mutate(term = gsub("ERSCode", "", term),
         ERS = gsub("ERSCode", "", ERS),
         term = replace_na(term, 1),
         reference=term) %>% 
  transform(term = ifelse(ERS == 'PercentCrop', ERS, reference),
            ERS = ifelse(ERS == 'PercentCrop', reference, ERS)) %>% 
  transform(term=ifelse(term=="1", ERS, term),
            ERS=ifelse(reference=="1", reference, ERS)) %>% 
  select(-reference) %>% 
  select(term, everything()) %>% 
  arrange(model, term,ERS)

ERS_df_coef<-rbind(Var6_df_coef, Var5_3yrs_df_coef, Var5_df_coef, Var6_crops_df_coef, Var5_3yrs_crops_df_coef, Var5_crops_df_coef) %>% 
  dplyr::filter(!term=="ERSCode2"&!term=="ERSCode3"&!term=="ERSCode4"&
                  !term=="ERSCode5"&!term=="ERSCode6"&!term=="ERSCode7"&
                  !term=="ERSCode8"&!term=="ERSCode9") %>% 
  separate(term, c("ERS", "term"), ":") %>% 
  mutate(term = gsub("ERSCode", "", term),
         ERS = gsub("ERSCode", "", ERS),
         term = replace_na(term, 1),
         reference=term) %>% 
  transform(term = ifelse(ERS == 'PercentCrop', ERS, reference),
            ERS = ifelse(ERS == 'PercentCrop', reference, ERS)) %>% 
  transform(term=ifelse(term=="1", ERS, term),
            ERS=ifelse(reference=="1", reference, ERS)) %>% 
  select(-reference) %>% 
  select(term, everything()) %>% 
  arrange(model, term,ERS)

# ERS_vif<-list(Var6_vif, Var5_3yrs_vif, Var5_vif, Var6_crops_vif, Var5_3yrs_crops_vif, Var5_crops_vif) %>% 
#   purrr::reduce(full_join, by = "term") %>% 
#   reshape2::melt(id.vars="term")
# colnames(ERS_vif)<-c("term", "model", "vif")

#Write csv for each of the created dataframes
write_csv(ERS_df, "Data/Analysis/df/ERS/ERS_df_shrubgrass_plandcrops.csv")
write_csv(ERS_df_coef, "Data/Analysis/df/ERS/ERS_df_coeftest_shrubgrass_plandcrops.csv") # No metadata yet
#write_csv(cp_vif, "Data/Analysis/df/ERS/ERS_vif_shrubgrass_plandcrops.csv")


