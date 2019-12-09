#Testing if results change based on when standardizing variables (balanced vs unbalanced panels)

#Packages
library(tidyverse) #Datatable manipulation
library(broom) #Convert statistical analysis into df
library(clubSandwich) #for coeftest (cluster robust standard errors)
library(car) # For calculating variance inflation factors (vif)
library(stats)

#Load finaldata in case not loaded
load("Data/DataProcessing/df/fulldata.rda")

###################
#Data

#Unbalanced standardization
finaldata_unbalanced<-fulldata %>% 
  select(FIPS, Year, ERSCode, ASDCode, insect_planted, pland_crops, SDI,
         largefarm_planted, mna_crops, natural_crops_ed, msidi,
         soysmallgrain_planted, corn_planted, fruitveg_planted,
         forest_crops_ed, shrubgrass_crops_ed) %>% 
  mutate_at(scale, .vars=vars(-FIPS, -Year, -ERSCode, -ASDCode, -insect_planted)) #Scale by subtracting column mean and dividing by column SD
finaldata_unbalanced<-finaldata_unbalanced[!is.na(fulldata$ASDCode),]#Remove rows with NA value for ASDcode
finaldf_unbalanced<-finaldata_unbalanced %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, natural_crops_ed, SDI)
final_unbalanced<-na.omit(finaldf_unbalanced)#8984 observations

#Balanced standardization
finaldata_balanced<-fulldata %>% 
  select(FIPS, Year, ERSCode, ASDCode, insect_planted, pland_crops, SDI,
         largefarm_planted, mna_crops, natural_crops_ed, msidi,
         soysmallgrain_planted, corn_planted, fruitveg_planted,
         forest_crops_ed, shrubgrass_crops_ed)
finaldata_balanced<-finaldata_balanced[!is.na(fulldata$ASDCode),]#Remove rows with NA value for ASDcode
finaldf_balanced<-finaldata_balanced %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, natural_crops_ed, SDI)
final_balanced<-na.omit(finaldf_balanced)#8984 observations
final_balanced_sd<-final_balanced %>% 
  mutate_at(scale, .vars=vars(-FIPS, -Year, -ERSCode, -ASDCode, -insect_planted)) #Scale by subtracting column mean and dividing by column SD


#Does change the coefficients a little

###########

df_unbalanced<-final_unbalanced %>%
  mutate(Insecticide=insect_planted,
         PercentCrop=pland_crops,
         LandDiversity=msidi,
         MeanCropArea=mna_crops,
         LargeFarms=largefarm_planted,
         NaturalCropEdge=natural_crops_ed,
         CropDiversity=SDI)
df_balanced<-final_balanced %>%
  mutate(Insecticide=insect_planted,
         PercentCrop=pland_crops,
         LandDiversity=msidi,
         MeanCropArea=mna_crops,
         LargeFarms=largefarm_planted,
         NaturalCropEdge=natural_crops_ed,
         CropDiversity=SDI)

Var6_unbalanced<-lm(Insecticide~PercentCrop+LandDiversity+MeanCropArea+
                   LargeFarms+NaturalCropEdge+CropDiversity+ factor(ASDCode) + factor(Year), data=df_unbalanced)
Var6_df_unbalanced<-broom::tidy(Var6_unbalanced) %>% 
  mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))

Var6_balanced<-lm(Insecticide~PercentCrop+LandDiversity+MeanCropArea+
                      LargeFarms+NaturalCropEdge+CropDiversity+ factor(ASDCode) + factor(Year), data=df_balanced)
Var6_df_balanced<-broom::tidy(Var6_balanced) %>% 
  mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))


Var6_df_coef_unbalanced<- Var6_unbalanced %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = df_unbalanced$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))

Var6_df_coef_balanced<- Var6_balanced %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = df_balanced$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))