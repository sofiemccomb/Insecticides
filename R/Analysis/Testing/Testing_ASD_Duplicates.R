#Dealing with duplicate record issue for ASDCode

#Load finaldata in case not loaded
finaldata<-readr::read_csv("Data/DataProcessing/df/finaldata.csv")#Change to wherever csv stored

#Packages
library(tidyverse) #Datatable manipulation
library(broom) #Convert statistical analysis into df
library(clubSandwich) #for coeftest (cluster robust standard errors)
library(car) # For calculating variance inflation factors (vif)
library(stats)


#All 6 variables and 5 variables but only 3 years (same dataframe as if all 6 variables as CropDiversity removes all year 2002)
finaldf<-finaldata %>% 
  select(FIPS, Year, ASDCode, ERSCode,
         insect_planted,pland_crops, msidi, mna_crops, largefarm_planted, natural_crops_ed, SDI) %>% 
  mutate(ERSCode=factor(ERSCode))
final<-na.omit(finaldf)#8984 observations


###########
#If attempting to just run with fixed effects as is
df<-final %>%
  mutate(Insecticide=insect_planted,
         PercentCrop=pland_crops,
         LandDiversity=msidi,
         MeanCropArea=mna_crops,
         LargeFarms=largefarm_planted,
         NaturalCropEdge=natural_crops_ed,
         CropDiversity=SDI)

Var6_noresid<-lm(Insecticide~PercentCrop+LandDiversity+MeanCropArea+
                   LargeFarms+NaturalCropEdge+CropDiversity+ factor(ASDCode) + factor(Year), data=df)
Var6_df_noresid<-broom::tidy(Var6_noresid) %>% 
  mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_df_coef_noresid<- Var6_noresid %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_vif_noresid<-rownames_to_column(as.data.frame(car::vif(Var6_noresid)), "term") %>% 
  setNames(c("term", "Var6"))


#Does it work for ERS-NO??? It go through but doesnt account for it and wrong coefficients and not for dfiferent ERS
Var6_noresid<-lm(Insecticide~PercentCrop*ERSCode + LandDiversity*ERSCode +
           MeanCropArea*ERSCode + LargeFarms*ERSCode + NaturalCropEdge*ERSCode +
           CropDiversity*ERSCode + factor(ASDCode)*ERSCode + factor(Year)*ERSCode, data=df)
Var6_df_noresid<-broom::tidy(Var6_noresid) %>% 
  mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))
Var6_df_coef_noresid<- Var6_noresid %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = df$ASDCode, test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model="Var6") %>% 
  dplyr::filter(!stringr::str_detect(term, "(Intercept)"))


#Current method with dealing with duplicates
#################
#With Residuals and County
resid_df<-final %>%
  select(FIPS, Year, ASDCode, ERSCode) %>% 
  mutate(Insecticide=residuals(lm(insect_planted~factor(ASDCode)+factor(Year), data=final)),
         PercentCrop=residuals(lm(pland_crops~factor(ASDCode)+factor(Year), data=final)),
         LandDiversity=residuals(lm(msidi~factor(ASDCode)+factor(Year), data=final)),
         MeanCropArea=residuals(lm(mna_crops~factor(ASDCode)+factor(Year), data=final)),
         LargeFarms=residuals(lm(largefarm_planted~factor(ASDCode)+factor(Year), data=final)),
         NaturalCropEdge=residuals(lm(natural_crops_ed~factor(ASDCode)+factor(Year), data=final)),
         CropDiversity=residuals(lm(SDI~factor(ASDCode)+factor(Year), data=final)))

Var6<-lm(Insecticide~PercentCrop+LandDiversity+MeanCropArea+
           LargeFarms+NaturalCropEdge+CropDiversity, data=resid_df)
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




#Testing again but for ERS
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
