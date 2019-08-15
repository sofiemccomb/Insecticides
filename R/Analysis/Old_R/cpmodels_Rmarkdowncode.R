load("Data/Analysis/models/CP/OriginalResults_finaldata/cpmodels_within.Rda")
load("Data/Analysis/models/CP/OriginalResults_finaldata/cpmodels_crops_within.Rda")
load("Data/Analysis/models/CP/OriginalResults_finaldata/cpmodels_crops_noNLCD_within.Rda")
load("Data/Analysis/models/CP/OriginalResults_finaldata/cpmodels_pooling.Rda")
load("Data/Analysis/models/CP/OriginalResults_finaldata/cpmodels_crops_pooling.Rda")
load("Data/Analysis/models/CP/OriginalResults_finaldata/cpmodels_crops_noNLCD_pooling.Rda")

#Combine within and pooling lists of models together to perform analyses
cpmodelswithin<-c(cpmodels_within, cpmodels_crops_within, cpmodels_crops_noNLCD_within)
cpmodelspooling<-c(cpmodels_pooling, cpmodels_crops_pooling, cpmodels_crops_noNLCD_pooling)


#Broom package used to put plm model information into tidy dataframe format- was using before using coef_test (check which correct)
#tidy(): cofficients, augment(): residuals, glance(): model statistic values

#Create cp_df for coefficient dataframe
cp_df<- cpmodelswithin[[1]] %>%
  broom::tidy() %>%
  dplyr::mutate(model = names(cpmodelswithin)[[1]])

#Fill dataframe with model results
for (i in 2:length(cpmodelswithin)){
  cp_model<-cpmodelswithin[[i]]
  cp_df<-rbind(cp_df, cp_model %>%
                 broom::tidy() %>%
                 mutate(model=names(cpmodelswithin)[[i]]))
} #end for loop of model dataframe


#Create cp_df for coefficient dataframe with cluster-robust standard errors, using Satterthwaite (default)
cp_df_coef<- cpmodelswithin[[1]] %>% 
  clubSandwich::coef_test(vcov = "CR2", cluster = "individual", test = "Satterthwaite") %>% 
  tibble::rownames_to_column(var = "term") %>% 
  rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
  dplyr::mutate(model=names(cpmodelswithin)[[1]])

#Fill dataframe with model results
for (i in 2:length(cpmodelswithin)){
  cp_model<-cpmodelswithin[[i]]
  cp_df_coef<-rbind(cp_df_coef, cp_model %>%
                      clubSandwich::coef_test(vcov = "CR2", cluster = "individual", test = "Satterthwaite") %>% 
                      tibble::rownames_to_column(var = "term") %>% 
                      rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
                      dplyr::mutate(model=names(cpmodelswithin)[[i]])
  )#end rbind
} #end for loop of model dataframe

#Run variance inflation factor (vif) to look at multicollinearity between variables and look at correlation matrices
#Can only run on model=pooling for plm
#However, since vif is about examining indepdenent variables, theoretically there is less need to control for effects
#VIF of 1=no correlation among predictors, >4 investigate, >10 serious correction

#VIF on pooled data
cp_vif<-lapply(cpmodelspooling, function(p){rownames_to_column(as.data.frame(car::vif(p)), "term")}) %>%
  purrr::reduce(full_join, by = "term") %>%
  setNames(c("term", names(cpmodelspooling))) %>% 
  reshape2::melt(id.vars="term")
colnames(cp_vif)<-c("term", "model", "vif")

#Correlation matrix on within data
cp_corr<-lapply(cpmodelswithin, function(v){cov2cor(v$vcov)})

#Write csv for each of the created dataframes and rda for the correlation matrix list (save under Data/Analysis/df/CP)
write_csv(cp_df, "Data/Analysis/df/CP/OriginalResults_finaldata/cp_df.csv")
write_csv(cp_df_coef, "Data/Analysis/df/CP/OriginalResults_finaldata/cp_df_coeftest.csv") # No metadata yet
write_csv(cp_vif, "Data/Analysis/df/CP/OriginalResults_finaldata/cp_vif.csv")
save(cp_corr, file="Data/Analysis/df/CP/OriginalResults_finaldata/cp_corr.Rda")