#Iterate through the ERS region models and perform analyses/create df for each ERS before combining
ERS<-1:9 #9 ERS Regions (defined in Markdown)
for (e in ERS){
  #Load all 4 models within each ERS subfolder, which will be overwritten for every ERS
  fn<-as.list(list.files(path=paste0("Data/Analysis/models/ERS/", e), full.names = TRUE ))
  lapply(fn,load,.GlobalEnv) #loaded 4 files: ERSmodels_within/pooling & ERSmodels_crops_within/pooling
  
  #Combine within and pooling lists of models together to perform analyses
  ERSmodelswithin<-c(ERSmodels_within, ERSmodels_crops_within)
  ERSmodelspooling<-c(ERSmodels_pooling, ERSmodels_crops_pooling)
  
  #Broom package used to put plm model information into tidy dataframe format
  #tidy(): cofficients, augment(): residuals, glance(): model statistic values
  
  #Create ERS_df for coefficient dataframe 
  ERS_df<- ERSmodelswithin[[1]] %>% 
    broom::tidy() %>%
    dplyr::mutate(model = names(ERSmodelswithin)[[1]]) %>% 
    dplyr::mutate(ERS=e)  #Add column for ERS code
  
  #Fill dataframe with model results
  for (i in 2:length(ERSmodelswithin)){
    ERS_model<-ERSmodelswithin[[i]]
    ERS_df<-rbind(ERS_df, ERS_model %>% 
                    broom::tidy() %>% 
                    mutate(model=names(ERSmodelswithin)[[i]]) %>% 
                    mutate(ERS=e)) #Add column for ERS code
    
  } #end for loop of model dataframe
  
  
  #Create ERS_df for coefficient dataframe with cluster-robust standard errors, using Satterthwaite (default)
  ERS_df_coef<- ERSmodelswithin[[1]] %>% 
    clubSandwich::coef_test(vcov = "CR2", cluster = "individual", test = "Satterthwaite") %>% 
    tibble::rownames_to_column(var = "term") %>% 
    rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
    dplyr::mutate(model=names(ERSmodelswithin)[[1]]) %>% 
    dplyr::mutate(ERS=e)  #Add column for ERS code
  
  #Fill dataframe with model results
  for (i in 2:length(ERSmodelswithin)){
    ERS_model<-ERSmodelswithin[[i]]
    ERS_df_coef<-rbind(ERS_df_coef, ERS_model %>%
                         clubSandwich::coef_test(vcov = "CR2", cluster = "individual", test = "Satterthwaite") %>% 
                         tibble::rownames_to_column(var = "term") %>% 
                         rename(estimate=beta, std.error=SE, statistic=tstat, p.value=p_Satt) %>% 
                         dplyr::mutate(model=names(ERSmodelswithin)[[i]]) %>% 
                         dplyr::mutate(ERS=e)  #Add column for ERS code
    )#end rbind
  } #end for loop of model dataframe
  
  
  #Run variance inflation factor (vif) to look at multicollinearity between variables and look at correlation matrices
  #Can only run on model=pooling for plm
  #However, since vif is about examining indepdenent variables, theoretically there is less need to control for effects
  #VIF of 1=no correlation among predictors, >4 investigate, >10 serious correction
  
  #VIF on pooled data
  ERS_vif<-lapply(ERSmodelspooling, function(p){rownames_to_column(as.data.frame(car::vif(p)), "term")}) %>%
    purrr::reduce(full_join, by = "term") %>%
    setNames(c("term", names(ERSmodelspooling))) %>%
    reshape2::melt(id.vars="term") %>% 
    mutate(ERS=e) #Add column for ERS code
  colnames(ERS_vif)<-c("term", "model", "vif", "ERS")
  
  #Correlation matrix on within data
  ERS_corr<-lapply(ERSmodelswithin, function(v){cov2cor(v$vcov)}) #will save in correct folder but not combine across ERS
  
  #Write csv for each of the created dataframes and rda for the correlation matrix list (save under Data/Analysis/df/ERS)
  write_csv(ERS_df, paste0("Data/Analysis/df/ERS/", e ,"/ERS_df.csv"))
  write_csv(ERS_df_coef, paste0("Data/Analysis/df/ERS/", e ,"/ERS_df_coeftest.csv"))
  write_csv(ERS_vif, paste0("Data/Analysis/df/ERS/", e ,"/ERS_vif.csv"))
  save(ERS_corr, file=paste0("Data/Analysis/df/ERS/", e ,"/ERS_corr.Rda"))
}

#Read in the dataframe for each ERS and rbind to create combined dataframe
#ERS_df
df_files <- list.files(pattern='ERS_df.csv', recursive=TRUE, full.names = TRUE)
ERS_all <- do.call(rbind , lapply(df_files, readr::read_csv)) %>%
  dplyr::arrange(term) #order by variable so ERS together

#ERS_df_coef
coef_files <- list.files(pattern='ERS_df_coeftest.csv', recursive=TRUE, full.names = TRUE)
ERS_coef_all <- do.call(rbind , lapply(coef_files, readr::read_csv)) %>%
  dplyr::arrange(term) #order by variable so ERS together

#ERS_vif
vif_files <- list.files(pattern='ERS_vif.csv', recursive=TRUE, full.names = TRUE)
ERS_vif_all <- do.call(rbind , lapply(vif_files, readr::read_csv)) %>%
  dplyr::arrange(term) #order by term so ERS together

#Write combined df to csv files under Data/Analysis/df/ERS as all files
write_csv(ERS_all, "Data/Analysis/df/ERS/ERS_df_all.csv")
write_csv(ERS_coef_all, "Data/Analysis/df/ERS/ERS_df_coeftest_all.csv") #No metadata yet
write_csv(ERS_vif_all, "Data/Analysis/df/ERS/ERS_vif_all.csv")