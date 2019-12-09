#This script performs the xtsum analysis (similar to XTSUM capabilities in STATA) on the fulldata dataframe. 
  #The XTSUM function is created to perform the analysis, and then applied to each of the variables to be potentially used in the regressions.
  #The results are then combined across variables (run for both fips and year) and saved to a loadable rda dataframe.

library(rlang)
library(dplyr)
library(gdata)
XTSUM <- function(data, varname, unit) {
  varname <- rlang::enquo(varname)
  loc.unit <- rlang::enquo(unit)
  ores <- data %>% 
    dplyr::summarise(ovr.mean=mean(!! varname, na.rm=TRUE), 
              ovr.sd=sd(!! varname, na.rm=TRUE), 
              ovr.min = min(!! varname, na.rm=TRUE), 
              ovr.max=max(!! varname, na.rm=TRUE), 
              ovr.N=sum(as.numeric((!is.na(!! varname)))))
  bmeans <- data %>% 
    dplyr::group_by(!! loc.unit) %>% 
    dplyr::summarise(meanx=mean(!! varname, na.rm=T), 
              t.count=sum(as.numeric(!is.na(!! varname))))
  bres <- bmeans %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(between.sd = sd(meanx, na.rm=TRUE), 
              between.min = min(meanx, na.rm=TRUE), 
              between.max=max(meanx, na.rm=TRUE), 
              Units=sum(as.numeric(!is.na(t.count))), 
              t.bar=mean(t.count, na.rm=TRUE))
  wdat <- data %>% 
    dplyr::group_by(!! loc.unit) %>% 
    dplyr::mutate(W.x = scale(!! varname, scale=FALSE))
  wres <- wdat %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(within.sd=sd(W.x, na.rm=TRUE), 
              within.min=min(W.x, na.rm=TRUE), 
              within.max=max(W.x, na.rm=TRUE))
  values<-cbind(ores, bres, wres)
  return(values)
}

#Load fulldata in case not loaded previously:
load("Data/DataProcessing/df/fulldata.rda")

#Perform xtsum function for each variable being considered in regression, for both individual (FIPS) and time (Year) effects
  #Have to perform each variable separately based on how xtsumR function created (global variable)
    #Pland_crops
      pland_crops_fips<-XTSUM(fulldata, varname=pland_crops, unit=FIPS)
      pland_crops_year<-XTSUM(fulldata, varname=pland_crops, unit=Year)
    #cropintensity
      cropintensity_fips<-XTSUM(fulldata, varname=cropintensity, unit=FIPS)
      cropintensity_year<-XTSUM(fulldata, varname=cropintensity, unit=Year)
    #msidi
      msidi_fips<-XTSUM(fulldata, varname=msidi, unit=FIPS)
      msidi_year<-XTSUM(fulldata, varname=msidi, unit=Year)
    #mna_crops
      mna_crops_fips<-XTSUM(fulldata, varname=mna_crops, unit=FIPS)
      mna_crops_year<-XTSUM(fulldata, varname=mna_crops, unit=Year)
    #largefarm_planted
      largefarm_planted_fips<-XTSUM(fulldata, varname=largefarm_planted, unit=FIPS)
      largefarm_planted_year<-XTSUM(fulldata, varname=largefarm_planted, unit=Year)
    #natural_crops_ed
      natural_crops_ed_fips<-XTSUM(fulldata, varname=natural_crops_ed, unit=FIPS)
      natural_crops_ed_year<-XTSUM(fulldata, varname=natural_crops_ed, unit=Year)
    #SDI
      SDI_fips<-XTSUM(fulldata, varname=SDI, unit=FIPS)
      SDI_year<-XTSUM(fulldata, varname=SDI, unit=Year)
    #soysmallgrain_planted
      soysmallgrain_planted_fips<-XTSUM(fulldata, varname=soysmallgrain_planted, unit=FIPS)
      soysmallgrain_planted_year<-XTSUM(fulldata, varname=soysmallgrain_planted, unit=Year)
    #corn_planted
      corn_planted_fips<-XTSUM(fulldata, varname=corn_planted, unit=FIPS)
      corn_planted_year<-XTSUM(fulldata, varname=corn_planted, unit=Year)
    #fruitveg_planted
      fruitveg_planted_fips<-XTSUM(fulldata, varname=fruitveg_planted, unit=FIPS)
      fruitveg_planted_year<-XTSUM(fulldata, varname=fruitveg_planted, unit=Year)
      
#Cbind all the results together, using the variable name as identifier
    xtsum_df<-gdata::combine(pland_crops_fips, pland_crops_year,
                             cropintensity_fips, cropintensity_year,
                             msidi_fips, msidi_year,
                             mna_crops_fips, mna_crops_year,
                             largefarm_planted_fips, largefarm_planted_year,
                             natural_crops_ed_fips, natural_crops_ed_year,
                             SDI_fips, SDI_year,
                             soysmallgrain_planted_fips, soysmallgrain_planted_year,
                             corn_planted_fips, corn_planted_year,
                             fruitveg_planted_fips,fruitveg_planted_year) %>% 
              select(source, everything())
    
#Save dataframe as rda file
  save(xtsum_df, file = paste0("Data/Analysis/xtsum/xtsum_df.Rda"))
      