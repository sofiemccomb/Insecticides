#This R Script gathers and formats the NLCD analysis data for regression analyses.
  #Read through R/DataProcessing/NF.R script for more information on where the data was sourced and how it was processed.

#Load Packages
  #Ensure necessary packages are installed and loaded
  library(tidyverse) #Datatable manipulation

#Read in the NLCD files and rbind
  file_names <- dir("Data/DataProcessing/NF", pattern = ".csv", full.names = TRUE) #files located per year
  nlcd_df <- do.call(rbind,lapply(file_names,readr::read_csv))

#Perform additional calculations
  #Add in columns for the percent shared: amount of total edge shared between lc types divided by total edge of that lc type
  nlcd<-as.data.frame(nlcd_df) %>%
    dplyr::mutate(te_shared_f_fc=tel_forestcrop_fc/tec_forest_fc, #Forest for forest-crops
           te_shared_c_fc=tel_forestcrop_fc/tec_crops_fc, #Crops for forest-crops
           te_shared_sg_sgc=tel_shrubgrasscrop_sgc/tec_shrubgrass_sgc, #Shrubgrass for shrubgrass-crops
           te_shared_c_sgc=tel_shrubgrasscrop_sgc/tec_crops_sgc, #Crops for shrubgrass-crops
           te_shared_df_dfLID=tel_deciforestLID_dfLID/tec_deciforest_dfLID, #DeciForest for deciforest-LID
           te_shared_LID_DFLID=tel_deciforestLID_dfLID/tec_LID_dfLID, #LID for deciforest-LID
           te_shared_mf_mfLID=tel_mixforestLID_mfLID/tec_mixforest_mfLID, #MixForest for Mixforest-LID
           te_shared_LID_mfLID=tel_mixforestLID_mfLID/tec_LID_mfLID, #LID for Mixforest-LID
           te_shared_n_nc=tel_naturalcrops_nc/tec_natural_nc, #Natural for Natural-crops
           te_shared_c_nc=tel_naturalcrops_nc/tec_crops_nc, #Crops for Natural-crops
           pland_natural=pland_df +pland_ef+pland_mf+pland_grass+pland_shrub+pland_wet) %>%  #percent natural land
    dplyr::rename(Year=year)

#Replace NA with zero (for calculations)
  nlcd[is.na(nlcd)] <- 0

#Change years to match those in the other datasets so can combine by FIPS and years
  #2001 to 2002, 2006 to 2007, 2011 to 2012, and 2016 to 2017
  nlcd$Year[nlcd$Year==2001] <- 2002
  nlcd$Year[nlcd$Year==2006] <- 2007
  nlcd$Year[nlcd$Year==2011] <- 2012
  nlcd$Year[nlcd$Year==2016] <- 2017

#Write final nlcd data to be used in combination with other dataframes
  write_csv(nlcd, "Data/DataProcessing/df/nlcd.csv")
  