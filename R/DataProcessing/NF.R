#Function created to perform landscape metric analyses of the National Land Cover Databases at the county level
  #The NLCD rasters and county shapefiles are too large to store onto Github and therefore this script was run on a hard-drive
  #The file_paths could be changed to another computer location to perform this analysis, which creates the values 
  #necessary to create the NLCD data table to be processed in the DataProcessing.Rmd file for use in the regressions.
  #This script takes a long time, and often needs a 32 GB ram computer in order to complete the memory processing.

  #NLCD raster data sourced from:
  #TIGER2018 County shapefile sourced from:

  #Year options: 2001, 2006, 2011, 2016
  #Performed by state ansi code for easier to handle datasets and processing

  #Once processing was completed, the state csvs were combined for each year through a rbind, using code similar to below:
    # file_names <- dir("Z:/Sofie/Data/NLCD/Data/NLCD_Frag_Cluster/nlcd_2001", pattern = ".csv", full.names = TRUE) #files located per year
    # nlcd <- do.call(rbind,lapply(file_names,read.csv))
    # write.csv(nlcd, "Z:/Sofie/Data/NLCD/Data/NLCD_Frag_Cluster/nlcd_2001/all.csv")
  #The final year products have been renamed by there year and are provided in the Data/DataProcessing/nf folder (nf stands for nlcd_fragstats)

nf=function(year, state){
  
  #Load in Packages
  ################################
  # install.packages("raster")
  # install.packages("sf")
  # install.packages("rgdal")
  # install.packages("landscapemetrics")
  # install.packages("tidyverse")
  library(raster) #rasters
  library(sf)#shapefiles
  library(landscapemetrics)#fragstats
  library(tidyverse) #datatable manipulation
  library(rgdal)
  options(scipen=999) #no scientific notation
  rasterOptions(tmpdir = "Z:/Sofie/Data/RasterTemp", tmptime=24)
  ################################
  
  #Set variables
  ##################
  year<-year
  i<-state
  
  #Read in Data
  ################################
  #Read in NLCD per year selected
  nlcd<-raster(paste0("Z:/Sofie/Data/NLCD/NLCD_", year, "_Land_Cover_L48_20190424.img"))
  #Read in counties layer (3232 rows) and subset needed counties
  counties<-read_sf("Z:/Sofie/Data/TIGER2018_Counties/tl_2018_us_county.shp") %>%
    dplyr::select(STATEFP,GEOID)
  counties<-st_transform(counties, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84
                       +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs ") #Project counties to nlcd projection
  contig48_counties<-subset(counties,counties$STATEFP!="02"& counties$STATEFP!="15"&
                              counties$STATEFP!="60"&counties$STATEFP!="66"&
                              counties$STATEFP!="69"&counties$STATEFP!="72"&
                              counties$STATEFP!="74"&counties$STATEFP!="78")
  
  #Analysis
  #################################
  states<-unique(contig48_counties$STATEFP)#Unique states to loop by
  states<-sort(states)
  
  #################################
  #Matrix per State
  ##################################
  state<-subset(contig48_counties, contig48_counties$STATEFP==i)#Subset counties for the state
  mat_total<-nrow(state) #size of matrix for the state
  
  #Reclassifies
  ###################################################################
  #Create the reclassification matrices to be applied to the NLCDs for landscape metric calculations
    #Includes all classes grouped in a logical fashion, as well as 5 binary reclassfications
  
  #All Class Reclassify
  ########################
  m <- c(-Inf,12.5,1, 20.5,22.5,2, 22.5,24.5,3, 30.5,31.5,4, 
         40.5,41.5,5, 41.5,42.5,6, 42.5,43.5,7, 50.5,52.5,8, 
         70.5,74.5,9, 80.5,81.5,10, 81.5,82.5,11, 89.5,Inf,12)#12 classifications based on reclass crosswalk
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  allmat<-as.data.frame(matrix(data=0,nrow=mat_total, ncol=50))
  colnames(allmat)<-c("FIPS","ed_1", "ed_2", "ed_3", "ed_4", "ed_5", "ed_6", "ed_7", "ed_8", "ed_9", "ed_10", "ed_11", "ed_12",
                      "el_1", "el_2", "el_3", "el_4", "el_5", "el_6", "el_7", "el_8", "el_9", "el_10", "el_11", "el_12",
                      "pland_1", "pland_2", "pland_3", "pland_4", "pland_5", "pland_6", "pland_7",
                      "pland_8", "pland_9","pland_10","pland_11","pland_12",
                      "mna_1", "mna_2", "mna_3", "mna_4", "mna_5", "mna_6", "mna_7", "mna_8", "mna_9", "mna_10", "mna_11", "mna_12",
                      "msidi")
  
  #Forest vs Crops Reclassify
  ########################
  #AgvsForest Reclass: Forest 1, Crops 2, Other NA
  m_forest_crop<-c(-Inf, 32, NA, 40, 44, 1, 50, 81.5, NA, 81.5, 83, 2, 84, Inf, NA)
  rclmat_forest_crop<-matrix(m_forest_crop, ncol=3, byrow=TRUE)
  forest_crop_mat<-as.data.frame(matrix(data=0,nrow=mat_total, ncol=4))
  colnames(forest_crop_mat)<-c("FIPS","tec_forest_fc", "tec_crops_fc", "tel_forestcrop_fc")
  
  #Grasslands/shrublands vs Crops Reclassify
  ########################
  #AgvsGrassland Reclass: Grassland 1, Crops 2, other NA
  m_shrubgrass_crop<-c(-Inf, 44, NA, 50, 75, 1, 80, 81.5, NA, 81.5, 83, 2, 84, Inf, NA)
  rclmat_shrubgrass_crop<-matrix(m_shrubgrass_crop, ncol=3, byrow=TRUE)
  shrubgrass_crop_mat<-as.data.frame(matrix(data=0,nrow=mat_total, ncol=4))
  colnames(shrubgrass_crop_mat)<-c("FIPS","tec_shrubgrass_sgc", "tec_crops_sgc", "tel_shrubgrasscrop_sgc")
  
  #Deciduous Forest vs LID Reclassify
  ########################
  #LIDvsDeciduous: Deciduous 1, LID 2, other NA
  m_deciforest_LID<-c(-Inf, 13, NA, 20, 22.5, 2, 22.5, 32, NA, 40.5, 41.5, 1, 41.5, Inf, NA)
  rclmat_deciforest_LID<-matrix(m_deciforest_LID, ncol=3, byrow=TRUE)
  deciforest_LID_mat<-as.data.frame(matrix(data=0,nrow=mat_total, ncol=4))
  colnames(deciforest_LID_mat)<-c("FIPS","tec_deciforest_dfLID", "tec_LID_dfLID", "tel_deciforestLID_dfLID")
  
  #Mixed Forest vs LID Reclassify
  ########################
  #LIDvsMixed: Mixed 1, LID 2, other NA
  m_mixforest_LID<-c(-Inf, 13, NA, 20, 22.5, 2, 22.5, 42.5, NA, 42.5, 43.5, 1, 44, Inf, NA)
  rclmat_mixforest_LID<-matrix(m_mixforest_LID, ncol=3, byrow=TRUE)
  mixforest_LID_mat<-as.data.frame(matrix(data=0,nrow=mat_total, ncol=4))
  colnames(mixforest_LID_mat)<-c("FIPS","tec_mixforest_mfLID", "tec_LID_mfLID", "tel_mixforestLID_mfLID")
  
  #Natural vs Crops Reclassify
  ########################
  #Natural vs Crops Reclass: Natural 1, Crops 2, Other NA
  #Natural defined as natural forests, shrublands, grasslands, wetlands
  m_natural_crops<-c(-Inf, 31.5, NA, 32, 74.5, 1, 75, 81.5, NA, 81.5, 82.5, 2,
                     83, Inf, 1)
  rclmat_natural_crops<-matrix(m_natural_crops, ncol=3, byrow=TRUE)
  natural_crops_mat<-as.data.frame(matrix(data=0,nrow=mat_total, ncol=4))
  colnames(natural_crops_mat)<-c("FIPS","tec_natural_nc", "tec_crops_nc", "tel_naturalcrops_nc")
  
  ####################################################################################
  
  #################################
  #For Loop
  ##################################
  #Looping through the counties in the state, and performing the reclassifications and analyses per county
    #Add every new county with computations to a new row
  counties<-unique(state$GEOID)
  total<-0
  for (c in counties){
    total<-total+1 #move to new row
    county<-subset(state, state$GEOID==c) #subset by county
    nlcd_crop<-raster::crop(nlcd, extent(county)) #crop nlcd raster to county
    nlcdcounty<-raster::mask(nlcd_crop, county) #mask nlcd raster to county
    fips<-c #get fips value
    #print(fips) #Check status
    #print(total/length(counties)*100) #Check status
    
    #All Class Reclassify
    #########################
    allmat[total, "FIPS"]<-fips #Set FIPS
    rc <- reclassify(nlcdcounty, rclmat)#Reclassify nlcd
    #Fragstat class and landscape metrics
    ed<-lsm_c_ed(rc)#edge density
    el<-lsm_c_te(rc)#edge length
    pland<-lsm_c_pland(rc)#percentage of landscape
    mna<-lsm_c_area_mn(rc)#mean patch area/size
    msidi<-lsm_l_sidi(rc)#simpsons diversity
    #Add values to matrix
    for (j in 1:12){
      if(nrow(subset(ed, ed$class==j))>0){allmat[total, paste0("ed_",j)]<-subset(ed$value, ed$class==j)}
      if(nrow(subset(el, el$class==j))>0){allmat[total, paste0("el_",j)]<-subset(el$value, el$class==j)}
      if(nrow(subset(pland, pland$class==j))>0){allmat[total, paste0("pland_",j)]<-subset(pland$value, pland$class==j)}
      if(nrow(subset(mna, mna$class==j))>0){allmat[total, paste0("mna_",j)]<-subset(mna$value, mna$class==j)}
    }#End matrix for loop
    allmat[total, "msidi"]<-msidi$value #Add msidi to matrix
    
    #Forest vs Crops Reclassify
    ########################
    forest_crop_mat[total, "FIPS"]<-fips
    rc_forest_crop <- reclassify(nlcdcounty, rclmat_forest_crop)
    if(length(unique(rc_forest_crop))>0){
      #Fragstat class and landscape metrics
      tec<-as.data.frame(lsm_c_te(rc_forest_crop)) #total edge class metrics
      tel<-as.data.frame(lsm_l_te(rc_forest_crop)) #total edge landscape metrics
      #Add values to matrix
      if(nrow(subset(tec, tec$class==1))>0){forest_crop_mat[total, "tec_forest_fc"]<-subset(tec$value, tec$class==1)}
      if(nrow(subset(tec, tec$class==2))>0){forest_crop_mat[total, "tec_crops_fc"]<-subset(tec$value, tec$class==2)}
      forest_crop_mat[total, "tel_forestcrop_fc"]<-tel$value[1]
    }
    
    
    #Grasslands/shrublands vs Crops Reclassify
    ########################
    shrubgrass_crop_mat[total, "FIPS"]<-fips
    rc_shrubgrass_crop <- reclassify(nlcdcounty, rclmat_shrubgrass_crop)
    if(length(unique(rc_shrubgrass_crop))>0){
      #Fragstat class and landscape metrics
      tec<-as.data.frame(lsm_c_te(rc_shrubgrass_crop)) #total edge class metrics
      tel<-as.data.frame(lsm_l_te(rc_shrubgrass_crop)) #total edge landscape metrics
      #Add values to matrix
      if(nrow(subset(tec, tec$class==1))>0){shrubgrass_crop_mat[total, "tec_shrubgrass_sgc"]<-subset(tec$value, tec$class==1)}
      if(nrow(subset(tec, tec$class==2))>0){shrubgrass_crop_mat[total, "tec_crops_sgc"]<-subset(tec$value, tec$class==2)}
      shrubgrass_crop_mat[total, "tel_shrubgrasscrop_sgc"]<-tel$value[1]
    }
    
    #Deciduous Forest vs LID Reclassify
    ########################
    deciforest_LID_mat[total, "FIPS"]<-fips
    rc_deciforest_LID <- reclassify(nlcdcounty, rclmat_deciforest_LID)
    if(length(unique(rc_deciforest_LID))>0){
      #Fragstat class and landscape metrics
      tec<-as.data.frame(lsm_c_te(rc_deciforest_LID)) #total edge class metrics
      tel<-as.data.frame(lsm_l_te(rc_deciforest_LID)) #total edge landscape metrics
      #Add values to matrix
      if(nrow(subset(tec, tec$class==1))>0){deciforest_LID_mat[total, "tec_deciforest_dfLID"]<-subset(tec$value, tec$class==1)}
      if(nrow(subset(tec, tec$class==2))>0){deciforest_LID_mat[total, "tec_LID_dfLID"]<-subset(tec$value, tec$class==2)}
      deciforest_LID_mat[total, "tel_deciforestLID_dfLID"]<-tel$value[1]
    }
    
    #Mixed Forest vs LID Reclassify
    ########################
    mixforest_LID_mat[total, "FIPS"]<-fips
    rc_mixforest_LID <- reclassify(nlcdcounty, rclmat_mixforest_LID)
    if(length(unique(rc_mixforest_LID))>0){
      #Fragstat class and landscape metrics
      tec<-as.data.frame(lsm_c_te(rc_mixforest_LID)) #total edge class metrics
      tel<-as.data.frame(lsm_l_te(rc_mixforest_LID)) #total edge landscape metrics
      #Add values to matrix
      if(nrow(subset(tec, tec$class==1))>0){mixforest_LID_mat[total, "tec_mixforest_mfLID"]<-subset(tec$value, tec$class==1)}
      if(nrow(subset(tec, tec$class==2))>0){mixforest_LID_mat[total, "tec_LID_mfLID"]<-subset(tec$value, tec$class==2)}
      mixforest_LID_mat[total, "tel_mixforestLID_mfLID"]<-tel$value[1]
    }
    
    #Natural vs Crops Reclassify
    ########################
    natural_crops_mat[total, "FIPS"]<-fips
    rc_natural_crops <- reclassify(nlcdcounty, rclmat_natural_crops)
    if(length(unique(rc_natural_crops))>0){
      #Fragstat class and landscape metrics
      tec<-as.data.frame(lsm_c_te(rc_natural_crops)) #total edge class metrics
      tel<-as.data.frame(lsm_l_te(rc_natural_crops)) #total edge landscape metrics
      #Add values to matrix
      if(nrow(subset(tec, tec$class==1))>0){natural_crops_mat[total, "tec_natural_nc"]<-subset(tec$value, tec$class==1)}
      if(nrow(subset(tec, tec$class==2))>0){natural_crops_mat[total, "tec_crops_nc"]<-subset(tec$value, tec$class==2)}
      natural_crops_mat[total, "tel_naturalcrops_nc"]<-tel$value[1]
    }
    
    
  }#End counties for loop
  
  colnames(allmat)<-c("FIPS","ed_wat", "ed_ldev", "ed_hdev", "ed_bare", "ed_df", "ed_ef",
                      "ed_mf", "ed_shrub", "ed_grass", "ed_pas", "ed_crops",  "ed_wet",
                      "el_wat", "el_ldev", "el_hdev", "el_bare", "el_df", "el_ef",
                      "el_mf", "el_shrub", "el_grass",  "el_pas", "el_crops",  "el_wet",
                      "pland_wat", "pland_ldev", "pland_hdev", "pland_bare", "pland_df",
                      "pland_ef", "pland_mf","pland_shrub", "pland_grass",
                      "pland_pas", "pland_crops", "pland_wet",
                      "mna_wat", "mna_ldev", "mna_hdev", "mna_bare", "mna_df", "mna_ef",
                      "mna_mf", "mna_shrub", "mna_grass", "mna_pas", "mna_crops", "mna_wet",
                      "msidi")
  
  #Clean up into final matrix
    #Remove extra FIPS columns for matrices except allmat
      forest_crop_mat<-forest_crop_mat[,-1]
      shrubgrass_crop_mat<-shrubgrass_crop_mat[,-1]
      deciforest_LID_mat<-deciforest_LID_mat[,-1]
      mixforest_LID_mat<-mixforest_LID_mat[,-1]
      natural_crops_mat<-natural_crops_mat[,-1]
    #Cbind all matrices
      finalmat<-cbind(allmat,forest_crop_mat,shrubgrass_crop_mat,
                    deciforest_LID_mat,mixforest_LID_mat,natural_crops_mat)
    #Add replace NA with zero (for calculations)
      finalmat[is.na(finalmat)] <- 0 
    #Add columns at the beginning with year 
      finalmat$year <- rep(year,nrow(finalmat))
    #Year column before everything
      finalmat<-finalmat %>% 
        dplyr::select(year, everything())
    #Get statename in order to save file
      statename<-state$STATEFP[1]
  #Write state csv
    write.csv(finalmat,
          file=paste0("Z:/Sofie/Data/NLCD_Frag_Cluster/nlcd_", year, "/", statename, ".csv"),
          row.names=FALSE)
  
} #end function