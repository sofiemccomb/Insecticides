nf=function(futureNLCD, year, state){#All in quotes
  

  #Load in Packages
  ################################
  library(raster) #rasters
  library(sf)#shapefiles
  library(landscapemetrics)#fragstats
  library(tidyverse) #datatable manipulation
  library(rgdal)
  options(scipen=999) #no scientific notation
  rasterOptions(tmpdir = "/home/sofie/Data/RasterTemp", tmptime=24)
  ################################
  
  #Set variables
  ##################
  nlcdras<-futureNLCD #options currently: "CONUS_Historical_y2005", "CONUS_B1_2050"
  year<-year
  i<-state

  #Read in Data
  ################################
  nlcd<-raster(paste0("Data/Forecast_NLCD/", nlcdras, ".tif"))
  #Read in counties layer (3232 rows)
  counties<-read_sf("Data/TIGER2018_Counties/tl_2018_us_county.shp") %>%
    dplyr::select(STATEFP,GEOID) %>% 
    st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
      #NLCD Forecast has slightly different st_transform than NLCD (but same projection of Albers Conical Equal Area)
  contig48_counties<-subset(counties,counties$STATEFP!="02"& counties$STATEFP!="15"&
                              counties$STATEFP!="60"&counties$STATEFP!="66"&
                              counties$STATEFP!="69"&counties$STATEFP!="72"&
                              counties$STATEFP!="74"&counties$STATEFP!="78")
  
  #Analysis
  #################################
  states<-unique(contig48_counties$STATEFP)#Unique states to cluster by
  states<-sort(states)
  
  #################################
  #Matrix per State
  ##################################
  state<-subset(contig48_counties, contig48_counties$STATEFP==i)#Subset counties for the state
  mat_total<-nrow(state) #size of matrix for the state
  
  #Reclassifies
  ###################################################################
  #All Class Reclassify
  ########################
m<-c(-Inf, 0.5, NA, #Reclass Background to NA
       2.5, 5.5, 3, #Reclass All Mechanically Disturbed Forests to one Number
       5.5, 7.5, 6, #Reclass Barren and Mining together
       14.5, 16.5, 15, #Reclass Wetlands to one number
       16.5, 17.5, 1 #Reclass snow and ice to water number
       )
  
  #Original
  #0=Background, 1=Water, 2=Developed, 3,4,5=Mechanically Disturbed Forests, 6=Mining, 7=Barren, 
    #8=Deciduous Forest, 9=Evergreen Forest, 10=Mixed Forest, 11=Grassland, 12=Shrubland, 13=Cropland,
      #14=Pasture, 15=Herbaceous Wetland, #16 Woody Wetland, #17 Ice/Snow
  
  #Reclass
  #Background is NA (0); 1=Water/Ice/Snow (1 and 17); 3=Mechanically Disturbed Forests (3,4,5),
    #6=Barren/Mining (6,7), 15=Wetland (15 and 16) ;AND THE REST STAY THE SAME (Final Numbers: NA, 1,2,3,6,8-15)
  
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  allmat<-as.data.frame(matrix(data=0,nrow=mat_total, ncol=50))
  colnames(allmat)<-c("FIPS",
                      "ed_1", "ed_2", "ed_3", "ed_6", "ed_8", "ed_9", "ed_10", "ed_11", "ed_12", "ed_13", "ed_14", "ed_15",
                      "el_1", "el_2", "el_3", "el_6", "el_8", "el_9", "el_10", "el_11", "el_12", "el_13", "el_14", "el_15",
                      "pland_1", "pland_2", "pland_3", "pland_6","pland_8", "pland_9","pland_10","pland_11","pland_12", 
                      "pland_13", "pland_14", "pland_15",
                      "mna_1", "mna_2", "mna_3", "mna_6", "mna_8", "mna_9", "mna_10", "mna_11", "mna_12",
                      "mna_13", "mna_14", "mna_15",
                      "msidi")
  
  #Natural vs Crops Reclassify
  ########################
  #Natural vs Crops Reclass: Natural 1, Crops 2, Other NA
    #Natural defined as natural forests, mechanical forests, shrublands, grasslands, wetlands
    #NA= water, developed, mining, barren, pasture, ice/snow
  m_natural_crops<-c(-Inf, 2.5, NA, #Background, Water, Developed as NA
                     2.5, 5.5, 1, #Mechanically Disturbed Forests as Natural 1
                     5.5, 7.5, NA, #Mining and Barren as NA
                     7.5, 12.5, 1, #Forests, Grassland, Shrubland as Natural 1
                     12.5, 13.5, 2, #Cropland as Crops 2
                     13.5, 14.5, NA, #Pasture as NA
                     14.5, 16.5, 1, #Wetlands as Nature 1
                     16.5, Inf, NA #Ice/Snow as NA
                     )
  rclmat_natural_crops<-matrix(m_natural_crops, ncol=3, byrow=TRUE)
  natural_crops_mat<-as.data.frame(matrix(data=0,nrow=mat_total, ncol=4))
  colnames(natural_crops_mat)<-c("FIPS","tec_natural_nc", "tec_crops_nc", "tel_naturalcrops_nc")
  
  #Forest vs Crops Reclassify
  ########################
  #AgvsForest Reclass: Forest 1, Crops 2, Other NA
    #Natural and mechanical Forest
  m_forest_crop<-c(-Inf, 2.5, NA, #Background, Water, Developed as NA
                    2.5, 5.5, 1, #Mechanically Disturbed Forests as Forests 1
                    5.5, 7.5, NA, #Mining and Barren as NA
                    7.5, 10.5, 1, #Forests as Forests 1
                    10.5, 12.5, NA, #Grassland and Shrubland as NA
                    12.5, 13.5, 2, #Cropland as Crops 2
                    13.5, Inf, NA #Pasture, Wetlands, Ice/Snow as NA
                  )
  rclmat_forest_crop<-matrix(m_forest_crop, ncol=3, byrow=TRUE)
  forest_crop_mat<-as.data.frame(matrix(data=0,nrow=mat_total, ncol=4))
  colnames(forest_crop_mat)<-c("FIPS","tec_forest_fc", "tec_crops_fc", "tel_forestcrop_fc")
  
  #Grasslands/shrublands vs Crops Reclassify
  ########################
  #AgvsGrassland Reclass: Grassland 1, Crops 2, other NA
  m_shrubgrass_crop<-c(-Inf, 10.5, NA, #Background, Water, Developed, All Forest, Mining/Barren  as NA
                   10.5, 12.5, 1, #Grassland and Shrubland as 1
                   12.5, 13.5, 2, #Cropland as Crops 2
                   13.5, Inf, NA #Pasture, Wetlands, Ice/Snow as NA
                  )
  rclmat_shrubgrass_crop<-matrix(m_shrubgrass_crop, ncol=3, byrow=TRUE)
  shrubgrass_crop_mat<-as.data.frame(matrix(data=0,nrow=mat_total, ncol=4))
  colnames(shrubgrass_crop_mat)<-c("FIPS","tec_shrubgrass_sgc", "tec_crops_sgc", "tel_shrubgrasscrop_sgc")
  
  ####################################################################################
  
  #################################
  #For Loop
  ##################################
  counties<-unique(state$GEOID)
  total<-0
  for (c in counties){
    total<-total+1
    county_sf<-subset(state, state$GEOID==c)
    county <- sf:::as_Spatial(county_sf)#sf back to sp (added line bc was causing issues)
    nlcd_crop<-raster::crop(nlcd, extent(county))#crop nlcd raster to county
    nlcdcounty<-raster::mask(nlcd_crop, county)#mask nlcd raster to county
    fips<-c
    print(fips)
    print(total/length(counties)*100)
    
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
    for (j in 1:15){
      if(nrow(subset(ed, ed$class==j))>0){allmat[total, paste0("ed_",j)]<-subset(ed$value, ed$class==j)}
      if(nrow(subset(el, el$class==j))>0){allmat[total, paste0("el_",j)]<-subset(el$value, el$class==j)}
      if(nrow(subset(pland, pland$class==j))>0){allmat[total, paste0("pland_",j)]<-subset(pland$value, pland$class==j)}
      if(nrow(subset(mna, mna$class==j))>0){allmat[total, paste0("mna_",j)]<-subset(mna$value, mna$class==j)}
    }#End matrix for loop
    allmat[total, "msidi"]<-msidi$value #Add msidi to matrix
    
    #Natural vs Crops Reclassify
    ########################
    natural_crops_mat[total, "FIPS"]<-fips
    rc_natural_crops <- reclassify(nlcdcounty, rclmat_natural_crops)
    if(length(unique(rc_natural_crops))>0){
      #Fragstat class and landscape metrics
      tec<-as.data.frame(lsm_c_te(rc_natural_crops))
      tel<-as.data.frame(lsm_l_te(rc_natural_crops))
      if(nrow(subset(tec, tec$class==1))>0){natural_crops_mat[total, "tec_natural_nc"]<-subset(tec$value, tec$class==1)}
      if(nrow(subset(tec, tec$class==2))>0){natural_crops_mat[total, "tec_crops_nc"]<-subset(tec$value, tec$class==2)}
      natural_crops_mat[total, "tel_naturalcrops_nc"]<-tel$value[1]
    }
    
    
    #Forest vs Crops Reclassify
    ########################
    forest_crop_mat[total, "FIPS"]<-fips
    rc_forest_crop <- reclassify(nlcdcounty, rclmat_forest_crop)
    if(length(unique(rc_forest_crop))>0){
      #Fragstat class and landscape metrics
      tec<-as.data.frame(lsm_c_te(rc_forest_crop))
      tel<-as.data.frame(lsm_l_te(rc_forest_crop))
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
      tec<-as.data.frame(lsm_c_te(rc_shrubgrass_crop))
      tel<-as.data.frame(lsm_l_te(rc_shrubgrass_crop))
      if(nrow(subset(tec, tec$class==1))>0){shrubgrass_crop_mat[total, "tec_shrubgrass_sgc"]<-subset(tec$value, tec$class==1)}
      if(nrow(subset(tec, tec$class==2))>0){shrubgrass_crop_mat[total, "tec_crops_sgc"]<-subset(tec$value, tec$class==2)}
      shrubgrass_crop_mat[total, "tel_shrubgrasscrop_sgc"]<-tel$value[1]
    }
    
    
  }#End counties for loop
  
  colnames(allmat)<-c("FIPS","ed_wat", "ed_dev", "ed_mdf", "ed_bare", "ed_df", "ed_ef",
                      "ed_mf", "ed_grass", "ed_shrub", "ed_crops", "ed_pas", "ed_wet", 
                      "el_wat", "el_dev", "el_mdf", "el_bare", "el_df", "el_ef",
                      "el_mf", "el_grass", "el_shrub", "el_crops", "el_pas", "el_wet", 
                      "pland_wat", "pland_dev", "pland_mdf", "pland_bare", "pland_df", "pland_ef",
                      "pland_mf", "pland_grass", "pland_shrub", "pland_crops", "pland_pas", "pland_wet", 
                      "mna_wat", "mna_dev", "mna_mdf", "mna_bare", "mna_df", "mna_ef",
                      "mna_mf", "mna_grass", "mna_shrub", "mna_crops", "mna_pas", "mna_wet", 
                      "msidi")
  
  
  #Clean up into final matrix
  #Remove extra FIPS columns for matrices except allmat
  natural_crops_mat<-natural_crops_mat[,-1]
  forest_crop_mat<-forest_crop_mat[,-1]
  shrubgrass_crop_mat<-shrubgrass_crop_mat[,-1]

  #Cbind all matrices
  finalmat<-cbind(allmat,natural_crops_mat,forest_crop_mat,shrubgrass_crop_mat)
  #Add replace NA with zero (for calculations)
  finalmat[is.na(finalmat)] <- 0 
  #Add columns at the beginning with year 
  finalmat$year <- rep(year,nrow(finalmat))  
  statename<-state$STATEFP[1]
  write.csv(finalmat,
            file=paste0("Data/NLCD_Frag_Cluster/nlcd_", year, "/", statename, ".csv"),
            row.names=FALSE)
} #end function