#Perform fragstats metrics for forecast NLCD tifs, including large farm patch analyses

#Load in Packages
################################
library(raster) #rasters
library(sf)#shapefiles
library(landscapemetrics)#fragstats
library(tidyverse) #datatable manipulation
library(rgdal)
library(doParallel)
options(scipen=999) #no scientific notation
rasterOptions(tmpdir = "/home/sofie/Data/RasterTemp", tmptime=24)


#Calc_NLCD function does all landscape metric calculations
calc_NLCD<-function(scenario, year){
  
  #Set variables
  ###########################################################
  scenario<-scenario #set scenario
  year<-year #set year
  
  #Read in Data
  ############################################################

  #NLCD (selected by year and scenario combination)
  nlcd<-raster(paste0("Data/Forecast_NLCD/CONUS_Landcover_",scenario, "/CONUS_", scenario, "_y", year, ".tif"))
  
  #Counties
  counties<-read_sf("Data/TIGER2018_Counties/tl_2018_us_county.shp") %>%
    dplyr::select(STATEFP,GEOID) %>% 
    st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0") #forecasted
  contig48_counties<-subset(counties,counties$STATEFP!="02"& counties$STATEFP!="15"&
                              counties$STATEFP!="60"&counties$STATEFP!="66"&
                              counties$STATEFP!="69"&counties$STATEFP!="72"&
                              counties$STATEFP!="74"&counties$STATEFP!="78")
  
  
  #Analyses
  ############################################################
  counties<-unique(contig48_counties$GEOID)#Counties to loop through
  
  registerDoParallel(cores=18)#initiate multiple cores for processing
  result<-foreach(c=1:length(counties), .combine=rbind)%dopar%{ #For each county
   
      #######################
      #Subset NLCD to county
      ######################
      fips<-counties[c] #get FIPS
      county_sf<-subset(contig48_counties, contig48_counties$GEOID==fips) #subset counties dataset to fips
      county <- sf:::as_Spatial(county_sf)#sf back to sp (added line bc was causing issues)
      nlcd_crop<-raster::crop(nlcd, extent(county))#crop nlcd raster to county
      nlcdcounty<-raster::mask(nlcd_crop, county)#mask nlcd raster to county
      
      
      ########################
      #All Class Reclassify
      ########################
      
      #Original Forecast Dataset
      #0=Background, 1=Water, 2=Developed, 3,4,5=Mechanically Disturbed Forests, 6=Mining, 7=Barren, 
      #8=Deciduous Forest, 9=Evergreen Forest, 10=Mixed Forest, 11=Grassland, 12=Shrubland, 13=Cropland,
      #14=Pasture, 15=Herbaceous Wetland, #16 Woody Wetland, #17 Ice/Snow
      
      #Reclass
      #Background is NA (0); 1=Water/Ice/Snow (1 and 17); 3=Mechanically Disturbed Forests (3,4,5),
      #6=Barren/Mining (6,7), 15=Wetland (15 and 16) ;AND THE REST STAY THE SAME (Final Numbers: NA, 1,2,3,6,8-15)
      
      #Reclass vector
      m_all<-c(-Inf, 0.5, NA, #Reclass Background to NA
           2.5, 5.5, 3, #Reclass All Mechanically Disturbed Forests to one Number
           5.5, 7.5, 6, #Reclass Barren and Mining together
           14.5, 16.5, 15, #Reclass Wetlands to one number
           16.5, 17.5, 1 #Reclass snow and ice to water number
      )
      rclmat_all <- matrix(m_all, ncol=3, byrow=TRUE) #reclass matrix
      rc_all <- reclassify(nlcdcounty, rclmat_all)#Reclassify nlcd
      
      #Fragstat class and landscape metrics
      ed<-lsm_c_ed(rc_all)#edge density
      el<-lsm_c_te(rc_all)#edge length
      pland<-lsm_c_pland(rc_all)#percentage of landscape
      mna<-lsm_c_area_mn(rc_all)#mean patch area/size
      msidi<-lsm_l_sidi(rc_all)#simpsons diversity
      
      #Matrix to add values to
      allmat<-as.data.frame(matrix(data=0,nrow=1, ncol=50))
      colnames(allmat)<-c("FIPS",
                          "ed_1", "ed_2", "ed_3", "ed_6", "ed_8", "ed_9", "ed_10", "ed_11", "ed_12", "ed_13", "ed_14", "ed_15",
                          "el_1", "el_2", "el_3", "el_6", "el_8", "el_9", "el_10", "el_11", "el_12", "el_13", "el_14", "el_15",
                          "pland_1", "pland_2", "pland_3", "pland_6","pland_8", "pland_9","pland_10","pland_11","pland_12", 
                          "pland_13", "pland_14", "pland_15",
                          "mna_1", "mna_2", "mna_3", "mna_6", "mna_8", "mna_9", "mna_10", "mna_11", "mna_12",
                          "mna_13", "mna_14", "mna_15",
                          "msidi")
      allmat[1, "FIPS"]<-fips #Set FIPS
      #Add values to matrix
      for (j in 1:15){
        if(nrow(subset(ed, ed$class==j))>0){allmat[1, paste0("ed_",j)]<-subset(ed$value, ed$class==j)}
        if(nrow(subset(el, el$class==j))>0){allmat[1, paste0("el_",j)]<-subset(el$value, el$class==j)}
        if(nrow(subset(pland, pland$class==j))>0){allmat[1, paste0("pland_",j)]<-subset(pland$value, pland$class==j)}
        if(nrow(subset(mna, mna$class==j))>0){allmat[1, paste0("mna_",j)]<-subset(mna$value, mna$class==j)}
      }#End matrix for loop
      allmat[1, "msidi"]<-msidi$value #Add msidi to matrix
      
      colnames(allmat)<-c("FIPS","ed_wat", "ed_dev", "ed_mdf", "ed_bare", "ed_df", "ed_ef",
                          "ed_mf", "ed_grass", "ed_shrub", "ed_crops", "ed_pas", "ed_wet", 
                          "el_wat", "el_dev", "el_mdf", "el_bare", "el_df", "el_ef",
                          "el_mf", "el_grass", "el_shrub", "el_crops", "el_pas", "el_wet", 
                          "pland_wat", "pland_dev", "pland_mdf", "pland_bare", "pland_df", "pland_ef",
                          "pland_mf", "pland_grass", "pland_shrub", "pland_crops", "pland_pas", "pland_wet", 
                          "mna_wat", "mna_dev", "mna_mdf", "mna_bare", "mna_df", "mna_ef",
                          "mna_mf", "mna_grass", "mna_shrub", "mna_crops", "mna_pas", "mna_wet", 
                          "msidi")
      
      
      
      ########################
      #Natural vs Crops Reclassify
      ########################
      #Natural vs Crops Reclass: Natural 1, Crops 2, Other NA
      #Natural defined as natural forests, mechanical forests, shrublands, grasslands, wetlands
      #NA= water, developed, mining, barren, pasture, ice/snow
      
      #Reclass vector
      m_natural_crops<-c(-Inf, 2.5, NA, #Background, Water, Developed as NA
                         2.5, 5.5, 1, #Mechanically Disturbed Forests as Natural 1
                         5.5, 7.5, NA, #Mining and Barren as NA
                         7.5, 12.5, 1, #Forests, Grassland, Shrubland as Natural 1
                         12.5, 13.5, 2, #Cropland as Crops 2
                         13.5, 14.5, NA, #Pasture as NA
                         14.5, 16.5, 1, #Wetlands as Nature 1
                         16.5, Inf, NA #Ice/Snow as NA
      )
      rclmat_natural_crops<-matrix(m_natural_crops, ncol=3, byrow=TRUE)#reclass matrix
      rc_natural_crops <- reclassify(nlcdcounty, rclmat_natural_crops)#reclass
      
      #Matrix to add values to
      natural_crops_mat<-as.data.frame(matrix(data=0,nrow=1, ncol=4))
      colnames(natural_crops_mat)<-c("FIPS","tec_natural_nc", "tec_crops_nc", "tel_naturalcrops_nc")
      natural_crops_mat[1, "FIPS"]<-fips #Add FIPS
      if(length(unique(rc_natural_crops))>0){
        #Fragstat class and landscape metrics
        tec<-as.data.frame(lsm_c_te(rc_natural_crops)) #Total edge length of each binary (class)
        tel<-as.data.frame(lsm_l_te(rc_natural_crops)) #Total shared edge length of binaries (landscape)
        if(nrow(subset(tec, tec$class==1))>0){natural_crops_mat[1, "tec_natural_nc"]<-subset(tec$value, tec$class==1)}
        if(nrow(subset(tec, tec$class==2))>0){natural_crops_mat[1, "tec_crops_nc"]<-subset(tec$value, tec$class==2)}
        natural_crops_mat[1, "tel_naturalcrops_nc"]<-tel$value[1]
      }
      
      
      
      ########################
      #Forest vs Crops Reclassify
      ########################
      #AgvsForest Reclass: Forest 1, Crops 2, Other NA
      #Natural and mechanical Forest
      
      #Reclass vector
      m_forest_crop<-c(-Inf, 2.5, NA, #Background, Water, Developed as NA
                       2.5, 5.5, 1, #Mechanically Disturbed Forests as Forests 1
                       5.5, 7.5, NA, #Mining and Barren as NA
                       7.5, 10.5, 1, #Forests as Forests 1
                       10.5, 12.5, NA, #Grassland and Shrubland as NA
                       12.5, 13.5, 2, #Cropland as Crops 2
                       13.5, Inf, NA #Pasture, Wetlands, Ice/Snow as NA
      )
      rclmat_forest_crop<-matrix(m_forest_crop, ncol=3, byrow=TRUE) #reclass matrix
      rc_forest_crop <- reclassify(nlcdcounty, rclmat_forest_crop) #reclass
      
      #Matrix to add values to
      forest_crop_mat<-as.data.frame(matrix(data=0,nrow=1, ncol=4))
      colnames(forest_crop_mat)<-c("FIPS","tec_forest_fc", "tec_crops_fc", "tel_forestcrop_fc")
      forest_crop_mat[1, "FIPS"]<-fips #Add FIPS
      #Add values to matrix
      if(length(unique(rc_forest_crop))>0){
        #Fragstat class and landscape metrics
        tec<-as.data.frame(lsm_c_te(rc_forest_crop))#Total edge length of each binary (class)
        tel<-as.data.frame(lsm_l_te(rc_forest_crop))#Total shared edge length of binaries (landscape)
        if(nrow(subset(tec, tec$class==1))>0){forest_crop_mat[1, "tec_forest_fc"]<-subset(tec$value, tec$class==1)}
        if(nrow(subset(tec, tec$class==2))>0){forest_crop_mat[1, "tec_crops_fc"]<-subset(tec$value, tec$class==2)}
        forest_crop_mat[1, "tel_forestcrop_fc"]<-tel$value[1]
      }
      
    
      
      ########################
      #Grasslands/shrublands vs Crops Reclassify
      ########################
      #AgvsGrassland Reclass: Grassland 1, Crops 2, other NA
      
      #Reclass vector
      m_shrubgrass_crop<-c(-Inf, 10.5, NA, #Background, Water, Developed, All Forest, Mining/Barren  as NA
                           10.5, 12.5, 1, #Grassland and Shrubland as 1
                           12.5, 13.5, 2, #Cropland as Crops 2
                           13.5, Inf, NA #Pasture, Wetlands, Ice/Snow as NA
      )
      rclmat_shrubgrass_crop<-matrix(m_shrubgrass_crop, ncol=3, byrow=TRUE) #reclass matrix
      rc_shrubgrass_crop <- reclassify(nlcdcounty, rclmat_shrubgrass_crop)#reclass
      
      #Matrix to add values to
      shrubgrass_crop_mat<-as.data.frame(matrix(data=0,nrow=1, ncol=4))
      colnames(shrubgrass_crop_mat)<-c("FIPS","tec_shrubgrass_sgc", "tec_crops_sgc", "tel_shrubgrasscrop_sgc")
      shrubgrass_crop_mat[1, "FIPS"]<-fips#Add FIPS
      #Add values
      if(length(unique(rc_shrubgrass_crop))>0){
        #Fragstat class and landscape metrics
        tec<-as.data.frame(lsm_c_te(rc_shrubgrass_crop))#Total edge length of each binary (class)
        tel<-as.data.frame(lsm_l_te(rc_shrubgrass_crop))#Total shared edge length of binaries (landscape)
        if(nrow(subset(tec, tec$class==1))>0){shrubgrass_crop_mat[1, "tec_shrubgrass_sgc"]<-subset(tec$value, tec$class==1)}
        if(nrow(subset(tec, tec$class==2))>0){shrubgrass_crop_mat[1, "tec_crops_sgc"]<-subset(tec$value, tec$class==2)}
        shrubgrass_crop_mat[1, "tel_shrubgrasscrop_sgc"]<-tel$value[1]
      }
      
    
      ###################
      #Large Farm Patches
      ##################
      #Reclassify county NLCD: Binary reclass- 1 for crops, 0 all else (13 is crops) for forecasted
      lfp <- c(-Inf,12.5,0, 12.5,13.5,1, 13.5,Inf,0) #reclass vector
      lfp_rclmat <- matrix(lfp, ncol=3, byrow=TRUE) #reclass matrix
      rc_lfp <- reclassify(nlcdcounty, lfp_rclmat)#Reclassify nlcd
      
      #Landscape metrics
        #Calculate area of each patch
        nlcd_p_area<-lsm_p_area(rc_lfp, directions=8) #Lets choose to stay with 8 directions over 4 directions so that patches must share an edge
        #Subset areas of crop patches
        nlcd_p_area_crops<-nlcd_p_area[nlcd_p_area$class==1,]
        #Select large patches (>500 acres, or >202.343 hectares)
        nlcd_large_crop_patch<-nlcd_p_area_crops[nlcd_p_area_crops$value>=202.343,]#Each patch in hectares- so we want only the patches >202.343 hectares, which is ~500 acres (definition of large farms)
        #Sum total area of large patches
        total_area_large_crop_patches<-as.numeric(sum(nlcd_large_crop_patch$value))#Value in hectares
        #Sum total area of county
        landscapearea_patches<-as.numeric(sum(nlcd_p_area$value)) #Using landscape area mean to calculate is same value so we good
        #Sum total cropland of county (large and small patches)
        croplandarea<-as.numeric(sum(nlcd_p_area_crops$value))
        #Calculate area of large patches per county cropland (large farms/total planted equivalent)
        large_crop_patches_per_cropland<-total_area_large_crop_patches/croplandarea
      
      #Combine metrics
      countydf<-data.frame(fips, total_area_large_crop_patches,landscapearea_patches,croplandarea, large_crop_patches_per_cropland,stringsAsFactors=FALSE)
      colnames(countydf)<-c("FIPS", "LCP_Area_ha", "CountyArea_ha", "CroplandArea_ha", "Prop_LCP")
  
      
      ###################
      #Combine all datasets
      ##################
      #Clean up into final dataframe
      #Remove extra FIPS columns for all df except allmat
      natural_crops_mat<-natural_crops_mat[,-1]
      forest_crop_mat<-forest_crop_mat[,-1]
      shrubgrass_crop_mat<-shrubgrass_crop_mat[,-1]
      countydf<-countydf[,-1]
      
      #Cbind all df and clean-up
      finaldf<-cbind(allmat,natural_crops_mat,forest_crop_mat,shrubgrass_crop_mat, countydf) %>% #bind all calculations
        dplyr::mutate(Year=year, #add year column,
                      Scenario=scenario,#add scenario column
                      pland_natural=pland_df +pland_ef+pland_mf+pland_grass+pland_shrub+pland_wet+pland_mdf,#add pland_natural
                      natural_crops_ed=tel_naturalcrops_nc/CountyArea_ha, #add meters shared edge per hectare county area for natural_crops
                      forest_crops_ed=tel_forestcrop_fc/CountyArea_ha,#add meters shared edge per hectare county area for forest_crops
                      shrubgrass_crops_ed=tel_shrubgrasscrop_sgc/CountyArea_ha) %>% #add meters shared edge per hectare county area for shrub_grass
        dplyr::select(FIPS, Year, Scenario, everything()) #FIPS, Year, and Scenario columns first
      finaldf[is.na(finaldf)] <- 0 #Add replace NA with zero (for calculations)
   
      ###################
      #Return final df
      ##################
      return(finaldf) #will be saved as result
      
  }#End cluster of counties 
  endCluster()

  final<-result
  return(final)#will be saved by variable name given to function call
  
}#End function


###############################
#Apply Function
###############################
#Apply function to desired year and scenario combinations and write csv

#B1_2050
B1_2050<-calc_NLCD(scenario="B1",year=2050) 
write_csv(B1_2050, "Data/nlcd_future/B1_2050.csv")
#B2_2050
B2_2050<-calc_NLCD(scenario="B2",year=2050)
write_csv(B2_2050, "Data/nlcd_future/B2_2050.csv")
#A1B_2050
A1B_2050<-calc_NLCD(scenario="A1B",year=2050)
write_csv(A1B_2050, "Data/nlcd_future/A1B_2050.csv")
#A2_2050
A2_2050<-calc_NLCD(scenario="A2",year=2050)
write_csv(A2_2050, "Data/nlcd_future/A2_2050.csv")



##########################
#Combine datasets
##########################

#Combine 2050

nlcd_2050<-rbind(B1_2050, B2_2050, A1B_2050, A2_2050)%>%
  dplyr::select(FIPS, Year, Scenario, everything()) %>% 
  arrange(FIPS)

write_csv(nlcd_2050, "Data/nlcd_future/nlcd_2050.csv")
