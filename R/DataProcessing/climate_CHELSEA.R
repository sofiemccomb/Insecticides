#Calculate climate statistics per county for forecasted climate data

#Load in Packages
################################
library(raster) #rasters
library(sf)#shapefiles
library(tidyverse) #datatable manipulation
library(rgdal) #shapefiles
library(doParallel) #parallel processing
library(dismo) #for biovars to calculate bioclim
library(exactextractr) #extract raster values by polygon accounting for cell coverage   
library(purrr) #For reduce function using full_join
#https://cran.r-project.org/web/packages/exactextractr/readme/README.html
options(scipen=999) #no scientific notation
rasterOptions(tmpdir = "/home/sofie/Data/RasterTemp", tmptime=24)


#Read in counties data
################################
#Counties
counties<-read_sf("Data/TIGER2018_Counties/tl_2018_us_county.shp") %>%
  dplyr::select(STATEFP,GEOID) %>% 
  st_transform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")#GCS for climate rasters below
contig48_counties<-subset(counties,counties$STATEFP!="02"& counties$STATEFP!="15"&
                            counties$STATEFP!="60"&counties$STATEFP!="66"&
                            counties$STATEFP!="69"&counties$STATEFP!="72"&
                            counties$STATEFP!="74"&counties$STATEFP!="78")

#Read in climate rasters and calculate climate variables
################################
country <- sf:::as_Spatial(contig48_counties)#sf back to sp (added line bc was causing issues) #Use to crop before summing


#Annual Precipitation (mm)
  #Current
  prec_current<-stack(raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_01_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_02_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_03_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_04_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_05_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_06_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_07_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_08_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_09_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_10_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_11_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_12_V1.2_land.tif"))
  prec_current_crop<-raster::crop(prec_current, extent(country))#Crop to conus extent
  annual_prec_current <- sum(prec_current_crop)#Get annual precipitation in mm
  #RCP45
  prec_rcp45<-stack(raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_1_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_2_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_3_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_4_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_5_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_6_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_7_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_8_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_9_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_10_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_11_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_12_2041-2060.tif"))
  prec_rcp45_crop<-raster::crop(prec_rcp45, extent(country))#Crop to conus extent
  #Replace negative minimum value with NA for precipitation (some kind of NA not removed; value of -32767) 
  precip_reclass <- function(x) {ifelse(x == -32767, NA, x)} 
  prec_rcp45_crop_reclass <- calc(prec_rcp45_crop, precip_reclass) #Make -32767 value NA
  annual_prec_rcp45 <- sum(prec_rcp45_crop_reclass)#Get annual precipitation in mm
  #RCP85
  prec_rcp85<-stack(raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_1_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_2_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_3_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_4_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_5_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_6_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_7_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_8_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_9_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_10_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_11_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_12_2041-2060.tif"))
  prec_rcp85_crop<-raster::crop(prec_rcp85, extent(country))#Crop to conus extent
  #Replace negative minimum value with NA for precipitation (some kind of NA not removed; value of -32767) 
  precip_reclass <- function(x) {ifelse(x == -32767, NA, x)} 
  prec_rcp85_crop_reclass <- calc(prec_rcp85_crop, precip_reclass) #Make -32767 value NA
  annual_prec_rcp85 <- sum(prec_rcp85_crop_reclass)#Get annual precipitation in mm

  
  
#Mean Temperatures (F)
  #Current
    #January
    tmean_current_Jan<-raster("Data/CHELSA/climatology/meantemperature/CHELSA_temp10_01_1979-2013_V1.2_land.tif")
    tmean_current_Jan_crop<-raster::crop(tmean_current_Jan, extent(country))#Crop to conus extent
    tmean_current_Jan_crop<-tmean_current_Jan_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_current_Jan_crop_F<-(tmean_current_Jan_crop*(9/5))+32 #Units in deg F
    #April
    tmean_current_Apr<-raster("Data/CHELSA/climatology/meantemperature/CHELSA_temp10_04_1979-2013_V1.2_land.tif")
    tmean_current_Apr_crop<-raster::crop(tmean_current_Apr, extent(country))#Crop to conus extent
    tmean_current_Apr_crop<-tmean_current_Apr_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_current_Apr_crop_F<-(tmean_current_Apr_crop*(9/5))+32 #Units in deg F
    #July
    tmean_current_Jul<-raster("Data/CHELSA/climatology/meantemperature/CHELSA_temp10_07_1979-2013_V1.2_land.tif")
    tmean_current_Jul_crop<-raster::crop(tmean_current_Jul, extent(country))#Crop to conus extent
    tmean_current_Jul_crop<-tmean_current_Jul_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_current_Jul_crop_F<-(tmean_current_Jul_crop*(9/5))+32 #Units in deg F
    #October
    tmean_current_Oct<-raster("Data/CHELSA/climatology/meantemperature/CHELSA_temp10_10_1979-2013_V1.2_land.tif")
    tmean_current_Oct_crop<-raster::crop(tmean_current_Oct, extent(country))#Crop to conus extent
    tmean_current_Oct_crop<-tmean_current_Oct_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_current_Oct_crop_F<-(tmean_current_Oct_crop*(9/5))+32 #Units in deg F
  #RCP45
    #January
    tmean_rcp45_Jan<-raster("Data/CHELSA/future/RCP45_4160/meantemperature/CHELSA_tas_mon_CCSM4_rcp45_r1i1p1_g025.nc_1_2041-2060_V1.2.tif")
    tmean_rcp45_Jan_crop<-raster::crop(tmean_rcp45_Jan, extent(country))#Crop to conus extent
    tmean_rcp45_Jan_crop<-tmean_rcp45_Jan_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_rcp45_Jan_crop_F<-(tmean_rcp45_Jan_crop*(9/5))+32 #Units in deg F
    #April
    tmean_rcp45_Apr<-raster("Data/CHELSA/future/RCP45_4160/meantemperature/CHELSA_tas_mon_CCSM4_rcp45_r1i1p1_g025.nc_4_2041-2060_V1.2.tif")
    tmean_rcp45_Apr_crop<-raster::crop(tmean_rcp45_Apr, extent(country))#Crop to conus extent
    tmean_rcp45_Apr_crop<-tmean_rcp45_Apr_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_rcp45_Apr_crop_F<-(tmean_rcp45_Apr_crop*(9/5))+32 #Units in deg F
    #July
    tmean_rcp45_Jul<-raster("Data/CHELSA/future/RCP45_4160/meantemperature/CHELSA_tas_mon_CCSM4_rcp45_r1i1p1_g025.nc_7_2041-2060_V1.2.tif")
    tmean_rcp45_Jul_crop<-raster::crop(tmean_rcp45_Jul, extent(country))#Crop to conus extent
    tmean_rcp45_Jul_crop<-tmean_rcp45_Jul_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_rcp45_Jul_crop_F<-(tmean_rcp45_Jul_crop*(9/5))+32 #Units in deg F
    #October
    tmean_rcp45_Oct<-raster("Data/CHELSA/future/RCP45_4160/meantemperature/CHELSA_tas_mon_CCSM4_rcp45_r1i1p1_g025.nc_10_2041-2060_V1.2.tif")
    tmean_rcp45_Oct_crop<-raster::crop(tmean_rcp45_Oct, extent(country))#Crop to conus extent
    tmean_rcp45_Oct_crop<-tmean_rcp45_Oct_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_rcp45_Oct_crop_F<-(tmean_rcp45_Oct_crop*(9/5))+32 #Units in deg F
  #RCP85
    #January
    tmean_rcp85_Jan<-raster("Data/CHELSA/future/RCP85_4160/meantemperature/CHELSA_tas_mon_CCSM4_rcp85_r1i1p1_g025.nc_1_2041-2060_V1.2.tif")
    tmean_rcp85_Jan_crop<-raster::crop(tmean_rcp85_Jan, extent(country))#Crop to conus extent
    tmean_rcp85_Jan_crop<-tmean_rcp85_Jan_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_rcp85_Jan_crop_F<-(tmean_rcp85_Jan_crop*(9/5))+32 #Units in deg F
    #April
    tmean_rcp85_Apr<-raster("Data/CHELSA/future/RCP85_4160/meantemperature/CHELSA_tas_mon_CCSM4_rcp85_r1i1p1_g025.nc_4_2041-2060_V1.2.tif")
    tmean_rcp85_Apr_crop<-raster::crop(tmean_rcp85_Apr, extent(country))#Crop to conus extent
    tmean_rcp85_Apr_crop<-tmean_rcp85_Apr_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_rcp85_Apr_crop_F<-(tmean_rcp85_Apr_crop*(9/5))+32 #Units in deg F
    #July
    tmean_rcp85_Jul<-raster("Data/CHELSA/future/RCP85_4160/meantemperature/CHELSA_tas_mon_CCSM4_rcp85_r1i1p1_g025.nc_7_2041-2060_V1.2.tif")
    tmean_rcp85_Jul_crop<-raster::crop(tmean_rcp85_Jul, extent(country))#Crop to conus extent
    tmean_rcp85_Jul_crop<-tmean_rcp85_Jul_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_rcp85_Jul_crop_F<-(tmean_rcp85_Jul_crop*(9/5))+32 #Units in deg F
    #October
    tmean_rcp85_Oct<-raster("Data/CHELSA/future/RCP85_4160/meantemperature/CHELSA_tas_mon_CCSM4_rcp85_r1i1p1_g025.nc_10_2041-2060_V1.2.tif")
    tmean_rcp85_Oct_crop<-raster::crop(tmean_rcp85_Oct, extent(country))#Crop to conus extent
    tmean_rcp85_Oct_crop<-tmean_rcp85_Oct_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmean_rcp85_Oct_crop_F<-(tmean_rcp85_Oct_crop*(9/5))+32 #Units in deg F
    

    
#Winter Severity (tmin average Jan, Feb, Mar) (degF)
  #Current
    tmin_current_stack<-stack(raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_01_1979-2013_V1.2_land.tif"),
                              raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_02_1979-2013_V1.2_land.tif"),
                              raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_03_1979-2013_V1.2_land.tif"))
    tmin_current_stack_crop<-raster::crop(tmin_current_stack, extent(country))#Crop to conus extent
    tmin_avg_current <- mean(tmin_current_stack_crop) #Get average minimum winter temperature
    tmin_avg_current<-tmin_avg_current/10 #To get units of Celsius (Chelsa was C*10)
    tmin_avg_current_F<-(tmin_avg_current*(9/5))+32 #Units in deg F
  #RCP45
    tmin_rcp45_stack<-stack(raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_1_2041-2060_V1.2.tif"),
                              raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_2_2041-2060_V1.2.tif"),
                              raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_3_2041-2060_V1.2.tif"))
    tmin_rcp45_stack_crop<-raster::crop(tmin_rcp45_stack, extent(country))#Crop to conus extent
    tmin_avg_rcp45 <- mean(tmin_rcp45_stack_crop) #Get average minimum winter temperature
    tmin_avg_rcp45<-tmin_avg_rcp45/10 #To get units of Celsius (Chelsa was C*10)
    tmin_avg_rcp45_F<-(tmin_avg_rcp45*(9/5))+32 #Units in deg F
  #RCP85
    tmin_rcp85_stack<-stack(raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_1_2041-2060_V1.2.tif"),
                            raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_2_2041-2060_V1.2.tif"),
                            raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_3_2041-2060_V1.2.tif"))
    tmin_rcp85_stack_crop<-raster::crop(tmin_rcp85_stack, extent(country))#Crop to conus extent
    tmin_avg_rcp85 <- mean(tmin_rcp85_stack_crop) #Get average minimum winter temperature
    tmin_avg_rcp85<-tmin_avg_rcp85/10 #To get units of Celsius (Chelsa was C*10)
    tmin_avg_rcp85_F<-(tmin_avg_rcp85*(9/5))+32 #Units in deg F

    

#Summer Severity (tmax average Jun, Jul, Aug) 
    #Current
    tmax_current_stack<-stack(raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_06_1979-2013_V1.2_land.tif"),
                              raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_07_1979-2013_V1.2_land.tif"),
                              raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_08_1979-2013_V1.2_land.tif"))
    tmax_current_stack_crop<-raster::crop(tmax_current_stack, extent(country))#Crop to conus extent
    tmax_avg_current <- mean(tmax_current_stack_crop) #Get average maximum summer temperature
    tmax_avg_current<-tmax_avg_current/10 #To get units of Celsius (Chelsa was C*10)
    tmax_avg_current_F<-(tmax_avg_current*(9/5))+32 #Units in deg F
    #RCP45
    tmax_rcp45_stack<-stack(raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_6_2041-2060_V1.2.tif"),
                            raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_7_2041-2060_V1.2.tif"),
                            raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_8_2041-2060_V1.2.tif"))
    tmax_rcp45_stack_crop<-raster::crop(tmax_rcp45_stack, extent(country))#Crop to conus extent
    tmax_avg_rcp45 <- mean(tmax_rcp45_stack_crop) #Get average maximum summer temperature
    tmax_avg_rcp45<-tmax_avg_rcp45/10 #To get units of Celsius (Chelsa was C*10)
    tmax_avg_rcp45_F<-(tmax_avg_rcp45*(9/5))+32 #Units in deg F
    #RCP85
    tmax_rcp85_stack<-stack(raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_6_2041-2060_V1.2.tif"),
                            raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_7_2041-2060_V1.2.tif"),
                            raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_8_2041-2060_V1.2.tif"))
    tmax_rcp85_stack_crop<-raster::crop(tmax_rcp85_stack, extent(country))#Crop to conus extent
    tmax_avg_rcp85 <- mean(tmax_rcp85_stack_crop) #Get average maximum summer temperature
    tmax_avg_rcp85<-tmax_avg_rcp85/10 #To get units of Celsius (Chelsa was C*10)
    tmax_avg_rcp85_F<-(tmax_avg_rcp85*(9/5))+32 #Units in deg F
    
    
#Growing Degree Day (using F)
  #Current
    tmin_current<-stack(raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_01_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_02_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_03_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_04_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_05_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_06_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_07_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_08_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_09_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_10_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_11_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_12_1979-2013_V1.2_land.tif"))
    tmin_current_crop<-raster::crop(tmin_current, extent(country))#Crop to conus extent
    tmin_current_crop<-tmin_current_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmin_current_crop_F<-(tmin_current_crop*(9/5))+32 #Units in deg F
    
    tmax_current<-stack(raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_01_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_02_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_03_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_04_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_05_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_06_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_07_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_08_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_09_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_10_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_11_1979-2013_V1.2_land.tif"),
                        raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_12_1979-2013_V1.2_land.tif"))
    tmax_current_crop<-raster::crop(tmax_current, extent(country))#Crop to conus extent
    tmax_current_crop<-tmax_current_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmax_current_crop_F<-(tmax_current_crop*(9/5))+32 #Units in deg F
    
    
    #RCP45
    tmin_rcp45<-stack(raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_1_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_2_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_3_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_4_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_5_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_6_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_7_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_8_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_9_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_10_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_11_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_12_2041-2060_V1.2.tif"))
    tmin_rcp45_crop<-raster::crop(tmin_rcp45, extent(country))#Crop to conus extent
    tmin_rcp45_crop<-tmin_rcp45_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmin_rcp45_crop_F<-(tmin_rcp45_crop*(9/5))+32 #Units in deg F
    
    
    tmax_rcp45<-stack(raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_1_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_2_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_3_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_4_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_5_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_6_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_7_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_8_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_9_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_10_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_11_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_12_2041-2060_V1.2.tif"))
    tmax_rcp45_crop<-raster::crop(tmax_rcp45, extent(country))#Crop to conus extent
    tmax_rcp45_crop<-tmax_rcp45_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmax_rcp45_crop_F<-(tmax_rcp45_crop*(9/5))+32 #Units in deg F
    
    
    #RCP85
    tmin_rcp85<-stack(raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_1_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_2_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_3_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_4_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_5_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_6_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_7_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_8_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_9_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_10_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_11_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_12_2041-2060_V1.2.tif"))
    tmin_rcp85_crop<-raster::crop(tmin_rcp85, extent(country))#Crop to conus extent
    tmin_rcp85_crop<-tmin_rcp85_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmin_rcp85_crop_F<-(tmin_rcp85_crop*(9/5))+32 #Units in deg F
    
    tmax_rcp85<-stack(raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_1_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_2_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_3_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_4_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_5_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_6_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_7_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_8_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_9_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_10_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_11_2041-2060_V1.2.tif"),
                      raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_12_2041-2060_V1.2.tif"))
    tmax_rcp85_crop<-raster::crop(tmax_rcp85, extent(country))#Crop to conus extent
    tmax_rcp85_crop<-tmax_rcp85_crop/10 #To get units of Celsius (Chelsa was C*10)
    tmax_rcp85_crop_F<-(tmax_rcp85_crop*(9/5))+32 #Units in deg F
    
    #Mean values of each
    tmean_current<-(tmax_current_crop_F+tmin_current_crop_F)/2 #Mean from monthly tmin and tmax for current
    tmean_rcp45<-(tmax_rcp45_crop_F+tmin_rcp45_crop_F)/2 #Mean from monthly tmin and tmax for rcp45
    tmean_rcp85<-(tmax_rcp85_crop_F+tmin_rcp85_crop_F)/2 #Mean from monthly tmin and tmax for rcp85
    
    #Calculate GDD from tmean values using GDD reclass parameters
    GDD_reclass <- function(x) {ifelse(x <= 50, 0, ifelse(x >= 90, 40, x-50))} #GDD reclass function
      #Current
      GDD_current <- calc(tmean_current, GDD_reclass) #Get GDD value using GDD reclass function
      monthly_GDD_current<-GDD_current*30 #Get monthly GDD
      annual_GDD_current<-sum(monthly_GDD_current) #Get annual GDD
      #RCP45
      GDD_rcp45 <- calc(tmean_rcp45, GDD_reclass) #Get GDD value using GDD reclass function
      monthly_GDD_rcp45<-GDD_rcp45*30 #Get monthly GDD
      annual_GDD_rcp45<-sum(monthly_GDD_rcp45) #Get annual GDD
      #RCP85
      GDD_rcp85 <- calc(tmean_rcp85, GDD_reclass) #Get GDD value using GDD reclass function
      monthly_GDD_rcp85<-GDD_rcp85*30 #Get monthly GDD
      annual_GDD_rcp85<-sum(monthly_GDD_rcp85) #Get annual GDD

      
#Prepare final climate variables for extraction to FIPS calculating areas to account for geographic CRS in weighting means in extract
        #Left in geographic CRS as projecting all the rasters takes an outrageous amount of time
    #Current
    stk_annualprec_current <- stack(list(ras=annual_prec_current, area=area(annual_prec_current)))
    stk_tmean_current_Jan<- stack(list(ras=tmean_current_Jan_crop_F, area=area(tmean_current_Jan_crop_F)))
    stk_tmean_current_Apr<- stack(list(ras=tmean_current_Apr_crop_F, area=area(tmean_current_Apr_crop_F)))
    stk_tmean_current_Jul<- stack(list(ras=tmean_current_Jul_crop_F, area=area(tmean_current_Jul_crop_F)))
    stk_tmean_current_Oct<- stack(list(ras=tmean_current_Oct_crop_F, area=area(tmean_current_Oct_crop_F)))
    stk_tminavg_current<- stack(list(ras=tmin_avg_current_F, area=area(tmin_avg_current_F)))
    stk_tmaxavg_current<- stack(list(ras=tmax_avg_current_F, area=area(tmax_avg_current_F)))
    stk_annualGDD_current<- stack(list(ras=annual_GDD_current, area=area(annual_GDD_current)))
    
    #RCP45
    stk_annualprec_rcp45<- stack(list(ras=annual_prec_rcp45, area=area(annual_prec_rcp45)))
    stk_tmean_rcp45_Jan<- stack(list(ras=tmean_rcp45_Jan_crop_F, area=area(tmean_rcp45_Jan_crop_F)))
    stk_tmean_rcp45_Apr<- stack(list(ras=tmean_rcp45_Apr_crop_F, area=area(tmean_rcp45_Apr_crop_F)))
    stk_tmean_rcp45_Jul<- stack(list(ras=tmean_rcp45_Jul_crop_F, area=area(tmean_rcp45_Jul_crop_F)))
    stk_tmean_rcp45_Oct<- stack(list(ras=tmean_rcp45_Oct_crop_F, area=area(tmean_rcp45_Oct_crop_F)))
    stk_tminavg_rcp45<- stack(list(ras=tmin_avg_rcp45_F, area=area(tmin_avg_rcp45_F)))
    stk_tmaxavg_rcp45<- stack(list(ras=tmax_avg_rcp45_F, area=area(tmax_avg_rcp45_F)))
    stk_annualGDD_rcp45<- stack(list(ras=annual_GDD_rcp45, area=area(annual_GDD_rcp45)))
    
    #RCP85
    stk_annualprec_rcp85<- stack(list(ras=annual_prec_rcp85, area=area(annual_prec_rcp85)))
    stk_tmean_rcp85_Jan<- stack(list(ras=tmean_rcp85_Jan_crop_F, area=area(tmean_rcp85_Jan_crop_F)))
    stk_tmean_rcp85_Apr<- stack(list(ras=tmean_rcp85_Apr_crop_F, area=area(tmean_rcp85_Apr_crop_F)))
    stk_tmean_rcp85_Jul<- stack(list(ras=tmean_rcp85_Jul_crop_F, area=area(tmean_rcp85_Jul_crop_F)))
    stk_tmean_rcp85_Oct<- stack(list(ras=tmean_rcp85_Oct_crop_F, area=area(tmean_rcp85_Oct_crop_F)))
    stk_tminavg_rcp85<- stack(list(ras=tmin_avg_rcp85_F, area=area(tmin_avg_rcp85_F)))
    stk_tmaxavg_rcp85<- stack(list(ras=tmax_avg_rcp85_F, area=area(tmax_avg_rcp85_F)))
    stk_annualGDD_rcp85<- stack(list(ras=annual_GDD_rcp85, area=area(annual_GDD_rcp85)))

 
    
    
    
    
#Extract climate data to each fips (accounting for GCS with weighting area)
  #######################
  counties<-unique(contig48_counties$GEOID)#Counties to loop through
  
  registerDoParallel(cores=18)#initiate multiple cores for processing
  result<-foreach(c=1:length(counties), .combine=rbind)%dopar%{ #For each county
      #Subset NLCD to county
      fips<-counties[c] #get FIPS
      county_sf<-subset(contig48_counties, contig48_counties$GEOID==fips) #subset counties dataset to fips
      
      #Current
        county_annual_prec_current <-exactextractr::exact_extract(stk_annualprec_current, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_current_Jan <-exactextractr::exact_extract(stk_tmean_current_Jan, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_current_Apr <-exactextractr::exact_extract(stk_tmean_current_Apr, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_current_Jul <-exactextractr::exact_extract(stk_tmean_current_Jul, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_current_Oct <-exactextractr::exact_extract(stk_tmean_current_Oct, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tminavg_current <-exactextractr::exact_extract(stk_tminavg_current, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmaxavg_current <-exactextractr::exact_extract(stk_tmaxavg_current, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_annualGDD_current <-exactextractr::exact_extract(stk_annualGDD_current, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        
        #RCP45
        county_annual_prec_rcp45 <-exactextractr::exact_extract(stk_annualprec_rcp45, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_rcp45_Jan <-exactextractr::exact_extract(stk_tmean_rcp45_Jan, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_rcp45_Apr <-exactextractr::exact_extract(stk_tmean_rcp45_Apr, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_rcp45_Jul <-exactextractr::exact_extract(stk_tmean_rcp45_Jul, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_rcp45_Oct <-exactextractr::exact_extract(stk_tmean_rcp45_Oct, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tminavg_rcp45 <-exactextractr::exact_extract(stk_tminavg_rcp45, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmaxavg_rcp45 <-exactextractr::exact_extract(stk_tmaxavg_rcp45, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_annualGDD_rcp45 <-exactextractr::exact_extract(stk_annualGDD_rcp45, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
      
        #RCP85
        county_annual_prec_rcp85 <-exactextractr::exact_extract(stk_annualprec_rcp85, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_rcp85_Jan <-exactextractr::exact_extract(stk_tmean_rcp85_Jan, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_rcp85_Apr <-exactextractr::exact_extract(stk_tmean_rcp85_Apr, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_rcp85_Jul <-exactextractr::exact_extract(stk_tmean_rcp85_Jul, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmean_rcp85_Oct <-exactextractr::exact_extract(stk_tmean_rcp85_Oct, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tminavg_rcp85 <-exactextractr::exact_extract(stk_tminavg_rcp85, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_tmaxavg_rcp85 <-exactextractr::exact_extract(stk_tmaxavg_rcp85, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
        county_annualGDD_rcp85 <-exactextractr::exact_extract(stk_annualGDD_rcp85, county_sf, function(values, coverage_frac)
          weighted.mean(values$ras, values$area*coverage_frac, na.rm=TRUE))
      
      #Combine values
        #Current
        df_current<-data.frame(county_annual_prec_current,county_tmean_current_Jan,county_tmean_current_Apr,
                               county_tmean_current_Jul, county_tmean_current_Oct, county_tminavg_current,
                               county_tmaxavg_current,county_annualGDD_current)
        colnames(df_current)<-c("annprcp", "tmean_Jan", "tmean_Apr", "tmean_Jul", "tmean_Oct",
                                "tminavg", "tmaxavg", "annualGDD")
        df_current<-df_current %>% dplyr::mutate(FIPS=fips, Scenario="Current") %>% dplyr::select(FIPS, Scenario, everything())
        #RCP45
        df_rcp45<-data.frame(county_annual_prec_rcp45,county_tmean_rcp45_Jan,county_tmean_rcp45_Apr,
                               county_tmean_rcp45_Jul, county_tmean_rcp45_Oct, county_tminavg_rcp45,
                               county_tmaxavg_rcp45,county_annualGDD_rcp45)
        colnames(df_rcp45)<-c("annprcp", "tmean_Jan", "tmean_Apr", "tmean_Jul", "tmean_Oct",
                              "tminavg", "tmaxavg", "annualGDD")
        df_rcp45<-df_rcp45 %>% dplyr::mutate(FIPS=fips,Scenario="RCP45") %>% dplyr::select(FIPS, Scenario, everything())
        #RCP85
        df_rcp85<-data.frame(county_annual_prec_rcp85,county_tmean_rcp85_Jan,county_tmean_rcp85_Apr,
                               county_tmean_rcp85_Jul, county_tmean_rcp85_Oct, county_tminavg_rcp85,
                               county_tmaxavg_rcp85,county_annualGDD_rcp85)
        colnames(df_rcp85)<-c("annprcp", "tmean_Jan", "tmean_Apr", "tmean_Jul", "tmean_Oct",
                              "tminavg", "tmaxavg", "annualGDD")
        df_rcp85<-df_rcp85 %>% dplyr::mutate(FIPS=fips,Scenario="RCP85") %>% dplyr::select(FIPS, Scenario, everything())
        
      #Combine dataframes
        # df_climate<-list(df_current, df_rcp45, df_rcp85) %>% 
        #   purrr::reduce(full_join, by="FIPS")
        df_climate<-rbind(df_current, df_rcp45, df_rcp85)

    
  } #End parallel processing
  endCluster()
  
write_csv(result, "Data/forecast_climate/forecastedclimate.csv")  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #Bioclim-rest of variables cropped-need to crop them and also divide by 10 for temperature
  ##############
  #Annual Precipitation
  country <- sf:::as_Spatial(contig48_counties)#sf back to sp (added line bc was causing issues) #Use to crop before summing
  #Current
  prec_current<-stack(raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_01_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_02_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_03_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_04_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_05_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_06_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_07_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_08_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_09_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_10_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_11_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/precipitation/CHELSA_prec_12_V1.2_land.tif"))
  prec_current_crop<-raster::crop(prec_current, extent(country))#Crop to conus extent
  annual_prec_current <- sum(prec_current_crop)#Get annual precipitation in mm
  #RCP45
  prec_rcp45<-stack(raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_1_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_2_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_3_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_4_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_5_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_6_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_7_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_8_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_9_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_10_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_11_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp45_r1i1p1_g025.nc_12_2041-2060.tif"))
  prec_rcp45_crop<-raster::crop(prec_rcp45, extent(country))#Crop to conus extent
  annual_prec_rcp45 <- sum(prec_rcp45_crop)#Get annual precipitation in mm
  #RCP85
  prec_rcp85<-stack(raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_1_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_2_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_3_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_4_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_5_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_6_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_7_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_8_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_9_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_10_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_11_2041-2060.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/precipitation/CHELSA_pr_mon_CCSM4_rcp85_r1i1p1_g025.nc_12_2041-2060.tif"))
  prec_rcp85_crop<-raster::crop(prec_rcp85, extent(country))#Crop to conus extent
  annual_prec_rcp85 <- sum(prec_rcp85_crop)#Get annual precipitation in mm
  
  
  
  
  
  
  
  tmin_current<-stack(raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_01_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_02_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_03_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_04_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_05_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_06_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_07_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_08_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_09_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_10_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_11_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/mintemperature/CHELSA_tmin10_12_1979-2013_V1.2_land.tif"))
  tmin_current_crop<-raster::crop(tmin_current, extent(country))#crop nlcd raster to county
  
  tmax_current<-stack(raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_01_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_02_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_03_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_04_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_05_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_06_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_07_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_08_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_09_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_10_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_11_1979-2013_V1.2_land.tif"),
                      raster("Data/CHELSA/climatology/maxtemperature/CHELSA_tmax10_12_1979-2013_V1.2_land.tif"))
  tmax_current_crop<-raster::crop(tmax_current, extent(country))#crop nlcd raster to county
  
  tmin_rcp45<-stack(raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_1_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_2_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_3_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_4_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_5_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_6_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_7_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_8_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_9_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_10_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_11_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp45_r1i1p1_g025.nc_12_2041-2060_V1.2.tif"))
  tmin_rcp45_crop<-raster::crop(tmin_rcp45, extent(country))#crop nlcd raster to county
  
  
  tmax_rcp45<-stack(raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_1_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_2_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_3_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_4_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_5_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_6_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_7_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_8_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_9_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_10_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_11_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP45_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp45_r1i1p1_g025.nc_12_2041-2060_V1.2.tif"))
  tmax_rcp45_crop<-raster::crop(tmax_rcp45, extent(country))#crop nlcd raster to county
  
  tmin_rcp85<-stack(raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_1_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_2_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_3_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_4_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_5_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_6_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_7_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_8_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_9_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_10_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_11_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/mintemperature/CHELSA_tasmin_mon_CCSM4_rcp85_r1i1p1_g025.nc_12_2041-2060_V1.2.tif"))
  tmin_rcp85_crop<-raster::crop(tmin_rcp85, extent(country))#crop nlcd raster to county
  
  tmax_rcp85<-stack(raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_1_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_2_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_3_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_4_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_5_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_6_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_7_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_8_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_9_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_10_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_11_2041-2060_V1.2.tif"),
                    raster("Data/CHELSA/future/RCP85_4160/maxtemperature/CHELSA_tasmax_mon_CCSM4_rcp85_r1i1p1_g025.nc_12_2041-2060_V1.2.tif"))
  tmax_rcp85_crop<-raster::crop(tmax_rcp85, extent(country))#crop nlcd raster to county
  
  #####################
  
  
  

  
#Bioclim
#######################
#Calculate bioclim variables for each raster stack
# bioclim_current<-dismo::biovars(prec_current_crop, tmin_current_crop, tmax_current_crop)
# bioclim_rcp45<-dismo::biovars(prec_rcp45_crop, tmin_rcp45_crop, tmax_rcp45_crop)
# bioclim_rcp85<-dismo::biovars(prec_rcp85_crop, tmin_rcp85_crop, tmax_rcp85_crop)
# as.matrix(b)
  
  
#Extract climate data to polygons
