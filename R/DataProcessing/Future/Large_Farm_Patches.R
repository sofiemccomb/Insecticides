#Area of Large Patches Per County
library(raster) #rasters
library(sf)#shapefiles
library(landscapemetrics)
library(doParallel)
library(tidyverse)

#To change between original and forecasted, change nlcd, change st_transform, and change reclassification

calc_LCP_year<-function(year){
  year<-year
  
  #NLCD
  #NLCD Rasters
  nlcd<-raster(paste0("Data/NLCD/NLCD_", year, "_Land_Cover_L48_20190424.img"))
  #NLCD Historical Forecast (2001)
  # nlcd<-raster(paste0("Data/Forecast_NLCD/CONUS_Historical_y", year, ".tif"))
  #NLCD B1 Forecast (2006, 2011, 2016)
  # nlcd<-raster(paste0("Data/Forecast_NLCD/CONUS_B1_y", year, ".tif"))
  
  #Counties
  counties<-read_sf("Data/TIGER2018_Counties/tl_2018_us_county.shp") %>%
    dplyr::select(STATEFP,GEOID) %>% 
    # st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0") #forecasted
    st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84
                         +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs ") #Original NLCD projection
  contig48_counties<-subset(counties,counties$STATEFP!="02"& counties$STATEFP!="15"&
                              counties$STATEFP!="60"&counties$STATEFP!="66"&
                              counties$STATEFP!="69"&counties$STATEFP!="72"&
                              counties$STATEFP!="74"&counties$STATEFP!="78")
  
  
  counties<-unique(contig48_counties$GEOID)#Counties to loop through
  
  registerDoParallel(cores=18)#initiate multiple cores for processing
  ptm <- proc.time() #time the process
  result<-foreach(c=1:length(counties), .combine=rbind)%dopar%{
    #Subset NLCD to county
    fips<-counties[c]
    county_sf<-subset(contig48_counties, contig48_counties$GEOID==fips)
    county <- sf:::as_Spatial(county_sf)#sf back to sp (added line bc was causing issues)
    nlcd_crop<-raster::crop(nlcd, extent(county))#crop nlcd raster to county
    nlcdcounty<-raster::mask(nlcd_crop, county)#mask nlcd raster to county
  
  
    #Reclassify county NLCD: Binary reclass- 1 for crops, 0 all else (82 is crops) for original
    m <- c(-Inf,81.5,0, 81.5,82.5,1, 89.5,Inf,0)
    #Reclassify county NLCD: Binary reclass- 1 for crops, 0 all else (13 is crops) for forecasted
    # m <- c(-Inf,12.5,0, 12.5,13.5,1, 13.5,Inf,0) 
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    rc <- reclassify(nlcdcounty, rclmat)#Reclassify nlcd
    
    #Landscape metrics
    #Calculate area of each patch
    nlcd_p_area<-lsm_p_area(rc, directions=8) #Lets choose to stay with 8 directions over 4 directions so that patches must share an edge
    #Subset areas of crop patches
    nlcd_p_area_crops<-nlcd_p_area[nlcd_p_area$class==1,]
    #Select large patches (>500 acres, or >202.343 hectares)
    nlcd_large_crop_patch<-nlcd_p_area_crops[nlcd_p_area_crops$value>=202.343,]#Each patch in hectares- so we want only the patches >202.343 hectares, which is ~500 acres (definition of large farms)
    #Sum total area of large patches
    total_area_large_crop_patches<-sum(nlcd_large_crop_patch$value)#Value in hectares
    #Sum total area of county
    landscapearea_patches<-sum(nlcd_p_area$value) #Using landscape area mean to calculate is same value so we good
    #Sum total cropland of county (large and small patches)
    croplandarea<-sum(nlcd_p_area_crops$value)
    #Calculate area of large patches per county cropland (large farms/total planted equivalent)
    large_crop_patches_per_county<-total_area_large_crop_patches/croplandarea
    county_row<-cbind(fips, total_area_large_crop_patches,landscapearea_patches,croplandarea, large_crop_patches_per_county)
    return(county_row)
  
    }#End cluster of counties #should take ~10 minutes each
    proc.time() - ptm
    endCluster()
    
  countydf<-as.data.frame(result)
  colnames(countydf)<-c("FIPS", "LCP_Area_ha", "CountyArea_ha", "CroplandArea_ha", "Prop_LCP")
  countydf<-countydf %>% 
    dplyr::mutate(Year=year)
 return(countydf)

}#End function


##############
#Apply function to 4 NLCD years
LCP_2001<-calc_LCP_year(2001) #If forecast: Turn on historical
LCP_2006<-calc_LCP_year(2006) #If forecast:Turn on B1
LCP_2011<-calc_LCP_year(2011) #If forecast:Turn on B1
LCP_2016<-calc_LCP_year(2016) #If forecast:Turn on B1

LCP<-rbind(LCP_2001, LCP_2006, LCP_2011, LCP_2016)%>%
  dplyr::select(Year, everything()) %>% 
  dplyr::select(FIPS, everything()) %>% 
  arrange(FIPS)


# Change years to match those in the other datasets so can combine by FIPS and years
LCP$Year[LCP$Year==2001] <- 2002
LCP$Year[LCP$Year==2006] <- 2007
LCP$Year[LCP$Year==2011] <- 2012
LCP$Year[LCP$Year==2016] <- 2017

#Save the file as a csv
write_csv(LCP, "Data/LargeCropPatch/LargeCropPatch.csv")
# write_csv(LCP, "Data/LargeCropPatch/LargeCropPatch_Forecasted.csv")

