#This R Script gathers and formats the USDA NASS Cropland Data Layer data for regression analyses.
  #Data source: Enter more
  #Give more background
  #This script will examine the CDL County_Pixel_Count csvs and calculate the diversity of crops in each county
  #Data downloaded in zip folder from the following source: https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php
  #Read me file in Data/CDL/County_Pixel_Count provided further informaiton on grouped columns and pixel counts

#Load Packages
    #Ensure necessary packages are installed an loaded
    library(tidyverse) #datatable manipulation
    library(janitor) #clean colnames
    library(vegan)
    options(scipen=999) #no scientific notation


#Read me file in folder (C:\Users\shemc\Documents\UCSB\NLCD\Data\CDL\County_Pixel_Count) details these are pixel counts
#Also there are grouped columns (in read me defined):
# Corn_all         = Category_001 + Category_225 + Category_226 + Category_237 + Category_241;
# Cotton_all       = Category_002 + Category_232 + Category_238 + Category_239;
# Sorghum_all      = Category_004 + Category_234 + Category_235 + Category_236;
# Soybeans_all     = Category_005 + Category_026 + Category_239 + Category_240 + Category_241 + Category_254;
# Barley_all       = Category_021 + Category_233 + Category_235 + Category_237 + Category_254;
# Wheat_Durum_all  = Category_022 + Category_230 + Category_234;
# Wheat_Winter_all = Category_024 + Category_026 + Category_225 + Category_236 + Category_238;
# Oats_all         = Category_028 + Category_226 + Category_240;
# Canteloupe_all   = Category_209 + Category_231;
# Lettuce_all      = Category_227 + Category_230 + Category_231 + Category_232 + Category_233

#Crosswalk file found here: https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php#Section1_6.0 #download attribute table of stats
crosswalk<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/CDL_crosswalk.csv")

#Bring in csvs for the years 2008, 2012, 2016 (2002 not available and 2007 incomplete), for both meters and acres
CDL_2008_acres<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Acres_2008.csv")
CDL_2008_30mpixel<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Pixel_2008_30m.csv")
CDL_2012_acres<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Acres_2012.csv")
CDL_2012_30mpixel<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Pixel_2012_30m.csv")
CDL_2016_acres<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Acres_2016.csv")
CDL_2016_30mpixel<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Pixel_2016_30m.csv")

#9 counties missing from CDL_2016_acres, but not pixels
#Missing: 51515, 51530, 51580, 51595, 51600, 51610, 51678,51685, 51720
#Investigated below

# #Load packages
# library(tidyverse)
# library(sf)
# library(ggrepel)
# library(ggspatial)
# 
# #Read in counties layer (3232 rows)
# counties<-read_sf("C:/Users/shemc/Documents/UCSB/NLCD/Data/TIGER2018_counties/tl_2018_us_county.shp")
# counties<-st_transform(counties, "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80
#                        +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs") #Project counties to nlcd projection
# 
# countiesmissing<-counties %>% 
#   filter(GEOID==51515| GEOID==51530| GEOID==51580| GEOID==51595|
#            GEOID==51600| GEOID==51610| GEOID==51678|GEOID==51685|
#            GEOID==51720)
# virgina<-counties %>%
#   filter(STATEFP==51)
# 
# ggplot()+
#   geom_sf(data = virgina, fill = "ivory2", color = "gray30", size = 0.2)+
#   geom_sf(data=countiesmissing, fill="red", color="black")+
#   labs(x = "", y = "", title = "Missing CDL Counties")+
#   coord_sf(datum=NA)+
#   ggrepel::geom_label_repel(data = countiesmissing, 
#                             aes(label = GEOID, geometry = geometry),
#                             stat = "sf_coordinates", min.segment.length = 0,
#                             colour = "black", segment.colour = "black", size=3)

#CSV saving options
CDL_mat_2008_acres<-"C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/CDL_Diversity/CDL_2008_acres.csv"
CDL_mat_2008_30mpixel<-"C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/CDL_Diversity/CDL_2008_30mpixel.csv"
CDL_mat_2012_acres<-"C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/CDL_Diversity/CDL_2012_acres.csv"
CDL_mat_2012_30mpixel<-"C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/CDL_Diversity/CDL_2012_30mpixel.csv"
CDL_mat_2016_acres<-"C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/CDL_Diversity/CDL_2016_acres.csv"
CDL_mat_2016_30mpixel<-"C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/CDL_Diversity/CDL_2016_30mpixel.csv"

#Have to do them all separately because different columns sometimes
###################################################################################
#CDL_2008_acres
######################################
#Remove the subcategories already accounted for in the all categories (some in multiple categories so only removed in the first category listed)
CDL_2008_acres_all<-CDL_2008_acres %>% 
  select(-c(Category_001,Category_225,Category_226,Category_237,Category_241)) %>% #remove corn subcategories
  select(-c(Category_002,Category_232,Category_238)) %>% #remove cotton subcategories (no 239 in dataset so removed)
  select(-c(Category_004,Category_234,Category_236)) %>% #remove sorghum subcategories (no 235 in dataset so removed)
  select(-c(Category_005,Category_026,Category_240,Category_254)) %>% #remove soybeans subcategories
  select(-c(Category_021)) %>% #remove barley subcategories #no 233 so removed
  select(-c(Category_022,Category_230)) %>% #remove wheat durum subcategories
  select(-c(Category_024)) %>% #remove wheat winter subcategories
  select(-c(Category_028)) %>% #remove oat subcategories
  select(-c(Category_209,Category_231)) %>% #remove canteloupe subcategories
  select(-c(Category_227)) #remove letuce subcategories

#Crosswalk
CDL_2008_acres_cols<-as.data.frame(colnames(CDL_2008_acres_all))
CDL_2008_acres_cols$mergecols<-colnames(CDL_2008_acres_all)
CDL_2008_acres_cols$colorder<-1:nrow(CDL_2008_acres_cols)
CDL_2008_acres_cols<-CDL_2008_acres_cols[,-1]
CDL_2008_acres_merge<-merge(CDL_2008_acres_cols, crosswalk, by.x="mergecols", by.y="CSV_Names", all.x=TRUE) 
CDL_2008_acres_merge<-CDL_2008_acres_merge[order(CDL_2008_acres_merge$colorder),]
CDL_2008_acres_merge$Ag_Names[is.na(CDL_2008_acres_merge$Ag_Names)] <- CDL_2008_acres_merge$mergecols[is.na(CDL_2008_acres_merge$Ag_Names)]
colnames(CDL_2008_acres_all)<-CDL_2008_acres_merge$Ag_Names

#Remove the non-ag rows
CDL_2008_acres_final<-CDL_2008_acres_all %>% 
  clean_names() %>% 
  select(-c(aquaculture, open_water, perennial_ice_snow, developed_open_space, developed_low_intensity,
            developed_med_intensity, developed_high_intensity, barren, deciduous_forest, evergreen_forest, 
            mixed_forest, shrubland, grassland_pasture, woody_wetlands, herbaceous_wetlands))

#Add column with calculation of total cropland area per county (for calculating diversity index)
CDL_2008_acres_final$totalcrop<-rowSums(CDL_2008_acres_final[,3:92])

#Simpson diversity analysis
CDL_2008_acres_simpdf<-CDL_2008_acres_final[,3:92]
CDL_2008_acres_simpson<- as.data.frame(diversity(CDL_2008_acres_simpdf, "simpson"))
colnames(CDL_2008_acres_simpson)<-c("simpson_diversity")
CDL_2008_acres_final$simpson_diversity<-CDL_2008_acres_simpson$simpson_diversity

#Write csv
#write.csv(CDL_2008_acres_final, CDL_mat_2008_acres, row.names=FALSE)
###################################################################################################################

###################################################################################
#CDL_2008_30mpixel
######################################
#Remove the subcategories already accounted for in the all categories (some in multiple categories so only removed in the first category listed)
CDL_2008_30mpixel_all<-CDL_2008_30mpixel %>% 
  select(-c(Category_001,Category_225,Category_226,Category_237,Category_241)) %>% #remove corn subcategories
  select(-c(Category_002,Category_232,Category_238)) %>% #remove cotton subcategories (no 239 in dataset so removed)
  select(-c(Category_004,Category_234,Category_236)) %>% #remove sorghum subcategories (no 235 in dataset so removed)
  select(-c(Category_005,Category_026,Category_240,Category_254)) %>% #remove soybeans subcategories
  select(-c(Category_021)) %>% #remove barley subcategories #no 233 so removed
  select(-c(Category_022,Category_230)) %>% #remove wheat durum subcategories
  select(-c(Category_024)) %>% #remove wheat winter subcategories
  select(-c(Category_028)) %>% #remove oat subcategories
  select(-c(Category_209,Category_231)) %>% #remove canteloupe subcategories
  select(-c(Category_227)) #remove letuce subcategories

#Crosswalk
CDL_2008_30mpixel_cols<-as.data.frame(colnames(CDL_2008_30mpixel_all))
CDL_2008_30mpixel_cols$mergecols<-colnames(CDL_2008_30mpixel_all)
CDL_2008_30mpixel_cols$colorder<-1:nrow(CDL_2008_30mpixel_cols)
CDL_2008_30mpixel_cols<-CDL_2008_30mpixel_cols[,-1]
CDL_2008_30mpixel_merge<-merge(CDL_2008_30mpixel_cols, crosswalk, by.x="mergecols", by.y="CSV_Names", all.x=TRUE) 
CDL_2008_30mpixel_merge<-CDL_2008_30mpixel_merge[order(CDL_2008_30mpixel_merge$colorder),]
CDL_2008_30mpixel_merge$Ag_Names[is.na(CDL_2008_30mpixel_merge$Ag_Names)] <- CDL_2008_30mpixel_merge$mergecols[is.na(CDL_2008_30mpixel_merge$Ag_Names)]
colnames(CDL_2008_30mpixel_all)<-CDL_2008_30mpixel_merge$Ag_Names

#Remove the non-ag rows
CDL_2008_30mpixel_final<-CDL_2008_30mpixel_all %>% 
  clean_names() %>% 
  select(-c(aquaculture, open_water, perennial_ice_snow, developed_open_space, developed_low_intensity,
            developed_med_intensity, developed_high_intensity, barren, deciduous_forest, evergreen_forest, 
            mixed_forest, shrubland, grassland_pasture, woody_wetlands, herbaceous_wetlands))

#Add column with calculation of total cropland area per county (for calculating diversity index)
CDL_2008_30mpixel_final$totalcrop<-rowSums(CDL_2008_30mpixel_final[,3:92])

#Simpson diversity analysis
CDL_2008_30mpixel_simpdf<-CDL_2008_30mpixel_final[,3:92]
CDL_2008_30mpixel_simpson<- as.data.frame(diversity(CDL_2008_30mpixel_simpdf, "simpson"))
colnames(CDL_2008_30mpixel_simpson)<-c("simpson_diversity")
CDL_2008_30mpixel_final$simpson_diversity<-CDL_2008_30mpixel_simpson$simpson_diversity

#Write csv
#write.csv(CDL_2008_30mpixel_final, CDL_mat_2008_30mpixel, row.names=FALSE)
###################################################################################################################

###################################################################################
#CDL_2012_acres
######################################
#Remove the subcategories already accounted for in the all categories (some in multiple categories so only removed in the first category listed)
CDL_2012_acres_all<-CDL_2012_acres %>% 
  select(-c(Category_001,Category_225,Category_226,Category_237,Category_241)) %>% #remove corn subcategories
  select(-c(Category_002,Category_232,Category_238, Category_239)) %>% #remove cotton subcategories 
  select(-c(Category_004,Category_234,Category_236, Category_235)) %>% #remove sorghum subcategories
  select(-c(Category_005,Category_026,Category_240,Category_254)) %>% #remove soybeans subcategories
  select(-c(Category_021)) %>% #remove barley subcategories #no 233 so removed
  select(-c(Category_022,Category_230)) %>% #remove wheat durum subcategories
  select(-c(Category_024)) %>% #remove wheat winter subcategories
  select(-c(Category_028)) %>% #remove oat subcategories
  select(-c(Category_209,Category_231)) %>% #remove canteloupe subcategories
  select(-c(Category_227)) #remove letuce subcategories

#Crosswalk
CDL_2012_acres_cols<-as.data.frame(colnames(CDL_2012_acres_all))
CDL_2012_acres_cols$mergecols<-colnames(CDL_2012_acres_all)
CDL_2012_acres_cols$colorder<-1:nrow(CDL_2012_acres_cols)
CDL_2012_acres_cols<-CDL_2012_acres_cols[,-1]
CDL_2012_acres_merge<-merge(CDL_2012_acres_cols, crosswalk, by.x="mergecols", by.y="CSV_Names", all.x=TRUE) 
CDL_2012_acres_merge<-CDL_2012_acres_merge[order(CDL_2012_acres_merge$colorder),]
CDL_2012_acres_merge$Ag_Names[is.na(CDL_2012_acres_merge$Ag_Names)] <- CDL_2012_acres_merge$mergecols[is.na(CDL_2012_acres_merge$Ag_Names)]
colnames(CDL_2012_acres_all)<-CDL_2012_acres_merge$Ag_Names

#Remove the non-ag rows
CDL_2012_acres_final<-CDL_2012_acres_all %>% 
  clean_names() %>% 
  select(-c(aquaculture, open_water, perennial_ice_snow, developed_open_space, developed_low_intensity,
            developed_med_intensity, developed_high_intensity, barren, deciduous_forest, evergreen_forest, 
            mixed_forest, shrubland, grassland_pasture, woody_wetlands, herbaceous_wetlands))

#Add column with calculation of total cropland area per county (for calculating diversity index)
CDL_2012_acres_final$totalcrop<-rowSums(CDL_2012_acres_final[,3:92])

#Simpson diversity analysis
CDL_2012_acres_simpdf<-CDL_2012_acres_final[,3:92]
CDL_2012_acres_simpson<- as.data.frame(diversity(CDL_2012_acres_simpdf, "simpson"))
colnames(CDL_2012_acres_simpson)<-c("simpson_diversity")
CDL_2012_acres_final$simpson_diversity<-CDL_2012_acres_simpson$simpson_diversity

#Write csv
#write.csv(CDL_2012_acres_final, CDL_mat_2012_acres, row.names=FALSE)
###################################################################################################################

###################################################################################
#CDL_2012_30mpixel
######################################
#Remove the subcategories already accounted for in the all categories (some in multiple categories so only removed in the first category listed)
CDL_2012_30mpixel_all<-CDL_2012_30mpixel %>% 
  select(-c(Category_001,Category_225,Category_226,Category_237,Category_241)) %>% #remove corn subcategories
  select(-c(Category_002,Category_232,Category_238, Category_239)) %>% #remove cotton subcategories 
  select(-c(Category_004,Category_234,Category_236, Category_235)) %>% #remove sorghum subcategories
  select(-c(Category_005,Category_026,Category_240,Category_254)) %>% #remove soybeans subcategories
  select(-c(Category_021)) %>% #remove barley subcategories #no 233 so removed
  select(-c(Category_022,Category_230)) %>% #remove wheat durum subcategories
  select(-c(Category_024)) %>% #remove wheat winter subcategories
  select(-c(Category_028)) %>% #remove oat subcategories
  select(-c(Category_209,Category_231)) %>% #remove canteloupe subcategories
  select(-c(Category_227)) #remove letuce subcategories

#Crosswalk
CDL_2012_30mpixel_cols<-as.data.frame(colnames(CDL_2012_30mpixel_all))
CDL_2012_30mpixel_cols$mergecols<-colnames(CDL_2012_30mpixel_all)
CDL_2012_30mpixel_cols$colorder<-1:nrow(CDL_2012_30mpixel_cols)
CDL_2012_30mpixel_cols<-CDL_2012_30mpixel_cols[,-1]
CDL_2012_30mpixel_merge<-merge(CDL_2012_30mpixel_cols, crosswalk, by.x="mergecols", by.y="CSV_Names", all.x=TRUE) 
CDL_2012_30mpixel_merge<-CDL_2012_30mpixel_merge[order(CDL_2012_30mpixel_merge$colorder),]
CDL_2012_30mpixel_merge$Ag_Names[is.na(CDL_2012_30mpixel_merge$Ag_Names)] <- CDL_2012_30mpixel_merge$mergecols[is.na(CDL_2012_30mpixel_merge$Ag_Names)]
colnames(CDL_2012_30mpixel_all)<-CDL_2012_30mpixel_merge$Ag_Names

#Remove the non-ag rows
CDL_2012_30mpixel_final<-CDL_2012_30mpixel_all %>% 
  clean_names() %>% 
  select(-c(aquaculture, open_water, perennial_ice_snow, developed_open_space, developed_low_intensity,
            developed_med_intensity, developed_high_intensity, barren, deciduous_forest, evergreen_forest, 
            mixed_forest, shrubland, grassland_pasture, woody_wetlands, herbaceous_wetlands))

#Add column with calculation of total cropland area per county (for calculating diversity index)
CDL_2012_30mpixel_final$totalcrop<-rowSums(CDL_2012_30mpixel_final[,3:92])

#Simpson diversity analysis
CDL_2012_30mpixel_simpdf<-CDL_2012_30mpixel_final[,3:92]
CDL_2012_30mpixel_simpson<- as.data.frame(diversity(CDL_2012_30mpixel_simpdf, "simpson"))
colnames(CDL_2012_30mpixel_simpson)<-c("simpson_diversity")
CDL_2012_30mpixel_final$simpson_diversity<-CDL_2012_30mpixel_simpson$simpson_diversity

#Write csv
#write.csv(CDL_2012_30mpixel_final, CDL_mat_2012_30mpixel, row.names=FALSE)
###################################################################################################################

###################################################################################
#CDL_2016_acres
######################################
#Remove the subcategories already accounted for in the all categories (some in multiple categories so only removed in the first category listed)
CDL_2016_acres_all<-CDL_2016_acres %>% 
  select(-c(Category_001,Category_225,Category_226,Category_237,Category_241)) %>% #remove corn subcategories
  select(-c(Category_002,Category_232,Category_238)) %>% #remove cotton subcategories (no 239 in dataset so removed)
  select(-c(Category_004,Category_236)) %>% #remove sorghum subcategories (no Category_234 or no 235 in dataset so removed)
  select(-c(Category_005,Category_026,Category_240,Category_254)) %>% #remove soybeans subcategories
  select(-c(Category_021)) %>% #remove barley subcategories #no 233 so removed
  select(-c(Category_022)) %>% #remove wheat durum subcategories # no Category_230
  select(-c(Category_024)) %>% #remove wheat winter subcategories
  select(-c(Category_028)) %>% #remove oat subcategories
  select(-c(Category_209)) %>% #remove canteloupe subcategories # no Category_231
  select(-c(Category_227)) #remove letuce subcategories

#Crosswalk
CDL_2016_acres_cols<-as.data.frame(colnames(CDL_2016_acres_all))
CDL_2016_acres_cols$mergecols<-colnames(CDL_2016_acres_all)
CDL_2016_acres_cols$colorder<-1:nrow(CDL_2016_acres_cols)
CDL_2016_acres_cols<-CDL_2016_acres_cols[,-1]
CDL_2016_acres_merge<-merge(CDL_2016_acres_cols, crosswalk, by.x="mergecols", by.y="CSV_Names", all.x=TRUE) 
CDL_2016_acres_merge<-CDL_2016_acres_merge[order(CDL_2016_acres_merge$colorder),]
CDL_2016_acres_merge$Ag_Names[is.na(CDL_2016_acres_merge$Ag_Names)] <- CDL_2016_acres_merge$mergecols[is.na(CDL_2016_acres_merge$Ag_Names)]
colnames(CDL_2016_acres_all)<-CDL_2016_acres_merge$Ag_Names

#Remove the non-ag rows
CDL_2016_acres_final<-CDL_2016_acres_all %>% 
  clean_names() %>% 
  select(-c(aquaculture, open_water, perennial_ice_snow, developed_open_space, developed_low_intensity,
            developed_med_intensity, developed_high_intensity, barren, deciduous_forest, evergreen_forest, 
            mixed_forest, shrubland, grassland_pasture, woody_wetlands, herbaceous_wetlands))

#Add column with calculation of total cropland area per county (for calculating diversity index)
CDL_2016_acres_final$totalcrop<-rowSums(CDL_2016_acres_final[,3:92])

#Simpson diversity analysis
CDL_2016_acres_simpdf<-CDL_2016_acres_final[,3:92]
CDL_2016_acres_simpson<- as.data.frame(diversity(CDL_2016_acres_simpdf, "simpson"))
colnames(CDL_2016_acres_simpson)<-c("simpson_diversity")
CDL_2016_acres_final$simpson_diversity<-CDL_2016_acres_simpson$simpson_diversity

#Write csv
#write.csv(CDL_2016_acres_final, CDL_mat_2016_acres, row.names=FALSE)
###################################################################################################################

###################################################################################
#CDL_2016_30mpixel
######################################
#Remove the subcategories already accounted for in the all categories (some in multiple categories so only removed in the first category listed)
CDL_2016_30mpixel_all<-CDL_2016_30mpixel %>% 
  select(-c(Category_001,Category_225,Category_226,Category_237,Category_241)) %>% #remove corn subcategories
  select(-c(Category_002,Category_232,Category_238)) %>% #remove cotton subcategories (no 239 in dataset so removed)
  select(-c(Category_004,Category_236)) %>% #remove sorghum subcategories (no Category_234 or no 235 in dataset so removed)
  select(-c(Category_005,Category_026,Category_240,Category_254)) %>% #remove soybeans subcategories
  select(-c(Category_021)) %>% #remove barley subcategories #no 233 so removed
  select(-c(Category_022)) %>% #remove wheat durum subcategories # no Category_230
  select(-c(Category_024)) %>% #remove wheat winter subcategories
  select(-c(Category_028)) %>% #remove oat subcategories
  select(-c(Category_209)) %>% #remove canteloupe subcategories # no Category_231
  select(-c(Category_227)) #remove letuce subcategories

#Crosswalk
CDL_2016_30mpixel_cols<-as.data.frame(colnames(CDL_2016_30mpixel_all))
CDL_2016_30mpixel_cols$mergecols<-colnames(CDL_2016_30mpixel_all)
CDL_2016_30mpixel_cols$colorder<-1:nrow(CDL_2016_30mpixel_cols)
CDL_2016_30mpixel_cols<-CDL_2016_30mpixel_cols[,-1]
CDL_2016_30mpixel_merge<-merge(CDL_2016_30mpixel_cols, crosswalk, by.x="mergecols", by.y="CSV_Names", all.x=TRUE) 
CDL_2016_30mpixel_merge<-CDL_2016_30mpixel_merge[order(CDL_2016_30mpixel_merge$colorder),]
CDL_2016_30mpixel_merge$Ag_Names[is.na(CDL_2016_30mpixel_merge$Ag_Names)] <- CDL_2016_30mpixel_merge$mergecols[is.na(CDL_2016_30mpixel_merge$Ag_Names)]
colnames(CDL_2016_30mpixel_all)<-CDL_2016_30mpixel_merge$Ag_Names

#Remove the non-ag rows
CDL_2016_30mpixel_final<-CDL_2016_30mpixel_all %>% 
  clean_names() %>% 
  select(-c(aquaculture, open_water, perennial_ice_snow, developed_open_space, developed_low_intensity,
            developed_med_intensity, developed_high_intensity, barren, deciduous_forest, evergreen_forest, 
            mixed_forest, shrubland, grassland_pasture, woody_wetlands, herbaceous_wetlands))

#Add column with calculation of total cropland area per county (for calculating diversity index)
CDL_2016_30mpixel_final$totalcrop<-rowSums(CDL_2016_30mpixel_final[,3:92])

#Simpson diversity analysis
CDL_2016_30mpixel_simpdf<-CDL_2016_30mpixel_final[,3:92]
CDL_2016_30mpixel_simpson<- as.data.frame(diversity(CDL_2016_30mpixel_simpdf, "simpson"))
colnames(CDL_2016_30mpixel_simpson)<-c("simpson_diversity")
CDL_2016_30mpixel_final$simpson_diversity<-CDL_2016_30mpixel_simpson$simpson_diversity

#Write csv
#write.csv(CDL_2016_30mpixel_final, CDL_mat_2016_30mpixel, row.names=FALSE)
###################################################################################################################



####################################################################
#Double categories (acres/meters squared) as fraction of the all categories that they each are in
#(calculate and report these to determine how big an issue the double counting in total crop is)
####################################################################
#Bring in csvs for the years 2008, 2012, 2016 (2002 not available and 2007 incomplete), for both meters and acres (just recopied over)
CDL_2008_acres<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Acres_2008.csv")
CDL_2008_30mpixel<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Pixel_2008_30m.csv")
CDL_2012_acres<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Acres_2012.csv")
CDL_2012_30mpixel<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Pixel_2012_30m.csv")
CDL_2016_acres<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Acres_2016.csv")
CDL_2016_30mpixel<-read_csv("C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/County_Pixel_Count/CDL_Cnty_Pixel_2016_30m.csv")


#Double categories as percent of acres and pixels of their all categories
#Categories to investigate: 026, 225, 226, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 254

#Just reporting acres (similar measurement to pixels) and for 2008 (should be similar across years)
#Double crops All 2008 Acres
#Ignore all the non-double categories(defined in word document but the first number for each all category)
# Corn_all         = Category_001 + Category_225 + Category_226 + Category_237 + Category_241;
corn<-(CDL_2008_acres$Category_225+CDL_2008_acres$Category_226+
         CDL_2008_acres$Category_237+CDL_2008_acres$Category_241)/
  (CDL_2008_acres$Corn_all)
# Cotton_all       = Category_002 + Category_232 + Category_238 + Category_239;
cotton<-(CDL_2008_acres$Category_232+CDL_2008_acres$Category_238)/
  (CDL_2008_acres$Cotton_all) #no 239
# Sorghum_all      = Category_004 + Category_234 + Category_235 + Category_236;
sorghum<-(CDL_2008_acres$Category_234+CDL_2008_acres$Category_236)/
  (CDL_2008_acres$Sorghum_all) #no 235
# Soybeans_all     = Category_005 + Category_026 + Category_239 + Category_240 + Category_241 + Category_254;
soybeans<-(CDL_2008_acres$Category_026+CDL_2008_acres$Category_240+
             CDL_2008_acres$Category_241+CDL_2008_acres$Category_254)/
  (CDL_2008_acres$Soybeans_all) #no 239
# Barley_all       = Category_021 + Category_233 + Category_235 + Category_237 + Category_254;
barley<-(CDL_2008_acres$Category_237+CDL_2008_acres$Category_254)/
  (CDL_2008_acres$Barley_all)# no 233 or 235
# Wheat_Durum_all  = Category_022 + Category_230 + Category_234;
wheatdurum<-(CDL_2008_acres$Category_230+CDL_2008_acres$Category_234)/
  (CDL_2008_acres$Wheat_Durum_all)
# Wheat_Winter_all = Category_024 + Category_026 + Category_225 + Category_236 + Category_238;
wheatwinter<-(CDL_2008_acres$Category_026+CDL_2008_acres$Category_225+
                CDL_2008_acres$Category_236+CDL_2008_acres$Category_238)/
  (CDL_2008_acres$Wheat_Winter_all)
# Oats_all         = Category_028 + Category_226 + Category_240;
oats<-(CDL_2008_acres$Category_226+CDL_2008_acres$Category_240)/
  (CDL_2008_acres$Oats_all)
# Canteloupe_all   = Category_209 + Category_231;
canteloupe<-(CDL_2008_acres$Category_231)/
  (CDL_2008_acres$Canteloupe_all)
# Lettuce_all      = Category_227 + Category_230 + Category_231 + Category_232 + Category_233
lettuce<-(CDL_2008_acres$Category_230+CDL_2008_acres$Category_231+
            CDL_2008_acres$Category_232)/
  (CDL_2008_acres$Lettuce_all)# no 233
#The no 233, 235, and 239 are for 2008 and 2012 (?), but not 2016

doublecrops<-CDL_2008_acres %>% 
  select(StPostal, Fips)

dc<-cbind(doublecrops, corn, cotton, sorghum, soybeans, barley, wheatdurum, wheatwinter, oats, canteloupe, lettuce)
dc_wide_save<-"C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/DoubleCrops/DoubleCrops_Percof_AllCrops_Wide.csv"
#write.csv(dc, dc_wide_save, row.names=FALSE)

library(reshape2)
# Specify id.vars: the variables to keep but not split apart on
dc_long<-melt(dc, id.vars=c("StPostal", "Fips"))
dc_long_save<-"C:/Users/shemc/Documents/UCSB/NLCD/Data/CDL/DoubleCrops/DoubleCrops_Percof_AllCrops_Long.csv"
#write.csv(dc_long, dc_long_save, row.names=FALSE)

#Out of 3109 counties
#Scatterplot of results
sp<-ggplot(dc_long, aes(x=Fips, y=value)) +
  geom_point(aes(color = value), alpha = 0.9)+
  theme_light() +
  facet_wrap(~variable)


#CDL Data

#Read in all the csvs of CDL data and combine
CDL_path<-"Z:/Sofie/Data/CDL/CDL_Diversity/"
#Reading in each of three csvs individually to ensure the correct year is read in and cleaning up so just fips and SDI (simpson diversity index)
CDL_2007<-read_csv(paste0(CDL_path, "CDL_2008_acres.csv")) %>% 
  mutate(Year="2007") %>% #Serving as 2007 data
  select(fips, Year, simpson_diversity)
CDL_2012<-read_csv(paste0(CDL_path, "CDL_2012_acres.csv")) %>% 
  mutate(Year="2012") %>% 
  select(fips, Year, simpson_diversity)
CDL_2017<-read_csv(paste0(CDL_path, "CDL_2016_acres.csv")) %>% 
  mutate(Year="2017") %>% #Serving as 2017 data
  select(fips, Year, simpson_diversity)
CDL<-list(CDL_2007, CDL_2012, CDL_2017) %>% 
  reduce(full_join, by=c("fips", "Year", "simpson_diversity")) %>% 
  mutate(FIPS=as.numeric(fips)) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  mutate(SDI=simpson_diversity) %>% 
  select(FIPS, Year, SDI) %>% 
  arrange(FIPS)
