#This R Script gathers and formats the USDA NASS Cropland Data Layer data for regression analyses.
  #Data source: Enter more
  #Give more background
  #This script will examine the CDL County_Pixel_Count csvs and calculate the diversity of crops in each county
  #Data downloaded in zip folder from the following source: https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php
  #Read me file in Data/CDL/County_Pixel_Count provided further informaiton on grouped columns and pixel counts
  #Crosswalk file found here: https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php#Section1_6.0 #download attribute table of stats
  #Also investigated 9 missing VA counties in 2016 acres (but not pixels) data, as well as double crop issue (scatterplot for how big of an issue),
  #But neither of those included in this script because not necessary for final data gathering/processing
  #Also orginally also performed analyses for 30m pixel for each of the three years as well, but not including here as decided to use
  #acres in the final analysis (very similar process per year between acres and pixel)

#Group categories as defined in readme file
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

#Load Packages
    #Ensure necessary packages are installed and loaded
    library(tidyverse) #Datatable manipulation
    library(janitor) #Clean colnames
    library(vegan) #Perform diversity calculation
    options(scipen=999) #No scientific notation

#Read in data
    #Read in crosswalk between category type name and category number name
    crosswalk<-readr::read_csv("Data/DataProcessing/CDL/CDL_crosswalk.csv")

    #Read in csvs for the years 2008, 2012, 2016 (1997 and 2002 not available and 2007 incomplete), in acres
    CDL_2008_acres<-readr::read_csv("Data/DataProcessing/CDL/County_Pixel_Count/CDL_Cnty_Acres_2008.csv")
    CDL_2012_acres<-readr::read_csv("Data/DataProcessing/CDL/County_Pixel_Count/CDL_Cnty_Acres_2012.csv")
    CDL_2016_acres<-readr::read_csv("Data/DataProcessing/CDL/County_Pixel_Count/CDL_Cnty_Acres_2016.csv")

#Cleanup CDL datasets by year
    #Remove the subcategories already accounted for in the all categories (some in multiple categories so only removed in the first category listed)
    CDL_2008_acres_all<-CDL_2008_acres %>% 
      dplyr::select(-c(Category_001,Category_225,Category_226,Category_237,Category_241)) %>% #remove corn subcategories
      dplyr::select(-c(Category_002,Category_232,Category_238)) %>% #remove cotton subcategories (no 239 in dataset so removed)
      dplyr::select(-c(Category_004,Category_234,Category_236)) %>% #remove sorghum subcategories (no 235 in dataset so removed)
      dplyr::select(-c(Category_005,Category_026,Category_240,Category_254)) %>% #remove soybeans subcategories
      dplyr::select(-c(Category_021)) %>% #remove barley subcategories #no 233 so removed
      dplyr::select(-c(Category_022,Category_230)) %>% #remove wheat durum subcategories
      dplyr::select(-c(Category_024)) %>% #remove wheat winter subcategories
      dplyr::select(-c(Category_028)) %>% #remove oat subcategories
      dplyr::select(-c(Category_209,Category_231)) %>% #remove canteloupe subcategories
      dplyr::select(-c(Category_227)) #remove letuce subcategories
    
    CDL_2012_acres_all<-CDL_2012_acres %>% 
      dplyr::select(-c(Category_001,Category_225,Category_226,Category_237,Category_241)) %>% #remove corn subcategories
      dplyr::select(-c(Category_002,Category_232,Category_238, Category_239)) %>% #remove cotton subcategories 
      dplyr::select(-c(Category_004,Category_234,Category_236, Category_235)) %>% #remove sorghum subcategories
      dplyr::select(-c(Category_005,Category_026,Category_240,Category_254)) %>% #remove soybeans subcategories
      dplyr::select(-c(Category_021)) %>% #remove barley subcategories #no 233 so removed
      dplyr::select(-c(Category_022,Category_230)) %>% #remove wheat durum subcategories
      dplyr::select(-c(Category_024)) %>% #remove wheat winter subcategories
      dplyr::select(-c(Category_028)) %>% #remove oat subcategories
      dplyr::select(-c(Category_209,Category_231)) %>% #remove canteloupe subcategories
      dplyr::select(-c(Category_227)) #remove letuce subcategories
    
    CDL_2016_acres_all<-CDL_2016_acres %>% 
      dplyr::select(-c(Category_001,Category_225,Category_226,Category_237,Category_241)) %>% #remove corn subcategories
      dplyr::select(-c(Category_002,Category_232,Category_238)) %>% #remove cotton subcategories (no 239 in dataset so removed)
      dplyr::select(-c(Category_004,Category_236)) %>% #remove sorghum subcategories (no Category_234 or no 235 in dataset so removed)
      dplyr::select(-c(Category_005,Category_026,Category_240,Category_254)) %>% #remove soybeans subcategories
      dplyr::select(-c(Category_021)) %>% #remove barley subcategories #no 233 so removed
      dplyr::select(-c(Category_022)) %>% #remove wheat durum subcategories # no Category_230
      dplyr::select(-c(Category_024)) %>% #remove wheat winter subcategories
      dplyr::select(-c(Category_028)) %>% #remove oat subcategories
      dplyr::select(-c(Category_209)) %>% #remove canteloupe subcategories # no Category_231
      dplyr::select(-c(Category_227)) #remove letuce subcategories
    
#Clean dataframe column names by applying crosswalk function
    #Create list of three dataframes in order
    cdl_list<-list(CDL_2008=CDL_2008_acres_all, CDL_2012=CDL_2012_acres_all, CDL_2016=CDL_2016_acres_all)
    
    #Create crosswalk function and apply to list to rename dataframe columns
    crosswalk_function<-function(c){
      #Performing the Crosswalk to rename dataframe columns
      c_cols<-as.data.frame(colnames(c)) #Create dataframe of the data columnnames
      c_cols$mergecols<-colnames(c) #Create column mergecols
      c_cols$colorder<-1:nrow(c_cols) #Add column with the nrow number (to sort by and apply later)
      c_cols<-c_cols[,-1] #Remove first column (not needed by used to establish as character instead of factor)
      c_merge<-merge(c_cols, crosswalk, by.x="mergecols", by.y="CSV_Names", all.x=TRUE) %>% #Use crosswalk to get names for each category number
        dplyr::arrange(colorder) #Arrange by column order
      c_merge$Ag_Names[is.na(c_merge$Ag_Names)] <- c_merge$mergecols[is.na(c_merge$Ag_Names)] #Replace ag name NA values with the mergecols value 
      colnames(c)<-c_merge$Ag_Names #Set crosswalked names as new dataset column names
      return(c) #Return dataframes with new cross-walked column-names
    }
    
    #Apply the crosswalk_function to all dataframes in list
    cdl_listed<-lapply(cdl_list, crosswalk_function)
    
#Calculate simpson diversity for crops
    #Create function to calculate simpson crop diversity on only ag rows
    calculate_diversity<-function(d){
      #Remove the non-ag rows
      d_final<-as.data.frame(d) %>% 
        janitor::clean_names() %>% #Clean column names with janitor package
        dplyr::select(-c(aquaculture, open_water, perennial_ice_snow, developed_open_space, developed_low_intensity,
                  developed_med_intensity, developed_high_intensity, barren, deciduous_forest, evergreen_forest, 
                  mixed_forest, shrubland, grassland_pasture, woody_wetlands, herbaceous_wetlands))
      #Add column with calculation of total cropland area per county
      d_final$totalcrop<-rowSums(d_final[,3:92])
      #Simpson diversity analysis
      d_simpdf<-d_final[,3:92] #Select needed columns for analysis
      d_simpson<- as.data.frame(vegan::diversity(d_simpdf, "simpson")) #Perform simpson diversity analysis
      colnames(d_simpson)<-c("simpson_diversity") #Give results column name
      d_final$simpson_diversity<-d_simpson$simpson_diversity #Add results into dataframe as new column
      return(d_final) #Return dataframes
    }
  
    #Apply the calculate_diversity to all dataframes in list
    cdl_df<-lapply(cdl_listed, calculate_diversity)
  
#Combine three dataframes into one
    #Create dataframe for each one in order to add year columns first (Order of dataframes is 2008, 2012, 2016)
    cdl_2008<-cdl_df[[1]]
    cdl_2012<-cdl_df[[2]]
    cdl_2016<-cdl_df[[3]]

    #Add year column to each of the dataframes (set 2008 as 2007, 2012 as 2012, and 2016 as 2017)
    cdl_2008$Year<-"2007"
    cdl_2012$Year<-"2012"
    cdl_2016$Year<-"2017"

    #Select only fips, Year, and simpson_diversity metrics from each dataframe (some years have different columns)
    cdl2007<-cdl_2008[, c("fips", "Year", "simpson_diversity")]
    cdl2012<-cdl_2012[, c("fips", "Year", "simpson_diversity")]
    cdl2017<-cdl_2016[, c("fips", "Year", "simpson_diversity")]

    #Combine all the dataframes together and format
    cdl<-rbind(cdl2007,cdl2012,cdl2017) %>% 
      dplyr::mutate(FIPS=as.numeric(fips),
           Year=as.numeric(Year),
           SDI=simpson_diversity) %>% 
      dplyr::select(FIPS, Year, SDI) %>% 
      dplyr::arrange(FIPS)

#Write final Cropland Data Layer Dataframe to be used in combination with other dataframes
    write_csv(cdl, "Data/DataProcessing/df/cdl.csv")
      