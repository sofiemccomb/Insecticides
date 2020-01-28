#This R script gathers and formats all the Census of Agriculture data (COA) needed for the regression analyses. 
#Data source: USDA National Agricultural Statistics Service (NASS) Quickstats:https://quickstats.nass.usda.gov/
#Downloaded data can be found under the Data/DataProcessing/CoA folder, which also shows the agriculture properties of interest..
#The metadata.xlsx shows snipped images of how the individual csvs were downloaded from Quickstats. 
#Data was gathered for 1997, 2002, 2007, 2012, and 2017, which are the years of interest, 
#and for the agricultural properties thought to be highly related to insectide use, based on previous literature.

#Load Packages
#Ensure necessary packages are installed and loaded
library(tidyverse)
library(tools)
library(zoo)
library(purrr)
options(scipen=999) #no scientific notation (all)

#Load in all csv CoA files
#List all file names
coa_names_csv<-tools::file_path_sans_ext(list.files(path="Data/DataProcessing/CoA_1987_1992", pattern=".csv", full.names=FALSE))
#Load in all csvs and give the datasets the name of the original csv file
for (i in coa_names_csv){
  assign(i, readr::read_csv(paste0("Data/DataProcessing/CoA_1987_1992/", i, ".csv", sep="")))
}

#Combine all CoA files in listed dataframe format
colnames<-c("X1", "DataItem", "County", "State", "Year", "Value", "StateFips", "CountyCode", "FIPS")

#Merge all dataframes into list, giving df list names
coa_df_list<-list(countyarea=ApproxLandArea8797_0215, harvcrop=TotalHarvCrop8792_0215, 
                  insecticides=Insect8792_0215, barley=Barley8792_0215, oat=Oats8792_0215,
                  wheat=Wheat8792_0215, soybeans=Soy8792_0215, corngrain=CornGr8792_0215,
                  cornsill=CornSil8792_0215, fruitnut=Orchard8792_0215,veg=VegHarv8792_0215)

#Still need cropnotharv, cropfail, farms_500, farms_1000

#Apply the column names defined earlier to all dataframes in list
coa_df_listed<-lapply(coa_df_list, setNames, colnames)


#Clean the dataframes
#Create function to clean all the data in the same format
clean_function<-function(w){
  w<-w[,c(9,5,2,6)] #Select desired columns (FIPS, Year, DataItem, Value)
  w$Value[w$Value==-9999]<-NA #Setting rows where value is not a number to NA
  w<-w %>% filter(Year==1987|Year==1992)
  w$FIPS<-as.numeric(w$FIPS) #Ensure FIPS code is numeric
  w<-w[!is.na(w$FIPS),] #Remove rows where FIPS code is NA (just to double check)
  return(w) #Return the cleaned data frame
}

#Apply function to list of dataframes
coa_list<-lapply(coa_df_listed,clean_function)


#Rename value columns 
#Rename value columns of each dataframe (column 4) to shorthand name of dataset so dataframes easy to cbind (long but clear way to do so)
colnames(coa_list$countyarea)[4]<-"countyarea"
colnames(coa_list$harvcrop)[4]<-"harvcrop"
colnames(coa_list$insecticides)[4]<-"insecticides"
colnames(coa_list$barley)[4]<-"barley"
colnames(coa_list$oat)[4]<-"oat"
colnames(coa_list$wheat)[4]<-"wheat"
colnames(coa_list$soybeans)[4]<-"soybeans"
colnames(coa_list$corngrain)[4]<-"corngrain"
colnames(coa_list$cornsill)[4]<-"cornsill"
colnames(coa_list$fruitnut)[4]<-"fruitnut"
colnames(coa_list$veg)[4]<-"veg"

#Select final desired columns to be cbinded
#create function to select desired columns from each dataframe
select_function<-function(p){
  p<-p[,c(1,2,4)] #select desired columns
  return(p) #return dataframes
}

#Apply select_function to list of dataframes
final_coa_list<-lapply(coa_list,select_function)

#Unlist dataframes
#Unlist dataframes and assign names from listed dataframes before they are merged
listnames<-names(final_coa_list)
for (l in seq(final_coa_list)){
  dfname<-listnames[l]
  assign(dfname, final_coa_list[[l]])
}

#Merge dataframes   
#List dataframes and reduce by full_join on FIPS and Year (combination gives unique rows)
mergelist<-list(countyarea, harvcrop, insecticides, corngrain, cornsill, oat, barley, wheat, soybeans, fruitnut, veg) %>% 
  purrr::reduce(full_join, by=c("FIPS", "Year"))

#Use 2017 county area to fill all other year county area so standardized per FIPS
coa<-mergelist %>%
  dplyr::group_by(FIPS) %>%
  arrange(desc(Year)) %>%
  dplyr::mutate(countyarea=countyarea[1]) %>%
  arrange(FIPS) %>%
  dplyr::ungroup()

# #Fix dataset issues (old way)
# #Fill in missing county area data for 1987 with 1992 values (assume same) 
# coa<-mergelist %>% #Arrange data so that 1997 and 2002 are next to each other for each FIPS
#   dplyr::select(FIPS, everything()) %>% 
#   dplyr::arrange(FIPS)
# #Fill in 1987 rows with 1992 values using na.locf function (Generic function for replacing each NA with the most recent non-NA prior to it)
# coa$countyarea<-zoo::na.locf(coa$countyarea) 

#################################
#Read in excel files and clean-up before combining with the csv files
library(readxl)
library(tidyverse)
library(janitor)

#Load in all xlsx CoA files
#List all file names
coa_names_xlsx<-tools::file_path_sans_ext(list.files(path="Data/DataProcessing/CoA_1987_1992", pattern=".xlsx", full.names=FALSE))
#Load in all csvs and give the datasets the name of the original csv file
for (i in coa_names_xlsx){
  assign(i, readxl::read_excel(paste0("Data/DataProcessing/CoA_1987_1992/", i, ".xlsx", sep="")))
}

#Get FIPS codes to add in FIPS column
FIPS_df<-as.data.frame(HavAcres500_999) %>% 
  janitor::clean_names() %>% 
  select(fips_code)

#Clean function to clean-up wanted dataframes into same format  
  clean_function_xlsx<-function(z){
      w<-as.data.frame(z[,1:5]) %>% 
        dplyr::mutate(FIPS=FIPS_df$fips_code)
      colnames(w)<-c("Geography", "Item","y1997", "y1992", "y1987", "FIPS")
      w<-w %>% gather(year, Value, y1997:y1987)
      w$Year<-ifelse(w$year=="y1997", 1997, 
                         ifelse(w$year=="y1992", 1992,
                                    ifelse(w$year=="y1987", 1987, NA)))
      w<-w[,c(3,6,5)] %>% filter(Year==1987|Year==1992) #Remove 1997 data
      w$Value[w$Value=="N"|w$Value=="D"]<-NA #Setting rows where value is not a number to NA
      w$Value <- as.numeric(gsub(",","",w$Value)) #For rows where the value has a comma (income), removing the comma
      w$FIPS<-ifelse(substring(w$FIPS,3,5)=="000", "Remove", 
                     ifelse(substring(w$FIPS,3,5)=="998", "Remove",w$FIPS))#Remove state and US and other counties
      w<- w %>% filter(FIPS!="Remove") %>% #remove state, us, other counties
        mutate(FIPS=as.numeric(FIPS))#numeric fips
      w<-w[!is.na(w$FIPS),] %>%  #Remove rows where FIPS code is NA (just to double check)
        arrange(FIPS)#arrange by FIPS
      return(w) #Return the cleaned data frame
  }
  
#Apply function to df and rename value column 
largefarms_500<-clean_function_xlsx(HavAcres500_999) %>% 
  rename(LargeFarms500=Value)
largefarms_1000<-clean_function_xlsx(HavAcres1000plus) %>% 
  rename(LargeFarms1000=Value)
cropfail<-clean_function_xlsx(OtherCroplandFail_0120) %>% 
  rename(cropfail=Value)
cropnotharv<-clean_function_xlsx(OtherCroplandTotal_0120) %>% 
  rename(cropnotharv=Value)

#Merge dataframes   
#List dataframes and reduce by full_join on FIPS and Year (combination gives unique rows)
coa2<-list(largefarms_500, largefarms_1000, cropfail, cropnotharv) %>% 
  purrr::reduce(full_join, by=c("FIPS", "Year"))

###########################################
#Merge coa and coa2 (csv and xlsx data)
coa_merge<-list(coa, coa2) %>% 
  purrr::reduce(full_join, by=c("FIPS", "Year"))

#Need to get rid of NA so that can do math
#Remove rows where harvcrop is 0 or NA (not useful measurements)
#Make rest of NA 0 (so can add across crops wtihout getting NA, etc.-other measurements not influencing as much)
coa_merge_noNA<-coa_merge %>% 
  filter(!is.na(harvcrop),
         !is.na(insecticides)) #removed about 200
coa_merge_noNA[is.na(coa_merge_noNA)] <- 0
#Influencing results too much if harvested crop or insecticides are 0 when they are not actually measured
#Other ones should not be as big of an influence- insecticides used even when only NLCD and need harvested as based of total planted also in NLCD (crop fail is small-0 less important)
#Need 0s for the rest otherwise cannot add otgether the columns-would ignore a lot of data  

#Create combined columns desired for analysis and remove unnecessary base columns
coa_df<-coa_merge_noNA %>%
  dplyr::mutate(totalplanted=harvcrop+cropfail,
                totalag=harvcrop+cropnotharv,
                largefarms=LargeFarms500+LargeFarms1000,
                soy_smallgrains=soybeans+oat+wheat+barley,
                corn=corngrain+cornsill,
                fruitveg=fruitnut+veg) %>% 
  dplyr::select(-c(LargeFarms500, LargeFarms1000,
                   soybeans, oat,wheat,barley,corngrain,cornsill,fruitnut,veg))


#Calculate needed ratios for regressions
final_coa<-coa_df %>%
  dplyr::arrange(FIPS) %>% 
  dplyr::mutate(insect_planted=insecticides/totalplanted,
                insect_ag=insecticides/totalag,
                harv_county=harvcrop/countyarea,
                largefarm_planted=largefarms/totalplanted,
                largefarm_ag=largefarms/totalag,
                soysmallgrain_planted=soy_smallgrains/totalplanted,
                corn_planted=corn/totalplanted,
                fruitveg_planted=fruitveg/totalplanted) %>% 
  dplyr::mutate(planted_county=totalplanted/countyarea,
                insect_harv=insecticides/harvcrop,
                largefarm_harv=largefarms/harvcrop,
                soysmallgrain_harv=soy_smallgrains/harvcrop,
                corn_harv=corn/harvcrop,
                fruitveg_harv=fruitveg/harvcrop)

#Drop counties with changed official county borders, etc. issues (same as Larsen 2015 paper)
finalcoa<-final_coa %>%
  filter(!(FIPS>=51200 & FIPS<=52000)) %>% #VA counties
  filter(!(FIPS>=2000 & FIPS<3000)) %>% #AK
  filter(!(FIPS>=15000 & FIPS<16000)) %>% #HI
  filter(!(FIPS==08001)&!(FIPS==08013)&!(FIPS==08059)&!(FIPS==08014)&!(FIPS==08123)&
           !(FIPS==30113)&!(FIPS==30031)&!(FIPS==30067)&!(FIPS==08031)&
           !(FIPS==24031)&!(FIPS==24033)&!(FIPS==37031)&!(FIPS==37049)&
           !(FIPS==12045)&!(FIPS==12037)&!(FIPS==11001)&!(FIPS==24510)&
           !(FIPS==29510)&!(FIPS==24510)&!(FIPS==12025)&!(FIPS==12086)) #Counties with changing borders

#Write final Census of Ag Dataframe to be used in combination with other dataframes
write_csv(finalcoa, "Data/DataProcessing/df/coa_8792.csv")

