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

#Load in all CoA files
    #List all file names
    coa_names<-tools::file_path_sans_ext(list.files(path="Data/DataProcessing/CoA", pattern=".csv", full.names=FALSE))
    #Load in all csvs and give the datasets the name of the original csv file
    for (i in coa_names){
      assign(i, readr::read_csv(paste0("Data/DataProcessing/CoA/", i, ".csv", sep="")))
    }

#Combine all CoA files in listed dataframe format
    #Establish column names to be applied to dataframes
    colnames <- c("Program", "Year","Period","Week_Ending","Geo_Level","State", "State_ANSI","Ag_District",
                  "Ag_District_Code","County","County_ANSI","Zip_Code","Region","watershed_code","Watershed","Commodity",
                  "Data_Item","Domain","Domain_Category","Value","CV") 
    
    #Combine all large farms datasets with rbind (were too big to download together) and only keep large farm rows (500+ and 1000+)
    farms<-rbind(largefarms_2017, largefarms_2012, largefarms_2007, largefarms_2002, largefarms_1997)
    colnames(farms)<-colnames
    farms_500<-farms[farms$Domain_Category=="AREA HARVESTED: (500 TO 999 ACRES)",]
    farms_1000<-farms[farms$Domain_Category=="AREA HARVESTED: (1,000 OR MORE ACRES)",]
    
    #Merge all dataframes into list, giving df list names
    coa_df_list<-list(countyarea=countyarea, harvcrop=harvestedcropland, cropnotharv=cropnotharv,  
                    cropfail=croplandfailed,farms_500=farms_500, farms_1000=farms_1000,income=netcashincome, 
                    fertilizer=aglandfertilizer, insecticides=aglandinsecticides, irrigate=cropharvirrigated,  
                    barley=barleyharvested,  oat=oatsharvested, wheat=wheatharvested, soybeans=soybeansharvested, 
                    corngrain=corngrainharvested, cornsill=cornsillageharvested, fruitnut=fruitnutorchardharvested, 
                    veg=vegetablesharvested)
  
    #Apply the column names defined earlier to all dataframes in list
    coa_df_listed<-lapply(coa_df_list, setNames, colnames)


#Clean the dataframes
    #Create function to clean all the data in the same format
    clean_function<-function(w){
      w<-w[,c(2,7,11,17,19,20)] #Select desired columns (Year, State_ANSI, county_ANSI, Data_Item, Domain_category, Value)
      w<-w[!is.na(w$County_ANSI),] #Remove rows with NA values for county number
      w$FIPS<-paste0(w$State_ANSI,w$County_ANSI) #Create column with county FIPS code, which is state and county ANSI pasted together
      w<-w[,c(1,4,5,6,7)] #Remove state ansi and county ansi columns by selecting the other columns
      w$Value[w$Value=="(D)"|w$Value==" (D)"|w$Value==" (Z)"|w$Value=="(Z)"]<-NA #Setting rows where value is not a number to NA
      w$Value <- as.numeric(gsub(",","",w$Value)) #For rows where the value has a comma (income), removing the comma
      w<-w[w$Domain_Category!="FERTILIZER: (MANURE)" & w$Domain_Category!="FERTILIZER: (ORGANIC)",] #Keep total fertilizer rows, but remove manure and organic
      w<-w[w$Domain_Category!="CHEMICAL, INSECTICIDE: (NEMATICIDES)",] #Keep only insecticides, not nematicides
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
    colnames(coa_list$cropnotharv)[4]<-"cropnotharv"
    colnames(coa_list$cropfail)[4]<-"cropfail"
    colnames(coa_list$farms_500)[4]<-"farms_500"
    colnames(coa_list$farms_1000)[4]<-"farms_1000"
    colnames(coa_list$income)[4]<-"income"
    colnames(coa_list$fertilizer)[4]<-"fertilizer"
    colnames(coa_list$insecticides)[4]<-"insecticides"
    colnames(coa_list$irrigate)[4]<-"irrigate"
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
      p<-p[,c(1,4,5)] #select desired columns
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
    mergelist<-list(countyarea, harvcrop, cropnotharv, cropfail, farms_500, farms_1000,
                income, fertilizer, insecticides, irrigate,
                corngrain, cornsill, oat, barley, wheat, soybeans, fruitnut, veg) %>% 
                purrr::reduce(full_join, by=c("FIPS", "Year"))

    
#Use 2017 county area to fill all other year county area so standardized per FIPS
coa<-mergelist %>%
  dplyr::group_by(FIPS) %>%
  arrange(desc(Year)) %>%
  dplyr::mutate(countyarea=countyarea[1]) %>%
  arrange(FIPS) %>%
  dplyr::ungroup()
    
# #Fix dataset issues (earlier version to do this)
#     #Fill in missing county area data for 1997 with 2002 values (assume same) 
#     coa<-mergelist %>% #Arrange data so that 1997 and 2002 are next to each other for each FIPS
#       dplyr::select(FIPS, everything()) %>% 
#       dplyr::arrange(FIPS)
#     #Fill in 1997 rows with 2002 values using na.locf function (Generic function for replacing each NA with the most recent non-NA prior to it)
#     coa$countyarea<-zoo::na.locf(coa$countyarea) 

    #Need to get rid of NA so that can do math
    #Remove rows where harvcrop or insecticides are NA (not useful measurements)
    #Make rest of NA 0 (so can add across crops wtihout getting NA, etc.-other measurements not influencing as much)
    coa_noNA<-coa %>% 
      filter(!is.na(harvcrop),
             !is.na(insecticides))
    coa_noNA[is.na(coa_noNA)] <- 0
    #Influencing results too much if harvested crop or insecticides are 0 when they are not actually measured
    #Other ones should not be as big of an influence- insecticides used even when only NLCD and need harvested as based of total planted also in NLCD (crop fail is small-0 less important)
    #Need 0s for the rest otherwise cannot add otgether the columns-would ignore a lot of data

#Create combined columns desired for analysis and remove unnecessary base columns
    coa_df<-coa_noNA %>%
      dplyr::mutate(totalplanted=harvcrop+cropfail,
                    totalag=harvcrop+cropnotharv,
                    largefarms=farms_500+farms_1000,
                    soy_smallgrains=soybeans+oat+wheat+barley,
                    corn=corngrain+cornsill,
                    fruitveg=fruitnut+veg) %>% 
      dplyr::select(-c(farms_500, farms_1000,
                soybeans, oat,wheat,barley,corngrain,cornsill,fruitnut,veg))

#Fix more dataset issue
    #Add column of total planted in hectares 
    coa_df$totalplanted_hectares<-coa_df$totalplanted/2.471

#Account for inflation
    #Inflate net income to 2019 values for each year (filter by year and apply conversion)
      #Derived cumulative inflation rates between years from: https://www.usinflationcalculator.com/ and 
      #https://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
      #Both derived from the U.S. Department of Labor Bureau of Labor Statistic
      #1997 to 2019 is: 58.4% (apply rate directly to value by 1+value)
      #2002 to 2019: 41.3%
      #2007 to 2019: 22.6%
      #2012 to 2019: 10.7%
      #2017 to 2019: 3.7%

    #Add in income_inflated column starting with base of current income values per year
    coa_df$income_inflated<-coa_df$income
    
    #Create mutate_cond function to do conditional mutations of income based on year (derived from dplyr::mutate_cond)
    mutate_cond <- function(.data, condition, ..., new_init = NA, envir = parent.frame()) {
      # Initialize any new variables as new_init
      new_vars <- substitute(list(...))[-1]
      new_vars %<>% sapply(deparse) %>% names %>% setdiff(names(.data))
      .data[, new_vars] <- new_init
      #Substitute old values with new values
      condition <- eval(substitute(condition), .data, envir)
      .data[condition, ] <- .data %>% filter(condition) %>% mutate(...)
      .data
    }
    
    #Apply mutate_cond function for each year
    coa_df<-coa_df %>% 
      mutate_cond(Year==2017, income_inflated=income*1.037) %>% 
      mutate_cond(Year==2012, income_inflated=income*1.107) %>% 
      mutate_cond(Year==2007, income_inflated=income*1.226) %>% 
      mutate_cond(Year==2002, income_inflated=income*1.413) %>% 
      mutate_cond(Year==1997, income_inflated=income*1.584)


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
                  fruitveg_planted=fruitveg/totalplanted,
                  irrigate_planted=irrigate/totalplanted,
                  income_planted=income_inflated/totalplanted_hectares) %>% 
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
    write_csv(finalcoa, "Data/DataProcessing/df/coa.csv")
