library(tidyverse) #datatable manipulation
options(scipen=999) #no scientific notation
library(tools)
library(purrr)
library(zoo)
library(ggplot2)
library(stargazer)
library(plm)
library(lmtest)
library(readxl)

#This script will re-perform the Larsen PNAS 2013 paper analysis with the new CoA data, CDL data, NLCD data, etc. 
#Will reperform the cross sectional analysis per year with additional county and year fixed effects models

#############################################################################################################################
#Step 1: Organize data in long panel version
#Columns to include: Year (1997, 2002, 2007, 2012, 2017), County (FIPS), CropArea(NLCD), Harvested Area (CoA), 
#                     Total Area of County (CoA), Total planted (CoA) (Crop Harv +CropnotHarv), Total failed (CoA), 
#                     Total insecticides (CoA), Fertilizer (CoA), Crop categories(CoA): corn grain, corn sillage, 
#                     soybeans, small grains (oats,barley, wehat), fruits/nuts, vegetables, Irrigation (CoA), 
#                     net income cash, Large farms >500 acres (CoA), Crop diversity (CDL), Patch/diversity metrics (NLCD)
#                     Also columns calculating fraction of specific matrics over total planted

#####################################
#CENSUS OF AGRICULTURE
#####################################
#Load in all CoA files
coa_fn_names<-list.files(path="Z:/Sofie/Data/CoA", pattern=".csv", full.names=FALSE)
coa_names<-tools::file_path_sans_ext(coa_fn_names)
for (i in coa_names){
  assign(i, read_csv(paste0("Z:/Sofie/Data/CoA/", i, ".csv", sep="")))
}

#Establish column names to be applied
colnames <- c("Program", "Year","Period","Week_Ending","Geo_Level","State", "State_ANSI","Ag_District",
              "Ag_District_Code","County","County_ANSI","Zip_Code","Region","watershed_code","Watershed","Commodity",
              "Data_Item","Domain","Domain_Category","Value","CV") 

#Combine all large farms datasets with rbind (were too big to download together) and only keep large farm rows (500+)
farms<-rbind(largefarms_2017, largefarms_2012, largefarms_2007, largefarms_2002, largefarms_1997)
colnames(farms)<-colnames
farms_500<-farms[farms$Domain_Category=="AREA HARVESTED: (500 TO 999 ACRES)",]
farms_1000<-farms[farms$Domain_Category=="AREA HARVESTED: (1,000 OR MORE ACRES)",]

#Merge all dataframes into list
coa_df_list<-list(countyarea=countyarea, harvcrop=harvestedcropland, cropnotharv=cropnotharv,  
                  cropfail=croplandfailed,farms_500=farms_500, farms_1000=farms_1000,income=netcashincome, 
                  fertilizer=aglandfertilizer, insecticides=aglandinsecticides, irrigate=cropharvirrigated,  
                  barley=barleyharvested,  oat=oatsharvested, wheat=wheatharvested, soybeans=soybeansharvested, 
                  corngrain=corngrainharvested, cornsill=cornsillageharvested, fruitnut=fruitnutorchardharvested, 
                  veg=vegetablesharvested)

coa_df_listed<-lapply(coa_df_list, setNames, colnames)

#Create function to clean all the data in the same format
clean_function<-function(w){
  w<-w[,c(2,7,11,17,19,20)]
  w<-w[!is.na(w$County_ANSI),] 
  w$FIPS<-paste0(w$State_ANSI,w$County_ANSI)
  w<-w[,c(1,4,5,6,7)]
  w$Value[w$Value=="(D)"|w$Value==" (D)"|w$Value==" (Z)"|w$Value=="(Z)"]<-NA
  w$Value <- as.numeric(gsub(",","",w$Value))
  w<-w[w$Domain_Category!="FERTILIZER: (MANURE)" & w$Domain_Category!="FERTILIZER: (ORGANIC)",] #keep total fertilizer
  w<-w[w$Domain_Category!="CHEMICAL, INSECTICIDE: (NEMATICIDES)",] #keep only insecticides
  w$FIPS<-as.numeric(w$FIPS) 
  w<-w[!is.na(w$FIPS),]
  return(w)
}

#Apply function to list of dataframes
coa_list<-lapply(coa_df_listed,clean_function)

#Rename value columns so easier to cbind (easiest way to do so)
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

row_func<-function(p){
  p<-p[,c(1,4,5)]
  return(p)
}

#Apply function to list of dataframes
final_coa_list<-lapply(coa_list,row_func)

#Unlist 
listnames<-names(final_coa_list)
for (l in seq(final_coa_list)){
  dfname<-listnames[l]
  assign(dfname, final_coa_list[[l]])
}

mergelist<-list(countyarea, harvcrop, cropnotharv, cropfail, farms_500, farms_1000,
                income, fertilizer, insecticides, irrigate,
             corngrain, cornsill, oat, barley, wheat, soybeans, fruitnut, veg) %>% 
  reduce(full_join, by=c("FIPS", "Year"))


#Fill in county area data for 1997 with 2002 
final_coa<-mergelist %>% 
  select(FIPS, everything()) %>% 
  arrange(FIPS)
coaarea<-final_coa$countyarea
coayearsarea<-na.locf(coaarea)
final_coa$countyarea<-coayearsarea

#Change all NA to 0 as assuming that is the value of acres (there are no NA for FIPS, Year, or county area)
#For income all seemingly accounted for, except 1997 which is missing them all (excluded in analysis)
final_coa[is.na(final_coa)] <- 0

#Add new additive columns and remove base columns
coa_df<-final_coa %>%
  mutate(totalplanted=harvcrop+cropnotharv) %>% 
  mutate(largefarms=farms_500+farms_1000) %>% 
  mutate(soy_smallgrains=soybeans+oat+wheat+barley) %>% 
  mutate(corn=corngrain+cornsill) %>% 
  mutate(fruitveg=fruitnut+veg) %>% 
  select(-c(cropnotharv, farms_500, farms_1000,
            soybeans, oat,wheat,barley,corngrain,cornsill,fruitnut,veg))

#If harvested cropland is 0 (NA/not reported), going to set planted to 0 because 
#     otherwise total planted is only the crop not harvested value and introducing inaccuracies
coa_df$totalplanted<- ifelse(coa_df$harvcrop ==0, coa_df$harvcrop, coa_df$totalplanted)

#Add column of total planted in hectares 
coa_df$totalplanted_hectares<-coa_df$totalplanted/2.471

#Inflate net income to 2019 values for each year (filter by year and apply conversion)
  #Derived cumulative inflation rates between years from: https://www.usinflationcalculator.com/ and 
  #https://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/
  #Both derived from the U.S. Department of Labor Bureau of Labor Statistic
  #1997 to 2019 is: 58.4% (apply rate directly to value by 1+value)
  #2002 to 2019: 41.3%
  #2007 to 2019: 22.6%
  #2012 to 2019: 10.7%
  #2017 to 2019: 3.7%

#Add in income inflated column starting with base of current income values per year
coa_df$income_inflated<-coa_df$income

#Create mutate_cond function to do conditional mutations of income based on year
mutate_cond <- function(.data, condition, ..., new_init = NA, envir = parent.frame()) {
  # Initialize any new variables as new_init
  new_vars <- substitute(list(...))[-1]
  new_vars %<>% sapply(deparse) %>% names %>% setdiff(names(.data))
  .data[, new_vars] <- new_init
  
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data %>% filter(condition) %>% mutate(...)
  .data
}

coa_df<-coa_df %>% 
  mutate_cond(Year==2017, income_inflated=income*1.037) %>% 
  mutate_cond(Year==2012, income_inflated=income*1.107) %>% 
  mutate_cond(Year==2007, income_inflated=income*1.226) %>% 
  mutate_cond(Year==2002, income_inflated=income*1.413) %>% 
  mutate_cond(Year==1997, income_inflated=income*1.584)


#Calculate ratios (use total planted in hectares for income planted and use inflated income)
coa_frac<-coa_df[c(1,2,3,10,4,5,11,6,7,8,9,12,13,14,15,16)] %>%
  arrange(FIPS) %>% 
  mutate(insect_planted=insecticides/totalplanted) %>% 
  mutate(harv_county=harvcrop/countyarea) %>% 
  mutate(soysmallgrain_planted=soy_smallgrains/totalplanted) %>% 
  mutate(corn_planted=corn/totalplanted) %>% 
  mutate(fruitveg_planted=fruitveg/totalplanted) %>% 
  mutate(income_planted=income_inflated/totalplanted_hectares) %>%  
  mutate(largefarm_planted=largefarms/totalplanted) %>% 
  mutate(irrigate_planted=irrigate/totalplanted)

#Remove NA, Inf, and NaN caused by dividing by zero for total planted
is.na(coa_frac)<-sapply(coa_frac, is.infinite)
coa_frac[is.na(coa_frac)]<-0

write_csv(coa_frac, "Z:/Sofie/Data/Regressions/coa_frac.csv")

########
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

#Merge CDL data with the coa_frac table to have all data together
agdata<-list(coa_frac, CDL) %>% 
  reduce(full_join, by=c("FIPS", "Year"))


#################
#Read in ERS file
ERS<-readxl::read_xls("Z:/Sofie/Data/ERSRegions_To_Counties.xls")
ERS<-ERS[,1:3]
colnames(ERS)<-c("FIPS", "ERSCode", "ERSName")

#Merge agdata with ERS names
agdata<-list(agdata, ERS) %>% 
  reduce(full_join, by=c("FIPS"))

################
#Drop the 9 VA counties 51515, 51530, 51580, 51595, 51600, 51610, 51678,51685, 51720 
  #with NA in CDL (and within other counties); also only 2007 and 2012 data

agdata<-agdata%>% 
  filter(!(FIPS==51515)&!(FIPS==51530)&!(FIPS==51580)&!(FIPS==51595)&!(FIPS==51600)&
           !(FIPS==51610)&!(FIPS==51678)&!(FIPS==51685)&!(FIPS==51720))

#################
write_csv(agdata, "Z:/Sofie/Data/Regressions/agdata.csv")



##############################################
#Read in NLCD analysis data (analysis performed in NF script and 
  #combined separately using do.call with rbind and lapply on the filenames-saved as all.csv per year)

#Read in the NLCD files
nlcd_2001<-read_csv("Z:/Sofie/Data/NLCD_Frag_Cluster/nlcd_2001/all.csv")
nlcd_2006<-read_csv("Z:/Sofie/Data/NLCD_Frag_Cluster/nlcd_2006/all.csv")
nlcd_2011<-read_csv("Z:/Sofie/Data/NLCD_Frag_Cluster/nlcd_2011/all.csv")
nlcd_2016<-read_csv("Z:/Sofie/Data/NLCD_Frag_Cluster/nlcd_2016/all.csv")

#Bind the nlcd files together
nlcd<-rbind(nlcd_2001, nlcd_2006, nlcd_2011, nlcd_2016)

#Add in columns for the percent shared: amount of total edge shared between lc types divided by total edge of that lc type
nlcddf<-nlcd %>%
  mutate(te_shared_f_fc=tel_forestcrop_fc/tec_forest_fc) %>% #Forest for forest-crops
  mutate(te_shared_c_fc=tel_forestcrop_fc/tec_crops_fc) %>% #Crops for forest-crops
  mutate(te_shared_sg_sgc=tel_shrubgrasscrop_sgc/tec_shrubgrass_sgc) %>% #Shrubgrass for shrubgrass-crops
  mutate(te_shared_c_sgc=tel_shrubgrasscrop_sgc/tec_crops_sgc) %>% #Crops for shrubgrass-crops
  mutate(te_shared_df_dfLID=tel_deciforestLID_dfLID/tec_deciforest_dfLID) %>% #DeciForest for deciforest-LID
  mutate(te_shared_LID_DFLID=tel_deciforestLID_dfLID/tec_LID_dfLID) %>% #LID for deciforest-LID
  mutate(te_shared_mf_mfLID=tel_mixforestLID_mfLID/tec_mixforest_mfLID) %>% #MixForest for Mixforest-LID
  mutate(te_shared_LID_mfLID=tel_mixforestLID_mfLID/tec_LID_mfLID) %>% #LID for Mixforest-LID
  mutate(te_shared_n_nc=tel_naturalcrops_nc/tec_natural_nc) %>% #Natural for Natural-crops
  mutate(te_shared_c_nc=tel_naturalcrops_nc/tec_crops_nc) %>%#Crops for Natural-crops 
  mutate(pland_natural=pland_df +pland_ef+pland_mf+pland_grass+pland_shrub+pland_wet) %>% #percent natural land
  rename(Year=year)

#Add replace NA with zero (for calculations)
nlcddf[is.na(nlcddf)] <- 0

#Change years to match those in the ag census so can combine by FIPS and years
#2001 to 2002, 2006 to 2007, 2011 to 2012, and 2016 to 2017
nlcddf$Year[nlcddf$Year==2001] <- 2002
nlcddf$Year[nlcddf$Year==2006] <- 2007
nlcddf$Year[nlcddf$Year==2011] <- 2012
nlcddf$Year[nlcddf$Year==2016] <- 2017

#agdata<-read_csv("Z:/Sofie/Data/Regressions/agdata.csv") #If need to pull in (starting here)

#Merge agdata with NLCD data
fulldata<-list(agdata, nlcddf) %>% 
  reduce(full_join, by=c("FIPS", "Year"))

# ################
#Drop the 9 VA counties 51515, 51530, 51580, 51595, 51600, 51610, 51678,51685, 51720 
#with NA in CDL (and within other counties); also only 2007 and 2012 data (again from combined data)

fulldata<-fulldata%>%
  filter(!(FIPS==51515)&!(FIPS==51530)&!(FIPS==51580)&!(FIPS==51595)&!(FIPS==51600)&
           !(FIPS==51610)&!(FIPS==51678)&!(FIPS==51685)&!(FIPS==51720))

#31 row difference later from missing data from one datasource- leaving for now and should be removed in analysis

#Write full data
write_csv(fulldata, "Z:/Sofie/Data/Regressions/fulldata.csv")

###############################

############################################################################################################################

#Step 2: Summarize data and also look at data with boxplots
##############################################################################################################

#Source this to another script/function?

ag_summary <- agdata %>% 
  group_by(Year) %>% 
  summarize(
    mean_insect_planted=mean(insect_planted, na.rm=TRUE),
    sd_insect_planted=sd(insect_planted, na.rm=TRUE),
    mean_harv_county=mean(harv_county, na.rm=TRUE),
    sd_harv_county=sd(harv_county, na.rm=TRUE),
    mean_soysmallgrain_planted=mean(soysmallgrain_planted, na.rm=TRUE),
    sd_soysmallgrain_planted=sd(soysmallgrain_planted, na.rm=TRUE),
    mean_corn_planted=mean(corn_planted, na.rm=TRUE),
    sd_corn_planted=sd(corn_planted, na.rm=TRUE),
    mean_fruitveg_planted=mean(fruitveg_planted, na.rm=TRUE),
    sd_fruitveg_planted=sd(fruitveg_planted, na.rm=TRUE),
    mean_income_planted=mean(income_planted, na.rm=TRUE),
    sd_income_planted=sd(income_planted, na.rm=TRUE),
    mean_irrigate_planted=mean(irrigate_planted, na.rm=TRUE),
    sd_irrigate_planted=sd(irrigate_planted, na.rm=TRUE),
    mean_SDI=mean(SDI, na.rm=TRUE),
    sd_SDI=sd(SDI, na.rm=TRUE)
  )

#Nice boxplot website: http://t-redactyl.io/blog/2016/04/creating-plots-in-r-using-ggplot2-part-10-boxplots.html 
#Eventually could find way to try to iterate through and just change y and the title

#Boxplot insecticides on planted cropland
fill <- "#4271AE"
lines <- "#1F3552"
insectplot<-ggplot(agdata, aes(x=factor(Year), y=insect_planted)) + 
  geom_boxplot(colour=lines, fill=fill, size=1, notch=TRUE)+
  scale_y_continuous(name="Prop.Cropland with Insecticides")+
  scale_x_discrete(name="Year")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

#Boxplot harvested cropland per county area
fill <- "#4271AE"
lines <- "#1F3552"
harvestplot<-ggplot(agdata, aes(x=factor(Year), y=harv_county)) + 
  geom_boxplot(colour=lines, fill=fill, size=1, notch=TRUE)+
  scale_y_continuous(name="Prop. County Harvested Cropland",
                     breaks = seq(0, 1, 0.2))+
  scale_x_discrete(name="Year")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

#Boxplot soybeans and small grains
fill <- "#4271AE"
lines <- "#1F3552"
soygrainplot<-ggplot(agdata, aes(x=factor(Year), y=soysmallgrain_planted)) + 
  geom_boxplot(colour=lines, fill=fill, size=1, notch=TRUE)+
  scale_y_continuous(name="Prop. Cropland Soybeans & Small Grains",
                     breaks = seq(0, 1, 0.2),
                     limits=c(0, 1))+
  scale_x_discrete(name="Year")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

#Boxplot corn
fill <- "#4271AE"
lines <- "#1F3552"
cornplot<-ggplot(agdata, aes(x=factor(Year), y=corn_planted)) + 
  geom_boxplot(colour=lines, fill=fill, size=1, notch=TRUE)+
  scale_y_continuous(name="Prop. Cropland Corn",
                     breaks = seq(0, 1, 0.2),
                     limits=c(0, 1))+
  scale_x_discrete(name="Year")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

#Boxplot fruit and veg
fill <- "#4271AE"
lines <- "#1F3552"
fruitvegplot<-ggplot(agdata, aes(x=factor(Year), y=fruitveg_planted)) + 
  geom_boxplot(colour=lines, fill=fill, size=1, notch=TRUE)+
  scale_y_continuous(name="Prop. Cropland Fruit & Vegetables",
                     breaks = seq(0, 1, 0.2))+
  scale_x_discrete(name="Year")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

#Boxplot income
fill <- "#4271AE"
lines <- "#1F3552"
incomeplot<-ggplot(agdata, aes(x=factor(Year), y=income_planted)) + 
  geom_boxplot(colour=lines, fill=fill, size=1, notch=TRUE)+
  scale_y_continuous(name="Prop. Income to Planted Cropland")+
  scale_x_discrete(name="Year")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

#Boxplot large farms
fill <- "#4271AE"
lines <- "#1F3552"
farmplot<-ggplot(agdata, aes(x=factor(Year), y=largefarm_planted)) + 
  geom_boxplot(colour=lines, fill=fill, size=1, notch=TRUE)+
  scale_y_continuous(name="Prop. Large Farms to Planted Cropland")+
  scale_x_discrete(name="Year")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

#Boxplot irrigate
fill <- "#4271AE"
lines <- "#1F3552"
irrigateplot<-ggplot(agdata, aes(x=factor(Year), y=irrigate_planted)) + 
  geom_boxplot(colour=lines, fill=fill, size=1, notch=TRUE)+
  scale_y_continuous(name="Prop. Irrigated Area to Planted Cropland")+
  scale_x_discrete(name="Year")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

#Boxplot SDI
fill <- "#4271AE"
lines <- "#1F3552"
SDIplot<-ggplot(agdata, aes(x=factor(Year), y=SDI)) + 
  geom_boxplot(colour=lines, fill=fill, size=1, notch=TRUE)+
  scale_y_continuous(name="Crop Diversity to Planted Cropland")+
  scale_x_discrete(name="Year")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.line = element_line(size=0.5, colour = "black"))

#Arrange all the covariate boxplots together

#Save plots
# ggsave("Z:/Sofie/Data/Regressions/boxplots/insectplot.png", insectplot)
# ggsave("Z:/Sofie/Data/Regressions/boxplots/harvestplot.png", harvestplot)
# ggsave("Z:/Sofie/Data/Regressions/boxplots/soygrainplot.png", soygrainplot)
# ggsave("Z:/Sofie/Data/Regressions/boxplots/cornplot.png", cornplot)
# ggsave("Z:/Sofie/Data/Regressions/boxplots/fruitvegplot.png", fruitvegplot)
# ggsave("Z:/Sofie/Data/Regressions/boxplots/incomeplot.png", incomeplot)
# ggsave("Z:/Sofie/Data/Regressions/boxplots/farmplot.png", farmplot)
# ggsave("Z:/Sofie/Data/Regressions/boxplots/irrigateplot.png", irrigateplot)
# ggsave("Z:/Sofie/Data/Regressions/boxplots/SDIplot.png", SDIplot)

####################################################################################################################

#Step 3: Regression of variables
####################################################################################################################

#To analyze whether landscape simplification drives insecticide use, I use both cross-sectional analyses for each 
#of the five census years and fixed-effects models on all five census years.

#Good article on missing NA impacting lm analysis: https://stats.idre.ucla.edu/r/faq/how-does-r-handle-missing-values/
#Default is na.omit-works to exclude the NA value rows from analysis


#Equation without any fixed effects across all years for testing
lm_model <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted +
                 largefarm_planted + income_planted +irrigate_planted, data = fulldata) # Run lm
summary(lm_model)

#Equation by year
#1997
ag_1997<-fulldata[fulldata$Year==1997,]
lm_model_1997 <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted+
                      largefarm_planted + irrigate_planted, data = ag_1997) # Run lm
summary(lm_model_1997) #no income harvest data available at all for 1997
#2002
ag_2002<-fulldata[fulldata$Year==2002,]
lm_model_2002 <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                      income_planted + largefarm_planted + irrigate_planted, data = ag_2002) # Run lm
summary(lm_model_2002)
#2007
ag_2007<-fulldata[fulldata$Year==2007,]
lm_model_2007 <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                      income_planted + largefarm_planted + irrigate_planted, data = ag_2007) # Run lm
summary(lm_model_2007)
#2012
ag_2012<-fulldata[fulldata$Year==2012,]
lm_model_2012 <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                      income_planted + largefarm_planted + irrigate_planted, data = ag_2012) # Run lm
summary(lm_model_2012)
#2017
ag_2017<-fulldata[fulldata$Year==2017,]
lm_model_2017 <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                      income_planted + largefarm_planted + irrigate_planted, data = ag_2017) # Run lm
summary(lm_model_2017)

#Figure out how to add title to these
cross_sectional_models<-stargazer(lm_model_1997, lm_model_2002, lm_model_2007, lm_model_2012, lm_model_2017, 
                                  type = "text", column.labels=c("1997", "2002", "2007", "2012", "2017"),
                                  out="Z:/Sofie/Data/Regressions/cross_sectional_models.txt") 

#Run time analysis but with CDL added in as well
#Equation by year
#1997
lm_model_1997_CDL <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted+
                      largefarm_planted + irrigate_planted, data = ag_1997) # Run lm #No SDI for 1997 so its the same
#2002
lm_model_2002_CDL <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                      income_planted + largefarm_planted + irrigate_planted, data = ag_2002) # Run lm #No SDI for 2002 so its the same
#2007
lm_model_2007_CDL <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                      income_planted + largefarm_planted + irrigate_planted + SDI, data = ag_2007) # Run lm
#2012
lm_model_2012_CDL <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                      income_planted + largefarm_planted + irrigate_planted + SDI, data = ag_2012) # Run lm
#2017
lm_model_2017_CDL <- lm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                      income_planted + largefarm_planted + irrigate_planted + SDI, data = ag_2017) # Run lm


cross_sectional_models_CDL<-stargazer(lm_model_1997_CDL, lm_model_2002_CDL, lm_model_2007_CDL, 
                                      lm_model_2012_CDL, lm_model_2017_CDL, 
                                  type = "text", column.labels=c("1997", "2002", "2007", "2012", "2017"),
                                  out="Z:/Sofie/Data/Regressions/cross_sectional_models_CDL.txt") 


#Panel Fixed effects lm

#Helpful websites: https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html 
#https://cran.r-project.org/web/packages/plm/vignettes/plmPackage.html

#Without CDL first
#Entity fixed effects model: county (FIPS)
county_model <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                      income_planted + largefarm_planted + irrigate_planted, 
                    data = fulldata,
                  index = c("FIPS","Year"),
                  model = "within",
                  effect="individual")

summary(county_model)
#lmtest::coeftest(county_model, vcov. = vcovHC(county_model, type = "HC1")) 
#fixef(county_model) #constants per county
#The coeff of x1 indicates how much Y changes overtime, on average per country, when X increases by one unit.

#Time fixed effects model: Year 
time_model <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                    income_planted + largefarm_planted + irrigate_planted, 
                    data = fulldata,
                    index = c("FIPS","Year"),
                    model = "within",
                    effect="time")

summary(time_model)

#Time and Entity fixed effects model: county (FIPS and Year)
countytime_model <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                          income_planted + largefarm_planted +irrigate_planted, 
                    data = fulldata,
                    index = c("FIPS","Year"),
                    model = "within",
                    effect = "twoways")

summary(countytime_model)

panel_models<-stargazer(county_model, time_model, countytime_model, 
                        type = "text", column.labels = c("County Fixed Effects", "Year Fixed Effects",
                                                         "County and Year Fixed Effects"),
                        out="Z:/Sofie/Data/Regressions/panel_models.txt") 


#With CDL
#Entity fixed effects model: county (FIPS)
county_model_CDL <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                      income_planted + largefarm_planted + irrigate_planted + SDI, 
                    data = fulldata,
                    index = c("FIPS","Year"),
                    model = "within",
                    effect="individual")

summary(county_model_CDL)
#lmtest::coeftest(county_model, vcov. = vcovHC(county_model, type = "HC1")) 
#fixef(county_model) #constants per county
#The coeff of x1 indicates how much Y changes overtime, on average per country, when X increases by one unit.

#Time fixed effects model: Year 
time_model_CDL <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                    income_planted + largefarm_planted + irrigate_planted + SDI, 
                  data = fulldata,
                  index = c("FIPS","Year"),
                  model = "within",
                  effect="time")

summary(time_model_CDL)

#Time and Entity fixed effects model: county (FIPS and Year)
countytime_model_CDL <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                          income_planted + largefarm_planted +irrigate_planted + SDI, 
                        data = fulldata,
                        index = c("FIPS","Year"),
                        model = "within",
                        effect = "twoways")

summary(countytime_model_CDL)

panel_models_CDL<-stargazer(county_model_CDL, time_model_CDL, countytime_model_CDL, 
                        type = "text", column.labels = c("County Fixed Effects", "Year Fixed Effects",
                                                         "County and Year Fixed Effects"),
                        out="Z:/Sofie/Data/Regressions/panel_models_CDL.txt") 

#######################################################################################


#######################################################################################
#Panel Fixed effects lm with CDL accounting for ERS

#Time and Entity fixed effects model: county (FIPS and Year) accounting for ERS without CDL 
#ERS 1: Heartland
ag_1<-fulldata[fulldata$ERSCode==1,]
countytime_model_ag1 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted, 
                                data = ag_1,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_ag1)

#ERS 2: Northern Crescent
ag_2<-fulldata[fulldata$ERSCode==2,]
countytime_model_ag2 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted, 
                                data = ag_2,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_ag2)

#ERS 3: Northern Great Plains
ag_3<-fulldata[fulldata$ERSCode==3,]
countytime_model_ag3 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted, 
                                data = ag_3,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_ag3)

#ERS 4: Prairie Gateway
ag_4<-fulldata[fulldata$ERSCode==4,]
countytime_model_ag4 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted, 
                                data = ag_4,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_ag4)

#ERS 5: Eastern Uplands
ag_5<-fulldata[fulldata$ERSCode==5,]
countytime_model_ag5 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted, 
                                data = ag_5,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_ag5)

#ERS 6: Southern Seaboard
ag_6<-fulldata[fulldata$ERSCode==6,]
countytime_model_ag6 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted, 
                                data = ag_6,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_ag6)

#ERS 7: Fruitful Rim
ag_7<-fulldata[fulldata$ERSCode==7,]
countytime_model_ag7 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted, 
                                data = ag_7,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_ag7)

#ERS 8: Basin and Range
ag_8<-fulldata[fulldata$ERSCode==8,]
countytime_model_ag8 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted, 
                                data = ag_8,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_ag8)

#ERS 9: Mississippi Portal
ag_9<-fulldata[fulldata$ERSCode==9,]
countytime_model_ag9 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted, 
                                data = ag_9,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_ag9)



panel_models_ERS<-stargazer(countytime_model_ag1, countytime_model_ag2, countytime_model_ag3, 
                                countytime_model_ag4, countytime_model_ag5, countytime_model_ag6, 
                                countytime_model_ag7, countytime_model_ag8, countytime_model_ag9,
                            type = "text", column.labels = c("Heartland", "Northern Crescent", "Northern Great Plains",
                                                             "Prairie Gateway", "Eastern Uplands", "Southern Seaboard",
                                                             "Fruitful Rim", "Basin and Range", "Mississippi Portal"),
                            out="Z:/Sofie/Data/Regressions/panel_models_EDS.txt") 

#For df for the boxplots of the model coefficients
countytime_model_ERS_coef<-cbind(countytime_model_ag1$coefficients, countytime_model_ag2$coefficients,
                                 countytime_model_ag3$coefficients,countytime_model_ag4$coefficients,
                                 countytime_model_ag5$coefficients,countytime_model_ag6$coefficients,
                                 countytime_model_ag7$coefficients,countytime_model_ag8$coefficients,
                                 countytime_model_ag9$coefficients)
countytime_model_ERS_coef<-as.data.frame(countytime_model_ERS_coef)
colnames(countytime_model_ERS_coef)<-c("1", "2", "3", "4", "5", "6", "7", "8", "9")
countytime_model_ERS_coef <- rownames_to_column(countytime_model_ERS_coef, "Coefficient")
countytime_model_ERS_coef_df<-gather(countytime_model_ERS_coef, "ERS", "Value", 2:10)
countytime_model_ERS_coef_df$Model<-"ERS_woCDL_5yrs"



#Time and Entity fixed effects model: county (FIPS and Year) accounting for ERS with CDL 
#ag_data already defined for the ERS

#ERS 1: Heartland
countytime_model_CDL_ag1 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted +SDI, 
                                data = ag_1,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag1)

#ERS 2: Northern Crescent
countytime_model_CDL_ag2 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted+SDI, 
                                data = ag_2,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag2)

#ERS 3: Northern Great Plains
countytime_model_CDL_ag3 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted+SDI, 
                                data = ag_3,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag3)

#ERS 4: Prairie Gateway
countytime_model_CDL_ag4 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted+SDI, 
                                data = ag_4,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag4)

#ERS 5: Eastern Uplands
countytime_model_CDL_ag5 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted+SDI, 
                                data = ag_5,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag5)

#ERS 6: Southern Seaboard
countytime_model_CDL_ag6 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted+SDI, 
                                data = ag_6,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag6)

#ERS 7: Fruitful Rim
countytime_model_CDL_ag7 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted+SDI, 
                                data = ag_7,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag7)

#ERS 8: Basin and Range
countytime_model_CDL_ag8 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted+SDI, 
                                data = ag_8,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag8)

#ERS 9: Mississippi Portal
countytime_model_CDL_ag9 <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                                  income_planted + largefarm_planted +irrigate_planted+SDI, 
                                data = ag_9,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag9)



panel_models_CDL_ERS<-stargazer(countytime_model_CDL_ag1, countytime_model_CDL_ag2, countytime_model_CDL_ag3, 
                                countytime_model_CDL_ag4, countytime_model_CDL_ag5, countytime_model_CDL_ag6, 
                                countytime_model_CDL_ag7, countytime_model_CDL_ag8, countytime_model_CDL_ag9,
                                type = "text", column.labels = c("Heartland", "Northern Crescent", "Northern Great Plains",
                                                                 "Prairie Gateway", "Eastern Uplands", "Southern Seaboard",
                                                                 "Fruitful Rim", "Basin and Range", "Mississippi Portal"),
                                out="Z:/Sofie/Data/Regressions/panel_models_CDL_EDS.txt") 

#For df for the boxplots of the model coefficients
countytime_model_CDL_ERS_coef<-cbind(countytime_model_CDL_ag1$coefficients, countytime_model_CDL_ag2$coefficients,
             countytime_model_CDL_ag3$coefficients,countytime_model_CDL_ag4$coefficients,
             countytime_model_CDL_ag5$coefficients,countytime_model_CDL_ag6$coefficients,
             countytime_model_CDL_ag7$coefficients,countytime_model_CDL_ag8$coefficients,
             countytime_model_CDL_ag9$coefficients)
countytime_model_CDL_ERS_coef<-as.data.frame(countytime_model_CDL_ERS_coef)
colnames(countytime_model_CDL_ERS_coef)<-c("1", "2", "3", "4", "5", "6", "7", "8", "9")
countytime_model_CDL_ERS_coef <- rownames_to_column(countytime_model_CDL_ERS_coef, "Coefficient")
countytime_model_CDL_ERS_coef_df<-gather(countytime_model_CDL_ERS_coef, "ERS", "Value", 2:10)
countytime_model_CDL_ERS_coef_df$Model<-"ERS_CDL"




#Time and Entity fixed effects model: county (FIPS and Year) accounting for ERS without CDL for just 3 years
#ERS 1: Heartland
ag_1_3yrs<-ag_1[ag_1$Year!=1997&ag_1$Year!=2002,]
countytime_model_ag1_3yrs <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                              income_planted + largefarm_planted +irrigate_planted, 
                            data = ag_1_3yrs,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag1_3yrs)

#ERS 2: Northern Crescent
ag_2_3yrs<-ag_2[ag_2$Year!=1997&ag_2$Year!=2002,]
countytime_model_ag2_3yrs <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                              income_planted + largefarm_planted +irrigate_planted, 
                            data = ag_2_3yrs,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag2_3yrs)

#ERS 3: Northern Great Plains
ag_3_3yrs<-ag_3[ag_3$Year!=1997&ag_3$Year!=2002,]
countytime_model_ag3_3yrs <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                              income_planted + largefarm_planted +irrigate_planted, 
                            data = ag_3_3yrs,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag3_3yrs)

#ERS 4: Prairie Gateway
ag_4_3yrs<-ag_4[ag_4$Year!=1997&ag_4$Year!=2002,]
countytime_model_ag4_3yrs <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                              income_planted + largefarm_planted +irrigate_planted, 
                            data = ag_4_3yrs,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag4_3yrs)

#ERS 5: Eastern Uplands
ag_5_3yrs<-ag_5[ag_5$Year!=1997&ag_5$Year!=2002,]
countytime_model_ag5_3yrs <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                              income_planted + largefarm_planted +irrigate_planted, 
                            data = ag_5_3yrs,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag5_3yrs)

#ERS 6: Southern Seaboard
ag_6_3yrs<-ag_6[ag_6$Year!=1997&ag_6$Year!=2002,]
countytime_model_ag6_3yrs <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                              income_planted + largefarm_planted +irrigate_planted, 
                            data = ag_6_3yrs,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag6_3yrs)

#ERS 7: Fruitful Rim
ag_7_3yrs<-ag_7[ag_7$Year!=1997&ag_7$Year!=2002,]
countytime_model_ag7_3yrs <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                              income_planted + largefarm_planted +irrigate_planted, 
                            data = ag_7_3yrs,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag7_3yrs)

#ERS 8: Basin and Range
ag_8_3yrs<-ag_8[ag_8$Year!=1997&ag_8$Year!=2002,]
countytime_model_ag8_3yrs <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                              income_planted + largefarm_planted +irrigate_planted, 
                            data = ag_8_3yrs,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag8_3yrs)

#ERS 9: Mississippi Portal
ag_9_3yrs<-ag_9[ag_9$Year!=1997&ag_9$Year!=2002,]
countytime_model_ag9_3yrs <- plm(insect_planted ~ harv_county + soysmallgrain_planted + corn_planted + fruitveg_planted + 
                              income_planted + largefarm_planted +irrigate_planted, 
                            data = ag_9_3yrs,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag9_3yrs)



panel_models_ERS<-stargazer(countytime_model_ag1_3yrs, countytime_model_ag2_3yrs, countytime_model_ag3_3yrs, 
                            countytime_model_ag4_3yrs, countytime_model_ag5_3yrs, countytime_model_ag6_3yrs, 
                            countytime_model_ag7_3yrs, countytime_model_ag8_3yrs, countytime_model_ag9_3yrs,
                            type = "text", column.labels = c("Heartland", "Northern Crescent", "Northern Great Plains",
                                                             "Prairie Gateway", "Eastern Uplands", "Southern Seaboard",
                                                             "Fruitful Rim", "Basin and Range", "Mississippi Portal"),
                            out="Z:/Sofie/Data/Regressions/panel_models_EDS_3yrs.txt")

#For df for the boxplots of the model coefficients
countytime_model_ERS_3yrs_coef<-cbind(countytime_model_ag1_3yrs$coefficients, countytime_model_ag2_3yrs$coefficients,
                                      countytime_model_ag3_3yrs$coefficients,countytime_model_ag4_3yrs$coefficients,
                                      countytime_model_ag5_3yrs$coefficients,countytime_model_ag6_3yrs$coefficients,
                                      countytime_model_ag7_3yrs$coefficients,countytime_model_ag8_3yrs$coefficients,
                                      countytime_model_ag9_3yrs$coefficients)
countytime_model_ERS_3yrs_coef<-as.data.frame(countytime_model_ERS_3yrs_coef)
colnames(countytime_model_ERS_3yrs_coef)<-c("1", "2", "3", "4", "5", "6", "7", "8", "9")
countytime_model_ERS_3yrs_coef <- rownames_to_column(countytime_model_ERS_3yrs_coef, "Coefficient")
countytime_model_ERS_3yrs_coef_df<-gather(countytime_model_ERS_3yrs_coef, "ERS", "Value", 2:10)
countytime_model_ERS_3yrs_coef_df$Model<-"ERS_woCDL_3yrs"


#######################################################################################

########################################################################################
#Running panel regression with only a few of the variables: harv_county, largefarm_SDI

#Not split by EDS
#Time and Entity fixed effects model: county (FIPS and Year) without SDI
countytime_model_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                            data = fulldata,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")

summary(countytime_model_3var)

#Time and Entity fixed effects model: county (FIPS and Year) without SDI for 3 yrs
fulldata_3yrs<-fulldata[fulldata$Year!=1997&fulldata$Year!=2002,]
countytime_model_3var_3yrs <- plm(insect_planted ~ harv_county + largefarm_planted, 
                             data = fulldata_3yrs,
                             index = c("FIPS","Year"),
                             model = "within",
                             effect = "twoways")

summary(countytime_model_3var_3yrs)

#Time and Entity fixed effects model: county (FIPS and Year) with SDI
countytime_model_CDL_3var <- plm(insect_planted ~ harv_county + largefarm_planted + SDI, 
                            data = fulldata,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")

summary(countytime_model_CDL_3var)

panel_models_3var<-stargazer(countytime_model_3var, countytime_model_3var_3yrs, countytime_model_CDL_3var, 
                            type = "text", column.labels = c("County and Year Fixed Effects w/o CDL",
                                                             "County and Year Fixed Effects w/o CDL 3 years",
                                                             "County and Year Fixed Effects w/ CDL"),
                            out="Z:/Sofie/Data/Regressions/panel_models_3var.txt") 

#By EDS
#Time and Entity fixed effects model: county (FIPS and Year) accounting for ERS without CDL for 3 var
#ERS 1: Heartland
ag_1<-fulldata[fulldata$ERSCode==1,]
countytime_model_ag1_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                            data = ag_1,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag1_3var)

#ERS 2: Northern Crescent
ag_2<-fulldata[fulldata$ERSCode==2,]
countytime_model_ag2_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                            data = ag_2,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag2_3var)

#ERS 3: Northern Great Plains
ag_3<-fulldata[fulldata$ERSCode==3,]
countytime_model_ag3_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                            data = ag_3,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag3_3var)

#ERS 4: Prairie Gateway
ag_4<-fulldata[fulldata$ERSCode==4,]
countytime_model_ag4_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                            data = ag_4,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag4_3var)

#ERS 5: Eastern Uplands
ag_5<-fulldata[fulldata$ERSCode==5,]
countytime_model_ag5_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                            data = ag_5,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag5_3var)

#ERS 6: Southern Seaboard
ag_6<-fulldata[fulldata$ERSCode==6,]
countytime_model_ag6_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                            data = ag_6,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag6_3var)

#ERS 7: Fruitful Rim
ag_7<-fulldata[fulldata$ERSCode==7,]
countytime_model_ag7_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                            data = ag_7,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag7_3var)

#ERS 8: Basin and Range
ag_8<-fulldata[fulldata$ERSCode==8,]
countytime_model_ag8_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                            data = ag_8,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag8_3var)

#ERS 9: Mississippi Portal
ag_9<-fulldata[fulldata$ERSCode==9,]
countytime_model_ag9_3var <- plm(insect_planted ~ harv_county + largefarm_planted,
                            data = ag_9,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag9_3var)



panel_models_ERS_3var<-stargazer(countytime_model_ag1_3var, countytime_model_ag2_3var, countytime_model_ag3_3var, 
                            countytime_model_ag4_3var, countytime_model_ag5_3var, countytime_model_ag6_3var, 
                            countytime_model_ag7_3var, countytime_model_ag8_3var, countytime_model_ag9_3var,
                            type = "text", column.labels = c("Heartland", "Northern Crescent", "Northern Great Plains",
                                                             "Prairie Gateway", "Eastern Uplands", "Southern Seaboard",
                                                             "Fruitful Rim", "Basin and Range", "Mississippi Portal"),
                            out="Z:/Sofie/Data/Regressions/panel_models_EDS_3var.txt") 


#For df for the boxplots of the model coefficients
countytime_model_ERS_3var_coef<-cbind(countytime_model_ag1_3var$coefficients, countytime_model_ag2_3var$coefficients,
                                      countytime_model_ag3_3var$coefficients,countytime_model_ag4_3var$coefficients,
                                      countytime_model_ag5_3var$coefficients,countytime_model_ag6_3var$coefficients,
                                      countytime_model_ag7_3var$coefficients,countytime_model_ag8_3var$coefficients,
                                      countytime_model_ag9_3var$coefficients)
countytime_model_ERS_3var_coef<-as.data.frame(countytime_model_ERS_3var_coef)
colnames(countytime_model_ERS_3var_coef)<-c("1", "2", "3", "4", "5", "6", "7", "8", "9")
countytime_model_ERS_3var_coef <- rownames_to_column(countytime_model_ERS_3var_coef, "Coefficient")
countytime_model_ERS_3var_coef_df<-gather(countytime_model_ERS_3var_coef, "ERS", "Value", 2:10)
countytime_model_ERS_3var_coef_df$Model<-"ERS_woCDL_3var"




#Time and Entity fixed effects model: county (FIPS and Year) accounting for ERS with CDL for 3 variables
#ag_data already defined for the ERS

#ERS 1: Heartland
countytime_model_CDL_ag1_3var <- plm(insect_planted ~ harv_county + largefarm_planted + SDI, 
                                data = ag_1,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag1_3var)

#ERS 2: Northern Crescent
countytime_model_CDL_ag2_3var <- plm(insect_planted ~ harv_county + largefarm_planted + SDI,  
                                data = ag_2,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag2_3var)

#ERS 3: Northern Great Plains
countytime_model_CDL_ag3_3var <- plm(insect_planted ~ harv_county + largefarm_planted + SDI, 
                                data = ag_3,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag3_3var)

#ERS 4: Prairie Gateway
countytime_model_CDL_ag4_3var <- plm(insect_planted ~ harv_county + largefarm_planted + SDI,  
                                data = ag_4,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag4_3var)

#ERS 5: Eastern Uplands
countytime_model_CDL_ag5_3var <- plm(insect_planted ~ harv_county + largefarm_planted + SDI, 
                                data = ag_5,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag5_3var)

#ERS 6: Southern Seaboard
countytime_model_CDL_ag6_3var <- plm(insect_planted ~ harv_county + largefarm_planted + SDI, 
                                data = ag_6,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag6_3var)

#ERS 7: Fruitful Rim
countytime_model_CDL_ag7_3var <- plm(insect_planted ~ harv_county + largefarm_planted + SDI, 
                                data = ag_7,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag7_3var)

#ERS 8: Basin and Range
countytime_model_CDL_ag8_3var <- plm(insect_planted ~ harv_county + largefarm_planted + SDI,  
                                data = ag_8,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag8_3var)

#ERS 9: Mississippi Portal
countytime_model_CDL_ag9_3var <- plm(insect_planted ~ harv_county + largefarm_planted + SDI, 
                                data = ag_9,
                                index = c("FIPS","Year"),
                                model = "within",
                                effect = "twoways")
summary(countytime_model_CDL_ag9_3var)



panel_models_CDL_ERS_3var<-stargazer(countytime_model_CDL_ag1_3var, countytime_model_CDL_ag2_3var, countytime_model_CDL_ag3_3var, 
                                countytime_model_CDL_ag4_3var, countytime_model_CDL_ag5_3var, countytime_model_CDL_ag6_3var, 
                                countytime_model_CDL_ag7_3var, countytime_model_CDL_ag8_3var, countytime_model_CDL_ag9_3var,
                                type = "text", column.labels = c("Heartland", "Northern Crescent", "Northern Great Plains",
                                                                 "Prairie Gateway", "Eastern Uplands", "Southern Seaboard",
                                                                 "Fruitful Rim", "Basin and Range", "Mississippi Portal"),
                                out="Z:/Sofie/Data/Regressions/panel_models_CDL_EDS_3var.txt") 


#For df for the boxplots of the model coefficients
countytime_model_CDL_ERS_3var_coef<-cbind(countytime_model_CDL_ag1_3var$coefficients, countytime_model_CDL_ag2_3var$coefficients,
                                          countytime_model_CDL_ag3_3var$coefficients,countytime_model_CDL_ag4_3var$coefficients,
                                          countytime_model_CDL_ag5_3var$coefficients,countytime_model_CDL_ag6_3var$coefficients,
                                          countytime_model_CDL_ag7_3var$coefficients,countytime_model_CDL_ag8_3var$coefficients,
                                          countytime_model_CDL_ag9_3var$coefficients)
countytime_model_CDL_ERS_3var_coef<-as.data.frame(countytime_model_CDL_ERS_3var_coef)
colnames(countytime_model_CDL_ERS_3var_coef)<-c("1", "2", "3", "4", "5", "6", "7", "8", "9")
countytime_model_CDL_ERS_3var_coef <- rownames_to_column(countytime_model_CDL_ERS_3var_coef, "Coefficient")
countytime_model_CDL_ERS_3var_coef_df<-gather(countytime_model_CDL_ERS_3var_coef, "ERS", "Value", 2:10)
countytime_model_CDL_ERS_3var_coef_df$Model<-"ERS_CDL_3var"


#Time and Entity fixed effects model: county (FIPS and Year) accounting for ERS without CDL for just 3 years for 3 variables
#ERS 1: Heartland
ag_1_3yrs<-ag_1[ag_1$Year!=1997&ag_1$Year!=2002,]
countytime_model_ag1_3yrs_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                                 data = ag_1_3yrs,
                                 index = c("FIPS","Year"),
                                 model = "within",
                                 effect = "twoways")
summary(countytime_model_ag1_3yrs_3var)

#ERS 2: Northern Crescent
ag_2_3yrs<-ag_2[ag_2$Year!=1997&ag_2$Year!=2002,]
countytime_model_ag2_3yrs_3var <- plm(insect_planted ~ harv_county + largefarm_planted,  
                                 data = ag_2_3yrs,
                                 index = c("FIPS","Year"),
                                 model = "within",
                                 effect = "twoways")
summary(countytime_model_ag2_3yrs_3var)

#ERS 3: Northern Great Plains
ag_3_3yrs<-ag_3[ag_3$Year!=1997&ag_3$Year!=2002,]
countytime_model_ag3_3yrs_3var <- plm(insect_planted ~ harv_county + largefarm_planted,  
                                 data = ag_3_3yrs,
                                 index = c("FIPS","Year"),
                                 model = "within",
                                 effect = "twoways")
summary(countytime_model_ag3_3yrs_3var)

#ERS 4: Prairie Gateway
ag_4_3yrs<-ag_4[ag_4$Year!=1997&ag_4$Year!=2002,]
countytime_model_ag4_3yrs_3var <- plm(insect_planted ~ harv_county + largefarm_planted,  
                                 data = ag_4_3yrs,
                                 index = c("FIPS","Year"),
                                 model = "within",
                                 effect = "twoways")
summary(countytime_model_ag4_3yrs_3var)

#ERS 5: Eastern Uplands
ag_5_3yrs<-ag_5[ag_5$Year!=1997&ag_5$Year!=2002,]
countytime_model_ag5_3yrs_3var <- plm(insect_planted ~ harv_county + largefarm_planted,  
                                 data = ag_5_3yrs,
                                 index = c("FIPS","Year"),
                                 model = "within",
                                 effect = "twoways")
summary(countytime_model_ag5_3yrs_3var)

#ERS 6: Southern Seaboard
ag_6_3yrs<-ag_6[ag_6$Year!=1997&ag_6$Year!=2002,]
countytime_model_ag6_3yrs_3var <- plm(insect_planted ~ harv_county + largefarm_planted,  
                                 data = ag_6_3yrs,
                                 index = c("FIPS","Year"),
                                 model = "within",
                                 effect = "twoways")
summary(countytime_model_ag6_3yrs_3var)

#ERS 7: Fruitful Rim
ag_7_3yrs<-ag_7[ag_7$Year!=1997&ag_7$Year!=2002,]
countytime_model_ag7_3yrs_3var <- plm(insect_planted ~ harv_county + largefarm_planted,  
                                 data = ag_7_3yrs,
                                 index = c("FIPS","Year"),
                                 model = "within",
                                 effect = "twoways")
summary(countytime_model_ag7_3yrs_3var)

#ERS 8: Basin and Range
ag_8_3yrs<-ag_8[ag_8$Year!=1997&ag_8$Year!=2002,]
countytime_model_ag8_3yrs_3var <- plm(insect_planted ~ harv_county + largefarm_planted, 
                                 data = ag_8_3yrs,
                                 index = c("FIPS","Year"),
                                 model = "within",
                                 effect = "twoways")
summary(countytime_model_ag8_3yrs_3var)

#ERS 9: Mississippi Portal
ag_9_3yrs<-ag_9[ag_9$Year!=1997&ag_9$Year!=2002,]
countytime_model_ag9_3yrs_3var <- plm(insect_planted ~ harv_county + largefarm_planted,  
                                 data = ag_9_3yrs,
                                 index = c("FIPS","Year"),
                                 model = "within",
                                 effect = "twoways")
summary(countytime_model_ag9_3yrs_3var)



panel_models_ERS_3yrs_3var<-stargazer(countytime_model_ag1_3yrs_3var, countytime_model_ag2_3yrs_3var, countytime_model_ag3_3yrs_3var, 
                            countytime_model_ag4_3yrs_3var, countytime_model_ag5_3yrs_3var, countytime_model_ag6_3yrs_3var, 
                            countytime_model_ag7_3yrs_3var, countytime_model_ag8_3yrs_3var, countytime_model_ag9_3yrs_3var,
                            type = "text", column.labels = c("Heartland", "Northern Crescent", "Northern Great Plains",
                                                             "Prairie Gateway", "Eastern Uplands", "Southern Seaboard",
                                                             "Fruitful Rim", "Basin and Range", "Mississippi Portal"),
                            out="Z:/Sofie/Data/Regressions/panel_models_EDS_3yrs_3var.txt") 


#For df for the boxplots of the model coefficients
countytime_model_ERS_3var_3yrs_coef<-cbind(countytime_model_ag1_3yrs_3var$coefficients, countytime_model_ag2_3yrs_3var$coefficients,
                                           countytime_model_ag3_3yrs_3var$coefficients,countytime_model_ag4_3yrs_3var$coefficients,
                                           countytime_model_ag5_3yrs_3var$coefficients,countytime_model_ag6_3yrs_3var$coefficients,
                                           countytime_model_ag7_3yrs_3var$coefficients,countytime_model_ag8_3yrs_3var$coefficients,
                                           countytime_model_ag9_3yrs_3var$coefficients)
countytime_model_ERS_3var_3yrs_coef<-as.data.frame(countytime_model_ERS_3var_3yrs_coef)
colnames(countytime_model_ERS_3var_3yrs_coef)<-c("1", "2", "3", "4", "5", "6", "7", "8", "9")
countytime_model_ERS_3var_3yrs_coef <- rownames_to_column(countytime_model_ERS_3var_3yrs_coef, "Coefficient")
countytime_model_ERS_3var_3yrs_coef_df<-gather(countytime_model_ERS_3var_3yrs_coef, "ERS", "Value", 2:10)
countytime_model_ERS_3var_3yrs_coef_df$Model<-"ERS_woCDL_3var_3yrs"


############################################################################################################
#Running Panel with fulldata (including NLCD)

#Regression rough attempts: Insecticides per harvested hectare~
#1) Percent cropland (NLCD), 
#2) harvested area/percent cropland*areas of county (aka intensity of harvest aka rotations both from CoA), 
#3) Crop diversity, 
#4) landscape diversity, 
#5) EL crops-natural, 
#6) crops mean patch area, 
#7) large farms

#Time and Entity fixed effects model: county (FIPS and Year) accounting for ERS with CDL and all datasets
#ERS 1: Heartland
ag_1<-fulldata[fulldata$ERSCode==1,]
countytime_model_ag1_fulldata <- plm(insect_planted ~ pland_crops+harv_county+SDI+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                            data = ag_1,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag1_fulldata)

#ERS 2: Northern Crescent
ag_2<-fulldata[fulldata$ERSCode==2,]
countytime_model_ag2_fulldata <- plm(insect_planted ~ pland_crops+harv_county+SDI+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                            data = ag_2,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag2_fulldata)

#ERS 3: Northern Great Plains
ag_3<-fulldata[fulldata$ERSCode==3,]
countytime_model_ag3_fulldata <- plm(insect_planted ~ pland_crops+harv_county+SDI+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                            data = ag_3,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag3_fulldata)

#ERS 4: Prairie Gateway
ag_4<-fulldata[fulldata$ERSCode==4,]
countytime_model_ag4_fulldata <- plm(insect_planted ~ pland_crops+harv_county+SDI+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                            data = ag_4,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag4_fulldata)

#ERS 5: Eastern Uplands
ag_5<-fulldata[fulldata$ERSCode==5,]
countytime_model_ag5_fulldata <- plm(insect_planted ~ pland_crops+harv_county+SDI+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                            data = ag_5,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag5_fulldata)

#ERS 6: Southern Seaboard
ag_6<-fulldata[fulldata$ERSCode==6,]
countytime_model_ag6_fulldata <- plm(insect_planted ~ pland_crops+harv_county+SDI+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                            data = ag_6,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag6_fulldata)

#ERS 7: Fruitful Rim
ag_7<-fulldata[fulldata$ERSCode==7,]
countytime_model_ag7_fulldata <- plm(insect_planted ~ pland_crops+harv_county+SDI+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                            data = ag_7,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag7_fulldata)

#ERS 8: Basin and Range
ag_8<-fulldata[fulldata$ERSCode==8,]
countytime_model_ag8_fulldata <- plm(insect_planted ~ pland_crops+harv_county+SDI+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                            data = ag_8,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag8_fulldata)

#ERS 9: Mississippi Portal
ag_9<-fulldata[fulldata$ERSCode==9,]
countytime_model_ag9_fulldata <- plm(insect_planted ~ pland_crops+harv_county+SDI+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                            data = ag_9,
                            index = c("FIPS","Year"),
                            model = "within",
                            effect = "twoways")
summary(countytime_model_ag9_fulldata)



panel_models_ERS<-stargazer(countytime_model_ag1_fulldata, countytime_model_ag2_fulldata, countytime_model_ag3_fulldata, 
                            countytime_model_ag4_fulldata, countytime_model_ag5_fulldata, countytime_model_ag6_fulldata, 
                            countytime_model_ag7_fulldata, countytime_model_ag8_fulldata, countytime_model_ag9_fulldata,
                            type = "text", column.labels = c("Heartland", "Northern Crescent", "Northern Great Plains",
                                                             "Prairie Gateway", "Eastern Uplands", "Southern Seaboard",
                                                             "Fruitful Rim", "Basin and Range", "Mississippi Portal"),
                            out="Z:/Sofie/Data/Regressions/panel_models_CDL_EDS_fulldata.txt") 

#Time and Entity fixed effects model: county (FIPS and Year) accounting for ERS without CDL and all datasets
#ERS 1: Heartland
ag_1<-fulldata[fulldata$ERSCode==1,]
countytime_model_ag1_full_woCDL <- plm(insect_planted ~ pland_crops+harv_county+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                                     data = ag_1,
                                     index = c("FIPS","Year"),
                                     model = "within",
                                     effect = "twoways")
summary(countytime_model_ag1_full_woCDL)

#ERS 2: Northern Crescent
ag_2<-fulldata[fulldata$ERSCode==2,]
countytime_model_ag2_full_woCDL <- plm(insect_planted ~ pland_crops+harv_county+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                                     data = ag_2,
                                     index = c("FIPS","Year"),
                                     model = "within",
                                     effect = "twoways")
summary(countytime_model_ag2_full_woCDL)

#ERS 3: Northern Great Plains
ag_3<-fulldata[fulldata$ERSCode==3,]
countytime_model_ag3_full_woCDL <- plm(insect_planted ~ pland_crops+harv_county+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                                     data = ag_3,
                                     index = c("FIPS","Year"),
                                     model = "within",
                                     effect = "twoways")
summary(countytime_model_ag3_full_woCDL)

#ERS 4: Prairie Gateway
ag_4<-fulldata[fulldata$ERSCode==4,]
countytime_model_ag4_full_woCDL <- plm(insect_planted ~ pland_crops+harv_county+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                                     data = ag_4,
                                     index = c("FIPS","Year"),
                                     model = "within",
                                     effect = "twoways")
summary(countytime_model_ag4_full_woCDL)

#ERS 5: Eastern Uplands
ag_5<-fulldata[fulldata$ERSCode==5,]
countytime_model_ag5_full_woCDL <- plm(insect_planted ~ pland_crops+harv_county+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                                     data = ag_5,
                                     index = c("FIPS","Year"),
                                     model = "within",
                                     effect = "twoways")
summary(countytime_model_ag5_full_woCDL)

#ERS 6: Southern Seaboard
ag_6<-fulldata[fulldata$ERSCode==6,]
countytime_model_ag6_full_woCDL <- plm(insect_planted ~ pland_crops+harv_county+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                                     data = ag_6,
                                     index = c("FIPS","Year"),
                                     model = "within",
                                     effect = "twoways")
summary(countytime_model_ag6_full_woCDL)

#ERS 7: Fruitful Rim
ag_7<-fulldata[fulldata$ERSCode==7,]
countytime_model_ag7_full_woCDL <- plm(insect_planted ~ pland_crops+harv_county+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                                     data = ag_7,
                                     index = c("FIPS","Year"),
                                     model = "within",
                                     effect = "twoways")
summary(countytime_model_ag7_full_woCDL)

#ERS 8: Basin and Range
ag_8<-fulldata[fulldata$ERSCode==8,]
countytime_model_ag8_full_woCDL <- plm(insect_planted ~ pland_crops+harv_county+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                                     data = ag_8,
                                     index = c("FIPS","Year"),
                                     model = "within",
                                     effect = "twoways")
summary(countytime_model_ag8_full_woCDL)

#ERS 9: Mississippi Portal
ag_9<-fulldata[fulldata$ERSCode==9,]
countytime_model_ag9_full_woCDL <- plm(insect_planted ~ pland_crops+harv_county+msidi+
                                       tel_naturalcrops_nc+mna_crops+largefarm_planted, 
                                     data = ag_9,
                                     index = c("FIPS","Year"),
                                     model = "within",
                                     effect = "twoways")
summary(countytime_model_ag9_full_woCDL)



panel_models_ERS<-stargazer(countytime_model_ag1_full_woCDL, countytime_model_ag2_full_woCDL, countytime_model_ag3_full_woCDL, 
                            countytime_model_ag4_full_woCDL, countytime_model_ag5_full_woCDL, countytime_model_ag6_full_woCDL, 
                            countytime_model_ag7_full_woCDL, countytime_model_ag8_full_woCDL, countytime_model_ag9_full_woCDL,
                            type = "text", column.labels = c("Heartland", "Northern Crescent", "Northern Great Plains",
                                                             "Prairie Gateway", "Eastern Uplands", "Southern Seaboard",
                                                             "Fruitful Rim", "Basin and Range", "Mississippi Portal"),
                            out="Z:/Sofie/Data/Regressions/panel_models_EDS_fulldata.txt") 

# #For df for the boxplots of the model coefficients
# countytime_model_ERS_coef<-cbind(countytime_model_ag1$coefficients, countytime_model_ag2$coefficients,
#                                  countytime_model_ag3$coefficients,countytime_model_ag4$coefficients,
#                                  countytime_model_ag5$coefficients,countytime_model_ag6$coefficients,
#                                  countytime_model_ag7$coefficients,countytime_model_ag8$coefficients,
#                                  countytime_model_ag9$coefficients)
# countytime_model_ERS_coef<-as.data.frame(countytime_model_ERS_coef)
# colnames(countytime_model_ERS_coef)<-c("1", "2", "3", "4", "5", "6", "7", "8", "9")
# countytime_model_ERS_coef <- rownames_to_column(countytime_model_ERS_coef, "Coefficient")
# countytime_model_ERS_coef_df<-gather(countytime_model_ERS_coef, "ERS", "Value", 2:10)
# countytime_model_ERS_coef_df$Model<-"ERS_woCDL_5yrs"
# 
















#Move everything below to separate scripts


# ######################################################
# #Boxplots
# 
# countytime_ERSmodels<-rbind(countytime_model_ERS_coef_df, countytime_model_CDL_ERS_coef_df,countytime_model_ERS_3yrs_coef_df,
#                             countytime_model_ERS_3var_coef_df, countytime_model_CDL_ERS_3var_coef_df, countytime_model_ERS_3var_3yrs_coef_df)
# 
# fill <- "#4271AE"
# lines <- "#1F3552"
# 
# ggplot(countytime_ERSmodels, aes(x=factor(ERS), y=Value))+
#   geom_boxplot(colour=lines, fill=fill)+
#   facet_wrap(~Coefficient)+
#   labs(x="ERS", y="Coefficient")+
#   theme_light()
# ggsave("Z:/Sofie/Data/Regressions/countytime_ERSmodels_boxplots_fixedscales.png")
# 
# ggplot(countytime_ERSmodels, aes(x=factor(ERS), y=Value))+
#   geom_boxplot(colour=lines, fill=fill, notch=TRUE)+
#   facet_wrap(~Coefficient, scales="free")+
#   labs(x="ERS", y="Coefficient")+
#   theme_light()
# ggsave("Z:/Sofie/Data/Regressions/countytime_ERSmodels_boxplots_freescales.png")
# 
# #View(countytime_ERSmodels)

# ############################################################################################################
# #How correlated is SDI with proportion fruits and veggies
# ############################################################################################################
# #Website for correlation matrices: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# cordata<-fulldata %>%
#   select(insect_planted, harv_county, soysmallgrain_planted, corn_planted, fruitveg_planted, 
#            income_planted, largefarm_planted, irrigate_planted, SDI)
# cordata<-na.omit(cordata)
# 
# correlate <- cor(cordata)
# correlate
# round(correlate,2)
# 
# library(Hmisc)
# corrsig <- rcorr(as.matrix(cordata))
# corrsig
# 
# flattenCorrMatrix <- function(cormat, pmat) {
#   ut <- upper.tri(cormat)
#   data.frame(
#     row = rownames(cormat)[row(cormat)[ut]],
#     column = rownames(cormat)[col(cormat)[ut]],
#     cor  =(cormat)[ut],
#     p = pmat[ut]
#   )
# }
# corrmat<-flattenCorrMatrix(corrsig$r, corrsig$P) %>% 
#   arrange(cor)
# 
# #symnum(correlate, abbr.colnames = FALSE)
# 
# library(corrplot)
# corrplot(correlate, type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)
# # Insignificant correlations are leaved blank
# corrplot(corrsig$r, type="upper", order="hclust", 
#          p.mat = corrsig$P, sig.level = 0.01, insig = "blank")
# 
# library(PerformanceAnalytics)
# chart.Correlation(cordata, histogram=TRUE, pch=19)
# 
# col<- colorRampPalette(c("blue", "white", "red"))(20)
# heatmap(x = correlate, col = col, symm = TRUE)



# ###############
# #Xtsum- within variation remaining
# ###############
# 
# #Figure out what wanted from Xt sum and then how to do it
# 
# xtsumR<-function(df,columns,individuals){
#   df<-dplyr::arrange(df,individuals)
#   panel<-tibble::tibble()
#   for (i in columns){
#     v<-df %>% dplyr::group_by_() %>%
#       dplyr::summarize_(
#         mean=mean(df[[i]]),
#         sd=sd(df[[i]]),
#         min=min(df[[i]]),
#         max=max(df[[i]])
#       )
#     v<-tibble::add_column(v,variacao="overal",.before=-1)
#     v2<-aggregate(df[[i]],list(df[[individuals]]),"mean")[[2]]
#     sdB<-sd(v2)
#     varW<-df[[i]]-rep(v2,each=12) #
#     varW<-varW+mean(df[[i]])
#     sdW<-sd(varW)
#     minB<-min(v2)
#     maxB<-max(v2)
#     minW<-min(varW)
#     maxW<-max(varW)
#     v<-rbind(v,c("between",NA,sdB,minB,maxB),c("within",NA,sdW,minW,maxW))
#     panel<-rbind(panel,v)
#   }
#   var<-rep(names(df)[columns])
#   n1<-rep(NA,length(columns))
#   n2<-rep(NA,length(columns))
#   var<-c(rbind(var,n1,n1))
#   panel$var<-var
#   panel<-panel[c(6,1:5)]
#   names(panel)<-c("variable","variation","mean","standard.deviation","min","max")
#   panel[3:6]<-as.numeric(unlist(panel[3:6]))
#   panel[3:6]<-round(unlist(panel[3:6]),2)
#   return(panel)
# }
# 
# xtsumdata<-fulldata %>%
#   mutate(FIPSYear=paste0(FIPS,Year)) %>% 
#   select(FIPSYear, insect_planted, harv_county, soysmallgrain_planted, corn_planted, fruitveg_planted, 
#          income_planted, largefarm_planted, irrigate_planted, SDI)
# #cordata<-na.omit(cordata)
# xtsumR(df=xtsumdata, columns=xtsumdata$insect_planted, individuals=xtsumdata$FIPSYear)
# 
# 
# 
# df<-dplyr::arrange(xtsumdata,xtsumdata$FIPSYear)
# panel<-tibble::tibble()
# for (i in columns){
#   v<-df %>% dplyr::group_by() %>%
#     dplyr::summarize(
#       mean=mean(df[[i]]),
#       sd=sd(df[[i]]),
#       min=min(df[[i]]),
#       max=max(df[[i]])
#     )
#   v<-tibble::add_column(v,variacao="overal",.before=-1)
#   v2<-aggregate(df[[i]],list(df[[individuals]]),"mean")[[2]]
#   sdB<-sd(v2)
#   varW<-df[[i]]-rep(v2,each=12) #
#   varW<-varW+mean(df[[i]])
#   sdW<-sd(varW)
#   minB<-min(v2)
#   maxB<-max(v2)
#   minW<-min(varW)
#   maxW<-max(varW)
#   v<-rbind(v,c("between",NA,sdB,minB,maxB),c("within",NA,sdW,minW,maxW))
#   panel<-rbind(panel,v)
# }
# var<-rep(names(df)[columns])
# n1<-rep(NA,length(columns))
# n2<-rep(NA,length(columns))
# var<-c(rbind(var,n1,n1))
# panel$var<-var
# panel<-panel[c(6,1:5)]
# names(panel)<-c("variable","variation","mean","standard.deviation","min","max")
# panel[3:6]<-as.numeric(unlist(panel[3:6]))
# panel[3:6]<-round(unlist(panel[3:6]),2)
# return(panel)
