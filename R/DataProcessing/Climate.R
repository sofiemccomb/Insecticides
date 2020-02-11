#Climate Script

#Variables to examine: Mean Monthly Temperature (January, April, July, October), Precipitation (Annual, Growing Season),
    #Frost Days (annual days with tmin<=32 F), GDD (annual DD based on 50 and 90 F)

#Load the needed packages, which are loaded in the R scrips as well (install first if necessary)
library(tidyverse) #Datatable manipulation (all functions)
library(tools) #For file_path sans_ext (COA.R)
library(purrr) #For reduce function using full_join (COA.R)
library(zoo) #For na.locf function (COA.R)
library(janitor) #Clean colnames (CDL.R)
library(vegan) #Perform diversity calculation (CDL.R)
library(readxl) #read excel .xlsx (ERS data)

#Set options
options(scipen=999) #no scientific notation (all)


#Climate data
clim_87_12<-read_csv("Data/DataProcessing/Climate/DAILY_IDW200_365_COA_1987_2012.csv")
clim_17<-readr::read_csv("Data/DataProcessing/Climate/DAILY_WEATHER_IDW200_365_2017.csv")

#Drop snow so same exact col order to rbind (and do not need snow)
clim_87_12<-clim_87_12 %>% dplyr::select(-snow)
clim_17<-clim_17 %>% dplyr::select(-snow)
clim<-rbind(clim_87_12, clim_17)

#Average mean temperature for Jan, Apr, Jul, Oct per month-year-FIPS
avg_month_temp<-clim %>% 
  group_by(fips, year, month) %>% 
  summarize(mean = mean(tmean)) %>% 
  ungroup()

Jan_avg_mean<-avg_month_temp %>% filter(month==1) %>% dplyr::select(-month) %>% rename(Jan_tmean_avg=mean)
Apr_avg_mean<-avg_month_temp %>% filter(month==4) %>% dplyr::select(-month) %>% rename(Apr_tmean_avg=mean)
Jul_avg_mean<-avg_month_temp %>% filter(month==7) %>% dplyr::select(-month) %>% rename(Jul_tmean_avg=mean)
Oct_avg_mean<-avg_month_temp %>% filter(month==10) %>% dplyr::select(-month) %>% rename(Oct_tmean_avg=mean)

month_avg_mean<-list(Jan_avg_mean,Apr_avg_mean,Jul_avg_mean,Oct_avg_mean) %>% 
  purrr::reduce(full_join, by=c("fips", "year"))

#Annual precipitation and growing season precipitation (October-April)
sum_month_precip<-clim %>% 
  group_by(fips, year, month) %>% 
  summarize(monthsum = sum(prcp)) %>% 
  ungroup()

annual_precip<-sum_month_precip %>% 
  group_by(fips, year) %>%
  summarize(annual_prcp=sum(monthsum)) %>% 
  ungroup()

#Convert to mm
annual_precip$annual_prcp<-annual_precip$annual_prcp*25.4 #convert to mm

growingseason_precip<-sum_month_precip %>% 
  filter(month==10|month==11|month==12|month==1|month==2|month==3|month==4) %>% 
  group_by(fips, year) %>% 
  summarize(growingseason_prcp=sum(monthsum)) %>% 
  ungroup()

#Convert to mm
growingseason_precip$growingseason_prcp<-growingseason_precip$growingseason_prcp*25.4 #convert to mm

precip<-list(annual_precip,growingseason_precip) %>% 
  purrr::reduce(full_join, by=c("fips", "year"))


# Calculate Frost Days: Count # of days annual with tmin value below 32 deg F
frost_days<-clim %>% 
  dplyr::mutate(frostday=ifelse(tmin<=32, 1, 0)) %>% 
  group_by(fips, year) %>% 
  summarize(ann_frostdays=sum(frostday)) %>% 
  ungroup()


#Growing Degree Days (GDD): annual DD from monthly tmin and tmax averages summed across the year (50, 90 F)
gdd<-clim %>% 
  group_by(fips, year, month) %>% 
  summarize(tmin = mean(tmin),
            tmax=mean(tmax)) %>% 
  ungroup() %>% 
  dplyr::mutate(tmean=(tmin+tmax)/2) %>% 
  dplyr::mutate(GDD=ifelse(tmean<=50, 0, 
                           ifelse(tmean>=90, 40,
                                  tmean-50)),
                GDD_month=GDD*30) %>% 
  group_by(fips, year) %>% 
  summarize(annual_GDD=sum(GDD_month)) %>% 
  ungroup()

#Winter Severity: average minimum temperature across Jan, Feb, March; and for each month separately; also maximum temperature for same

avg_min_temp_3month<-clim %>% 
  filter(month==1|month==2|month==3) %>% 
  group_by(fips, year) %>% 
  summarize(tmin_avg_JanFebMar=mean(tmin)) %>% 
  ungroup()

avg_min_temp_2month<-clim %>% 
  filter(month==1|month==2) %>% 
  group_by(fips, year) %>% 
  summarize(tmin_avg_JanFeb=mean(tmin)) %>% 
  ungroup()

avg_min_temp_bymonth<-clim %>% 
  group_by(fips, year, month) %>% 
  summarize(mean = mean(tmin)) %>% 
  ungroup()

Jan_avg_tmin<-avg_min_temp_bymonth %>% filter(month==1) %>% dplyr::select(-month) %>% rename(Jan_tmin_avg=mean)
Feb_avg_tmin<-avg_min_temp_bymonth %>% filter(month==2) %>% dplyr::select(-month) %>% rename(Feb_tmin_avg=mean)
Mar_avg_tmin<-avg_min_temp_bymonth %>% filter(month==3) %>% dplyr::select(-month) %>% rename(Mar_tmin_avg=mean)


winter_tmin_avg<-list(avg_min_temp_3month,avg_min_temp_2month,Jan_avg_tmin,Feb_avg_tmin,Mar_avg_tmin) %>% 
  purrr::reduce(full_join, by=c("fips", "year"))


avg_max_temp_3month<-clim %>% 
  filter(month==1|month==2|month==3) %>% 
  group_by(fips, year) %>% 
  summarize(tmax_avg_JanFebMar=mean(tmax)) %>% 
  ungroup()

avg_max_temp_2month<-clim %>% 
  filter(month==1|month==2) %>% 
  group_by(fips, year) %>% 
  summarize(tmax_avg_JanFeb=mean(tmax)) %>% 
  ungroup()

avg_max_temp_bymonth<-clim %>% 
  group_by(fips, year, month) %>% 
  summarize(mean = mean(tmax)) %>% 
  ungroup()

Jan_avg_tmax<-avg_max_temp_bymonth %>% filter(month==1) %>% dplyr::select(-month) %>% rename(Jan_tmax_avg=mean)
Feb_avg_tmax<-avg_max_temp_bymonth %>% filter(month==2) %>% dplyr::select(-month) %>% rename(Feb_tmax_avg=mean)
Mar_avg_tmax<-avg_max_temp_bymonth %>% filter(month==3) %>% dplyr::select(-month) %>% rename(Mar_tmax_avg=mean)


winter_tmax_avg<-list(avg_max_temp_3month,avg_max_temp_2month,Jan_avg_tmax,Feb_avg_tmax,Mar_avg_tmax) %>% 
  purrr::reduce(full_join, by=c("fips", "year"))


#Heat Stress

avg_max_temp_summer<-clim %>% 
  filter(month==6|month==7|month==8) %>% 
  group_by(fips, year) %>% 
  summarize(tmax_avg_JunJulAug=mean(tmax)) %>% 
  ungroup()

avg_max_temp_bymonth_summer<-clim %>% 
  group_by(fips, year, month) %>% 
  summarize(mean = mean(tmax)) %>% 
  ungroup()

Jun_avg_tmax<-avg_max_temp_bymonth_summer %>% filter(month==6) %>% dplyr::select(-month) %>% rename(Jun_tmax_avg=mean)
Jul_avg_tmax<-avg_max_temp_bymonth_summer %>% filter(month==7) %>% dplyr::select(-month) %>% rename(Jul_tmax_avg=mean)
Aug_avg_tmax<-avg_max_temp_bymonth_summer %>% filter(month==8) %>% dplyr::select(-month) %>% rename(Aug_tmax_avg=mean)


summer_tmax_avg<-list(avg_max_temp_summer,Jun_avg_tmax,Jul_avg_tmax,Aug_avg_tmax) %>% 
  purrr::reduce(full_join, by=c("fips", "year"))

#Combine all climate data
clim_df<-list(month_avg_mean,precip, frost_days,gdd, winter_tmin_avg, winter_tmax_avg, summer_tmax_avg) %>% 
  purrr::reduce(full_join, by=c("fips", "year"))

write_csv(clim_df, "Data/DataProcessing/df/climate.csv")
