##### Examine the NCCV data for Rhode Island and attempt aggregations

library(tidyverse)
library(janitor)

#Currently using mean model

RI_files<-list.files("C:/Users/shemc/Documents/UCSB/NLCD/Data/NCCV/44", full.names = TRUE)
RI_fn<-list.files("C:/Users/shemc/Documents/UCSB/NLCD/Data/NCCV/44")
for (i in 1:length(RI_files)){
  fips<-substring(RI_fn[i], 1, 5)
  df<-readr::read_csv(RI_files[i], skip = 19) %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(Date=date) %>% 
    tidyr::separate(Date, sep="/", into = c("Month", "Day", "Year"))
  df_avg<-df %>% 
    dplyr::mutate(rcp4_5_monthavg=(rcp4_5_maximum_2_m_air_temperature_c+rcp4_5_minimum_2_m_air_temperature_c)/2,
                   rcp8_5_monthavg=(rcp8_5_maximum_2_m_air_temperature_c+rcp8_5_minimum_2_m_air_temperature_c)/2) %>% 
    group_by(Year) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
    dplyr::mutate(FIPS=fips) %>% 
    select(Year, FIPS, everything())
}

#Then can use the forloop and rbind everything as we go


# http://regclim.coas.oregonstate.edu/NEX-DCP30_app/data/counties_20160309/summaries//48043/48043.pdf

# http://regclim.coas.oregonstate.edu/NEX-DCP30_app/data/counties_20160309/csv/48043_CCSM4_metrics.csv
X <- read.csv("http://regclim.coas.oregonstate.edu/NEX-DCP30_app/data/counties_20160309/csv/48043_CCSM4_metric.csv")
write.csv()


df<-readr::read_csv("http://regclim.coas.oregonstate.edu/NEX-DCP30_app/data/counties_20160309/csv/48043_CCSM4_metric.csv", skip = 19) %>% 
  janitor::clean_names()

df2<-readr::read_csv("http://regclim.coas.oregonstate.edu/NEX-DCP30_app/data/counties_20160309/csv/48045_CCSM4_metric.csv", skip = 19) %>% 
  janitor::clean_names()

