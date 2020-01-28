#Perform NF_Future function for all NLDCS
#Save this as NLCD_Future instead
source("NF_Future.R")
#nf(futureNLCD, year, state)

#FutureNLCD: "CONUS_Historical_y2005", "CONUS_B1_y2050", "CONUS_B1_y2006"
#Year: "2005" or "2050"
#state-iterate through cluster state_list below

library(doParallel)

#State numbers
state_list<-c("01", "04", "05", "06", "08", "09", "10", "11", "12", "13", "16", "17", "18", "19", "20", "21",
              "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37",
              "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55",
              "56")

registerDoParallel(cores=18)#initiate multiple cores for processing
ptm <- proc.time() #time the process
result<-foreach(i=1:length(state_list))%dopar%{
  state<-state_list[i]
  # nf("CONUS_Historical_y2005", "2005", state)
  # nf("CONUS_B1_y2050", "2050", state)
  # nf("CONUS_B1_y2006", "2006", state)
  # nf("CONUS_B1_y2011", "2011", state)
  # nf("CONUS_B1_y2016", "2016", state)
  nf("CONUS_Historical_y1996", "1996", state)
  nf("CONUS_Historical_y2001", "2001", state)
}
proc.time() - ptm
endCluster()


#Read in the NLCD files and rbind per year
library(tidyverse)
#2005
fn_2005 <- dir("Data/NLCD_Frag_Cluster/nlcd_2005", pattern = ".csv", full.names = TRUE) #files located per year
nlcd_df_2005 <- do.call(rbind,lapply(fn_2005,readr::read_csv))
#2050
fn_2050 <- dir("Data/NLCD_Frag_Cluster/nlcd_2050", pattern = ".csv", full.names = TRUE) #files located per year
nlcd_df_2050 <- do.call(rbind,lapply(fn_2050,readr::read_csv))
#2006
fn_2006 <- dir("Data/NLCD_Frag_Cluster/nlcd_2006", pattern = ".csv", full.names = TRUE) #files located per year
nlcd_df_2006 <- do.call(rbind,lapply(fn_2006,readr::read_csv))

fn_2011 <- dir("Data/NLCD_Frag_Cluster/nlcd_2011", pattern = ".csv", full.names = TRUE) #files located per year
nlcd_df_2011 <- do.call(rbind,lapply(fn_2011,readr::read_csv))

fn_2016 <- dir("Data/NLCD_Frag_Cluster/nlcd_2016", pattern = ".csv", full.names = TRUE) #files located per year
nlcd_df_2016 <- do.call(rbind,lapply(fn_2016,readr::read_csv))

fn_1996 <- dir("Data/NLCD_Frag_Cluster/nlcd_1996", pattern = ".csv", full.names = TRUE) #files located per year
nlcd_df_1996 <- do.call(rbind,lapply(fn_1996,readr::read_csv))

fn_2001 <- dir("Data/NLCD_Frag_Cluster/nlcd_2001", pattern = ".csv", full.names = TRUE) #files located per year
nlcd_df_2001 <- do.call(rbind,lapply(fn_2001,readr::read_csv))


#Perform additional calculations
#Add in columns for the percent shared: amount of total edge shared between lc types divided by total edge of that lc type
#Double check on additional categories-may not need them

create_nlcd_col<-function(nlcd_df){
        nlcd_df<-as.data.frame(nlcd_df) %>%
          dplyr::mutate(te_shared_n_nc=tel_naturalcrops_nc/tec_natural_nc, #Natural for Natural-crops
                        te_shared_c_nc=tel_naturalcrops_nc/tec_crops_nc, #Crops for Natural-crops
                        te_shared_f_fc=tel_forestcrop_fc/tec_forest_fc, #Forest for forest-crops
                        te_shared_c_fc=tel_forestcrop_fc/tec_crops_fc, #Crops for forest-crops
                        te_shared_sg_sgc=tel_shrubgrasscrop_sgc/tec_shrubgrass_sgc, #Shrubgrass for shrubgrass-crops
                        te_shared_c_sgc=tel_shrubgrasscrop_sgc/tec_crops_sgc, #Crops for shrubgrass-crops
                        pland_natural=pland_df +pland_ef+pland_mf+pland_grass+pland_shrub+pland_wet+pland_mdf) %>%  #percent natural land
          dplyr::rename(Year=year) %>% 
          dplyr::select(Year, everything()) %>% 
          dplyr::select(FIPS, everything())
        #Replace NA with zero (for calculations)
        nlcd_df[is.na(nlcd_df)] <- 0
        return(nlcd_df)
  }#End Fucntion

nlcd_2005<-create_nlcd_col(nlcd_df_2005)
nlcd_2050<-create_nlcd_col(nlcd_df_2050)
nlcd_2006<-create_nlcd_col(nlcd_df_2006)
nlcd_2011<-create_nlcd_col(nlcd_df_2011)
nlcd_2016<-create_nlcd_col(nlcd_df_2016)
nlcd_1996<-create_nlcd_col(nlcd_df_1996)
nlcd_2001<-create_nlcd_col(nlcd_df_2001)

write_csv(nlcd_2005, "Data/nlcd_future/nlcd_2005.csv")
write_csv(nlcd_2050, "Data/nlcd_future/nlcd_2050.csv")
write_csv(nlcd_2006, "Data/nlcd_future/nlcd_2006.csv")
write_csv(nlcd_2011, "Data/nlcd_future/nlcd_2011.csv")
write_csv(nlcd_2016, "Data/nlcd_future/nlcd_2016.csv")
write_csv(nlcd_1996, "Data/nlcd_future/nlcd_1996.csv")
write_csv(nlcd_2001, "Data/nlcd_future/nlcd_2001.csv")

