# #Creating NF Script for the Server only, for higher processing power and memory, and better parallel processing.
# library(doParallel)
# #Create function to perform zonal statistics
# compute_ecozone_stats<-function(polygonlayer, rasterstack){
#   
#   registerDoParallel(cores=19) #initiate multiple core for processing
#   #ptm <- proc.time() #time the process
#   result<-foreach (i = 1:nrow(polygonlayer), .combine=rbind) %dopar% { #number of polygons to iterate through
#     #BUNCH OF STUFF HAPPENS RESULTING IN FINAL DF-FINAL SHOULD BE FIPS< AND THEN RBIND ACROSS STATE
#     #DO FOR EACH STATE< WHICH IS POLYGON LAYER an dYEAR
#     return(final)
#   }
#   return(result)
#   #proc.time() - ptm
#   endCluster()
#   
# } #end function compute_ecozone_stats


