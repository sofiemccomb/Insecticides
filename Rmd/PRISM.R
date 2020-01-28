library(prism) #https://cran.r-project.org/web/packages/prism/prism.pdf
options(prism.path = "Data/DataProcessing/Climate/PRISM")
# get_prism_annual(type="tmean", year = 2002, keepZip=FALSE) #"ppt", "tmean", "tmin", or "tmax"
# # get_prism_monthlys(type="tmean", years = 2002, mon = 1, keepZip=FALSE)
prism::get_prism_dailys(type="tmean", minDate = "2012-06-01", maxDate = "2012-06-30", keepZip=FALSE)





#https://rpubs.com/collnell/get_prism
##convert raster to point data frame
library(devtools) #needed to download prism from github
library(reshape2) ##melting dataframes
library(dplyr) #data wrangling
library(raster) ##working with raster data
library(sp) ##manipulationg spatial data
ls_prism_data(name=TRUE)
RS <- prism_stack(ls_prism_data())##raster file of data
# RS <- prism_stack(ls_prism_data()[1,1]) ##raster file of data
proj4string(RS)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") ##assign projection info


df <- data.frame(rasterToPoints(RS))
m.df <- melt(df, c("x", "y"))
names(m.df)[1:2] <- c("lon", "lat") #rename columns

prismdf<-m.df %>% 
  dplyr::mutate(variable=as.character(variable),
                date=substr(variable, 26, 33),
                year=substr(variable, 26, 29),
                month=substr(variable, 30, 31),
                day=substr(variable, 32, 33)) %>% 
  arrange(lon, lat)

daily_dd<-prismdf %>% 
  dplyr::mutate(DD=ifelse(value<=10, 0, 
                          ifelse(value>=30, 20,value-10))) %>% 
  dplyr::select(lon, lat, day, DD)%>% 
  group_by(lon, lat) %>% 
  summarise(DD_day=sum(DD)) %>% 
  ungroup()

monthly_dd<-prismdf %>% 
  dplyr::select(lon, lat, value, date) %>% 
  group_by(lon, lat) %>% 
  summarise(meantemp=mean(value)) %>% 
  dplyr::mutate(DD=ifelse(meantemp<=10, 0, 
                          ifelse(meantemp>=30, 20,meantemp-10)),
                DD_month=DD*30) %>% 
  dplyr::select(lon, lat, DD_month) %>% 
  ungroup()

dd<-full_join(daily_dd, monthly_dd, by=c("lon", "lat"))

# dd_f<-dd %>% #Have to change to farenheit earlier (before calculations)
#   dplyr::mutate(DD_day_F=((9/5)*DD_day)+32,
#          DD_month_F=((9/5)*DD_month)+32)

diff<-dd %>%
  dplyr::mutate(Diff=abs(DD_day-DD_month),#absolute difference
                Diff_noabs=DD_day-DD_month)


#Set options
options(scipen=999) #no scientific notation (all)
ggplot2::ggplot(diff, aes(x=Diff_noabs))+
  geom_histogram(bins=60,colour="#1F3552", fill="#4271AE") +
  labs(x="Difference in Degree Days, deg.C", y="Count", title="Difference in Degree Days in degrees Celsius between daily vs monthly summations")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.title=element_text(colour="black", size = 14),
        axis.line = element_line(size=0.5, colour = "black"))

diff_gather<-diff %>% 
  dplyr::select(DD_day,DD_month) %>% 
  tidyr::gather(Variable, Value, DD_day:DD_month, factor_key=TRUE)
ggplot2::ggplot(diff_gather, aes(x=Value,fill=Variable))+
  geom_histogram(bins=60)+
  scale_fill_manual(values=c("#228b22", "#add8e6"))+
  labs(x="Degree Days, deg.C", y="Count", title="Degree Days in degrees Celsius for daily and monthly summations")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.title=element_text(colour="black", size = 14),
        axis.line = element_line(size=0.5, colour = "black"))


ggplot2::ggplot(diff_gather, aes(x=Value,fill=Variable))+
  geom_histogram(bins=60)+
  scale_fill_manual(values=c("#228b22", "#add8e6"))+
  labs(x="Degree Days, deg.C", y="Count", title="Degree Days in degrees Celsius for daily and monthly summations")+
  theme_light()+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text=element_text(colour="black", size = 12),
        axis.title=element_text(colour="black", size = 14),
        axis.line = element_line(size=0.5, colour = "black"))+
  facet_wrap(~Variable)


