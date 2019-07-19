#This script develops a function to create boxplots of variable trends over time.

  #The function is used in the Visualize.Rmd.

#Load necessary packages
library(ggplot2)

#Load fulldata to pull from
load("Data/DataProcessing/df/fulldata.rda")

#Example of function use: var_boxplot(variable=fulldata$insect_planted, yaxis="Prop.Cropland with Insecticides")

var_boxplot<-function(variable, yaxis){
  #Create Boxplot 
  plot<-ggplot(fulldata, aes(x=factor(Year), y=variable)) + 
    geom_boxplot(colour="#1F3552", fill="#4271AE", size=1, notch=TRUE)+
    scale_y_continuous(name=yaxis)+
    scale_x_discrete(name="Year")+
    theme_light()+
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          axis.text=element_text(colour="black", size = 12),
          axis.title=element_text(colour="black", size = 16),
          axis.line = element_line(size=0.5, colour = "black"))
  return(plot)
}
