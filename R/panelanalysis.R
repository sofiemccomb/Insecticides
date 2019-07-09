#Regression of variables
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
