#Oragnizing and cleaning data for macronutrient analyses

#This is for chapter 2 analyses of my masters thesis#
#Start:: summary.csv, shrimp.csv, plab.csv (no edits)
#End::   summary_PC.csv, plab.csv

### SUMMARY FILE ####
summary <- read.csv("Bombing/Summary.csv", na.string = "")

#overview of the data
names(summary)
str(summary)
head(summary) 
summary(summary)

## Making PreyCat
summary$PreyCat <- NA
summary$PreyCat <- ifelse(summary$Species == "apc", "Cucumber", 
                          ifelse(summary$Species == "cln" | summary$Species == "prs" | summary$Species == "sag" |
                                   summary$Species == "pab", "Clam", 
                                 ifelse(summary$Species == "cam" | summary$Species == "cap" | summary$Species == "cao" | 
                                          summary$Species == "tec"| summary$Species == "pas" | summary$Species == "pup", "Crab", 
                                        ifelse(summary$Species == "cef"  | summary$Species == "nul" | summary$Species == "lid", "Carn Snail",
                                               ifelse(summary$Species == "map"| summary$Species == "tes", "Herb Snail",
                                                      ifelse(summary$Species == "pio" | summary$Species == "evt", "Star", 
                                                             ifelse(summary$Species == "stf"| summary$Species == "std", "Urchin",
                                                                    ifelse(summary$Species == "mtr", "Mussel",
                                                                           ifelse(summary$Species == "hak", "Abalone", 
                                                                                  ifelse(summary$Species == "crs", "Chiton" ,
                                                                                         ifelse(summary$Species == "crg" | summary$Species == "pom" | 
                                                                                                  summary$Species == "chr", "Scallop", NA)))))))))))




#Frozen.Weight is a character - should be numeric
summary$Frozen.Weight <- as.numeric(as.character(summary$Frozen.Weight))

#Making dates into date format
summary$Date.Collected <- parse_date_time(summary$Date.Collected, orders = "mdy")
summary$Date.Collected <- as.Date(summary$Date.Collected)

summary$Date.Dissected <- parse_date_time(summary$Date.Dissected, orders = "mdy")
summary$Date.Dissected <- as.Date(summary$Date.Dissected)

#######
#add shrimp to summary
shrimp <- read.csv("Bombing/shrimp.csv")

#change weight column title
shrimp <- rename(shrimp, "Live.Weight"="weight.g")
#change species column title (we need species to be pas in the summary file)
shrimp <- rename(shrimp, "Notes"="Species")

#change shrimp to numeric
shrimp$Live.Weight <- as.numeric(as.character(shrimp$Live.Weight))
shrimp$Size.mm <- as.numeric(as.character(shrimp$Size.mm))

#make date collected column
date <- "2020-04-15"
shrimp$Date.Collected <- as.Date(date, "%Y-%m-%d")
#make location column
shrimp$Site.location <- "StJamesBay_Juneau"
#make tissue column
shrimp$Tissue <- "whole"
#make species column
shrimp$Species <- "pas"

#add shrimp to summary
summary <- bind_rows(summary, shrimp)

write.csv(summary, "Bombing/Summary_PC.csv")



########################################################################
##
### PLAB

plab <- read.csv("Bombing/plab.csv", na.string = "")

summary(plab)

#All looks fine to me! 