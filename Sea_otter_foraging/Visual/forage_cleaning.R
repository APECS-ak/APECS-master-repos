#Cleaning Forage raw file

##load programs
library(ggplot2)
library(lubridate)
library(lattice)
library(dplyr)
library(scales)
library(tidyr)


## RAW DATA 
forage<-read.csv("Visual/2018_Foraging.csv")


forage$PreyCat <- NA
forage$PreyCat <- ifelse(forage$PreyItem == "APC" | forage$PreyItem == "CUC" |  forage$PreyItem == "CUM", 
                         "Cucumber", 
                         ifelse(forage$PreyItem == "CLA" | forage$PreyItem == "CLN" | forage$PreyItem == "GAC" | 
                                  forage$PreyItem == "MAN" | forage$PreyItem == "MAP" | forage$PreyItem == "MAS" | 
                                  forage$PreyItem == "MYA" | forage$PreyItem == "MYS" | forage$PreyItem == "MYT" | 
                                  forage$PreyItem == "PRS" | forage$PreyItem == "SAG" | forage$PreyItem == "TRC", "Clam", 
                                ifelse(forage$PreyItem == "STF" | forage$PreyItem == "URC" | forage$PreyItem == "STD" | 
                                         forage$PreyItem == "STP", "Urchin", 
                                       ifelse(forage$PreyItem == "CAM" | forage$PreyItem == "CAN" | forage$PreyItem == "CAP" | 
                                                forage$PreyItem == "CRA" | forage$PreyItem == "KCR" | forage$PreyItem == "PUP" | 
                                                forage$PreyItem == "PUS" | forage$PreyItem == "TEC", "Crab",
                                              ifelse(forage$PreyItem == "CEF" | forage$PreyItem == "SNA", "Snail", 
                                                     ifelse(forage$PreyItem == "MUS" | forage$PreyItem == "MTR" | forage$PreyItem == "MOM", "Mussel", 
                                                            ifelse(forage$PreyItem == "PIO" | forage$PreyItem == "EVT" | forage$PreyItem == "PES", "Star", NA)))))))

forage$Occupation <- NA
forage$Occupation <- ifelse(forage$YEAR == "1975", "40 years", 
                            ifelse(forage$YEAR == "1988", "30 years", 
                                   ifelse(forage$YEAR == "1994", "15 years", 
                                          ifelse(forage$YEAR == "2003", "15 years", 
                                                 ifelse(forage$YEAR == "2010", "8 years", NA)))))


#remove Lat min and long min (blank cols) and the "true" for dive number as this was a measurement in 
##my google doc to count total dives during the season
forage = subset(forage, select = -c(ObsLatMin, ObsLongMin, OtterLatMin, OtterLongMin, DIVE.NUMBER))#changing all blanks to NA

#changing blanks to NA
forage <- forage %>% mutate_all(na_if,"")

#Changing date to julian date.
forage$Date <- as.Date(forage$Date, "%m/%d/%y")
forage$julian <- yday(forage$Date)

#calculate Spring/ Summer turnover (Using our seasons June 21)
solstice <- as.Date("06-21", format = "%m-%d")
yday(solstice)

#Now make seasons column
forage$Season <- NA
forage$Season <- ifelse(forage$julian<173, "Spring", "Summer")



#checking for errors in prey number 
#checking for errors in prey ID
#checking for errors in ...

# checking the data frame
is.data.frame(forage) # will say if this is a dataframe, should say TRUE
dim(forage)
names(forage)
str(forage)
head(forage) 
summary(forage)

#save final CSV
write.csv(forage, "Visual/forage_final.csv")

#load and check the species list 
##(I don't use this, but helpful to understand the species for those not familiar)
species <- read.csv("Visual/Prey_Class.csv")