#Cleaning file for the Stable isotope data (prey and whiskers)

#This is a part of my masters thesis

#Load libraries
library(tidyr)
library(dplyr)


#load stable isotope prey
#load files
si.test <- read.csv("SI/SI_raw.csv", na.strings = "")


#make new line with overall prey cat
si.test$PreyCat <- NA

## PreyCat only 5 catagories including Cucumber.Snail (combined, to match mixing model)
si.test$PreyCat <- ifelse(si.test$Species == "apc" | si.test$Species == "tes", "Cucumber.Snail", 
                          ifelse(si.test$Species == "cln" | si.test$Species == "prs" | si.test$Species == "sag", "Clam", 
                                 ifelse(si.test$Species == "cam" | si.test$Species == "cap" | si.test$Species == "cao" | 
                                          si.test$Species == "tec" | si.test$Species == "pup",  "Crab", 
                                        ifelse(si.test$Species == "std" | si.test$Species == "stf", "Urchin",
                                               ifelse(si.test$Species == "mtr", "Mussel",NA)))))

### Fixing C for high fat critters. Anything above 3.5:1 ratio
si.test$Cnorm <- NA
si.test$Cnorm<- ifelse(si.test$CN >= 3.5, si.test$C-3.32+(0.99*si.test$CN), si.test$C)
si.test$SiteNumber<-as.character(si.test$SiteNumber)

#checking the data
summary(si.test)
#all looks good, so can make csv


#make final CSV for prey
write.csv(si.test, "SI/sifinal.csv")

##################
## Next Whiskers!

#load whisker data
whisker <- read.csv("SI/whiskers_raw.csv")

# There is a weird outlier that may have had some errors in processing,
## we have chosen to remove this sea otter from the data.
whisker<- filter(whisker, OtterID != "163532")

#checking the data
summary(whisker)
#looks ok

#write CSV
write.csv(whisker, "SI/whisker_final.csv")
