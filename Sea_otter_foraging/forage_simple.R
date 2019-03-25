####### First look at foraging data #####

library(ggplot2)
library(lattice)
require(stats)
library(tidyverse)
library(plyr)
setwd("/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging")

###  DATA  ###
s.prop <- read.csv("s_prop.csv") # import a file with the male/female proportions
age.prop <- read.csv("age_prop.csv")
all.prop <- read.csv("all_prop.csv") # issue with this one, gave a warning
year.prop <- read.csv("year_prop.csv")
otter.prop<- read.csv("otter_by_prop.csv")
otter.gram<- read.csv("otter_by_gram.csv", row.names=1)
ott.sum <- read.csv("otter_sum.csv", row.names=1)
ott.raw <- read.csv("2018_Foraging_data_RAW.csv")
s.gram<- read_csv("sex_table.csv")
prey <- read.csv("Prey_Class.csv")
sex.prey <- read.csv("sex_prey.csv")

# stack the "all" data
all <- stack(all.prop)
names(all) <- c("prop", "species")

# to make the classes order in the way I want
as.factor()

## add a prey class into raw data
#ott.raw$PreyItem%%select()

ott.raw$PreyCat <- NA
ott.raw$PreyCat <- ifelse(ott.raw$PreyItem == "APC" | ott.raw$PreyItem == "CUC" |  ott.raw$PreyItem == "CUM", 
                   "Cucumber", ifelse(ott.raw$PreyItem == "CLA" | ott.raw$PreyItem == "CLN" |  
                   ott.raw$PreyItem == "GAC" | ott.raw$PreyItem == "MAN" | ott.raw$PreyItem == "MAP" | 
                   ott.raw$PreyItem == "MAS" | ott.raw$PreyItem == "MYA" | ott.raw$PreyItem == "MYS" | 
                   ott.raw$PreyItem == "MYT" | ott.raw$PreyItem == "PRS" | ott.raw$PreyItem == "SAG" | 
                   ott.raw$PreyItem == "TRC", "Clam", ifelse(ott.raw$PreyItem == "STF" | 
                   ott.raw$PreyItem == "URC" | ott.raw$PreyItem == "STD" | ott.raw$PreyItem == "STP",
                   "Urchin", ifelse(ott.raw$PreyItem == "CAM" | ott.raw$PreyItem == "CAN" |
                   ott.raw$PreyItem == "CAP" | ott.raw$PreyItem == "CRA" | ott.raw$PreyItem == "KCR" | 
                   ott.raw$PreyItem == "PUP" | ott.raw$PreyItem == "PUS" | ott.raw$PreyItem == "TEC", "Crab",
                   ifelse(ott.raw$PreyItem == "CEF" | ott.raw$PreyItem == "SNA", "Snail", 
                   ifelse(ott.raw$PreyItem == "MUS" | ott.raw$PreyItem == "MTR" | ott.raw$PreyItem == "MOM", 
                   "Mussel", ifelse(ott.raw$PreyItem == "PIO" | ott.raw$PreyItem == "EVT" | 
                   ott.raw$PreyItem == "PES", "Star", "")))))))

# checking the data frame
is.data.frame(all) # will say if this is a dataframe, should say TRUE
dim(s.prop)
names(s.gram)
str(s.prop)
str(age.prop)
str(year.prop)
head(otter.gram) 


#sort the data (using dplyr)
all<-arrange(all, desc(prop))
age.prop<-arrange(age.prop, desc(prop))
s.prop<-arrange(s.prop, desc(prop))
year.prop<-arrange(year.prop, desc(prop))

# ALL BAR GRAPH # -- I can't figure out how to sort largest to smallest
ggplot(data = all) +
  theme_classic() +
  geom_col( 
    mapping= aes(x = species, y = prop)
  )
  
# AGE BAR GRAPH #
ggplot(data = age.prop) +
  theme_classic() +
  geom_col( 
    mapping= aes(x = species, y = prop, fill = age),
    position = "dodge"
  )

# SEX #
ggplot(data = s.prop) + 
  theme_classic() +
  geom_col( 
    mapping= aes(x = species, y = prop, fill = sex),
    position = "dodge"
  )

# YEAR # The area list isn't working because it is a number...
ggplot(data = year.prop) +
  theme_classic() +
  geom_col( 
    mapping= aes(x = species, y = prop, fill = area),
    position = "dodge"
  )

# graph for Clam size vs year
ggplot(data= filter(ott.raw, PreyCat=="Clam")) +
  theme_classic() + labs(y = "Clam Size (mm)", x= "Survey Year") +
  geom_point(mapping = aes(x= YEAR, y= Size), 
           position = "jitter")
# graph for SAG size vs region
ggplot(data= filter(ott.raw, PreyCat=="Clam")) +
  theme_classic() + 
  geom_point(mapping = aes(x= Size, y= Region),
             position = "jitter")


# graph showing Clam size by "where"
ggplot(data= filter(ott.raw, PreyCat == "Clam" & where == "KC"| where =="OW")) +
  theme_classic() +
  geom_point(mapping = aes(x= where, y= Size),
             position = "jitter")

ggplot(data= filter(ott.raw, PreyItem!="UNK")) +
  theme_classic() +
  geom_point(mapping = aes(x= where, y= PreyItem),
             position = "jitter", na.rm=T)

ggplot(data= ott.raw) +
  theme_classic() +
  geom_col(mapping = aes(x= PreyCat, y= Size, fill=where),
             position = "dodge", na.rm=T)
## Stat for Sex differences ##
s.prop.aov <-aov(prop~species+sex, s.prop)
summary(s.prop.aov)

sex.prey<- na.omit(sex.prey)
chisq.test(sex.prey)
########## Prey by otter ##############
# USING OTTER_SUM AND OTTER_BY_GRAM

#make sure ott sum and ott gram are equal
identical(row.names(ott.sum), row.names(otter.gram))

# make one data frame
otter <- cbind(otter.gram, Ageclass = ott.sum$Ageclass, Sex = ott.sum$Sex, Area = ott.sum$Area)

#pca of just prey types
otter.pca <- prcomp(otter[,1:11])
summary(otter.pca)
biplot(otter.pca)
otter.pca

ott.pc <- cbind(otter, otter.pca$x[,1:2])  # Save PC scores 
head(ott.pc)

# none of these graphs are interesting. Doesn't say anything about the variables
ggplot(ott.pc, aes(PC1, PC2, color=factor(Ageclass))) +
  geom_point(size=5)

ggplot(ott.pc, aes(PC1, PC2, color=factor(Area))) +
  geom_point(size=5)

ggplot(ott.pc, aes(PC1, PC2, color=factor(Sex))) +
  geom_point(size=5)



