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

# stack the "all" data
all <- stack(all.prop)
names(all) <- c("prop", "species")


# checking the data frame
is.data.frame(all) # will say if this is a dataframe, should say TRUE
dim(s.prop)
names(s.prop)
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
  geom_col( 
    mapping= aes(x = species, y = prop)
  )
  
# AGE BAR GRAPH #
ggplot(data = age.prop) +
  geom_col( 
    mapping= aes(x = species, y = prop, fill = age),
    position = "dodge"
  )

# SEX #
ggplot(data = s.prop) +
  geom_col( 
    mapping= aes(x = species, y = prop, fill = sex),
    position = "dodge"
  )

# YEAR # The area list isn't working because it is a number...
ggplot(data = year.prop) +
  geom_col( 
    mapping= aes(x = species, y = prop, fill = area),
    position = "dodge"
  )

# Prey by otter # USING OTTER_SUM AND OTTER_BY_GRAM

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
