############  FISH631 Project  ############

# I am stting the working dir at the usual foraging folder, but I may want to make a new folder
setwd("/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging")

#programs
library(ggplot2)
library(lattice)
require(stats)
library(tidyverse)
library(plyr)
library(scales)

#load files
otter.gram<- read.csv("Otter_gram.csv", row.names=1)
ott.sum <- read.csv("otter_summary.csv", row.names=1)
ott.raw <- read.csv("2018_Foraging_data_RAW.csv")

#stack otter.gram
stack.gram <- stack(otter.gram)
names(stack.gram) <- c("gram", "species")

#combine files
otter <- cbind(otter.gram, Ageclass = ott.sum$Ageclass, Sex = ott.sum$Sex, Area = ott.sum$Area)

# plot hist of all the grams/min
ggplot(data=stack.gram, aes(x=gram)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(species))

hist(otter.gram$clam)
hist(otter.gram$crab)


### Standardize to species maximum:
MAX <- apply(otter.gram,2,max)    # Maximum values in each column
gram.std <- scale(otter.gram, center=F, scale = MAX)
head(gram.std)

#stack gram.std
stack.gram.std <- stack(gram.std)
names(stack.gram.std) <- c("gram", "species")

#combine files
otter <- cbind(otter.gram, Ageclass = ott.sum$Ageclass, Sex = ott.sum$Sex, Area = ott.sum$Area)

# plot hist of all the grams/min
ggplot(data=stack.gram, aes(x=gram)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(species))

# info from ott.sum
par(mfrow= c(2,2))
plot(ott.sum$Sex, main = "Sex")
plot(ott.sum$Ageclass, main = "Age")
plot(ott.sum$Area, main = "Locations (year)")
hist(ott.sum$success_rt,  main = "Histogram of Sucess rate", xlab= "", ylab= "")
