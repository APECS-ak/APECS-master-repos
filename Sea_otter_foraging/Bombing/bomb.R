#### BOMBING DATA #####
# Preliminary Analysis#

setwd("/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging")


#load programs
library(ggplot2)
library(lattice)
library(dplyr)

#load files
bomb.test <- read.csv("Bombing/bomb_test.csv")
summary <- read.csv("Bombing/summary.csv")

ggplot(data=bomb.test, aes(x=KJ)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(Species))

#reduce bomb file to just clam
bomb.clam<- filter(bomb.test, Species== "cln" | Species == "sag" | Species == "prs")
bomb.crab<- filter(bomb.test, Species== "cam" | Species == "cao" | Species== "cap" | Species== "tec")

#anova
clam.aov <- aov(KJ~Season + Site + Species, data = bomb.clam)
summary(clam.aov)

crab.aov <- aov(KJ~Season + Site + Species, data = bomb.crab)
summary(crab.aov)
