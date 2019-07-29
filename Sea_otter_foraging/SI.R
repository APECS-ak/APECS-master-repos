### SI ###

setwd("/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging")


#load programs
library(ggplot2)
library(lattice)
library(dplyr)

#load files
si.test <- read.csv("SI/SI.csv")

#make new line with overall prey cat
si.test$PreyCat <- NA
si.test$PreyCat <- ifelse(si.test$Species == "apc", "Cucumber", 
                          ifelse(si.test$Species == "cln" | si.test$Species == "prs" | si.test$Species == "sag", "Clam", 
                                        ifelse(si.test$Species == "cam" | si.test$Species == "cap" | si.test$Species == "cao" | 
                                                 si.test$Species == "tec", "Crab", ifelse(si.test$Species == "cef" | 
                                                                                            si.test$Species == "tes", "Snail", ""))))

ggplot(data=si.test, aes(x=N)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(vars(Species))

ggplot(data=si.test, aes(x=C)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(vars(Species))

ggplot(data= si.test, aes(x=C, y=N)) +
  geom_point(aes(color=Species, shape=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  theme_classic()
  
  

#reduce si file to just clam
si.clam<- filter(si.test, Species== "cln" | Species == "sag" | Species == "prs")
si.crab<- filter(si.test, Species== "cam" | Species == "cao" | Species== "cap" | Species== "tec")
si.snail<- filter(si.test, Species =="tes" | Species == "cef")

#anova for Nitrogen
clam.aov <- aov(N~Season + Site + Species, data = si.clam)
summary(clam.aov)

crab.aov <- aov(N~Season + Site + Species, data = si.crab)
summary(crab.aov)

#Species is significant for clams but not for crabs

#anova for Carbon
clam.aov <- aov(C~Season + Site + Species, data = si.clam)
summary(clam.aov)

crab.aov <- aov(C~Season + Site + Species, data = si.crab)
summary(crab.aov)

#Species is NOT significant for clams or crabs

#all species Carbon by site
siC.aov <- aov(C~Species*Site, data = si.test)
summary(siC.aov)

siN.aov <- aov(N~Species*Site, data = si.test)
summary(siN.aov)

plot(si.test$Species, si.test$N)
plot(si.test$Species, si.test$C)

