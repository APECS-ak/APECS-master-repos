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

#add prey catagory
bomb.test$PreyCat <- NA
bomb.test$PreyCat <- ifelse(bomb.test$Species == "apc", "Cucumber", 
                          ifelse(bomb.test$Species == "cln" | bomb.test$Species == "prs" | bomb.test$Species == "sag", "Clam", 
                                 ifelse(bomb.test$Species == "cam" | bomb.test$Species == "cap" | bomb.test$Species == "cao" | 
                                          bomb.test$Species == "tec"| bomb.test$Species == "pas", "Crab", ifelse(bomb.test$Species == "cef" | 
                                                    bomb.test$Species == "tes" | bomb.test$Species == "nul", "Snail", 
                                                    ifelse(bomb.test$Species == "pio" | bomb.test$Species == "evt", "Star", 
                                                           ifelse(bomb.test$Species == "stf"| bomb.test$Species == "std", "Urchin",  ""))))))

#Histogram of all KJ by species - hard to really see what is going on
ggplot(data=bomb.test, aes(x=KJ)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(Species))

#Graph of KJ by season separated by species and prey cat (not a good depiction of this)
ggplot(data= bomb.test) +
  geom_point(aes(x=Season, y=KJ, color=Species, shape=PreyCat)) +
  labs(x="Season", y="KJ (g)") +
  theme_classic()

#Graph of KJ by season only by prey cat (Still not a good depiction of the data)
ggplot(data= bomb.test) +
  geom_point(aes(x=Season, y=KJ, color=PreyCat, shape=PreyCat)) +
  labs(x="Season", y="KJ (g)") +
  theme_classic()

#KJ for each species by season
ggplot(data= bomb.test, aes(y=KJ, x=Season)) +
  geom_point() +
  labs(x="Season", y="KJ (g)") +
  facet_wrap(vars(Species))

#KJ by prey type for each season
ggplot(data= bomb.test, aes(y=KJ, x=Season)) +
  geom_point() +
  labs(x="Season", y="KJ (g)") +
  facet_wrap(vars(PreyCat))

#looking at just snails
bomb.snail <- filter(bomb.test, PreyCat == "Snail")
plot(y=bomb.snail$KJ,x=bomb.snail$Season)


#reduce bomb file to just clam
bomb.clam<- filter(bomb.test, Species== "cln" | Species == "sag" | Species == "prs")
bomb.crab<- filter(bomb.test, Species== "cam" | Species == "cao" | Species== "cap" | Species== "tec")

#anova
clam.aov <- aov(KJ~Season + Site + Species, data = bomb.clam)
summary(clam.aov)

crab.aov <- aov(KJ~Season + Site + Species, data = bomb.crab)
summary(crab.aov)

#looking at SAG KJ
ggplot(data=filter(bomb.clam, Species == "sag"), aes(x=KJ)) + 
  geom_histogram(binwidth = .5)

#SAG by size
sag.size<-filter(bomb.test, Species == "sag")
plot(sag.size$size, sag.size$KJ)

#crab by size
plot(bomb.crab$size, bomb.crab$KJ)

#clam by size
plot(bomb.clam$size, bomb.clam$KJ)

