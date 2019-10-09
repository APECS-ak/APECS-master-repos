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
                     ifelse(bomb.test$Species == "cln" | bomb.test$Species == "prs" | bomb.test$Species == "sag" |
                            bomb.test$Species == "pab", "Clam", 
                     ifelse(bomb.test$Species == "cam" | bomb.test$Species == "cap" | bomb.test$Species == "cao" | 
                            bomb.test$Species == "tec"| bomb.test$Species == "pas" | bomb.test$Species == "pup", "Crab", 
                     ifelse(bomb.test$Species == "cef" | bomb.test$Species == "tes" | bomb.test$Species == "nul" | 
                            bomb.test$Species == "lid", "Snail", 
                     ifelse(bomb.test$Species == "pio" | bomb.test$Species == "evt", "Star", 
                     ifelse(bomb.test$Species == "stf"| bomb.test$Species == "std", "Urchin",
                     ifelse(bomb.test$Species == "mtr", "Mussel",
                     ifelse(bomb.test$Species == "hak", "Abalone", 
                     ifelse(bomb.test$Species == "crs", "Chiton" ,
                     ifelse(bomb.test$Species == "crg" | bomb.test$Species == "pom" | bomb.test$Species == "chr", "Scallop", NA))))))))))

#STARS and CRABS - Want to only look at whole parts in total bomb.test analysis
bomb.star<-filter(bomb.test, PreyCat == "Star")
bomb.crab<- filter(bomb.test, PreyCat == "Crab")
bomb.clam<- filter(bomb.test, Species == "cln" | Species == "sag" | Species == "prs")
bomb.urch<- filter(bomb.test, Species == "std" | Species == "stf")
bomb.snail<- filter(bomb.test, PreyCat == "Snail")
bomb.mussel<- filter (bomb.test, PreyCat == "Mussel")
bomb.bivalve <- filter(bomb.test, PreyCat == "Clam" | PreyCat == "Scallop")
bomb.urchin <- filter (bomb.test, PreyCat == "Urchin")

bomb.test<-filter(bomb.test, Tissue == "whole" )

##########################################################################
##########################################################################
# Visualization #

#Histogram of all KJ by species - 
# hard to really see what is going on
ggplot(data=bomb.test, aes(x=KJ)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(Species))

#Graph of KJ by season separated by species and prey cat 
# (not a good depiction of this)
ggplot(data= bomb.test) +
  geom_point(aes(x=Season, y=KJ, color=Species, shape=PreyCat)) +
  labs(x="Season", y="KJ (g)") +
  theme_classic()

#Graph of KJ by season only by prey cat 
# (Still not a good depiction of the data)
ggplot(data= bomb.test) +
  geom_point(aes(x=Season, y=KJ, color=PreyCat, shape=PreyCat)) +
  labs(x="Season", y="KJ (g)") +
  theme_classic()

#### In this graph I want to draw a red line with the averages and the light bands for the SD?
#KJ for each species by season
ggplot(data= bomb.test, aes(y=KJ, x=Season)) +
  geom_point() +
  labs(x="Season", y="KJ (g)") +
  facet_wrap(vars(Species))

ggsave("bomb_all.png", device = "png", path = "Bombing/", width = 8, 
       height = 6, units = "in", dpi = 300)

####
#KJ by prey type for each season
ggplot(data= bomb.test[!is.na(bomb.test$PreyCat),], aes(y=KJ, x=Season)) +
  geom_point() +
  labs(x="Season", y="KJ (g)") +
  facet_wrap(vars(PreyCat), nrow=2)

ggsave("bomb_cat.png", device = "png", path = "Bombing/", width = 8, 
       height = 6, units = "in", dpi = 300)


#boxplot for all prey cats - I like this plot 
ggplot(data= bomb.test[!is.na(bomb.test$PreyCat),], aes(y=KJ, x=Season)) +
  geom_boxplot(aes(color = Species)) +
  facet_wrap(vars(PreyCat), ncol = 5) +
  theme(legend.position="bottom")

ggsave("bomb_cat_color.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)

#looking at just snails
plot(y=bomb.snail$KJ,x=bomb.snail$Season)

#Stars by tissue type
ggplot(data= bomb.star, aes(x=Season, y=KJ)) +
  geom_point(aes(color=Tissue, shape= Species)) +
  labs(x="Season", y="KJ per dry gram") +
  facet_wrap(vars(Tissue))

ggsave("star_tissue.png", device = "png", path = "Bombing/", width = 6, 
       height = 8, units = "in", dpi = 300)

##################################################################
##################################################################
#ANOVA#
clam.aov <- aov(KJ~Season + Site + Species + size, data = bomb.clam)
summary(clam.aov)

bomb.crab2<- filter(bomb.test, PreyCat == "Crab")
crab.aov <- aov(KJ~Season + Site + Species, data = bomb.crab2)
summary(crab.aov)

snail.aov <- aov(KJ~Season + Site + Species, data = bomb.snail)
summary(snail.aov)

mussel.aov <- aov(KJ~Season + Site, data = bomb.mussel)
summary(mussel.aov)

bomb.star2<- filter(bomb.test, PreyCat == "Star")
star.aov <- aov(KJ~Season + Site + Species, data = bomb.star2)
summary(star.aov)

bivalve.aov <- aov(KJ~Season + Site + Species, data = bomb.bivalve)
summary(bivalve.aov)

urchin.aov <- aov(KJ~Season + Site +Species, data = bomb.urchin)
summary(urchin.aov)
######################################################################
######################################################################


#looking at SAG KJ
ggplot(data=filter(bomb.clam, Species == "sag"), aes(x=KJ)) + 
  geom_histogram(binwidth = .5)

#SAG by size
sag.size<-filter(bomb.test, Species == "sag")
ggplot(data=sag.size, aes(x=size, y=KJ)) +
  geom_point() +
  labs(x="Saxidomus Width (mm)", y="KJ per dry gram") +
  theme_classic()


#clam size by KJ
ggplot(data= bomb.clam, aes(x=size, y=KJ)) +
  geom_point(aes(color=Species)) +
  labs(x="Clam Width (mm)", y="KJ per dry gram") +
  theme_classic()

#Find the problem child ( I removed this from the bomb.test file because it was redone)
#plot(bomb.clam$size, bomb.clam$KJ)
#identify(bomb.clam$size, bomb.clam$KJ) #45

#Remove the problem child from bomb.clam (but may want to remove from bomb.test at a later date)
#bomb.clam<- #### look up how to remove. I forget

#Any other problem children?
plot(bomb.crab$size, bomb.crab$KJ)
identify(bomb.crab$size, bomb.crab$KJ) #13 and 15 are low, could look at why.

plot(bomb.test$size, bomb.test$KJ)
identify(bomb.test$size, bomb.test$KJ) # low 146 and high 160 which are a pio stomach and an stf.

#crab size by KJ
ggplot(data= bomb.crab, aes(x=size, y=KJ)) +
  geom_point(aes(color=Species)) +
  labs(x="Crab Width (mm)", y="KJ per dry gram") +
  theme_classic()
