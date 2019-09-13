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
                          si.test$Species == "tec", "Crab", 
                   ifelse(si.test$Species == "cef" | si.test$Species == "tes" | si.test$Species == "nul", "Snail", 
                   ifelse(si.test$Species == "pio" | si.test$Species == "evt", "Star",
                   ifelse(si.test$Species == "std" | si.test$Species == "stf", "Urchin",""))))))

#reduce si file to just clam/crab/snail/urchin
si.clam<- filter(si.test, Species== "cln" | Species == "sag" | Species == "prs")
si.crab<- filter(si.test, Species== "cam" | Species == "cao" | Species== "cap" | Species== "tec")
si.snail<- filter(si.test, Species =="tes" | Species == "cef" | Species == "nul")
si.urch<- filter(si.test, Species =="std" | Species == "stf")
si.star<- filter(si.test, Species =="pio" | Species == "evt")

# plot of all species Nitrogen
ggplot(data=si.test, aes(x=N)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(vars(Species))

#plot of all species Carbon
ggplot(data=si.test, aes(x=C)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(vars(Species))

#Biplot with catagories
ggplot(data= si.test, aes(x=C, y=N)) +
  geom_point(aes(color=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=PreyCat)) +
  theme_classic()

ggplot(data= si.test, aes(x=C, y=N)) +
  geom_point(aes(color=PreyCat, shape= Site)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=PreyCat)) +
  theme_classic()
  
#CLAM - CLN different,
#       Sites differ, but nossuk overlaps SB and Craig
ggplot(data= si.clam, aes(x=C, y=N)) +
  geom_point(aes(color=Species, shape= Site)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Species)) +
  theme_classic()

ggplot(data= si.clam, aes(x=C, y=N)) +
  geom_point(aes(color=Site, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Site)) +
  theme_classic()

ggplot(data= si.clam, aes(x=Size, y=C)) +
  geom_point(aes(color=Site, shape= Species)) +
  labs(x="Size", y=expression(paste(delta^13, "C (\u2030)" ))) +
  theme_classic()

#CRAB  - Nossuk and Soda Bay overlap
ggplot(data= si.crab, aes(x=C, y=N)) +
  geom_point(aes(color=Site, shape=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Site)) +
  theme_classic()

#URCHIN - Not enough data, but very different by site
ggplot(data= si.urch, aes(x=C, y=N)) +
  geom_point(aes(color=Season, shape=Site)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  theme_classic()

#STAR - Tissue differences
ggplot(data= si.star, aes(x=C, y=N)) +
  geom_point(aes(color=Tissue, shape=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Tissue)) +
  theme_classic()

#look at seasonal changes in one site for clams - NO DIFFERENCE for Craig
sb.si.clam <-filter(si.clam, Site=="Soda Bay")
c.si.clam <- filter(si.clam, Site=="Craig")

ggplot(data= sb.si.clam, aes(x=C, y=N)) +
  geom_point(aes(color=Season, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

ggplot(data= c.si.clam, aes(x=C, y=N)) +
  geom_point(aes(color=Season, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

#look at seasonal changes in one site for crab - Not enough data
sb.si.crab <-filter(si.crab, Site=="Soda Bay")
c.si.crab <- filter(si.crab, Site=="Craig") # not enough crabs

ggplot(data= c.si.crab, aes(x=C, y=N)) +
  geom_point(aes(color=Season, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

#anova for Nitrogen
clam.aov <- aov(N~Season + Site + Species, data = si.clam)
summary(clam.aov)

crab.aov <- aov(N~Season + Site + Species, data = si.crab)
summary(crab.aov)

#Species is significant for clams and crabs, but crabs are only 0.03
#site is also sigg for clams

#anova for Carbon
clam.aov <- aov(C~Season + Site + Species, data = si.clam)
summary(clam.aov)

crab.aov <- aov(C~Season + Site + Species, data = si.crab)
summary(crab.aov)

#Species is NOT significant for clams or crabs, but site is significant for both

#all species Carbon by site
siC.aov <- aov(C~Species*Site, data = si.test)
summary(siC.aov)

siN.aov <- aov(N~Species*Site, data = si.test)
summary(siN.aov)

plot(si.test$Species, si.test$N)
plot(si.test$Species, si.test$C)


#d13C vs CN ratio
ggplot(data= si.star, aes(x=C, y=C_N)) +
  geom_point(aes(color=Tissue, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

# C:N ratio for all species except stars  - Looks like Urchins also are very high
xstar<- filter(si.test, PreyCat != "Star" & PreyCat != "Urchin")
ggplot(data= xstar, aes(x=C, y=C_N)) +
  geom_point(aes(color=Species, shape= PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

# C:N ratio of just urchins
ggplot(data= si.urch, aes(x=C, y=C_N)) +
  geom_point(aes(color=Species, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

# C:N ratio of just clams
ggplot(data= si.clam, aes(x=C, y=C_N)) +
  geom_point(aes(color=Species, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

ggplot(data= si.crab, aes(x=C, y=N)) +
  geom_point(aes(color=Site, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  theme_classic()

## Mixing Models ##

library(MixSIAR)
browseVignettes("MixSIAR")

#load whisker data
whisker <- read.csv("SI/whiskers.csv")
whisker$OtterID<-as.character(whisker$OtterID)
whisker<-whisker[-c(64:68), ]

ggplot(data= whisker, aes(x=distance, y=C)) +
  geom_line(aes(color=OtterID)) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^13, "C (\u2030)" )))  +
  theme_classic()


ggplot(data=whisker, aes(x=distance, y=N)) +
  geom_line(aes(color=OtterID)) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  theme_classic()

#creating a secondary axis = sec.axis
ggplot(data=whisker) +
  geom_line(aes(x=distance, y=N, color="N")) +
  geom_line(aes(x=distance, y=C+28, color="C")) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )), 
       colour = "Isotope")  +
  scale_y_continuous(sec.axis = sec_axis(~.-28, 
         name = expression(paste(delta^13, "C (\u2030)" )))) +
  facet_wrap(vars(OtterID), nrow=3) +
  theme_light()
