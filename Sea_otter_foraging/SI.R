### SI ###

setwd("/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging")


#load programs
library(ggplot2)
library(lattice)
library(dplyr)

library(car)
library(compute.es)
library(effects)
library(multcomp)
library(pastecs)
library(HH)
library(psych)
library(RVAideMemoire)
library(rcompanion)

#load files
si.test <- read.csv("SI/SI.csv")

#until final data is run, delete rows 205-272
si.test<-si.test[-c(205:272), ] 

#make new line with overall prey cat
si.test$PreyCat <- NA
si.test$PreyCat <- ifelse(si.test$Species == "apc", "Cucumber", 
                   ifelse(si.test$Species == "cln" | si.test$Species == "prs" | si.test$Species == "sag", "Clam", 
                   ifelse(si.test$Species == "cam" | si.test$Species == "cap" | si.test$Species == "cao" | 
                          si.test$Species == "tec", "Crab", 
                   ifelse(si.test$Species == "cef" | si.test$Species == "tes" | si.test$Species == "nul", "Snail", 
                   ifelse(si.test$Species == "pio" | si.test$Species == "evt", "Star",
                   ifelse(si.test$Species == "std" | si.test$Species == "stf", "Urchin",
                   ifelse(si.test$Species == "mtr", "Mussel","")))))))

### Fixing C for high fat critters. Anything above 3.5:1 ratio
si.test$Cnorm <- NA
si.test$Cnorm<- ifelse(si.test$CN >= 3.5, si.test$C-3.32+(0.99*si.test$CN), si.test$C)
si.test$SiteNumber<-as.character(si.test$SiteNumber)

#load whisker data
whisker <- read.csv("SI/whiskers.csv")
whisker$OtterID<-as.character(whisker$OtterID)

# for right now, I want to remove 163532. I removed 160478 and 77298 from the CSV
oddotter<- filter(whisker, OtterID=="163532")
whisker<- filter(whisker, OtterID != "163532")

#whisker<-whisker[-c(64:68), ] #this version has extra NA rows, so deleting, may not be needed in future

#reduce si file to just clam/crab/snail/urchin
si.clam<- filter(si.test, Species== "cln" | Species == "sag" | Species == "prs")
si.crab<- filter(si.test, Species== "cam" | Species == "cao" | Species== "cap" | Species== "tec")
si.snail<- filter(si.test, Species =="tes" | Species == "cef" | Species == "nul")
si.urch<- filter(si.test, Species =="std" | Species == "stf")
si.star<- filter(si.test, Species =="pio" | Species == "evt")
si.mus<- filter(si.test, Species =="mtr")

#################################################################################
#####                          Prey Visualizations                          #####
#################################################################################

# plot of all species Nitrogen
ggplot(data=si.test, aes(x=N)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(vars(Species))

#plot of all species Carbon
ggplot(data=si.test, aes(x=C)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(vars(Species))

ggplot(data=si.test, aes(x=C, y=N, color=PreyCat)) + 
  geom_point() 
  
#now all normalized carbon
ggplot(data=si.test, aes(x=Cnorm)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(vars(Species))

############ Individual Prey Catagories #####################

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

#site
ggplot(data= si.clam, aes(x=Size, y=C)) +
  geom_point(aes(color=Site, shape= Species)) +
  labs(x="Size", y=expression(paste(delta^13, "C (\u2030)" ))) +
  theme_classic()

#season
ggplot(data= si.clam, aes(x=C, y=N)) +
  geom_point(aes(color=Season, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
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

#Mussel - difference in sites

ggplot(data= si.mus, aes(x=C, y=N)) +
  geom_point(aes(color=Site)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Site)) +
  theme_classic()

ggsave("mussel.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)


ggplot(data= si.mus, aes(x=C, y=N)) +
  geom_point(aes(color=Season, shape= Site)) +
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


#################################################################################
#####                                 ANOVA                                 #####
#################################################################################


#Nitrogen
clam.aov <- aov(N~Season + Site + Species + Size, data = si.clam)
summary(clam.aov)

crab.aov <- aov(N~Season + Site + Species, data = si.crab)
summary(crab.aov)

snail.aov <- aov(N~Season + Species, data = si.snail)
summary(snail.aov)

mus.aov <- aov(N~Season + Site, data = si.mus)
summary(mus.aov)

urch.aov <- aov(N~Season + Site + Species, data = si.urch)
summary(urch.aov)

#Only GONAD? Not Enough samples!! 
si.star2<-filter(si.star, Tissue == "gonads")
star.aov <- aov(N~Season + Site, data = si.star2)
summary(star.aov)

#Species is significant for clams and crabs, but crabs are only 0.03
#site is also sigg for clams

#anova for Carbon
clam.aov <- aov(C~Season + Site + Species + Size, data = si.clam)
summary(clam.aov)

crab.aov <- aov(C~Season + Site + Species, data = si.crab)
summary(crab.aov)

snail.aov <- aov(C~Season + Species, data = si.snail)
summary(snail.aov)

mus.aov <- aov(C~Season + Site, data = si.mus)
summary(mus.aov)

urch.aov <- aov(C~Season + Site + Species, data = si.urch)
summary(urch.aov)

star.aov <- aov(C~Season + Site + Species, data = si.star)
summary(star.aov)

#Species is NOT significant for clams or crabs, but site is significant for both

#all species Carbon by site
siC.aov <- aov(C~Species*Site, data = si.test)
summary(siC.aov)

siN.aov <- aov(N~Species*Site, data = si.test)
summary(siN.aov)

plot(si.test$Species, si.test$N)
plot(si.test$Species, si.test$C)


######################################################################
##                     Carbon Normalization                         ##  
######################################################################

#Making mean and min/max for each prey type using Cnorm
si.mean <-si.test %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(Cnorm), Nmean=mean(N), Cmin=min(Cnorm), Cmax= max(Cnorm), 
            Csd=sd(Cnorm), Nsd=sd(N), Cse=sd(Cnorm)/sqrt(length(Cnorm)), Nse=sd(N)/sqrt(length(N)))
si.mean<-si.mean[-1,] #removing blank
si.mean<-si.mean[-6,] #removing stars

#prey with SE as errorbars
ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Cse,xmax = Cmean+Cse, color= PreyCat), height=0) +
  theme_classic()

ggsave("prey.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)


#Now looking at just the summer values 
si.summer<-filter(si.test, Season=="Summer")
si.mean2 <-si.summer %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(Cnorm), Nmean=mean(N), Cmin=min(Cnorm), Cmax= max(Cnorm), 
            Csd=sd(Cnorm), Nsd=sd(N), Cse=sd(Cnorm)/sqrt(length(Cnorm)), Nse=sd(N)/sqrt(length(N)))
si.mean2<-si.mean2[-1,]

ggplot(data= si.mean2, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Cse,xmax = Cmean+Cse, color= PreyCat), height=0) +
  theme_classic()

#Now looking at just the Craig values 
si.craig<-filter(si.test, Site=="Craig")
si.mean3 <-si.craig %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(Cnorm), Nmean=mean(N), Cmin=min(Cnorm), Cmax= max(Cnorm), 
            Csd=sd(Cnorm), Nsd=sd(N), Cse=sd(Cnorm)/sqrt(length(Cnorm)), Nse=sd(N)/sqrt(length(N)))

ggplot(data= si.mean3, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Cse,xmax = Cmean+Cse, color= PreyCat), height=0) +
  theme_classic()

#Now changing the SB sites according to mussels
mussel<-filter(si.test, PreyCat=="Mussel")
Cmussel<-filter(mussel, Site=="Craig")
Smussel<-filter(mussel, Site=="Soda Bay")

(mean(Smussel$Cnorm))#  -18.32564
(mean(Smussel$N))#  8.206364
(mean(Cmussel$Cnorm))#  -16.52315
(mean(Cmussel$N))#  9.426667
(mus.n<-mean(Cmussel$N)-mean(Smussel$N))#  1.220303
(mus.c<-mean(Cmussel$Cnorm)-mean(Smussel$Cnorm))#1.802491

si.test$Cadjust<-NA
si.test$Nadjust<-NA
si.test$Cadjust<-ifelse(si.test$Site=="Soda Bay",si.test$Cnorm+mus.c,si.test$Cnorm)
si.test$Nadjust<-ifelse(si.test$Site=="Soda Bay",si.test$N+mus.n,si.test$N)

si.mean <-si.test %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(Cadjust), Nmean=mean(Nadjust), Csd=sd(Cadjust), Nsd=sd(Nadjust), 
            Cse=sd(Cadjust)/sqrt(length(Cadjust)), Nse=sd(Nadjust)/sqrt(length(Nadjust)))
si.mean<-si.mean[-1,] #removing blank
si.mean<-si.mean[-6,] #removing stars

ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmean-Nsd, ymax = Nmean+Nsd, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Csd,xmax = Cmean+Csd, color= PreyCat), height=0) +
  theme_classic()
#############################################################################
##                      Trophic Descrimination                             ##                
#############################################################################

#changing whiskers for TDF values that are listed in Tyrell paper
#makes the values way too low. Deleted.

#Adding Mean, SD, AND SE
whis.mean<-whisker %>%
  group_by(OtterID) %>%
  summarise(Cmean=mean(C), Nmean=mean(N), Csd=sd(C), Nsd=sd(N), 
            Cse=sd(C)/sqrt(length(C)), Nse=sd(N)/sqrt(length(N)))

#TDF for otters changes to 2/3.5 makes a better looking graph
whis.mean$TDFC<-NA; whis.mean$TDFN<-NA
whis.mean$TDFC <- whis.mean$Cmean-2
whis.mean$TDFN <- whis.mean$Nmean-2.5


ggplot(data= whis.mean, aes(x=TDFC, y=TDFN)) +
  geom_point() +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = TDFN-Nse,ymax = TDFN+Nse), width=0) + 
  geom_errorbarh(aes(xmin = TDFC-Cse,xmax = TDFC+Cse), height=0) +
  theme_classic()

#putting it all together
ggplot() +
  geom_point(data= si.mean, aes(x=Cmean, y=Nmean, color=PreyCat), size=3) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  scale_color_discrete(name  ="Prey Group") +
  geom_errorbar(data= si.mean, aes(x=Cmean, y=Nmean, ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Cmean, y=Nmean, xmin = Cmean-Cse,xmax = Cmean+Cse, color= PreyCat), height=0) +
  geom_point(data=whis.mean, aes(x=TDFC, y=TDFN), size=2)+
  geom_errorbar(data=whis.mean, aes(x=TDFC, y=TDFN, ymin = TDFN-Nse, ymax = TDFN+Nse), width=0) + 
  geom_errorbarh(data=whis.mean, aes(x=TDFC, y=TDFN, xmin = TDFC-Cse, xmax = TDFC+Cse), height=0) +
  theme_few()

ggsave("whis_preySE.png", device = "png", path = "SI/", width = 8, 
       height = 7, units = "in", dpi = 300)

#now without error bars for otters (all points separate) too many points!!
ggplot() +
  geom_point(data= si.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= si.mean, aes(x=Cmean, y=Nmean, ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Cmean, y=Nmean, xmin = Cmean-Cse,xmax = Cmean+Cse, color= PreyCat), height=0) +
  geom_point(data=whisker, aes(x=C-2.5, y=N-3))+
  scale_color_brewer(palette="Dark2")+
  theme_classic()

#Want to add otters as another color, but need to change the pallet.

ggsave("whis_prey_nobars_points.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

#Ellipses instead of error bars - this isn't working...
ggplot() +
  geom_point(data= si.mean, aes(x=Cmean, y=Nmean, color=PreyCat), size=3) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  stat_ellipse(data=si.mean, aes(x=Cmean, y=Nmean, fill=PreyCat), type="euclid", geom="polygon") +
  geom_point(data=whis.mean, aes(x=TDFC, y=TDFN), size=2)+
  geom_errorbar(data=whis.mean, aes(x=TDFC, y=TDFN, ymin = TDFN-Nse, ymax = TDFN+Nse), width=0) + 
  geom_errorbarh(data=whis.mean, aes(x=TDFC, y=TDFN, xmin = TDFC-Cse, xmax = TDFC+Cse), height=0) +
  theme_classic()

#ODD otter on top of prey graph

ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmean-Nsd, ymax = Nmean+Nsd, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Csd, xmax = Cmean+Csd, color= PreyCat), height=0) +
  geom_point(data=oddotter, x=oddotter$C-2, y=oddotter$N-3.5) +
  xlim(-22.5,-9) + ylim(4,14.5) +
  theme_classic()

ggsave("odd_otter.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

########################################################################
#####                            WHISKERS                         #####
########################################################################

#Whisker Graphs
ggplot(data= whisker, aes(x=distance, y=C)) +
  geom_line(aes(color=OtterID)) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^13, "C (\u2030)" )))  +
  theme_classic()

ggsave("whis_carbon.png", device = "png", path = "SI/", width = 7, 
       height = 8, units = "in", dpi = 300)

ggplot(data=whisker, aes(x=distance, y=N)) +
  geom_line(aes(color=OtterID)) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  theme_classic()

ggsave("whis_nitrogen.png", device = "png", path = "SI/", width = 7, 
       height = 8, units = "in", dpi = 300)

#Now all otters in their own plot
#creating a secondary axis = sec.axis
ggplot(data=whisker) +
  geom_line(aes(x=distance, y=N, color="N")) +
  geom_line(aes(x=distance, y=C+28, color="C")) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )), 
       colour = "Isotope")  +
  scale_y_continuous(sec.axis = sec_axis(~.-28, 
       name = expression(paste(delta^13, "C (\u2030)" )))) +
  facet_wrap(vars(OtterID), nrow=5) +
  theme_light()

ggsave("whiskers.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

#Looking at seasonality
ggplot(data=whisker) +
  geom_point(aes(x=Season, y=N, color="N")) +
  geom_point(aes(x=Season, y=C+28, color="C")) +
  labs(x= "Season", 
       y=expression(paste(delta^15, "N (\u2030)" )), 
       colour = "Isotope")  +
  scale_y_continuous(sec.axis = sec_axis(~.-28, 
                        name = expression(paste(delta^13, "C (\u2030)" )))) +
  facet_wrap(vars(OtterID), nrow=5) +
  theme_light()

#Anova for season Carbon
#this season 

whisker$Season<-as.factor(whisker$Season)
whisker$OtterID<-as.factor(whisker$OtterID)


season.aov <- aov(C~Season + OtterID + Season:OtterID, data = whisker)
summary(season.aov)

#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#  Season          3  11.88   3.959   8.985 2.72e-05 ***
#  OtterID        16 115.30   7.206  16.353  < 2e-16 ***
#  Season:OtterID 45  48.20   1.071   2.431 0.000151 *** 
#  Residuals      94  41.42   0.441 

season.aov <- aov(N~Season + OtterID + Season:OtterID, data = whisker)
summary(season.aov)

#                 Df Sum Sq Mean Sq F value  Pr(>F)    
#  Season          3   3.40   1.133   5.347 0.00191 ** 
#  OtterID        16  54.71   3.419  16.139 < 2e-16 ***
#  Season:OtterID 45  16.41   0.365   1.721 0.01402 *  
#  Residuals      94  19.92   0.212 

Cdistance.lm <-lm(C~factor(OtterID)*distance, data = whisker)
sum<-summary(Cdistance.lm)
Cdistance.aov <- aov(Cdistance.lm)
summary(Cdistance.aov)


#                   Df Sum Sq Mean Sq F value   Pr(>F)    
#  distance          1   0.21   0.214   0.371 0.54364     
#  OtterID          16 121.05   7.566  13.102 < 2e-16 ***
#  distance:OtterID 16  23.35   1.459   2.527 0.00214 **   
#  Residuals       125  72.18   0.577 

Anova(distance.aov, type="III") 
posth=glht(distance.aov, linfct=mcp(factorvariable="Tukey"))  ##gives the post-hoc Tukey analysis
summary(posth)

ancova(C~OtterID*distance, whisker)
y.hat<-fitted.values(Cdistance.lm)
e<-residuals(Cdistance.lm)
h<-hatvalues(Cdistance.lm)
r<-e/(sum$sigma*sqrt(1-h))
d<-rstudent(Cdistance.lm)

plot(e~y.hat,xlab="Fitted Values", ylab="Residuals")
stripchart(data= whisker, e~OtterID,vertical=T, method="jitter", xlab="treatment", ylab="Residuals")
plot(e~jitter(y.hat))
qqnorm(r,main=NULL); abline(a=0,b=1,lty=3)
qq.cortest(r,0.05) #not normal
shapiro.test(r) #not normal

#Mood's test allows for an ANOVA like analysis for non-normal data
mood.medtest(C ~ OtterID,
             data  = whisker,
             exact = FALSE)

### Order groups by median
#?? whisker$OtterID = factor(whisker$OtterID)
### Pairwise median tests
PT = pairwiseMedianTest(C ~ OtterID,
                        data   = whisker,
                        exact  = NULL,
                        method = "fdr")
# Adjusts p-values for multiple comparisons;
# See ?p.adjust for options

#Letter version to show who is different
cldList(p.adjust ~ Comparison,
        data = PT,
        threshold = 0.05)

Sig<-filter(PT,p.adjust<=.05)

PT

library(multcompView)

PT = pairwiseMedianMatrix(C ~ OtterID,
                          data   = whisker,
                          exact  = NULL,
                          method = "fdr")

PT

library(multcompView)

multcompLetters(PT$Adjusted,
                compare="<",
                threshold=0.05,
                Letters=letters)

write.csv(PT$Adjusted, "Significant.csv")

Ndistance.lm <-lm(N~factor(OtterID)*distance, data = whisker)
summary(Ndistance.lm)
Ndistance.aov <- aov(Ndistance.lm)
summary(Ndistance.aov)

#                   Df Sum Sq Mean Sq F value   Pr(>F)    
#  distance          1   5.29   5.285  21.374 9.28e-06 ***  
#  OtterID          16  53.03   3.314  13.403  < 2e-16 ***
#  distance:OtterID 16   5.22   0.326   1.318    0.196    
#  Residuals       125  30.91   0.247    

ancova(N~OtterID*distance, whisker)
y.hat<-fitted.values(Ndistance.lm)
e<-residuals(Ndistance.lm)
h<-hatvalues(Ndistance.lm)
r<-e/(sum$sigma*sqrt(1-h))
d<-rstudent(Ndistance.lm)

plot(e~y.hat,xlab="Fitted Values", ylab="Residuals")
stripchart(data= whisker, e~OtterID,vertical=T, method="jitter", xlab="treatment", ylab="Residuals")
plot(e~jitter(y.hat))
qqnorm(r,main=NULL); abline(a=0,b=1,lty=3)
qq.cortest(r,0.05) #normal
shapiro.test(r) #normal


#LS means package
#install.packages("lsmeans")
library(lsmeans)
leastsquare = lsmeans(Cdistance.lm,
                      C ~ OtterID:distance,
                      adjust = "tukey")
leastsquare$contrasts
cld(leastsquare)


ggplot(data=whisker) +
  geom_boxplot(aes(x=Season, y=N)) +
  labs(x= "Season", 
       y=expression(paste(delta^15, "N (\u2030)")))  +
  theme_classic()

ggplot(data=whisker) +
  geom_boxplot(aes(x=Season, y=C+28)) +
  labs(x= "Season", 
       y=expression(paste(delta^13, "C (\u2030)" )))  +
  theme_classic()

whisker$distance<-as.character(whisker$distance)
whis.mean2<-whisker %>%
  group_by(distance) %>%
  summarise(Cmean=mean(C), Nmean=mean(N), Csd=sd(C), Nsd=sd(N))

ggplot(data= whis.mean2, aes(x=distance, y=Cmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Cmean-Csd,ymax = Cmean+Csd), width=.2) + 
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^13, "C (\u2030)" )))  +
  theme_classic()

ggplot(data= whis.mean2, aes(x=distance, y=Nmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Nmean-Nsd,ymax = Nmean+Nsd), width=.2) + 
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  theme_classic()

#variability

#For individual otters?
#Carbon
Cdist<-filter(whisker, OtterID == "163520")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 6.251 on 1 and 10 DF,  p-value: 0.03143
Cdist<-filter(whisker, OtterID == "163521")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.1071 on 1 and 10 DF,  p-value: 0.7502
Cdist<-filter(whisker, OtterID == "163522")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 14.04 on 1 and 9 DF,  p-value: 0.004578
Cdist<-filter(whisker, OtterID == "160523")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.1362 on 1 and 8 DF,  p-value: 0.7217
Cdist<-filter(whisker, OtterID == "163524")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.9172 on 1 and 7 DF,  p-value: 0.3701
Cdist<-filter(whisker, OtterID == "163533")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.009771 on 1 and 6 DF,  p-value: 0.9245
Cdist<-filter(whisker, OtterID == "163534")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 1.741 on 1 and 7 DF,  p-value: 0.2286
Cdist<-filter(whisker, OtterID == "163535")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.5315 on 1 and 9 DF,  p-value: 0.4845
Cdist<-filter(whisker, OtterID == "160479")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 13.54 on 1 and 7 DF,  p-value: 0.007857
Cdist<-filter(whisker, OtterID == "160480")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 16.19 on 1 and 7 DF,  p-value: 0.005038
Cdist<-filter(whisker, OtterID == "163525")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 22.44 on 1 and 9 DF,  p-value: 0.001064
Cdist<-filter(whisker, OtterID == "163526")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 2.185 on 1 and 8 DF,  p-value: 0.1776
Cdist<-filter(whisker, OtterID == "163527")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 1.623 on 1 and 7 DF,  p-value: 0.2434
Cdist<-filter(whisker, OtterID == "163528")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 1.841 on 1 and 8 DF,  p-value: 0.2118
Cdist<-filter(whisker, OtterID == "163529")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 17.11 on 1 and 6 DF,  p-value: 0.006105
Cdist<-filter(whisker, OtterID == "163530")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.4251 on 1 and 7 DF,  p-value: 0.5352
Cdist<-filter(whisker, OtterID == "77280")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.2358 on 1 and 12 DF,  p-value: 0.636
Cdist<-filter(whisker, OtterID == "77281")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.3106 on 1 and 7 DF,  p-value: 0.5947
Cdist<-filter(whisker, OtterID == "77284")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 1.061 on 1 and 10 DF,  p-value: 0.3273

#Nitrogen
Ndist<-filter(whisker, OtterID == "163520")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 16.67 on 1 and 10 DF,  p-value: 0.002205
Ndist<-filter(whisker, OtterID == "163521")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.1572 on 1 and 10 DF,  p-value: 0.7001
Ndist<-filter(whisker, OtterID == "163522")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.415 on 1 and 9 DF,  p-value: 0.5355
Ndist<-filter(whisker, OtterID == "160523")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 6.886 on 1 and 8 DF,  p-value: 0.03045
Ndist<-filter(whisker, OtterID == "163524")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 7.703 on 1 and 7 DF,  p-value: 0.02748
Ndist<-filter(whisker, OtterID == "163533")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.9326 on 1 and 6 DF,  p-value: 0.3715
Ndist<-filter(whisker, OtterID == "163534")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.7286 on 1 and 7 DF,  p-value: 0.4216
Ndist<-filter(whisker, OtterID == "163535")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 26.84 on 1 and 9 DF,  p-value: 0.0005785
Ndist<-filter(whisker, OtterID == "160479")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 38.57 on 1 and 7 DF,  p-value: 0.0004407
Ndist<-filter(whisker, OtterID == "160480")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.3462 on 1 and 7 DF,  p-value: 0.5748
Ndist<-filter(whisker, OtterID == "163525")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 1.64 on 1 and 9 DF,  p-value: 0.2323
Ndist<-filter(whisker, OtterID == "163526")
Ndist.lm<-lm(N~distance, data = Cdist)
summary(Ndist.lm)
#F-statistic: 7.703 on 1 and 7 DF,  p-value: 0.02748
Ndist<-filter(whisker, OtterID == "163527")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.08946 on 1 and 7 DF,  p-value: 0.7736
Ndist<-filter(whisker, OtterID == "163528")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 66.41 on 1 and 8 DF,  p-value: 3.821e-05
Ndist<-filter(whisker, OtterID == "163529")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 5.212 on 1 and 6 DF,  p-value: 0.06255
Ndist<-filter(whisker, OtterID == "163530")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 2.245 on 1 and 7 DF,  p-value: 0.1778
Ndist<-filter(whisker, OtterID == "77280")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.3143 on 1 and 12 DF,  p-value: 0.5854
Ndist<-filter(whisker, OtterID == "77281")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 19.58 on 1 and 7 DF,  p-value: 0.003061
Ndist<-filter(whisker, OtterID == "77284")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.005928 on 1 and 10 DF,  p-value: 0.9401


Cnotsig<-filter(whisker, OtterID != "163520" & OtterID != "163522" & OtterID != "160479" & 
                  OtterID != "160480" & OtterID != "163525" & OtterID != "163529")

Csig<-filter(whisker, OtterID == "163520" | OtterID == "163522" | OtterID == "160479" | 
                  OtterID == "160480" | OtterID == "163525" | OtterID == "163529")

Nnotsig<-filter(whisker, OtterID != "163520" & OtterID != "163523" & OtterID != "160479" & 
                  OtterID != "163535" & OtterID != "163526" & OtterID != "163528" & OtterID != "77281")

Nsig<-filter(whisker, OtterID == "163520" | OtterID == "163523" | OtterID == "160479" | 
                  OtterID == "163535" | OtterID == "163526" | OtterID == "163528" | OtterID == "77281")

#Shortening to only 8cm max
whisker.short<-filter(whisker, distance <= 8)

ggplot(data=whisker.short, aes(x=distance, y=C)) +
  geom_line(aes(group=OtterID)) +
  geom_smooth(aes(color=Csig), size=2, span=0.5, se= FALSE) 

ggplot(data=whisker.short, aes(x=distance, y=N)) +
  geom_line(aes(group=OtterID)) +
  geom_smooth(aes(color=Nsig), size=2, span=0.5, se= FALSE) 


ggplot(data=whisker.short, aes(x=distance, y=C)) +
  geom_line(aes(group=OtterID), color = "Gray81") +
  geom_smooth(aes(color=Csig), size=3, span=0.8, se= FALSE, show.legend = FALSE) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^13, "C (\u2030)" )), tag = "A")  +
  theme_few()

ggsave("C_sig.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)

ggplot(data=whisker.short, aes(x=distance, y=N)) +
  geom_line(aes(group=OtterID), color = "Gray81") +
  geom_smooth(aes(color=Nsig), size=3, span=0.8, se= FALSE, show.legend = FALSE) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )), element_text(size = 22), tag = "B")  +
  theme_few()

ggsave("N_sig.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)
