### SI ###

setwd("/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging")


#load programs
library(ggplot2) #always
library(lattice) #always
library(dplyr) #always
library(ggthemes) #for graphs 
library(car) #for stats
library(compute.es) #for stats
library(effects) 
library(multcomp) #for stats
library(pastecs)
library(HH) #for Anova (not normal one)
library(psych)
library(RVAideMemoire) # for moods
library(rcompanion) #for moods

#load files
si.test <- read.csv("SI/SI.csv")

#until final data is run, delete rows 205-272
#si.test<-si.test[-c(205:272), ] 

#make new line with overall prey cat
si.test$PreyCat <- NA
si.test$PreyCat <- ifelse(si.test$Species == "apc", "Cucumber", 
                   ifelse(si.test$Species == "cln" | si.test$Species == "prs" | si.test$Species == "sag", "Clam", 
                   ifelse(si.test$Species == "cam" | si.test$Species == "cap" | si.test$Species == "cao" | 
                          si.test$Species == "tec" | si.test$Species == "pup",  "Crab", 
                   ifelse(si.test$Species ==  "tes" , "Tegula", 
                   ifelse(si.test$Species == "cef" | si.test$Species == "nul", "Snail",
                   ifelse(si.test$Species == "pio" | si.test$Species == "evt", "Star",
                   ifelse(si.test$Species == "std" | si.test$Species == "stf", "Urchin",
                   ifelse(si.test$Species == "mtr", "Mussel",""))))))))

### Fixing C for high fat critters. Anything above 3.5:1 ratio
si.test$Cnorm <- NA
si.test$Cnorm<- ifelse(si.test$CN >= 3.5, si.test$C-3.32+(0.99*si.test$CN), si.test$C)
si.test$SiteNumber<-as.character(si.test$SiteNumber)

#load whisker data
whisker <- read.csv("SI/whiskers.csv")
whisker$OtterID<-as.factor(whisker$OtterID)

# for right now, I want to remove 163532. I removed 160478 and 77298 from the CSV
oddotter<- filter(whisker, OtterID=="163532")
whisker<- filter(whisker, OtterID != "163532")
#remove first 299, since only one right now - Temp
whisker<-whisker[-286, ] 

#Shortening to only 8cm max for graphs
whisker.short<-filter(whisker, distance <= 8)


#reduce si file to just clam/crab/snail/urchin
si.clam<- filter(si.test, Species== "cln" | Species == "sag" | Species == "prs")
si.crab<- filter(si.test, Species== "cam" | Species == "cao" | Species== "cap" | Species== "tec" | Species == "pup")
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

#Crab - species/site
ggplot(data= si.crab, aes(x=C, y=N)) +
  geom_point(aes(color=Species, shape=Site)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Species)) +
  theme_classic()

#Crab - season, mostly full overlap
ggplot(data= si.crab, aes(x=C, y=N)) +
  geom_point(aes(color=Season, shape=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

#URCHIN - Species don't differ, season doesn't differ?, site very different
ggplot(data= si.urch, aes(x=C, y=N)) +
  geom_point(aes(color=Site, shape=Season)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Site)) +
  theme_classic()

#Urchin only Craig - not enough STD!
urch.craig<-filter(si.urch, Site == "Craig")
ggplot(data= urch.craig, aes(x=C, y=N)) +
  geom_point(aes(color=Season, shape=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

#SNAIL
ggplot(data= si.snail, aes(x=C, y=N)) +
  geom_point(aes(color=Species, shape=Season)) +
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

ggplot(data= sb.si.crab, aes(x=C, y=N)) +
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


#Now looking at just the summer values - The SNAIL values change greatly!
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

(mean(Smussel$Cnorm))#  -18.40197
(mean(Smussel$N))#  8.195714
(mean(Cmussel$Cnorm))#  -16.52315
(mean(Cmussel$N))#  9.426667
(mus.n<-mean(Cmussel$N)-mean(Smussel$N))#  1.230952
(mus.c<-mean(Cmussel$Cnorm)-mean(Smussel$Cnorm))#1.878819

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
  geom_errorbar(aes(ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Cse,xmax = Cmean+Cse, color= PreyCat), height=0) +
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
            Cse=sd(C)/sqrt(length(C)), Nse=sd(N)/sqrt(length(N)), 
            Crange=(max(C)-min(C)), Nrange=max((N)-min(N)))

#TDF for otters changes to 2/3.5 makes a better looking graph
whis.mean$TDFC<-NA; whis.mean$TDFN<-NA
whis.mean$TDFC <- whis.mean$Cmean-2
whis.mean$TDFN <- whis.mean$Nmean-2.8


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
  geom_point(data=whisker, aes(x=C-2.5, y=N-3), colour="gray")+
  geom_point(data= si.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= si.mean, aes(x=Cmean, y=Nmean, ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Cmean, y=Nmean, xmin = Cmean-Cse,xmax = Cmean+Cse, color= PreyCat), height=0) +
  scale_color_brewer(palette="Dark2")+
  theme_classic()


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
  geom_errorbar(aes(ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Cse, xmax = Cmean+Cse, color= PreyCat), height=0) +
  geom_point(data=oddotter, x=oddotter$C-2, y=oddotter$N-3.5) +
  xlim(-22.5,-9) + ylim(4,14.5) +
  theme_classic()

ggsave("odd_otter.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)


#now looking at just the standard deviations of the whiskers
ggplot(data= whis.mean, aes(x=Csd, y=Nsd)) +
  geom_point() +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
 # geom_smooth(method="lm") +
 # stat_ellipse()+ #looking to see which ones fall out of 95% CI
  theme_few()

  #now looking at just the standard deviations of the whiskers without the outliers
whis2.mean<- filter(whis.mean, Crange < 2)  
ggplot(data=whis2.mean, aes(x=Crange, y=Nrange)) +
    geom_point() +
    labs(x=expression(paste(delta^13, "C (\u2030)")), 
         y=expression(paste(delta^15, "N (\u2030)" )))  +
  theme_few()
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
  geom_line(aes(x=distance, y=N), colour="tomato") +
  geom_line(aes(x=distance, y=C+25), colour="lightseagreen") +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )), 
       colour = "Isotope")  +
  scale_y_continuous(sec.axis = sec_axis(~.-25, 
       name = expression(paste(delta^13, "C (\u2030)" )))) +
  facet_wrap(vars(OtterID), nrow=6) +
  theme_light()

ggsave("whiskers.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

#sub sample of otters for WSN poster

wsn<-filter(whisker, OtterID=="163520" | OtterID=="163521" | OtterID=="163536" | OtterID == "163526")
ggplot(data=wsn) +   theme_few() +
  geom_line(aes(x=distance, y=N), colour="tomato") + 
  geom_line(aes(x=distance, y=C+28), colour="lightseagreen") +
  labs(x= "Distance from root (cm)", y=expression(paste(delta^15, "N (\u2030)" )))  +
  scale_y_continuous(sec.axis = sec_axis(~.-28, name = expression(paste(delta^13, "C (\u2030)" )))) +
  facet_wrap(vars(OtterID), nrow=2) +
  theme(axis.title = element_text(size=18, face = "bold"), 
        axis.text= element_text(size = 14, face = "bold"), legend.position = "none", legend.title = element_blank()) 


ggsave("wsn.png", device = "png", path = "SI/", width = 8, 
       height = 8, units = "in", dpi = 300)


#Looking at seasonality
ggplot(data=whisker) +
  geom_point(aes(x=Season, y=N, color="N")) +
  geom_point(aes(x=Season, y=C+28, color="C")) +
  labs(x= "Season", 
       y=expression(paste(delta^15, "N (\u2030)" )), 
       colour = "Isotope")  +
  scale_y_continuous(sec.axis = sec_axis(~.-28, 
                        name = expression(paste(delta^13, "C (\u2030)" )))) +
  geom_smooth(aes(x=Season, y=N, color="N"), method= "loess", se= FALSE, show.legend = FALSE) +
  facet_wrap(vars(OtterID), nrow=5) +
  theme_light()

#all together on one graph
whisker$Season<-factor(whisker$Season , levels=c("Summer", "Spring", "Fall", "Winter"))

ggplot(data=whisker) + theme_few()+
  geom_point(aes(x=Season, y=N, colour = "Nitrogen")) +
  geom_point(aes(x=Season, y=C+25, colour = "Carbon")) +
  stat_smooth(aes(x=as.numeric(Season), y=N, colour = "Nitrogen"), se= FALSE) +
  stat_smooth(aes(x=as.numeric(Season), y=C+25, colour = "Carbon"), se= FALSE) +
  labs(x= "Season", 
       y=expression(paste(delta^15, "N (\u2030)" )), 
       colour = "Isotope")  +
  scale_y_continuous(sec.axis = sec_axis(~.-25, name = expression(paste(delta^13, "C (\u2030)" )))) +
  scale_colour_manual(values = c("lightseagreen", "tomato")) 
  
#each otter separate
  ggplot(data=whisker) + theme_few()+
  geom_point(aes(x=Season, y=N, colour = "Nitrogen")) +
    geom_point(aes(x=Season, y=C+25, colour = "Carbon")) +
    stat_smooth(aes(x=as.numeric(Season), y=N, colour = "Nitrogen"), se= FALSE) +
    stat_smooth(aes(x=as.numeric(Season), y=C+25, colour = "Carbon"), se= FALSE) +
    labs(x= "Season", 
         y=expression(paste(delta^15, "N (\u2030)" )), 
         colour = "Isotope")  +
    scale_y_continuous(sec.axis = sec_axis(~.-25, name = expression(paste(delta^13, "C (\u2030)" )))) +
    scale_colour_manual(values = c("lightseagreen", "tomato")) +
    facet_wrap(vars(OtterID), nrow= 6)

#################################
# Residual Whiskers ANOVA       #
#################################

#this is the fix that Matt C. did. 
MEANS<-whisker.short%>%
  group_by(OtterID)%>%
  summarise(MC=mean(C),
            MN=mean(N))
MEANS

whisky<-left_join(whisker.short, MEANS, by="OtterID")
whisky$Cr<-whisky$C-whisky$MC
whisky$Nr<-whisky$N-whisky$MN

cfit<-lm(Cr~distance*OtterID, whisky)
summary(cfit)

nfit<-lm(Nr~distance*OtterID, whisky)
summary(nfit)


#plot of whisker with means overlaid
ggplot(data=whisky, aes(x=distance, y=C)) +
  geom_line(aes(group=OtterID), color = "Gray81") +
  geom_smooth(aes(color=MC), size=3, span=0.8, se= FALSE, show.legend = FALSE) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^13, "C (\u2030)" )), tag = "A")  +
  theme_few()

ggsave("C_sig.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)

ggplot(data=whisky, aes(x=distance, y=N)) +
  geom_line(aes(group=OtterID), color = "Gray81") +
  geom_smooth(aes(color=MN), size=3, span=0.8, se= FALSE, show.legend = FALSE) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )), element_text(size = 22), tag = "B")  +
  theme_few()

ggsave("N_sig.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)

#back to numeric for Distance
whisker.short$distance<-as.numeric(whisker.short$distance)
#plot of residuals with mean overlay
ggplot(data=whisky, aes(x=distance, y=Nr)) +   theme_few() +
  geom_line(aes(group=OtterID), color = "Gray81") +
  geom_smooth(size=3, span=0.5, show.legend = FALSE) +
  labs(x= "Distance from root (cm)", 
       y=expression(paste("Residual from mean ",delta^15, "N" )), tag = "B")

#THIS IS THE GRAPH I USED
#Nitrogen with mean overlay
ggplot(data=whisky, aes(x=distance, y=N)) +   theme_few() +
  geom_point() +
  geom_smooth(size=2, span=0.5, show.legend = FALSE, colour="tomato") +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N(\u2030)" )), tag = "B") +
  theme(axis.title = element_text(size=18))

ggsave("N_residuals2.png", device = "png", path = "SI/", width = 10, 
       height = 4, units = "in", dpi = 300)

ggplot(data=whisky, aes(x=distance, y=C)) +   theme_few() +
  geom_point() +
  geom_smooth(size=2, span=0.5, show.legend = FALSE, colour="lightseagreen") +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^13, "C(\u2030)" )), tag = "A") +
  theme(axis.title = element_text(size=18))

ggsave("C_residuals2.png", device = "png", path = "SI/", width = 10, 
       height = 4, units = "in", dpi = 300)

#Nitrogen best fit line
fit <- lm(N~poly(distance,2,raw=TRUE), data=whisker.short)
summary(fit) #R^2 = .2168
predicted_dist <- data.frame(N_pred = predict(fit, whisker.short), distance=whisker.short$distance)


ggplot(data=whisker.short, aes(x=distance, y=N)) +   theme_few() +
  geom_point() +
  geom_smooth(size=2, show.legend = FALSE, formula = y~poly(x,2), colour="tomato") +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N(\u2030)" ))) +
  theme(axis.title = element_text(size=18))

#With line predicted by lm
ggplot() +   theme_few() +
  geom_point(data=whisker.short, aes(x=distance, y=N)) +
  geom_line(data = predicted_dist, aes(x=distance, y=N_pred), colour="tomato") +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N(\u2030)" ))) +
  theme(axis.title = element_text(size=18))

ggplot(data=whisker.short) +   theme_few() +
  geom_point(aes(x=Season, y=N)) +
  geom_smooth(aes(x=as.numeric(Season), y= N), size=2, show.legend = FALSE, 
              colour="tomato") +
  labs(x= "Season", 
       y=expression(paste(delta^15, "N(\u2030)" ))) +
  theme(axis.title = element_text(size=18))


#location with Shinaku highlighted
ggplot(data=whisker.short, aes(x=distance, y=C, colour=Site)) +   
  theme_few() +
  geom_point() +
  geom_line(data=filter(whisker.short, Site=="Shinaku"), 
            aes(x=distance, y=C, factor=OtterID), colour= "tomato") +
#  geom_smooth(size=2, span=0.5, show.legend = FALSE, colour="lightseagreen") +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^13, "C(\u2030)" )))
#  theme(axis.title = element_text(size=18))
  
# looking at Distance = 0 for carbon
ggplot(data=filter(whisker.short, distance == "0")) +   
  theme_few() +
  geom_histogram(aes(x=C, fill=Site), bins = 2) +
  labs(x= "Count", 
       x=expression(paste(delta^13, "C(\u2030)" ))) 
#  +  facet_wrap(vars(Site))
#  +  theme(axis.title = element_text(size=18))
    
 
# Season Histogram Carbon
ggplot(data=whisker.short) +   
    theme_few() +
    geom_histogram(aes(x=C, fill = Season), bins = 15) +
    labs(x= "Count", 
           x=expression(paste(delta^13, "C(\u2030)" ))) +
    facet_wrap(vars(Season)) 
#   + theme(axis.title = element_text(size=18)) 

#Now all overlaid with Carbon
ggplot() +   
  theme_few() +
  geom_density(data=filter(whisker.short, Season == "Summer"), 
                 aes(x=C), alpha=.5, fill= "darkgoldenrod1") +
  geom_density(data=filter(whisker.short, Season == "Spring"), 
                 aes(x=C), alpha=.5, fill= "palegreen3") +
  geom_density(data=filter(whisker.short, Season == "Fall"), 
                 aes(x=C), alpha=.5, fill= "darkorange3") +
  geom_density(data=filter(whisker.short, Season == "Winter"), 
                 aes(x=C), alpha=.5, fill = "paleturquoise1") +
  labs(x=expression(paste(delta^13, "C" ))) 

#Now all overlaid with Nitrogen
ggplot() +   
  theme_few() +
  geom_density(data=filter(whisker.short, Season == "Summer"), 
               aes(x=N), alpha=.9, fill= "darkgoldenrod1") +
  geom_density(data=filter(whisker.short, Season == "Spring"), 
               aes(x=N), alpha=.6, fill= "palegreen3") +
  geom_density(data=filter(whisker.short, Season == "Fall"), 
               aes(x=N), alpha=.5, fill= "darkorange3") +
  geom_density(data=filter(whisker.short, Season == "Winter"), 
               aes(x=N), alpha=.4, fill = "paleturquoise1") +
  labs(x=expression(paste(delta^15, "N" ))) 

#Summer/spring vs fall/winter - Nitrogen
ggplot() +   
  theme_few() +
  geom_density(data=filter(whisker, Season == "Fall" | Season == "Winter"), 
               aes(x=N, colour= "Fall/Winter"), alpha=.5, fill= "lightseagreen") +
  geom_density(data=filter(whisker.short, Season == "Spring"| Season =="Summer"), 
               aes(x=N, colour = "Spring/Summer"), alpha=.5, fill= "darkgoldenrod1") +
  scale_colour_manual(name='', values=c("Fall/Winter"="lightseagreen", 
                                        "Spring/Summer"="darkgoldenrod1")) +
  theme(axis.title = element_blank(), axis.text= element_text(size = 16, face= "bold"), 
        legend.position = "none", legend.title = element_blank()) 


ggsave("N_whisker_seasons.png", device = "png", path = "SI/", width = 8, 
       height = 6.5, units = "in", dpi = 300)

#carbon
ggplot() +   
  theme_few() +
  geom_density(data=filter(whisker, Season == "Fall" | Season == "Winter"), 
               aes(x=C, colour= "Fall/Winter"), alpha=.5, fill= "lightseagreen") +
  geom_density(data=filter(whisker.short, Season == "Spring"| Season =="Summer"), 
               aes(x=C, colour = "Spring/Summer"), alpha=.5, fill= "darkgoldenrod1") +
  scale_colour_manual(name='', values=c("Fall/Winter"="lightseagreen", 
                                        "Spring/Summer"="darkgoldenrod1")) +
  theme(axis.title = element_blank(), 
        axis.text= element_text(size = 16, face= "bold"), legend.position = "none", legend.title = element_blank()) 

ggsave("C_whisker_seasons.png", device = "png", path = "SI/", width = 8, 
       height = 6.5, units = "in", dpi = 300)

#now look at the prey - mussel
ggplot() +   
  theme_few() +
  geom_density(data=filter(si.mus, Season == "Summer"), 
               aes(x=N), alpha=.9, fill= "#E69F00") +
  geom_density(data=filter(si.mus, Season == "Spring"), 
               aes(x=N), alpha=.5, fill= "#009E73") +
  geom_density(data=filter(si.mus, Season == "Winter"), 
               aes(x=N), alpha=.4, fill = "#56B4E9") +
  labs(x=expression(paste(delta^15, "N" ))) 

#now look at the prey - clam
ggplot() +   
  theme_few() +
  geom_density(data=filter(si.clam, Season == "Summer"), 
               aes(x=N), alpha=.9, fill= "#E69F00") +
  geom_density(data=filter(si.clam, Season == "Spring"), 
               aes(x=N), alpha=.5, fill= "#009E73") +
  geom_density(data=filter(si.clam, Season == "Winter"), 
               aes(x=N), alpha=.4, fill = "#56B4E9") +
  labs(x=expression(paste(delta^15, "N" ))) 


#all prey
ggplot() +   
  theme_few() +
  geom_density(data=filter(si.test, Season == "Summer"), 
               aes(x=N), alpha=.9, fill= "#E69F00") +
  geom_density(data=filter(si.test, Season == "Spring"), 
               aes(x=N), alpha=.5, fill= "#009E73") +
  geom_density(data=filter(si.test, Season == "Winter"), 
               aes(x=N), alpha=.4, fill = "#56B4E9") +
  labs(x=expression(paste(delta^15, "N" ))) 

#clam with spring and summer combined
ggplot() +   
  theme_few() +
  geom_density(data=filter(si.clam, Season == "Summer"| Season == "Spring"), 
               aes(x=N), alpha=.5, fill= "darkgoldenrod1") +
  geom_density(data=filter(si.clam, Season == "Winter"), 
               aes(x=N), alpha=.5, fill = "lightseagreen") +
  labs(x=expression(paste(delta^15, "N" ))) 

#all prey with spring and summer combined
ggplot() +   
  theme_few() +
  geom_density(data=filter(si.test, Season == "Winter"), 
               aes(x=N, colour = "Winter"), alpha=.5, fill = "lightseagreen") +
  geom_density(data=filter(si.test, Season == "Summer"| Season == "Spring"), 
               aes(x=N, colour= "Spring/Summer"), alpha=.5, fill = "darkgoldenrod1") +
  labs(y = "Density", x=expression(paste(delta^15, "N (\u2030)" ))) +
  scale_colour_manual(name='', values=c("Winter"="lightseagreen", "Spring/Summer"="darkgoldenrod1")) +
  theme(axis.title = element_text(size=20, face = "bold"),  
        axis.text= element_text(size = 14, face= "bold"), legend.position = "bottom", 
        legend.title = element_blank(), legend.text = element_text(size = 14, face = "bold")) 

ggsave("N_prey_seasons.png", device = "png", path = "SI/", width = 8, 
       height = 7, units = "in", dpi = 300)

ggplot() +   
  theme_few() +
  geom_density(data=filter(si.test, Season == "Winter"), 
               aes(x=C, colour = "Winter"), alpha=.5, fill = "lightseagreen") +
  geom_density(data=filter(si.test, Season == "Summer"| Season == "Spring"), 
               aes(x=C, colour= "Spring/Summer"), alpha=.5, fill = "darkgoldenrod1") +
  labs(y= "Denisty", x=expression(paste(delta^13, "C (\u2030)" ))) +
  scale_colour_manual(name='', values=c("Spring/Summer"="darkgoldenrod1", "Winter"="lightseagreen"))+
  theme(axis.title = element_text(size=20, face = "bold"), 
        axis.text= element_text(size = 14, face = "bold"), legend.position = "none", legend.title = element_blank()) 

ggsave("C_prey_seasons.png", device = "png", path = "SI/", width = 8, 
       height = 6.4, units = "in", dpi = 300)
