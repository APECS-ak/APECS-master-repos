### SI ###

setwd("/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI")


#load programs
library(ggplot2) #always
library(lattice) #always
library(dplyr) #always
library(ggthemes) #for graphs 
library(scales) # for graphs
library(car) #for stats
library(compute.es) #for stats
library(effects) 
library(multcomp) #for stats
library(pastecs)
library(HH) #for Anova (not normal one)
library(psych)
library(RVAideMemoire) # for moods
library(rcompanion) #for moods
library(corrgram) # for correlation graphs at the end of the script
library(gridExtra) # making multi pane graphs
library(grid) # making multi pane graphs

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
                   ifelse(si.test$Species == "tes" , "Herb Snail", 
                   ifelse(si.test$Species == "cef" | si.test$Species == "nul", "Carn Snail",
                   ifelse(si.test$Species == "pio" | si.test$Species == "evt", "Star",
                   ifelse(si.test$Species == "std" | si.test$Species == "stf", "Urchin",
                   ifelse(si.test$Species == "mtr", "Mussel",""))))))))

## New PreyCat only 5 catagories including Cucumber.Snail
si.test$PreyCat <- ifelse(si.test$Species == "apc" | si.test$Species == "tes", "Cucumber.Snail", 
                          ifelse(si.test$Species == "cln" | si.test$Species == "prs" | si.test$Species == "sag", "Clam", 
                                 ifelse(si.test$Species == "cam" | si.test$Species == "cap" | si.test$Species == "cao" | 
                                          si.test$Species == "tec" | si.test$Species == "pup",  "Crab", 
                                                             ifelse(si.test$Species == "std" | si.test$Species == "stf", "Urchin",
                                                                    ifelse(si.test$Species == "mtr", "Mussel","")))))

### Fixing C for high fat critters. Anything above 3.5:1 ratio
si.test$Cnorm <- NA
si.test$Cnorm<- ifelse(si.test$CN >= 3.5, si.test$C-3.32+(0.99*si.test$CN), si.test$C)
si.test$SiteNumber<-as.character(si.test$SiteNumber)

#make final CSV
write.csv(si.test, "SI/sifinal.csv")

si.prey <- si.test

#_______________________________________________________________________________________
## load final CSV 
si.prey <- read.csv("SI/sifinal.csv")

#load whisker data
whisker <- read.csv("SI/whiskers.csv")
whisker$OtterID<-as.factor(whisker$OtterID)

# for right now, I want to remove 163532. I removed 160478 and 77298 from the CSV
#oddotter<- filter(whisker, OtterID=="163532")
whisker<- filter(whisker, OtterID != "163532")
#remove first 299, since only one right now - Temp
#whisker<-whisker[-286, ] 

#Shortening to only 8cm max for graphs
whisker.short<-filter(whisker, distance <= 8)


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
ggplot(data= filter(si.test, PreyCat== "Clam"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Species, shape= Site)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Species)) +
  theme_classic()

ggplot(data= filter(si.test, PreyCat== "Clam"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Site, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Site)) +
  theme_classic()

#site
ggplot(data= filter(si.test, PreyCat== "Clam"), aes(x=Size, y=Cnorm)) +
  geom_point(aes(color=Site, shape= Species)) +
  labs(x="Size", y=expression(paste(delta^13, "C (\u2030)" ))) +
  theme_classic()

#season
ggplot(data= filter(si.test, PreyCat== "Clam"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Season, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

#CRAB  - Nossuk and Soda Bay overlap
ggplot(data= filter(si.test, PreyCat== "Crab"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Site, shape=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Site)) +
  theme_classic()

#Crab - species/site
ggplot(data= filter(si.test, PreyCat== "Crab"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Species, shape=Site)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Species)) +
  theme_classic()

#Crab - season, mostly full overlap
ggplot(data= filter(si.test, PreyCat== "Crab"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Season, shape=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

#URCHIN - Species don't differ, season doesn't differ?, site very different
ggplot(data= filter(si.test, PreyCat== "Urchin"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Site, shape=Season)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Site)) +
  theme_classic()

#Urchin only Craig - not enough!
urch.craig<-filter(si.test, Site == "Craig" & PreyCat == "Urchin")
ggplot(data= urch.craig, aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Season, shape=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

#SNAIL
ggplot(data= filter(si.test, PreyCat== "Herb Snail" | PreyCat == "Carn Snail"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Species, shape=Season)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  theme_classic()

#STAR - Tissue differences
ggplot(data= filter(si.test, PreyCat== "Star"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Tissue, shape=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Tissue)) +
  theme_classic()

#look at seasonal changes in one site for clams - NO DIFFERENCE for Craig
ggplot(data= filter(si.test, Site=="Soda Bay" & PreyCat == "Clam"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Season, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

ggplot(data= filter(si.test, Site=="Craig" & PreyCat == "Clam"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Season, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

#Mussel - difference in sites

ggplot(data = filter(si.test, PreyCat == "Mussel"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Site)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Site)) +
  theme_classic()

ggsave("mussel.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)


ggplot(data= filter(si.test, PreyCat == "Mussel"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Season, shape= Site)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

#look at seasonal changes in one site for crab
ggplot(data= filter(si.test, Site=="Soda Bay" & PreyCat == "Crab"), aes(x=Cnorm, y=N)) +
  geom_point(aes(color=Season, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=Season)) +
  theme_classic()

ggplot(data= filter(si.test, Site=="Craig" & PreyCat == "Crab"), aes(x=Cnorm, y=N)) +
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
si.mean <-si.prey %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(Cnorm), Csd=sd(Cnorm), Nmean=mean(N), Nsd=sd(N))
si.mean<-si.mean[-1,] #removing blank
#si.mean<-si.mean[-6,] #removing stars

#TDF
si.mean$Cmean <- si.mean$Cmean+2
si.mean$Nmean <- si.mean$Nmean+2.8

si.count <- si.prey %>%
  group_by(PreyCat) %>%
  count()
si.count<-si.count[-1,] #removing blank

si.mean<- left_join(si.mean, si.count)
#make csv
write.csv(si.mean, "SI/si.mean_TDF.csv")

# seasonal means
si.season <-si.prey %>%
  group_by(PreyCat, Season) %>% 
  summarise(Cmean=mean(Cnorm), Nmean=mean(N), Csd=sd(Cnorm), Nsd=sd(N), 
            Cse=sd(Cnorm)/sqrt(length(Cnorm)), Nse=sd(N)/sqrt(length(N)))
si.season<-si.season[-1,] #removing blank row
si.season<-si.season[-1,] #removing blank row

#filter only summer
si.summer <- filter(si.season, Season == "Summer")
write.csv(si.summer, "SI/si.season.csv")

#prey with SD as errorbars
ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmean-Nsd, ymax = Nmean+Nsd, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Csd,xmax = Cmean+Csd, color= PreyCat), height=0) +
  theme_classic()

ggsave("prey.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)


#Now looking at just the summer values
si.summer<-filter(si.test, Season=="Summer")
si.mean.su <-si.summer %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(Cnorm), Nmean=mean(N), Cmin=min(Cnorm), Cmax= max(Cnorm), 
            Csd=sd(Cnorm), Nsd=sd(N), Cse=sd(Cnorm)/sqrt(length(Cnorm)), Nse=sd(N)/sqrt(length(N)))
si.mean.su<-si.mean.su[-1,]
#si.mean.su<-si.mean.su[-6,] #removing stars

#Winter values
si.winter<-filter(si.test, Season=="Winter")
si.mean.wi <-si.winter %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(Cnorm), Nmean=mean(N), Cmin=min(Cnorm), Cmax= max(Cnorm), 
            Csd=sd(Cnorm), Nsd=sd(N), Cse=sd(Cnorm)/sqrt(length(Cnorm)), Nse=sd(N)/sqrt(length(N)))
si.mean.wi<-si.mean.wi[-1,]
#si.mean.wi<-si.mean.wi[-6,] #removing stars

#Spring values
si.spring<-filter(si.test, Season=="Spring")
si.mean.sp <-si.spring %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(Cnorm), Nmean=mean(N), Cmin=min(Cnorm), Cmax= max(Cnorm), 
            Csd=sd(Cnorm), Nsd=sd(N), Cse=sd(Cnorm)/sqrt(length(Cnorm)), Nse=sd(N)/sqrt(length(N)))
#si.mean.sp<-si.mean.sp[-6,] #removing stars

#make csvs
write.csv(si.mean.sp, "si.mean.sp.csv")
write.csv(si.mean.su, "si.mean.su.csv")
write.csv(si.mean.wi, "si.mean.wi.csv")

#count of each prey cat
count<- si.test %>%
  count(PreyCat, Season)
count

ggplot(data= si.mean.su, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Cse,xmax = Cmean+Cse, color= PreyCat), height=0) +
  theme_classic()

#Now look at changes between seasons
ggplot() +
  geom_point(data= si.mean.su, aes(x=Cmean, y=Nmean, color=PreyCat), shape=4) +
  geom_point(data= si.mean.wi, aes(x=Cmean, y=Nmean, color=PreyCat), shape=1) +
  geom_point(data= si.mean.sp, aes(x=Cmean, y=Nmean, color=PreyCat), shape=0) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  theme_classic()

ggsave("prey_season.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

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
  group_by(OtterID, Site) %>%
  summarise(Cmean=mean(C), Csd=sd(C), Nmean=mean(N), Nsd=sd(N)) 

#make counts
whis.count <- whisker %>%
  group_by(OtterID) %>%
  count()

whis.mean <- left_join(whis.mean, whis.count)

#Not using SE right now            
#Cse=sd(C)/sqrt(length(C)), Nse=sd(N)/sqrt(length(N)), 
#Crange=(max(C)-min(C)), Nrange=max((N)-min(N)))

#print csv vof means (for table)
write.csv(whis.mean, "SI/whis.mean.csv")

#season mean
season.mean<-whisker %>%
  group_by(Season) %>%
  summarise(Cmean=mean(C), Nmean=mean(N), Csd=sd(C), Nsd=sd(N), 
            Cse=sd(C)/sqrt(length(C)), Nse=sd(N)/sqrt(length(N)), 
            Crange=(max(C)-min(C)), Nrange=max((N)-min(N)))

#print csv vof means (for table)
write.csv(season.mean, "SI/season.mean.csv")

#Whiskers mean for each season group
whisker$Season2 <- NA
whisker$Season2 <- ifelse(whisker$Season=="Fall", "Winter", 
                          ifelse(whisker$Season=="Winter", "Winter", 
                                 ifelse(whisker$Season=="Spring", "Summer", 
                                        ifelse(whisker$Season=="Summer","Summer",""))))
whis.mean.season <- whisker %>%
  group_by(OtterID, Season2) %>%
  summarise(Cmean=mean(C), Nmean=mean(N), Csd=sd(C), Nsd=sd(N), 
            Cse=sd(C)/sqrt(length(C)), Nse=sd(N)/sqrt(length(N)), 
            Crange=(max(C)-min(C)), Nrange=max((N)-min(N)))

#TDF for otters changes to 2/2.8 makes a better looking graph
whis.mean$TDFC<-NA; whis.mean$TDFN<-NA
whis.mean$TDFC <- whis.mean$Cmean-2
whis.mean$TDFN <- whis.mean$Nmean-2.8
whis.mean.season$TDFC<-NA; whis.mean.season$TDFN<-NA
whis.mean.season$TDFC <- whis.mean.season$Cmean-2
whis.mean.season$TDFN <- whis.mean.season$Nmean-2.8

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

#Separate by season - Winter

ggplot() +
  geom_point(data= si.mean.wi, aes(x=Cmean, y=Nmean, color=PreyCat), size=3) + theme_few() +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )), title = "Winter")  +
  scale_color_discrete(name  ="Prey Group") +
  geom_errorbar(data= si.mean.wi, aes(x=Cmean, y=Nmean, ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean.wi, aes(x=Cmean, y=Nmean, xmin = Cmean-Cse,xmax = Cmean+Cse, color= PreyCat), height=0) +
  geom_point(data=filter(whis.mean.season, Season2=="Winter"), aes(x=TDFC, y=TDFN), size=2)+
  geom_errorbar(data=filter(whis.mean.season, Season2=="Winter"), aes(x=TDFC, y=TDFN, ymin = TDFN-Nse, ymax = TDFN+Nse), width=0) + 
  geom_errorbarh(data=filter(whis.mean.season, Season2=="Winter"), aes(x=TDFC, y=TDFN, xmin = TDFC-Cse, xmax = TDFC+Cse), height=0) 

ggsave("whis_prey_winter.png", device = "png", path = "SI/", width = 8, 
       height = 7, units = "in", dpi = 300)

#Separate by season - Summer

ggplot() +
  geom_point(data= si.mean.su, aes(x=Cmean, y=Nmean, color=PreyCat), size=3) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )), title = "Summer")  +
  scale_color_discrete(name  ="Prey Group") +
  geom_errorbar(data= si.mean.su, aes(x=Cmean, y=Nmean, ymin = Nmean-Nse, ymax = Nmean+Nse, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean.su, aes(x=Cmean, y=Nmean, xmin = Cmean-Cse,xmax = Cmean+Cse, color= PreyCat), height=0) +
  geom_point(data=filter(whis.mean.season, Season2=="Summer"), aes(x=TDFC, y=TDFN), size=2)+
  geom_errorbar(data=filter(whis.mean.season, Season2=="Summer"), aes(x=TDFC, y=TDFN, ymin = TDFN-Nse, ymax = TDFN+Nse), width=0) + 
  geom_errorbarh(data=filter(whis.mean.season, Season2=="Summer"), aes(x=TDFC, y=TDFN, xmin = TDFC-Cse, xmax = TDFC+Cse), height=0) +
  theme_few()

ggsave("whis_prey_summer.png", device = "png", path = "SI/", width = 8, 
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

#sub sample of otters for WSN poster (and WMMC poster)

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
  
## ODD OTTER

#load whisker data
oddotter <- read.csv("SI/oddotter.csv")
oddotter$OtterID<-as.factor(oddotter$OtterID)
oddotter$Run<-as.factor(oddotter$Run)

#plot two runs separate
ggplot(data=oddotter) +
  geom_line(aes(x=distance, y=N), colour="tomato") +
  geom_line(aes(x=distance, y=C+25), colour="lightseagreen") +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )), 
       colour = "Isotope")  +
  scale_y_continuous(sec.axis = sec_axis(~.-25, 
                                         name = expression(paste(delta^13, "C (\u2030)" )))) +
  facet_wrap(vars(Run), nrow=2) +
  theme_light()

#plot together different colors - Nitrogen
ggplot(data=oddotter, aes(x=distance, y=N, colour=Run)) +
  geom_line() +
  geom_point() +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  theme_few()

#plot together different colors - Carbon
ggplot(data=oddotter, aes(x=distance, y=C, colour=Run)) +
  geom_line() +
  geom_point() +
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^13, "C (\u2030)" )))  +
  theme_few()

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

###############################################################
#               Residual Whiskers ANOVA                       #
###############################################################

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

cseason <- lm(Cr~Season*OtterID, whisky)
summary(cseason)


cseason.aov <- aov(C~Season, data = whisker.short)
summary(cseason.aov)

nseason.aov <- aov(N~Season, data = whisker.short)
summary(nseason.aov)

seasonmean.long <- whisker%>%
  group_by(Season)%>%
  summarise(MC=mean(C),
            MN=mean(N))

pairwise.t.test(x=whisker$C, g=whisker$Season, p.adj="bonferroni")
pairwise.t.test(x=whisker$N, g=whisker$Season, p.adj="bonferroni")



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

###############################################################################
#                                 Correlations                                #
###############################################################################


#making one otter test
cor533 <- whisker %>%
  filter(OtterID=="163533") %>%
  select(distance, C, N)
  
corrgram(cor533, lower.panel = panel.cor)

#For each otter-avg cor?
cor<- whisker%>%
  group_by(OtterID)%>%
  summarize(COR=mean(cor(C,N)))

ggplot(cor, aes(x=COR)) +
  geom_density(fill="gray", color="gray") +
  xlim(-1,1) +
  labs(x= "Correlation") +
  theme_few()

ggplot(data=whisker, aes(C,N)) +
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  labs(x= expression(paste(delta^13, "C (\u2030)")), 
       y= expression(paste(delta^15, "N (\u2030)" )))+
  facet_wrap(vars(OtterID), nrow=6) +
  theme_light()

ggplot(cor.whis, aes(x=correlation)) +
  geom_bar(color="red") +
  xlim(-1,1) +
  theme_classic()


summary(whisker)

variation<-whisker %>%
  group_by(OtterID) %>%
  summarise(Cmax=max(C), Nmax=max(N), Cmin=min(C), Nmin=min(N))

variation$Crange <- variation$Cmax-variation$Cmin
variation$Nrange <- variation$Nmax-variation$Nmin
summary(variation)

summary(filter(whisker, OtterID =="163521"))

counted<- whisker %>%
  group_by(OtterID) %>%
  count()

############################################################################
###### Graphs for paper
############################################################################
ggplot() +
  geom_point(data= si.mean, aes(x=Ctdf, y=Ntdf, shape=PreyCat), size =4) +
  scale_shape_manual(values=c(6, 4, 16, 17, 18, 8, 9), name="Prey Group")+
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= si.mean, aes(x=Ctdf, y=Ntdf, ymin = Ntdf-Nse, ymax = Ntdf+Nse,
                                   shape= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Ctdf, y=Ntdf, xmin = Ctdf-Cse,xmax = Ctdf+Cse,
                                    shape= PreyCat), height=0) +
  geom_point(data=whisker, aes(x=C, y=N, color=Season), size=2)+
  scale_color_grey()+
  theme_few()


#somthing to think about - adding the prey cat names
#geom_text(size=8,hjust=-0.5)


ggsave("biplot_BW.png", device = "png", path = "SI/", width = 8, 
       height = 7, units = "in", dpi = 300)

ggplot() +
  geom_point(data= si.mean, aes(x=Ctdf, y=Ntdf, color=PreyCat), size=3) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  scale_color_discrete(name  ="Prey Group") +
  geom_errorbar(data= si.mean, aes(x=Ctdf, y=Ntdf, ymin = Ntdf-Nsd, ymax = Ntdf+Nsd,
                                   color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Ctdf, y=Ntdf, xmin = Ctdf-Csd,xmax = Ctdf+Csd,
                                    color= PreyCat), height=0) +
  geom_point(data=whisker, aes(x=C, y=N, shape=Season), size=2)+
  theme_few()

ggsave("biplot.png", device = "png", path = "SI/", width = 11, 
       height = 7, units = "in", dpi = 300)

A <- ggplot(data=whisker) +
  geom_boxplot(aes(x=Season, y=C)) +
  labs(y=expression(paste(delta^13, "C" )), x=NULL, tag = "A") +
  theme_few() 


B <- ggplot(data=whisker) +
  geom_boxplot(aes(x=Season, y=N)) +
  labs(y=expression(paste(delta^15, "N" )), tag = "B") +
  theme_few()

grid.arrange(A, B, nrow = 2)
g <- arrangeGrob(A, B, nrow=2) #generates g
ggsave("CN_season.png", g, device = "png", path = "SI/", width = 9, 
       height = 6, units = "in", dpi = 300)


a <- ggplot(data=filter(whisker, OtterID=="163520")) +   theme_few() +
  geom_line(aes(x=distance, y=N), colour="tomato") + 
  geom_line(aes(x=distance, y=C+25), colour="lightseagreen") +
  labs(x= NULL, y=expression(paste(delta^15, "N (\u2030)" )), tag = "A")  +
  scale_y_continuous(sec.axis = sec_axis(~.-25, name = NULL), 
                     breaks= c(9, 11, 13, 15), limits = c(8.5,15)) +
  theme(axis.title = element_text(size=18, face = "bold"), 
        axis.text= element_text(size = 14, face = "bold"), 
        legend.position = "none", legend.title = element_blank(), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  ggtitle("520")

b <- ggplot(data=filter(whisker, OtterID=="163521")) +   theme_few() +
  geom_line(aes(x=distance, y=N), colour="tomato") + 
  geom_line(aes(x=distance, y=C+25), colour="lightseagreen") +
  labs(x= NULL, y= NULL, tag = "B")  +
  scale_y_continuous(sec.axis = sec_axis(~.-25,
                    name = expression(paste(delta^13, "C (\u2030)" ))), 
                    breaks= c(9, 11, 13, 15), limits = c(8.5,15)) +
  theme(axis.title = element_text(size=18, face = "bold"), 
        axis.text= element_text(size = 14, face = "bold"),
        legend.position = "none", legend.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  ggtitle("521")

c <- ggplot(data=filter(whisker,OtterID=="163524")) +   theme_few() +
  geom_line(aes(x=distance, y=N), colour="tomato") + 
  geom_line(aes(x=distance, y=C+25), colour="lightseagreen") +
  labs(x= "Distance from root (cm)", y=expression(paste(delta^15, "N (\u2030)" )), tag = "C")  +
  scale_y_continuous(sec.axis = sec_axis(~.-25, name = NULL), 
                     breaks= c(9, 11, 13, 15), limits = c(8.5,15)) +
  xlim(0,6) +
  theme(axis.title = element_text(size=18, face = "bold"), 
        axis.text= element_text(size = 14, face = "bold"), 
        legend.position = "none", legend.title = element_blank(), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  ggtitle("524")

d <-ggplot(data=filter(whisker, OtterID=="77287")) +   theme_few() +
  geom_line(aes(x=distance, y=N), colour="tomato") + 
  geom_line(aes(x=distance, y=C+25), colour="lightseagreen") +
  labs(x= "Distance from root (cm)", y=NULL, tag = "D")  +
  xlim(0,6) +
  scale_y_continuous(sec.axis = sec_axis(~.-25, 
           name = expression(paste(delta^13, "C (\u2030)" ))), 
           breaks= c(9, 11, 13, 15), limits = c(8.5,15)) +
  theme(axis.title = element_text(size=18, face = "bold"), 
        axis.text= element_text(size = 14, face = "bold"), 
        legend.position = "none", legend.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  ggtitle("287")

gr <- arrangeGrob(a, b, c, d, nrow=2) 
ggsave("INDIV.png", gr, device = "png", path = "SI/", width = 9, 
       height = 7.5, units = "in", dpi = 300)



####***************************************************************

#test box/whisker alternative

ggplot(aes(y = Mean, x = Season), data = mix.mod) +
  theme_bw() +
  geom_point(aes(color=Season), size =4) +
  geom_errorbar(aes(ymin = Mean-SD,ymax = Mean+SD), width=.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  ylab("Diet proportion") + theme(legend.position = "none") +
  facet_wrap(vars(PreyCat), nrow = 2)

ggsave("mixing.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)



cseas.aov <- aov(C~Season, data = si.prey)
summary(cseas.aov)

nseas.aov <- aov(N~Season, data = si.prey)
summary(nseas.aov)

pairwise.t.test(x=si.prey$C, g=si.prey$Season, p.adj="bonferroni")



count.season<- si.prey %>%
  group_by(PreyCat, Season) %>%
  count()


# means for table 
whisker.mean <-whisker %>%
  group_by(Season, Site) %>%
  summarise(Cmean=mean(C), Csd=sd(C), 
            Nmean=mean(N), Nsd=sd(N))

count.means <- whisker %>%
  group_by(Season, Site) %>%
  count()

whisker.mean <- left_join(whisker.mean, count.means)
write.csv(whisker.mean, "SI/whisker_means.csv")


