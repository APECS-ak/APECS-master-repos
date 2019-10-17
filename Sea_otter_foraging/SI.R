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
                   ifelse(si.test$Species == "std" | si.test$Species == "stf", "Urchin",
                   ifelse(si.test$Species == "mtr", "Mussel","")))))))

### Fixing C for high fat critters. Anything above 3.5:1 ratio
si.test$Cnorm <- NA
si.test$Cnorm<- ifelse(si.test$CN >= 3.5, si.test$C-3.32+(0.99*si.test$CN), si.test$C)
si.test$SiteNumber<-as.character(si.test$SiteNumber)

#load whisker data
whisker <- read.csv("SI/whiskers.csv")
whisker$OtterID<-as.character(whisker$OtterID)

#whisker<-whisker[-c(64:68), ] #this version has extra NA rows, so deleting, may not be needed in future

#reduce si file to just clam/crab/snail/urchin
si.clam<- filter(si.test, Species== "cln" | Species == "sag" | Species == "prs")
si.crab<- filter(si.test, Species== "cam" | Species == "cao" | Species== "cap" | Species== "tec")
si.snail<- filter(si.test, Species =="tes" | Species == "cef" | Species == "nul")
si.urch<- filter(si.test, Species =="std" | Species == "stf")
si.star<- filter(si.test, Species =="pio" | Species == "evt")
si.mus<- filter(si.test, Species =="mtr")

# plot of all species Nitrogen
ggplot(data=si.test, aes(x=N)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(vars(Species))

#plot of all species Carbon
ggplot(data=si.test, aes(x=C)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(vars(Species))

#now all normalized carbon
ggplot(data=si.test, aes(x=Cnorm)) + 
  geom_histogram(binwidth = .5) +
  facet_wrap(vars(Species))

#Biplot with catagories seperated by species
#kind of confusing with a lot of overlap
ggplot(data= si.test, aes(x=C, y=N)) +
  geom_point(aes(color=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=PreyCat)) +
  theme_classic()

#separated by site and catagory
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

#anova for Nitrogen
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


#d13C vs CN ratio
ggplot(data= si.star, aes(x=C, y=CN)) +
  geom_point(aes(color=Tissue, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

# C:N ratio for all species except stars  - Looks like Urchins also are very high
xstar<- filter(si.test, PreyCat != "Star" & PreyCat != "Urchin")
ggplot(data= xstar, aes(x=C, y=CN)) +
  geom_point(aes(color=Species, shape= PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

# C:N ratio of just urchins
ggplot(data= si.urch, aes(x=C, y=CN)) +
  geom_point(aes(color=Species, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

# C:N ratio of just clams
ggplot(data= si.clam, aes(x=C, y=CN)) +
  geom_point(aes(color=Species, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

ggplot(data= si.crab, aes(x=C, y=N)) +
  geom_point(aes(color=Site, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  theme_classic()

#Making mean and min/max for each prey type using Cnorm
si.mean <-si.test %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(Cnorm), Nmean=mean(N), Cmin=min(Cnorm), Cmax= max(Cnorm), Nmin=min(N), Nmax=max(N))
si.mean<-si.mean[-1,]

ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  theme_classic()

ggsave("prey.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

#changing whiskers for TDF values that are listed in Tyrell paper
##This makes the values way too low. 
whis.mean<-whisker %>%
  group_by(OtterID) %>%
  summarise(Cmean=mean(C), Nmean=mean(N), Cmin=min(C), Cmax= max(C), Nmin=min(N), Nmax=max(N))

whis.mean<-whisker %>%
  group_by(OtterID) %>%
  summarise(Cmean=mean(C), Nmean=mean(N), Csd=sd(C), Nsd=sd(N))

#TDF for otters changes to 2/3.5 makes a better looking graph
whis.mean$TDFC<-NA; whis.mean$TDFN<-NA
whis.mean$TDFC <- whis.mean$Cmean-2
whis.mean$TDFN <- whis.mean$Nmean-3.5


ggplot(data= whis.mean, aes(x=TDFC, y=TDFN)) +
  geom_point() +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = TDFN-Nsd,ymax = TDFN+Nsd), width=0) + 
  geom_errorbarh(aes(xmin = TDFC-Csd,xmax = TDFC+Csd), height=0) +
  theme_classic()

#putting it all together
ggplot() +
  geom_point(data= si.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= si.mean, aes(x=Cmean, y=Nmean, ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Cmean, y=Nmean, xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  geom_point(data=whis.mean, aes(x=TDFC, y=TDFN))+
  geom_errorbar(data=whis.mean, aes(x=TDFC, y=TDFN, ymin = TDFN-Nsd, ymax = TDFN+Nsd), width=0) + 
  geom_errorbarh(data=whis.mean, aes(x=TDFC, y=TDFN, xmin = TDFC-Csd, xmax = TDFC+Csd), height=0) +
  theme_classic()

ggsave("whis_prey.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

#now without error bars for otters (but still avg of each otter)
ggplot() +
  geom_point(data= si.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= si.mean, aes(x=Cmean, y=Nmean, ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Cmean, y=Nmean, xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  geom_point(data=whis.mean, aes(x=TDFC, y=TDFN))+
  theme_classic()

ggsave("whis_prey_nobars.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

#now without error bars for otters (all points separate)
ggplot() +
  geom_point(data= si.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= si.mean, aes(x=Cmean, y=Nmean, ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Cmean, y=Nmean, xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  geom_point(data=whisker, aes(x=C-2.5, y=N-3))+
  theme_classic()

ggsave("whis_prey_nobars_points.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

  
#without TDFs
ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  geom_point(data=whis.mean) +
  geom_errorbar(data=whis.mean, aes(ymin = Nmean-Nsd, ymax = Nmean+Nsd), width=0) + 
  geom_errorbarh(data=whis.mean, aes(xmin = Cmean-Csd, xmax = Cmean+Csd), height=0) +
  theme_classic()

#one otter on top of prey graph
oddotter<- filter(whisker, OtterID=="163532")

ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  geom_point(data=oddotter, x=oddotter$C-2, y=oddotter$N-3.5) +
  xlim(-22.5,-9) + ylim(4,14.5) +
  theme_classic()

##Now I want to look at the non-normalized carbon data
#Making mean and min/max for each prey type using C
sic.mean <-si.test %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(C), Nmean=mean(N), Cmin=min(C), Cmax= max(C), Nmin=min(N), Nmax=max(N))
sic.mean<-sic.mean[-1,]

ggplot() +
  geom_point(data= sic.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= sic.mean, aes(x=Cmean, y=Nmean, ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(data= sic.mean, aes(x=Cmean, y=Nmean, xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  geom_point(data=whis.mean, aes(x=TDFC, y=TDFN))+
  geom_errorbar(data=whis.mean, aes(x=TDFC, y=TDFN, ymin = TDFN-Nsd, ymax = TDFN+Nsd), width=0) + 
  geom_errorbarh(data=whis.mean, aes(x=TDFC, y=TDFN, xmin = TDFC-Csd, xmax = TDFC+Csd), height=0) +
  theme_classic()

ggplot() +
  geom_point(data= sic.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= sic.mean, aes(x=Cmean, y=Nmean, ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(data= sic.mean, aes(x=Cmean, y=Nmean, xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  geom_point(data=whisker, aes(x=C-2.5, y=N-3))+
  theme_classic()

ggsave("prey_nocarbonnorm.png", device = "png", path = "SI/", width = 6, 
       height = 8, units = "in", dpi = 300)

#################################################
#################################################
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


## Mixing Models ##

library(MixSIAR)
library(RGtk2)
browseVignettes("MixSIAR")
mixsiar_gui()
