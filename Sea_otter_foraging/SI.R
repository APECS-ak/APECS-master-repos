### SI ###

setwd("/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging")


#load programs
library(ggplot2)
library(lattice)
library(dplyr)



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

#################################################################################
#####                            CARBON:NITROGEN                            #####
#################################################################################


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

######################################################################
##              Carbon Normalization

#Making mean and min/max for each prey type using Cnorm
si.mean <-si.test %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(Cnorm), Nmean=mean(N), Cmin=min(Cnorm), Cmax= max(Cnorm), 
            Nmin=min(N), Nmax=max(N), Csd=sd(Cnorm), Nsd=sd(N))
si.mean<-si.mean[-1,]

#plot with prey min/max error bars
ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  theme_classic()

ggsave("prey.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

#prey with SD as errorbars
ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmean-Nsd, ymax = Nmean+Nsd, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Csd,xmax = Cmean+Csd, color= PreyCat), height=0) +
  theme_classic()

#################################################################################
##            Trophic Descrimination                              


#changing whiskers for TDF values that are listed in Tyrell paper
##This makes the values way too low. Deleted.

#First add means and Min/max
#whis.mean<-whisker %>%
#  group_by(OtterID) %>%
#  summarise(Cmean=mean(C), Nmean=mean(N), Cmin=min(C), Cmax= max(C), Nmin=min(N), Nmax=max(N))

#now do sd instead of min max
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
  geom_errorbar(data= si.mean, aes(x=Cmean, y=Nmean, ymin = Nmean-Nsd, ymax = Nmean+Nsd, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Cmean, y=Nmean, xmin = Cmean-Csd,xmax = Cmean+Csd, color= PreyCat), height=0) +
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
  geom_errorbar(data= si.mean, aes(x=Cmean, y=Nmean, ymin = Nmean-Nsd, ymax = Nmean+Nsd, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Cmean, y=Nmean, xmin = Cmean-Csd,xmax = Cmean+Csd, color= PreyCat), height=0) +
  geom_point(data=whis.mean, aes(x=TDFC, y=TDFN))+
  theme_classic()

ggsave("whis_prey_nobars.png", device = "png", path = "SI/", width = 8, 
       height = 6, units = "in", dpi = 300)

#now without error bars for otters (all points separate)
ggplot() +
  geom_point(data= si.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= si.mean, aes(x=Cmean, y=Nmean, ymin = Nmean-Nsd, ymax = Nmean+Nsd, color= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x=Cmean, y=Nmean, xmin = Cmean-Csd,xmax = Cmean+Csd, color= PreyCat), height=0) +
  geom_point(data=whisker, aes(x=C-2.5, y=N-3))+
  scale_color_brewer(palette="Dark2")+
  theme_classic()

#Want to add otters as another color, but need to change the pallet.

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

#ODD otter on top of prey graph
oddotter<- filter(whisker, OtterID=="163532")

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
  geom_point(data=whisker, aes(x=C-2.5, y=N-3, color=OtterID))+
  theme_classic()

ggsave("prey_nocarbonnorm.png", device = "png", path = "SI/", width = 6, 
       height = 8, units = "in", dpi = 300)

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
#  Season          3  10.46   3.487   5.128  0.00341 ** 
#  OtterID         9  65.87   7.319  10.764 2.37e-09 ***
#  Season:OtterID 24  40.28   1.678   2.468  0.00303 ** 
#  Residuals      54  36.72   0.680  

season.aov <- aov(C~Season:OtterID, data = whisker)
summary(season.aov)

#                 Df Sum Sq Mean Sq F value  Pr(>F)    
#  Season:OtterID 36 116.61   3.239   4.764 1.4e-07 ***
#  Residuals      54  36.72   0.680


distance.aov <- aov(C~distance + OtterID + distance:OtterID, data = whisker)
summary(distance.aov)

#                   Df Sum Sq Mean Sq F value   Pr(>F)    
#  distance          1   0.14   0.143   0.155   0.6951    
#  OtterID           9  69.90   7.767   8.432 1.91e-08 ***
#  distance:OtterID  9  17.89   1.987   2.158   0.0354 *  
#  Residuals        71  65.40   0.921   

distance.aov <- aov(N~distance + OtterID + distance:OtterID, data = whisker)
summary(distance.aov)

#                   Df Sum Sq Mean Sq F value   Pr(>F)    
#  distance          1  2.443  2.4425   6.514   0.0129 *  
#  OtterID           9 23.670  2.6300   7.014 3.54e-07 ***
#  distance:OtterID  9  1.849  0.2055   0.548   0.8344    
#  Residuals        71 26.624  0.3750    

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

########################################################################
##                         Mixing Models                              ##
##                            MixSIAR                                 ##
########################################################################

library(MixSIAR)
browseVignettes("MixSIAR")
mixsiar_gui() # this won't work and I cannot figure out why
mixsiar.dir <- find.package("MixSIAR")
paste0(mixsiar.dir,"/example_scripts")
source(paste0(mixsiar.dir,"/example_scripts/mixsiar_script_wolves.R"))

#working dir for consumer (whisker)
mix.filename <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/whis_consumer.csv"

# Load the mixture/consumer data
mix <- load_mix_data(filename=mix.filename, 
                     iso_names=c("C","N"), 
                     factors=c("OtterID"), 
                     fac_random=TRUE, 
                     fac_nested=NULL, 
                     cont_effects=NULL)

# working dir for source (prey)
source.filename <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/prey_sources.csv"

# Load the source data
source <- load_source_data(filename=source.filename,
                           source_factors=NULL, 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

# working dir for discrimination factors (prey)
discr.filename <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/prey_discrimination.csv"

# Load the discrimination/TDF data
discr <- load_discr_data(filename=discr.filename, mix)

# Make an isospace plot
plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

# default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
plot_prior(alpha.prior=1,source)

# Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)


jags.1 <- run_model(run="test", mix, source, discr, model_filename,
                    alpha.prior = 1, resid_err, process_err)

jags.1 <- run_model(run="normal", mix, source, discr, model_filename,
                    alpha.prior = 1, resid_err, process_err)

output_options <- list(summary_save = TRUE,
                       summary_name = "summary_statistics",
                       sup_post = FALSE,
                       plot_post_save_pdf = TRUE,
                       plot_post_name = "posterior_density",
                       sup_pairs = FALSE,
                       plot_pairs_save_pdf = TRUE,
                       plot_pairs_name = "pairs_plot",
                       sup_xy = TRUE,
                       plot_xy_save_pdf = FALSE,
                       plot_xy_name = "xy_plot",
                       gelman = TRUE,
                       heidel = FALSE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "diagnostics",
                       indiv_effect = FALSE,
                       plot_post_save_png = FALSE,
                       plot_pairs_save_png = FALSE,
                       plot_xy_save_png = FALSE)

output_JAGS(jags.1, mix, source, output_options)
