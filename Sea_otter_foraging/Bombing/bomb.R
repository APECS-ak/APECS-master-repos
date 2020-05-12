#### BOMBING DATA #####
# Preliminary Analysis#

setwd("/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging")


#load programs
library(ggplot2)
library(lattice)
library(dplyr)

#load files
bomb.test <- read.csv("Bombing/bomb_test.csv")
#summary <- read.csv("Bombing/summary.csv")

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

#removing squid eggs
bomb.test<- filter(bomb.test, PreyCat != is.na(PreyCat))


bomb.test<-filter(bomb.test, Tissue == "whole" )

##########################################################################
##########################################################################
# Visualization #

#Histogram of all KJ by species - 
# hard to really see what is going on
ggplot(data=bomb.test, aes(x=KJ)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(Species))

#Histogram of all KJ by PreyCat - 
# hard to really see what is going on
ggplot(data=bomb.test, aes(x=KJ)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(PreyCat))

#Graph of KJ by season separated by species and prey cat 
# (not a good depiction of this)
ggplot(data= bomb.test) +
  geom_point(aes(x=Season, y=KJ, color=Species, shape=PreyCat)) +
  labs(x="Season", y="KJ (g)") +
  theme_classic()

#Boxplot of KJ by season separated by prey cat - this is better
ggplot(data= bomb.test) +
  geom_boxplot(aes(x=Season, y=KJ, color=PreyCat)) +
  labs(x="Season", y="KJ (g)") +
  theme_classic()

#box plot only by season
ggplot(data= bomb.test) +
  geom_boxplot(aes(x=Season, y=KJ)) +
  labs(x="Season", y="KJ (g)") +
  theme_classic()

#### In this graph I want to draw a line with the averages and the light bands for the SD?
#KJ for each species by season

#Making season a factor to be able to add line
bomb.test$Season<-factor(bomb.test$Season , levels=c("Spring", "Summer", "Winter"))

ggplot(data= bomb.test, aes(y=KJ, x=Season)) +
  geom_point() +
  stat_smooth(aes(x=as.numeric(Season), y=KJ)) +
  labs(x="Season", y="KJ (g)") +
  facet_wrap(vars(Species))

ggsave("bomb_all.png", device = "png", path = "Bombing/", width = 8, 
       height = 6, units = "in", dpi = 300)

#now by PreyCat - also removing Ab, Chiton, Scallop and Mussel
bomb.test2<-filter(bomb.test, PreyCat != "Abalone" & PreyCat != "Chiton" & 
                     PreyCat != "Mussel" & PreyCat != "Scallop")
ggplot(data= bomb.test2, aes(y=KJ, x=Season)) +
  geom_point() +
  stat_smooth(aes(x=as.numeric(Season), y=KJ)) +
  labs(x="Season", y="KJ (g)") +
  facet_wrap(vars(PreyCat))

ggsave("KJ_PreyCat_season.png", device = "png", path = "Bombing/", width = 8, 
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


#######################################################################

#######################################################################

bomb <- read.csv("Bombing/Bomb.csv")
moisture <- read.csv("Bombing/Moisture.csv")
pla <- read.csv("Bombing/pla.csv")
summary <- read.csv("Bombing/Summary.csv")

#combine summary and bomb data
summary.urch <- summary %>%
  filter(Species == "std" | Species == "stf") %>%
  select(SIN, Size.mm, Species, Frozen.Weight,
                        Live.Weight, Dissected.Weight, Site.location, Season)

moisture.urch <- left_join(summary.urch, moisture, by="SIN")
urchin <- left_join(moisture.urch, bomb, by="SIN")
urchin$KJ.wetgram <- NA
urchin$KJ.wetgram <- (1-(urchin$Moisture/100))*urchin$Gross.Heat
urchin$calorie <-urchin$KJ.wetgram*239.006
urchin$calorie.dry <- urchin$Gross.Heat*239.006


#####################################################################################
#
#####################################################################################
#load new summary file
summary <- read.csv("Bombing/Summary_PC.csv")
plab <- read.csv("Bombing/plab.csv")
summary.short <- select(summary, SIN, Size.mm, Species, Frozen.Weight,
                        Live.Weight, Dissected.Weight, Site.location, Season, Tissue, PreyCat)
all <- left_join(plab, summary.short, by="SIN")

# remember moisture is currently in xx.xx, and we need 0.xxxx, also need lipid dry
all$KJ.wetgram <-NA
all$KJ.wetgram <- (1-(all$moisture/100))*all$KJ
all$lipid_dry <- NA
all$lipid_dry <- (all$lipid_wet*100)/(100-all$moisture)
#Removing NA in PreyCat
all<-all %>% drop_na(PreyCat)

#now by PreyCat - also removing Ab, Chiton, Scallop and Mussel
main <- filter(all, PreyCat != "Abalone" & PreyCat != "Scallop" & PreyCat != "Chiton")
#some stars are whole and some are parts - removing all "parts"
main <- filter (main, Tissue == "whole")

#Just printing crab and clam data
bomb.short<-filter(all, PreyCat == "Clam" | PreyCat == "Crab")
ggplot(data= bomb.short, aes(y=KJ, x=Season)) +
  theme_few() +
  geom_point() +
  stat_smooth(aes(x=as.numeric(Season), y=KJ)) +
  labs(x=NULL, y="KJ/ dry gram") +
  facet_wrap(vars(PreyCat))

ggsave("KJ_cc.png", device = "png", path = "Bombing/", width = 12, 
       height = 3, units = "in", dpi = 300)


# kj/dry gram with all main species
ggplot(data= main, aes(y=KJ, x=Season)) +
  theme_few() +
  geom_point() +
  stat_smooth(aes(x=as.numeric(Season), y=KJ)) +
  labs(x=NULL, y="KJ/ dry gram") +
  facet_wrap(vars(PreyCat), scales = "free")

ggsave("KJ_main.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)

#% protein vs %lipid (wet)
lipid<-all %>% drop_na(lipid_wet)
ggplot(data= lipid, aes(x=protein_wet, y=lipid_wet, color = PreyCat)) +
  theme_few() +
  geom_point() +
  stat_smooth(formula = y~x, se= FALSE, method= lm)

ggsave("lp_wet_main.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)

#% protein vs %lipid (dry) - something is wrong with urchins, so removing for the time being

ggplot(data= filter(lipid, PreyCat != "Urchin"), aes(x=protein_dry/KJ, y=lipid_dry/KJ, color = Species)) +
  theme_few() +
  geom_point() +
  stat_smooth(formula = y~x, se= FALSE, method= lm) +
  facet_wrap(vars(Species))

ggsave("lp_dry_main.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)

#dry by species
ggplot(data= filter(lipid, PreyCat != "Urchin"), aes(x=protein_dry, y=lipid_dry)) +
  theme_few() +
  geom_point() +
  stat_smooth(formula = y~x, se= FALSE, method= lm) +
  facet_wrap(vars(Species), scales = "free")

ggsave("lp_dry_species.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)


# Dry Lipid by season
ggplot(data= lipid, aes(x=Season, y=lipid_dry)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=as.numeric(Season), y=lipid_dry)) +
  labs(x= "", y="% Lipid")  +
  facet_wrap(vars(Species), scales = "free", nrow=4)

ggsave("lipid_season.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)


# Dry Protein by season
protein <- all %>% drop_na(protein_wet)
ggplot(data= filter(protein, Species != "map" & Species != "evt" & Species != "pup" & Species != "cao"), aes(x=Season, y=protein_dry)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=as.numeric(Season), y=protein_dry), method = "loess", formula = y~x) +
  labs(x= "", y="% Protein")  +
  facet_wrap(vars(Species), scales = "free", nrow=4)

ggsave("protein_season.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)


# Moisture by season
moisture <- protein %>% drop_na(moisture)
ggplot(data= filter(moisture, Species != "map" & Species != "evt" & Species != "pup" & Species != "cao"), aes(x=Season, y=moisture)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=as.numeric(Season), y=moisture), method = "loess", formula = y~x) +
  labs(x= "", y="% Moisture")  +
  facet_wrap(vars(Species), scales = "free", nrow=4)

ggsave("moisture.p_season.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)

# Ash by season
ash <- all %>% drop_na(ash)
ggplot(data= ash, aes(x=Season, y=ash)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=as.numeric(Season), y=ash)) +
  labs(x= "", y="% Ash")  +
  facet_wrap(vars(PreyCat), scales = "free", nrow=4)

ggsave("ash_season.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)



# Ash by size
ash <- all %>% drop_na(ash)
ggplot(data= ash, aes(x=Size.mm, y=ash)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=Size.mm, y=ash)) +
  labs(x= "Size (mm)", y="% Ash")  +
  facet_wrap(vars(PreyCat), scales = "free", nrow=3)

ggsave("ash_size.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)


# Moisture by size

ggplot(data= moisture, aes(x=Size.mm, y=moisture)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=Size.mm, y=moisture)) +
  labs(x= "Size (mm)", y="% Moisture")  +
  facet_wrap(vars(PreyCat), scales = "free", nrow=3)

ggsave("moisture_size.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)


# Protein by size

ggplot(data= protein, aes(x=Size.mm, y=protein_dry)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=Size.mm, y=protein_dry)) +
  labs(x= "Size (mm)", y="% Protein (dry)")  +
  facet_wrap(vars(PreyCat), scales = "free", nrow=3)

ggsave("protein_size.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)

# Lipid by size
ggplot(data= lipid, aes(x=Size.mm, y=lipid_dry)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=Size.mm, y=lipid_dry)) +
  labs(x= "Size (mm)", y="% Lipid (dry)")  +
  facet_wrap(vars(PreyCat), scales = "free", nrow=3)

ggsave("lipid_size.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)


# Energy by size (PreyCat)
ggplot(data= all, aes(x=Size.mm, y=KJ)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=Size.mm, y=KJ)) +
  labs(x= "Size (mm)", y="KJ/g (dry)")  +
  facet_wrap(vars(PreyCat), scales = "free", nrow=3)

ggsave("energy_size.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)


# Energy vs Protein (PreyCat)
ggplot(data= protein, aes(x=protein_dry, y=KJ)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=protein_dry, y=KJ)) +
  labs(x= "% Protein", y="KJ/g (dry)")  +
  facet_wrap(vars(PreyCat), scales = "free", nrow=3)

ggsave("energy_protein.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)


# Energy vs Lipid (PreyCat)
ggplot(data= lipid, aes(x=lipid_dry, y=KJ)) +
  geom_point() + theme_few() +
  stat_smooth(aes(x=lipid_dry, y=KJ)) +
  labs(x= "% Lipid", y="KJ/g (dry)")  +
  facet_wrap(vars(PreyCat), scales = "free", nrow=3)

ggsave("energy_lipid.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)


# Moisture
ggplot(data= moisture, aes(x=Size.mm, y=moisture)) +
  geom_boxplot() + theme_few() +
  labs(x= "", y="% Moisture")  +
  facet_wrap(vars(PreyCat), scales = "free", nrow=3)

ggsave("moisture_size.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)



####### Crabs by sex 
###############################

crab.count <- plab %>%
  filter(PreyCat=="Crab") %>%
  count(Species, SEX)

crab.mean <- plab %>%
  filter(PreyCat=="Crab") %>%
  group_by(Species, SEX) #%>%
  #summarise(KJ.Mean=mean(Cnorm), Nmean=mean(N), Csd=sd(Cnorm), Nsd=sd(N))
  

ggplot(data=filter(plab, PreyCat=="Crab"), aes(x=Season, y=kcal.dry, color=SEX, shape = gravid)) +
  theme_few() +
  geom_point()+
  facet_wrap(vars(Species))

cam <- filter(plab, Species == "cam")

ggplot(data=filter(plab, PreyCat=="Crab"), aes(x=Season, y=lipid.dry, color=SEX, shape = gravid)) +
  theme_few() +
  geom_point()+
  facet_wrap(vars(Species))

ggplot(data=filter(plab, PreyCat=="Crab"), aes(x=Season, y=protein_dry, color=SEX, shape = gravid)) +
  theme_few() +
  geom_point()+
  facet_wrap(vars(Species))

ggplot(data=filter(plab, PreyCat=="Crab"), aes(x=Season, y=kcal.wet, color=SEX, shape = gravid)) +
  theme_few() +
  geom_point()+
  facet_wrap(vars(Species))


# looking at egg values
#run code from macronutrients
all <- drop_na(plab)
all <- left_join(all, summary, by="SIN")
plab2 <- left_join(plab, summary, by="SIN")
plab2<-plab2 %>% drop_na(PreyCat)
plab2$gravid <- if(plab2$gravid == "") {"n"}

ggplot(data=filter(plab2, Species=="cam"), aes(x=Season, y=protein_dry, color=gravid)) +
  theme_few() +
  geom_point()

ggplot(data=filter(plab2, Species=="cam"), aes(x=Season, y=lipid.dry, color=gravid)) +
  theme_few() +
  geom_point()

ggplot(data=filter(plab2, Species=="cam"), aes(x=month, y=kcal.dry, color=gravid)) +
  theme_few() +
  geom_point() +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), 
               expand = c(0.1,0))+
  labs(x="", y="Kcal (per dry gram)") +
  scale_color_manual(values=c("steelblue2", "springgreen2", "tomato2"), 
                    name="Gravid",
                    breaks=c("", "y", "e"),
                    labels=c("No", "Yes", "Eggs"))

ggsave("cam_cal.png", device = "png", path = "Bombing/", width = 8, 
       height = 6, units = "in", dpi = 300)

ggplot(data=filter(plab2, Species=="cam"), aes(x=month, y=kcal.wet, color=gravid)) +
  theme_few() +
  geom_point() +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), 
               expand = c(0.1,0))+
  labs(x="", y="Kcal (per wet gram)") +
  scale_color_manual(values=c("steelblue2", "springgreen2", "tomato2"), 
                     name="Gravid",
                     breaks=c("", "y", "e"),
                     labels=c("No", "Yes", "Eggs"))



############################################################################################
##                                         Tables                                         ##
############################################################################################

#data from macronutrients.rmd

plab.mean <-plab %>%
  group_by(Season, Species) %>%
  summarise(length.mean= mean(Size.mm, na.rm = T), length.sd= sd(Size.mm, na.rm = T), 
            length.n=length(Size.mm[!is.na(Size.mm)]), mass.mean=mean(Dissected.Weight, na.rm = T), 
            mass.sd=sd(Dissected.Weight, na.rm = T), mass.n=length(Dissected.Weight[!is.na(Dissected.Weight)]), 
            edible.mean=mean(edible, na.rm = T), edible.sd=sd(edible, na.rm = T), 
            edible.n=length(edible[!is.na(edible)]), Kcal.mean=mean(kcal.dry, na.rm = T), 
            Kcal.sd=sd(kcal.dry, na.rm = T), Kcal.n=length(kcal.dry[!is.na(kcal.dry)]), 
            protein.mean=mean(protein_dry, na.rm = T), protein.sd=sd(protein_dry, na.rm = T), 
            protein.n=length(protein_dry[!is.na(protein_dry)]), lipid.mean=mean(lipid_dry, na.rm = T), 
            lipid.sd=sd(lipid_dry, na.rm = T), lipid.n=length(lipid_dry[!is.na(lipid_dry)]), 
            ash.mean=mean(ash, na.rm = T), ash.sd=sd(ash, na.rm = T), ash.n=length(ash[!is.na(ash)]),
            moisture.mean=mean(moisture, na.rm = T), moisture.sd= sd(moisture, na.rm = T), 
            moisture.n=length(moisture[!is.na(moisture)]))
write.csv(plab.mean, "Bombing/plab_means.csv")


plab.mean.preycat <-plab %>%
  group_by(Season, PreyCat) %>%
  summarise(length.mean= mean(Size.mm, na.rm = T), length.sd= sd(Size.mm, na.rm = T), 
            length.n=length(Size.mm[!is.na(Size.mm)]), mass.mean=mean(Dissected.Weight, na.rm = T), 
            mass.sd=sd(Dissected.Weight, na.rm = T), mass.n=length(Dissected.Weight[!is.na(Dissected.Weight)]), 
            edible.mean=mean(edible, na.rm = T), edible.sd=sd(edible, na.rm = T), 
            edible.n=length(edible[!is.na(edible)]), Kcal.mean=mean(kcal.dry, na.rm = T), 
            Kcal.sd=sd(kcal.dry, na.rm = T), Kcal.n=length(kcal.dry[!is.na(kcal.dry)]), 
            protein.mean=mean(protein_dry, na.rm = T), protein.sd=sd(protein_dry, na.rm = T), 
            protein.n=length(protein_dry[!is.na(protein_dry)]), lipid.mean=mean(lipid_dry, na.rm = T), 
            lipid.sd=sd(lipid_dry, na.rm = T), lipid.n=length(lipid_dry[!is.na(lipid_dry)]), 
            ash.mean=mean(ash, na.rm = T), ash.sd=sd(ash, na.rm = T), ash.n=length(ash[!is.na(ash)]),
            moisture.mean=mean(moisture, na.rm = T), moisture.sd= sd(moisture, na.rm = T), 
            moisture.n=length(moisture[!is.na(moisture)]))
write.csv(plab.mean.preycat, "Bombing/plab_means_preycat.csv")
