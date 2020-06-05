####### First look at foraging data #####

#load programs
library(ggplot2)
library(lubridate)
library(lattice)
library(dplyr)
library(scales)
library(tidyr)

#loading final
forage<-read.csv("Visual/forage_final.csv")


#### Save this for stats later (comment made 5/12/20)
### Chi Sq for years by clams

clam <- filter(forage, PreyCat=="Clam")

chisq.test(ott.raw$Size, ott.raw$Occupation, simulate.p.value = TRUE)

chisq.test(ott.raw$PreySz, ott.raw$Occupation)

hist(ott.raw$Size)
plot(ott.raw$where)

###############################################################
##                    Frequency of Occurance                 ##
###############################################################

#need to group by bout, and preycat then count
count.forage<- forage %>%
  count(BoutID, PreyCat) %>%
  na.omit() %>%
  count(PreyCat)

#Now counting how many for each prey cat and devide by total bouts
count.forage$fo<-NA
count.forage$fo<-(count.forage$n / 362)


#Make the same thing for each season

#first find out how many bouts for each season:

sp<- forage %>% 
  filter(forage$Season == "Spring") %>%
  count(BoutID)
sp #182

su<- forage %>% 
  filter(forage$Season == "Summer") %>%
  count(BoutID)
su #180

#remove NA from preycat
forage <- filter(forage, PreyCat != is.na(PreyCat))
count.season<- forage %>%
  count(BoutID, PreyCat, Season) %>%
  na.omit() %>%
  count(PreyCat, Season)


count.season$fo<-NA
count.season$fo<-(count.season$n / 181)
#only do once, 
#write.csv(count.season, "Visual/fo_season.csv")
#graphing the seasons
ggplot(data= count.season, aes(x= PreyCat, y=fo)) +
  geom_col(aes(fill=Season), position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(y= "Frequency of Occurrence (per bout)", x=NULL) +
  scale_fill_grey() +
  theme_classic()

ggsave("fo_season.png", device = "png", path = "Visual/", width = 8, 
       height = 7, units = "in", dpi = 300)


##############################################################################
#FO for each dive
forage.dive<- forage %>% unite("dive.no", BoutID, DiveNo, sep="-")

#How many dives
count.dive<- forage.dive %>% 
  filter(Suc != "N" & Suc != "I") %>%
  count(dive.no) 

#3,522 inlcuding no success
#3,178 not including no success

#need to group by dive, and preycat then count
count.forage<- forage.dive %>%
  count(dive.no, PreyCat) %>%
  na.omit() %>%
  count(PreyCat)

#Now counting how many for each prey cat and devide by total bouts
count.forage$fo<-NA
count.forage$fo.s<-NA
count.forage$fo<-(count.forage$n / 3522)
count.forage$fo.s<-(count.forage$n / 3167)

# ENTER GRAPH HERE?

#FO by dive by season
sp.dive<- forage.dive %>% 
  filter(forage.dive$Season == "Spring") %>%
  filter(Suc != "N" & Suc != "I") %>%
  count(dive.no)
sp.dive #1610

su.dive<- forage.dive %>% 
  filter(forage.dive$Season == "Summer") %>%
  filter(Suc != "N" & Suc != "I") %>%
  count(dive.no)
su.dive #1557

count.season<- forage.dive %>%
  count(dive.no, PreyCat, Season) %>%
  na.omit() %>%
  count(PreyCat, Season)

count.season$fo<-NA
count.season$fo<-ifelse(count.season$Season== "Summer", count.season$n / 1557, count.season$n / 1610)

#make csv, if needed 
write.csv(count.season, "Visual/fo_season.csv")

#graphing the seasons by dive
ggplot(data= count.season, aes(x= PreyCat, y=fo)) +
  geom_col(aes(fill=Season), position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(y= "Frequency of Occurrence (per dive)", x=NULL) +
  scale_fill_grey() +
  theme_classic()

ggsave("fo_season_dive.png", device = "png", path = "Visual/", width = 8, 
       height = 7, units = "in", dpi = 300)


#FO by bout
forage.filter <- filter(forage, PreyCat != is.na(PreyCat))
f.count<- forage.filter %>%
  count(BoutID, PreyCat) %>%
  na.omit() %>%
  count(PreyCat)

f.count$fo<-NA
f.count$fo<-(f.count$n / 362)

#FO for each season by bout
#first find out how many bouts for each season:

sp<- forage %>% 
  filter(forage$Season == "Spring") %>%
  count(BoutID)
sp #182

su<- forage %>% 
  filter(forage$Season == "Summer") %>%
  count(BoutID)
su #180

#remove NA from preycat
forage <- filter(forage, PreyCat != is.na(PreyCat))
count.season<- forage %>%
  count(BoutID, PreyCat, Season) %>%
  na.omit() %>%
  count(PreyCat, Season)


count.season$fo<-NA
count.season$fo<-(count.season$n / 181)
#only do once, 
#write.csv(count.season, "Visual/fo_season.csv")
#graphing the seasons
ggplot(data= count.season, aes(x= PreyCat, y=fo)) +
  geom_col(aes(fill=Season), position = "dodge") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(y= "Frequency of Occurrence (per bout)", x=NULL) +
  scale_fill_grey() +
  theme_classic()

ggsave("fo_season.png", device = "png", path = "Visual/", width = 8, 
       height = 7, units = "in", dpi = 300)

#printing all species listed in forage data
species <- summary(forage$PreyItem)
species

ggplot(data= species.forage) +
  theme_few() +
  geom_col(aes(x=PreyItem, y = n, fill = observer), position = "dodge")

ggplot(data= size.forage) +
  theme_few() +
  geom_col(aes(x=PreySz, y= n, fill = observer), position = "dodge")

species.forage <- forage %>% 
  drop_na(PreyItem) %>%
  group_by(observer) %>%
  count(PreyItem)

species.forage2 <- spread(species.forage, PreyItem, n)

species.forage2[is.na(species.forage2)] <- 0
chisq.test(species.forage2)

species.forage <- forage %>% 
  drop_na(PreyItem) %>%
  group_by(observer) %>%
  count(PreyItem)


size.forage <- forage %>% 
  drop_na(PreySz) %>%
  group_by(observer) %>%
  count(PreySz)


size.forage2 <- spread(size.forage, PreySz, n)


size.forage2 <- size.forage2[-2,] 
size.forage2<-size.forage2[,-5]
size.forage2<-size.forage2[,-11]
size.forage2<-size.forage2[,-11]
size.forage2<-size.forage2[,-1]
chisq.test(size.forage2)
#data:  size.forage2
#X-squared = 37.764, df = 9, p-value = 1.922e-05



#####################
##  Compare to mix model
#####################

#need biomass values

ggplot(aes(y = value, x = source, fill = source), data = forage.all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  facet_grid(Season~Site) +
  theme( axis.title=element_text(size=16), legend.position = "none", 
         axis.text.x = element_text(angle = -45, hjust=0, size =12), 
         axis.text.y = element_text(size = 12),
         strip.text=element_text(size = 12, face="bold"))


#list species
species.count <- forage %>%
  group_by(PreyItem) %>%
  count()


## Looking at intertidal dives
forage$where <- as.factor(forage$where)
summary(forage$where)

intertidal <- filter(forage, where == "IN" | where == "ER")

ggplot(data= intertidal) +
  theme_few() +
  geom_bar(aes(x= Region))


# dive times for females with pup
## biomass from biomass.Rmd

status <- biomass %>%
  drop_na(Status) %>%
  drop_na(MnDvTm)

ggplot(status, aes(x = Status, y= MnDvTm))+
  theme_few()+
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=MnDvTm-sdDvTm, ymax=MnDvTm+sdDvTm), width = .2)



crab <- plab %>%
  filter(PreyCat == "Crab") %>%
  group_by(Species, Season, SEX) %>%
  summarise(mean= mean(lipid_dry, na.rm=T), sd= sd(lipid_dry, na.rm=T))

lipid <- plab %>%
  group_by(Species, Season, SEX) %>%
  summarise(mean= mean(lipid_dry, na.rm=T), sd= sd(lipid_dry, na.rm=T))


