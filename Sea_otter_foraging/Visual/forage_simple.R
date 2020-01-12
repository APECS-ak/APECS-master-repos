####### First look at foraging data #####

#load programs
library(ggplot2)
library(lubridate)
library(lattice)
library(dplyr)

###  OLD DATA  ###
#s.prop <- read.csv("s_prop.csv") # import a file with the male/female proportions
#age.prop <- read.csv("age_prop.csv")
#all.prop <- read.csv("all_prop.csv")
#year.prop <- read.csv("year_prop.csv")
#otter.prop<- read.csv("otter_by_prop.csv")
#otter.gram<- read.csv("otter_by_gram.csv", row.names=1)
#ott.sum <- read.csv("otter_sum.csv", row.names=1)
#ott.raw <- read.csv("2018_Foraging_data_RAW.csv")
#s.gram<- read_csv("sex_table.csv")
#prey <- read.csv("Prey_Class.csv")
#sex.prey <- read.csv("sex_prey.csv")
#compare.prop <- read.csv("compare.csv")

prop<-read.csv("Visual/SOFA/short_prop.csv")
fo<-read.csv("Visual/SOFA/fo.csv")

## RAW DATA 
forage<-read.csv("2018_Foraging.csv")

# to make the classes order in the way I want
as.factor()

## add a prey class into raw data
#ott.raw$PreyItem%%select()

forage$PreyCat <- NA
forage$PreyCat <- ifelse(forage$PreyItem == "APC" | forage$PreyItem == "CUC" |  forage$PreyItem == "CUM", 
                   "Cucumber", 
                  ifelse(forage$PreyItem == "CLA" | forage$PreyItem == "CLN" | forage$PreyItem == "GAC" | 
                           forage$PreyItem == "MAN" | forage$PreyItem == "MAP" | forage$PreyItem == "MAS" | 
                           forage$PreyItem == "MYA" | forage$PreyItem == "MYS" | forage$PreyItem == "MYT" | 
                           forage$PreyItem == "PRS" | forage$PreyItem == "SAG" | forage$PreyItem == "TRC", "Clam", 
                   ifelse(forage$PreyItem == "STF" | forage$PreyItem == "URC" | forage$PreyItem == "STD" | 
                            forage$PreyItem == "STP", "Urchin", 
                   ifelse(forage$PreyItem == "CAM" | forage$PreyItem == "CAN" | forage$PreyItem == "CAP" | 
                            forage$PreyItem == "CRA" | forage$PreyItem == "KCR" | forage$PreyItem == "PUP" | 
                            forage$PreyItem == "PUS" | forage$PreyItem == "TEC", "Crab",
                   ifelse(forage$PreyItem == "CEF" | forage$PreyItem == "SNA", "Snail", 
                   ifelse(forage$PreyItem == "MUS" | forage$PreyItem == "MTR" | forage$PreyItem == "MOM", "Mussel", 
                   ifelse(forage$PreyItem == "PIO" | forage$PreyItem == "EVT" | forage$PreyItem == "PES", "Star", NA)))))))

forage$Occupation <- NA
forage$Occupation <- ifelse(forage$YEAR == "1975", "40 years", ifelse(forage$YEAR == "1988", "30 years",
                      ifelse(forage$YEAR == "1994", "15 years", ifelse(forage$YEAR == "2003", "15 years", 
                      ifelse(forage$YEAR == "2010", "8 years", NA)))))

#Changing date to julian date.
forage$Date <- as.Date(forage$Date, "%m/%d/%y")
forage$julian <- yday(forage$Date)

#calculate Spring/ Summer turnover (Using our seasons June 21)
solstice <- as.Date("06-21", format = "%m-%d")
yday(solstice)

#Now make seasons column
forage$Season <- NA
forage$Season <- ifelse(forage$julian<173, "Spring", "Summer")
                             
 
# checking the data frame
is.data.frame(forage) # will say if this is a dataframe, should say TRUE
dim(forage)
names(forage)
str(forage)
head(forage) 


#sort the data (using dplyr)
#all<-arrange(all, desc(prop))
#age.prop<-arrange(age.prop, desc(prop))
#s.prop<-arrange(s.prop, desc(prop))
#year.prop<-arrange(year.prop, desc(prop))

# ALL Proportions BAR GRAPH #
ggplot(data = prop) +
  theme_few() +
  geom_col(mapping= aes(x = species, y = prop, fill=species)) +
  scale_x_discrete(limits=c("clam","crab","snail","cucumber", "urchin", "mussel")) +
  geom_errorbar(aes(x= species, ymin=prop-sd, ymax=prop+sd), width=.2) +
  scale_y_continuous(labels =scales::percent_format(accuracy = 1), breaks=c(0,.1,.2,.3,.4,.5,.6,.7)) +
  labs(x= "", y= "Proportion of diet (in biomass)", tag="A") +
  theme(axis.text.y = element_text(size=12),
      axis.text.x = element_text(size=14, angle=45, hjust=1),
      axis.title.y=element_text(size=12))
  

ggsave("prop_bar.png", device = "png", path = "Visual/", width = 8, 
       height = 7, units = "in", dpi = 300)

# Frequency of Occurance BAR GRAPH #
ggplot(data = fo) +
  theme_few() +
  geom_col(mapping= aes(x = species, y = fo, fill=species)) +
  scale_x_discrete(limits=c("clam","crab","snail","cucumber", "urchin", "mussel")) +
  scale_y_continuous(labels =scales::percent_format(accuracy = 1), breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9, 1)) +
  expand_limits(y = c(0, 1)) +
  labs(x= "", y= "Frequency of occurrence (foraging bouts)") +
  theme(axis.text.y = element_text(size=14, face = "bold"),
        axis.text.x = element_text(size=18, angle=45, hjust=1, face = "bold"),
        axis.title.y=element_text(size=18, face = "bold"), legend.position = "none")


ggsave("fo_bar.png", device = "png", path = "Visual/", width = 8, 
       height = 7, units = "in", dpi = 300)

# AGE BAR GRAPH #
#ggplot(data = age.prop) +
#  geom_col(mapping= aes(x = species, y = prop, fill = age),
#    position = "dodge")


# SEX #
ggplot(data = s.prop) + 
  theme_classic() +
  geom_col(mapping= aes(x = species, y = prop, fill = sex),
    position = "dodge") +
  scale_y_continuous(labels = percent, breaks=c(0,.1,.2,.3,.4,.5,.6,.7)) +
  scale_x_discrete(limits=c("clam","crab","snail","cucumber", "urchin", "mussel", "star", 
                              "chiton", "abalone")) +
  labs(x= "", y= "Proportion of diet in biomass") +
  theme(axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=14, angle=45, hjust=1), 
        axis.title.y = element_text(size = 12))


# Comparing Nicole and Zac #
dodge <- position_dodge(width=0.9)
ggplot(data = compare.prop, aes(x = species, y = prop, fill = year)) + 
  theme_classic() + 
  geom_col(position = "dodge") +
  geom_errorbar(aes(x= species, ymin=prop-sd, ymax=prop+sd), 
                position = position_dodge(0.9), width=.4) +
  scale_y_continuous(labels = percent, breaks=c(0,.1,.2,.3,.4,.5,.6,.7)) +
  scale_x_discrete(limits=c("clam","crab","snail","cucumber", "urchin", "mussel", "star", 
                            "chiton", "abalone")) +
  labs(x= "", y= "Proportion of diet in biomass") +
  scale_fill_manual(values=c("#999999", "#0072B2")) +
  theme(axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=14, angle=45, hjust=1), 
        axis.title.y = element_text(size = 12))

# YEAR # The area list isn't working because it is a number...
#ggplot(data = year.prop) +
#  theme_classic() +
#  geom_col(mapping= aes(x = species, y = prop, fill = area),
#    position = "dodge")

# graph for Clam size vs year
ggplot(data= filter(ott.raw, PreyCat=="Clam")) +
  theme_classic() + labs(y = "Clam Size (mm)", x= "Survey Year") +
  geom_point(mapping = aes(x= YEAR, y= Size), 
           position = "jitter")
# graph for SAG size vs region
ggplot(data= filter(ott.raw, PreyCat=="Clam")) +
  theme_classic() + 
  geom_point(mapping = aes(y= Size, x= Region),
             position = "jitter")
# graphing clam density at different sizes for occupation time. 
# scale_fill_brewer(palette="Spectral")
# scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73"))
# add in adjust = 5 to geom_density to make a smooth histogram
ggplot(data= filter(ott.raw, PreyCat=="Clam")) +
  theme_classic() + 
  geom_density(mapping = aes(x= Size, color = Occupation)) +
  scale_y_continuous(labels = percent, breaks=c(0,.05, .1, .15)) +
  scale_fill_brewer(palette="Set1") +
  labs(x= "Clam size", y= "Percent frequency") +
  theme(axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12))

ggplot(data= filter(ott.raw, PreyItem=="SAG")) +
  theme_classic() + 
  geom_density(mapping = aes(x= Size, color = Occupation), adjust= 5)

# graph showing Clam size by "where"
ggplot(data= filter(ott.raw, PreyCat == "Clam" & where == "KC"| where =="OW")) +
  theme_classic() +
  geom_point(mapping = aes(x= where, y= Size),
             position = "jitter")

ggplot(data= filter(ott.raw, PreyCat == "Clam" & where == "KC"| where =="OW")) +
  theme_classic() +
  geom_freqpoly(mapping = aes(x= Size, color = where), bins = 3)

ggplot(data= filter(ott.raw, PreyCat == "Clam" & where == "KC"| where =="OW")) +
  theme_classic() +
  geom_violin(mapping = aes(y= Size, x = where), scale= "area")

ggplot(data= filter(ott.raw, PreyItem!="UNK")) +
  theme_classic() +
  geom_point(mapping = aes(x= where, y= PreyItem),
             position = "jitter", na.rm=T)

ggplot(data= ott.raw) +
  theme_classic() +
  geom_col(mapping = aes(x= PreyCat, y= Size, fill=where),
             position = "dodge", na.rm=T)
## Stat for Sex differences ##
s.prop.aov <-aov(prop~species+sex, s.prop)
summary(s.prop.aov)

sex.prey<- na.omit(sex.prey)
chisq.test(sex.prey)


########################## Prey by otter ###########################
####################################################################

# USING OTTER_SUM AND OTTER_BY_GRAM

#make sure ott sum and ott gram are equal
identical(row.names(ott.sum), row.names(otter.gram))

# make one data frame
otter <- cbind(otter.gram, Ageclass = ott.sum$Ageclass, Sex = ott.sum$Sex, Area = ott.sum$Area)

#pca of just prey types
otter.pca <- prcomp(otter[,1:11])
summary(otter.pca)
biplot(otter.pca)
otter.pca

ott.pc <- cbind(otter, otter.pca$x[,1:2])  # Save PC scores 
head(ott.pc)

# none of these graphs are interesting. Doesn't say anything about the variables
ggplot(ott.pc, aes(PC1, PC2, color=factor(Ageclass))) +
  geom_point(size=5)

ggplot(ott.pc, aes(PC1, PC2, color=factor(Area))) +
  geom_point(size=5)

ggplot(ott.pc, aes(PC1, PC2, color=factor(Sex))) +
  geom_point(size=5)



### Chi Sq for years by clams

clam <- filter(ott.raw, PreyCat=="Clam")

chisq.test(ott.raw$Size, ott.raw$Occupation, simulate.p.value = TRUE)

chisq.test(ott.raw$PreySz, ott.raw$Occupation)

hist(clam$Size)
plot(clam$where)

###############################################################
##                    Frequency of Occurance                 ##
###############################################################

#write.csv(forage, "forage.csv")


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

count.season<- forage %>%
  count(BoutID, PreyCat, Season) %>%
  na.omit() %>%
  count(PreyCat, Season)

count.season$fo<-NA
count.season$fo<-(count.season$n / 181)

#graphing the seasons
ggplot(data= count.season, aes(x= PreyCat, y=fo)) +
  geom_col(aes(fill=Season), position = "dodge") +
  theme_classic()


