############  FISH631 Project  ############

# I am stting the working dir at the usual foraging folder, but I may want to make a new folder
setwd("/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging")

#programs
library(ggplot2)
library(lattice)
require(stats)
library(tidyverse)
library(scales)

#load files
otter.gram<- read.csv("Visual/SOFA/Otter_gram.csv", row.names=1)
ott.sum <- read.csv("Visual/SOFA/Otter_summary.csv", row.names=1)
ott.raw <- read.csv("Visual/2018_Foraging_data_RAW.csv")

#stack otter.gram
stack.gram <- stack(otter.gram)
names(stack.gram) <- c("gram", "species")

stack.gram[stack.gram==0]<-NA

# plot hist of all the grams/min
ggplot(data=stack.gram, aes(x=gram)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(species))

hist(otter.gram$clam)
hist(otter.gram$crab)


# rename areas
ott.sum$Area<-recode_factor(ott.sum$Area, "EIGHTYEIGHT"=">30 years", 
                            "OHTHREE"=">15 years", "TEN"="> 8 years", .ordered = FALSE)
#recode_factor(.x, ..., .default = NULL, .missing = NULL,.ordered = FALSE)

### Standardize to species maximum: - Don't think this will be useful in the end
#MAX <- apply(otter.gram,2,max)    # Maximum values in each column
#gram.std <- scale(otter.gram, center=F, scale = MAX)
#head(gram.std)

#stack gram.std - doesn't work
#stack.gram.std <- stack(gram.std)
#names(stack.gram.std) <- c("gram", "species")

#Transform otter.gram with square root
(otter.gram.tf<- otter.gram^.25)
#stack transformed data
stack.gram.tf <- stack(otter.gram.tf)
names(stack.gram.tf) <- c("gram", "species")

stack.gram.tf[stack.gram.tf==0]<-NA

# plot hist of all the grams/min transformed
ggplot(data=stack.gram.tf, aes(x=gram)) + 
  geom_histogram(binwidth = .2) +
  facet_wrap(vars(species)) +
  theme_bw()

# info from ott.sum
par(mfrow= c(2,2))
plot(ott.sum$Sex, main = "Sex")
plot(ott.sum$Ageclass, main = "Age")
plot(ott.sum$Area, main = "Locations (year)")
hist(ott.sum$success_rt,  main = "Histogram of Sucess rate", xlab= "", ylab= "")
par(mfrow= c(1,1))
#combine files
otter <- cbind(otter.gram.tf, Ageclass = ott.sum$Ageclass, Sex = ott.sum$Sex, Area = ott.sum$Area, 
               Season = ott.sum$Season)
otter
# advice from Franz - gives # of 0 in each prey type
apply(otter.gram,2,function(x) sum(x==0))

# create Bray-curtis distances
library(vegan)
library(MASS)
otter.bray <- vegdist(otter.gram.tf, method = "bray")
round(otter.bray,2)

#betadispr with Area
(disp <- betadisper(otter.bray, otter$Area))
PCoA.d <- dist(disp$vectors[,1:10])
plot(otter.bray, PCoA.d); cor(otter.bray, PCoA.d)
anova(disp)
TukeyHSD(disp)
plot(disp) 
plot(disp, c(2,3))
#betadispr with Sex (including unknown)
(disp <- betadisper(otter.bray, otter$Sex))
PCoA.d <- dist(disp$vectors[,1:10])
plot(otter.bray, PCoA.d); cor(otter.bray, PCoA.d)
anova(disp)
TukeyHSD(disp)
plot(disp) 
plot(disp, c(2,3))
#betadispr with Ageclass
(disp <- betadisper(otter.bray, otter$Ageclass))
PCoA.d <- dist(disp$vectors[,1:10])
plot(otter.bray, PCoA.d); cor(otter.bray, PCoA.d)
anova(disp)
TukeyHSD(disp)
plot(disp) 
plot(disp, c(2,3))
#betadispr with Season
(disp <- betadisper(otter.bray, otter$Season))
PCoA.d <- dist(disp$vectors[,1:10])
plot(otter.bray, PCoA.d); cor(otter.bray, PCoA.d)
anova(disp)
TukeyHSD(disp)
plot(disp) 
plot(disp, c(2,3))

# All of these metrics were not significant, which is good! But season is really close to 0.05


# NMDS - can use 2D because is simpler
otter.nmds <- isoMDS(otter.bray, k=2) 
#type I PERMANOVA
adonis(otter.bray ~ Season + Ageclass + Sex + Area + Site, data=ott.sum, perm=999) 
adonis(otter.bray ~ Sex + Area + Site + Sex:Area, data=ott.sum, perm=999)
adonis(otter.bray ~ Sex + Area + Site, data=ott.sum, perm=999) # best model, only explains 19%
# type III PERMANOVA
adonis2(otter.bray ~ Sex + Ageclass + Season + Area + Site, data=ott.sum, by="margin", perm=999)
adonis2(otter.bray ~ Sex + Area + Site, data=ott.sum, by="margin", perm=999)

ott.mds<-metaMDS(otter.gram.tf, k=2, try=20, autotransform = FALSE)
ott.mds$stress
stressplot(ott.mds)
ordiplot(ott.mds, display=c("sites", "species"), type="t", cex=1)
plot(ott.mds$points, col=as.numeric(ott.sum$Area), pch=16, cex=2, asp=1)
plot(ott.mds$points, col=as.numeric(ott.sum$Sex), pch=16, cex=2, asp=1)
plot(ott.mds$points, col=as.numeric(ott.sum$Site), pch=16, cex=2, asp=1)

######################################################################################################
#Now lets remove worm, chiton, barnicle, and abalone (these make up a very small percent of the diet)
ott.slim <- otter.gram.tf[,c(1:6,10)] 

#combine files
otter.slim <- cbind(ott.slim, Ageclass = ott.sum$Ageclass, Sex = ott.sum$Sex, Area = ott.sum$Area, Site = ott.sum$Site)
otter.slim

# create Bray-curtis distances
otter.bray2 <- vegdist(ott.slim, method = "bray")
round(otter.bray2,2)
# MDS - can use 2D because is simpler
otter2.nmds <- isoMDS(otter.bray2, k=2) 
#type I PERMANOVA
adonis(otter.bray2 ~ Sex + Area + Site + Sex:Area, data=ott.sum, perm=999)
slim.perm<-adonis(otter.bray2 ~ Sex + Area + Site, data=ott.sum, perm=999) # best model (others not sig)
slim.perm
# type III PERMANOVA
#adonis2(otter.bray2 ~ Sex + Ageclass + Area + Site, data=ott.sum, by="margin", perm=999)
#adonis2(otter.bray2 ~ Sex + Area, data=ott.sum, by="margin", perm=9999)

ott2.mds<-metaMDS(ott.slim, k=2, try=50, autotransform = FALSE)
ott2.mds$stress
stressplot(ott2.mds)
#ordiplot(ott2.mds, display=c("sites", "species"), type="t", cex=1)
#plot(ott2.mds$points, col=as.numeric(otter.slim$Area), pch=16, cex=2, asp=1)
#plot(ott2.mds$points, col=as.numeric(otter.slim$Sex), pch=16, cex=2, asp=1)
#plot(ott2.mds$points, col=as.numeric(otter.slim$Season), pch=16, cex=2, asp=1)
#plot(ott2.mds$points, col=as.numeric(otter.slim$Site), pch=16, cex=2, asp=1) # not helpful

(vec.slim <- envfit(ott2.mds$points, ott.slim, perm=1000))# circular because no env data
(sim.area <- simper(otter.gram.tf>0, otter.slim$Area, perm=999))
summary(sim.area)
(sim.sex <- simper(otter.gram.tf>0, otter.sex$Sex))
summary(sim.sex)

# make ordination plot in ggplot for Areas

#first make a data frame of the scores
area.scores <- as.data.frame(scores(ott2.mds))
# create a column of site names, from the rownames of data.scores
area.scores$otter <- rownames(area.scores)  
area.scores <- cbind(area.scores, Area = otter.slim$Area, Site = otter.slim$Site)
head(area.scores)

#Using the scores function from vegan to extract the species scores and convert to a data.frame
species2.scores <- as.data.frame(scores(ott2.mds, "species"))  
# create a column of species, from the rownames of species.scores
species2.scores$species <- rownames(species2.scores)  
head(species2.scores)

ggplot() + 
  geom_text(data=species2.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=area.scores,aes(x=NMDS1,y=NMDS2,shape=Area,colour=Area),size=3) + # add the point markers
  scale_colour_manual(values=c(">15 years" = "#E69F00", "> 8 years" = "#56B4E9", ">30 years" = "#999999")) +
  coord_equal() + 
  theme_bw() +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


#placing a hull for area
area.88 <- area.scores[area.scores$Area == ">30 years", ][chull(area.scores[area.scores$Area == 
                                                                   ">30 years", c("NMDS1", "NMDS2")]), ]  # hull values for 1988
area.03 <- area.scores[area.scores$Area == ">15 years", ][chull(area.scores[area.scores$Area == 
                                                                   ">15 years", c("NMDS1", "NMDS2")]), ]  # hull values for 2003
area.10 <- area.scores[area.scores$Area == "> 8 years", ][chull(area.scores[area.scores$Area == 
                                                                         "> 8 years", c("NMDS1", "NMDS2")]), ]  # hull values for 2010

hull2.data <- rbind(area.88, area.03, area.10)  #combine grp.a and grp.b
hull2.data
ggplot() + 
  geom_polygon(data=hull2.data,aes(x=NMDS1,y=NMDS2,fill=Area,group=Area),alpha=0.30) + 
  scale_fill_manual(values=c(">15 years" = "#E69F00", "> 8 years" = "#56B4E9", ">30 years" = "#999999")) +
  geom_point(data=area.scores,aes(x=NMDS1,y=NMDS2,shape=Area,colour=Area),size=4) + # add the point markers
  scale_colour_manual(values=c(">15 years" = "#E69F00", "> 8 years" = "#56B4E9", ">30 years" = "#999999")) +
  geom_text(data=species2.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

#####################################################################################################
#Now run with ONLY known sex!
# want to filter the data
otter.sex <- filter(otter.slim, Sex == "M"| Sex == "F")

#now make a file of the distances only by removing them from ott.sex
ott.sex <- otter.sex[,1:7]

# create Bray-curtis distances
otter.bray3 <- vegdist(ott.sex, method = "bray")
round(otter.bray3,2)
# MDS - can use 2D because is simpler
otter3.nmds <- isoMDS(otter.bray3, k=2) 
#type I PERMANOVA
sex.perm<-adonis2(otter.bray3 ~ Sex + Area + Site, data=otter.sex, perm=9999)
# type III PERMANOVA
adonis2(otter.bray3 ~ Sex + Ageclass + Area + Site, data=otter.sex, by="margin", perm=9999)
adonis2(otter.bray3 ~ Sex + Area, data=otter.sex, by="margin", perm=9999)

ott3.mds<-metaMDS(ott.sex, k=2, try=20, autotransform = FALSE)
ott3.mds$stress
stressplot(ott3.mds)
#ordiplot(ott3.mds, display=c("sites", "species"), type="t", cex=1)
#plot(ott3.mds$points, col=as.numeric(otter.sex$Area), pch=16, cex=2, asp=1)
#plot(ott3.mds$points, col=as.numeric(otter.sex$Sex), pch=16, cex=2, asp=1)
#plot(ott3.mds$points, col=as.numeric(otter.sex$Site), pch=16, cex=2, asp=1)

# make ordination plot in ggplot

#first make a data frame of the scores
data.scores <- as.data.frame(scores(ott3.mds))
# create a column of site names, from the rownames of data.scores
data.scores$otter <- rownames(data.scores)  
data.scores <- cbind(data.scores, Ageclass = otter.sex$Ageclass, Sex = otter.sex$Sex, 
                     Area = otter.sex$Area, Site = otter.sex$Site)
head(data.scores)

#Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores <- as.data.frame(scores(ott3.mds, "species"))  
# create a column of species, from the rownames of species.scores
species.scores$species <- rownames(species.scores)  
head(species.scores)

ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Sex,colour=Sex),size=3) + # add the point markers
  scale_colour_manual(values=c("F" = "red", "M" = "blue")) +
  coord_equal() +
  theme_bw() +
  theme(axis.text.x = element_blank(),  # remove x-axis text
      axis.text.y = element_blank(), # remove y-axis text
      axis.ticks = element_blank(),  # remove axis ticks
      axis.title.x = element_text(size=18), # remove x-axis labels
      axis.title.y = element_text(size=18), # remove y-axis labels
      panel.background = element_blank(), 
      panel.grid.major = element_blank(),  #remove major-grid labels
      panel.grid.minor = element_blank(),  #remove minor-grid labels
      plot.background = element_blank())

#placing a hull for sex
sex.f <- data.scores[data.scores$Sex == "F", ][chull(data.scores[data.scores$Sex == 
                                                                   "F", c("NMDS1", "NMDS2")]), ]  # hull values for female
sex.m <- data.scores[data.scores$Sex == "M", ][chull(data.scores[data.scores$Sex == 
                                                                   "M", c("NMDS1", "NMDS2")]), ]  # hull values for male

hull.data <- rbind(sex.f, sex.m)  #combine grp.a and grp.b
hull.data
ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=Sex,group=Sex),alpha=0.30) + # add the convex hulls
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Sex,colour=Sex),size=4) + # add the point markers
  scale_colour_manual(values=c("F" = "red", "M" = "blue")) +
  scale_fill_manual(values=c("F" = "red", "M" = "blue")) +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
