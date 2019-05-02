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

#combine files
otter <- cbind(otter.gram, Ageclass = ott.sum$Ageclass, Sex = ott.sum$Sex, Area = ott.sum$Area)

# plot hist of all the grams/min
ggplot(data=stack.gram, aes(x=gram)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(species))

hist(otter.gram$clam)
hist(otter.gram$crab)


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

# plot hist of all the grams/min transformed
ggplot(data=stack.gram.tf, aes(x=gram)) + 
  geom_histogram(binwidth = .2) +
  facet_wrap(vars(species))

# info from ott.sum
par(mfrow= c(2,2))
plot(ott.sum$Sex, main = "Sex")
plot(ott.sum$Ageclass, main = "Age")
plot(ott.sum$Area, main = "Locations (year)")
hist(ott.sum$success_rt,  main = "Histogram of Sucess rate", xlab= "", ylab= "")

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

#betadispr
(disp <- betadisper(otter.bray, otter$Area))
PCoA.d <- dist(disp$vectors[,1:10])
plot(otter.bray, PCoA.d); cor(otter.bray, PCoA.d)
anova(disp)
TukeyHSD(disp)
plot(disp) 
plot(disp, c(2,3))
# MDS - can use 2D because is simpler
otter.nmds <- isoMDS(otter.bray, k=2) 
#type I PERMANOVA
adonis2(otter.bray ~ Sex + Season + Area + Site, data=ott.sum, perm=9999)
adonis2(otter.bray ~ Sex + Area + Site, data=ott.sum, perm=9999)
# type III PERMANOVA
adonis2(otter.bray ~ Sex + Ageclass + Season + Area + Site, data=ott.sum, by="margin", perm=9999)
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
adonis2(otter.bray2 ~ Sex + Area + Site, data=ott.sum, perm=9999)
# type III PERMANOVA
adonis2(otter.bray2 ~ Sex + Ageclass + Area + Site, data=ott.sum, by="margin", perm=9999)
adonis2(otter.bray2 ~ Sex + Area, data=ott.sum, by="margin", perm=9999)

ott2.mds<-metaMDS(ott.slim, k=2, try=20, autotransform = FALSE)
ott2.mds$stress
stressplot(ott2.mds)
ordiplot(ott2.mds, display=c("sites", "species"), type="t", cex=1)
plot(ott2.mds$points, col=as.numeric(ott.sum$Area), pch=16, cex=2, asp=1)
plot(ott2.mds$points, col=as.numeric(ott.sum$Sex), pch=16, cex=2, asp=1)
plot(ott2.mds$points, col=as.numeric(ott.sum$Site), pch=16, cex=2, asp=1)

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
adonis2(otter.bray3 ~ Sex + Area + Site, data=otter.sex, perm=9999)
# type III PERMANOVA
adonis2(otter.bray3 ~ Sex + Ageclass + Area + Site, data=otter.sex, by="margin", perm=9999)
adonis2(otter.bray3 ~ Sex + Area, data=otter.sex, by="margin", perm=9999)

ott3.mds<-metaMDS(ott.sex, k=2, try=20, autotransform = FALSE)
ott3.mds$stress
stressplot(ott3.mds)
ordiplot(ott3.mds, display=c("sites", "species"), type="t", cex=1)
plot(ott3.mds$points, col=as.numeric(ott.sum$Area), pch=16, cex=2, asp=1)
plot(ott3.mds$points, col=as.numeric(ott.sum$Sex), pch=16, cex=2, asp=1)
plot(ott3.mds$points, col=as.numeric(ott.sum$Site), pch=16, cex=2, asp=1)