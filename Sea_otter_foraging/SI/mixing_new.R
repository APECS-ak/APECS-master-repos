# New mixing script with final model

library(tidyr)
library(dplyr)
library(MixSIAR)
library(ggplot2)
library(R2jags)
library(tidyr)

#TONOWEK - very long

#Load image Very long seasons (this is converged)
load(file="SI/tonowek_verylong2.RData")

attach.jags(jags.tonowek) # adding model

dim(p.fac1) # 3000, 4, 5 -> 3000 iterations of 4 seasons and 5 sources
mean(p.fac1[,1,1]) #median of season 1, source 1
p.fac1[,2,1]


#Making data frame for graph
ton.fall <- data.frame(Site = "Tonowek", Season = "Fall",Clam = p.fac1[,1,1], Crab = p.fac1[,1,2], 
                       Cucumber.Snail = p.fac1[,1,3], Mussel = p.fac1[,1,4], Urchin = p.fac1[,1,5])
ton.spring <- data.frame(Site = "Tonowek", Season = "Spring",Clam = p.fac1[,2,1], Crab = p.fac1[,2,2], 
                         Cucumber.Snail = p.fac1[,2,3], Mussel = p.fac1[,2,4], Urchin = p.fac1[,2,5])
ton.summer <- data.frame(Site = "Tonowek", Season = "Summer",Clam = p.fac1[,3,1], Crab = p.fac1[,3,2], 
                         Cucumber.Snail = p.fac1[,3,3], Mussel = p.fac1[,3,4], Urchin = p.fac1[,3,5])
ton.winter <- data.frame(Site = "Tonowek", Season = "Winter",Clam = p.fac1[,4,1], Crab = p.fac1[,4,2], 
                         Cucumber.Snail = p.fac1[,4,3], Mussel = p.fac1[,4,4], Urchin = p.fac1[,4,5])
fall.ton <- ton.fall %>% gather(source,value,3:7)
spring.ton <- ton.spring %>% gather(source,value,3:7)
summer.ton <- ton.summer %>% gather(source,value,3:7)
winter.ton <- ton.winter %>% gather(source,value,3:7)
all.ton <- rbind(spring.ton, summer.ton, fall.ton, winter.ton)

ggplot(aes(y = value, x = source, fill = Season), data = all.ton) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  #scale_fill_manual(values=c("black","white"), name="") + # Seagrass is black, coral is white
  theme( axis.title=element_text(size=16), legend.position = "none")

detach.jags(jags.tonowek)


#SHINKAKU

#Load image Very long seasons (this is converged)
load(file="SI/shinaku_long.RData") 

attach.jags(jags.shinaku) # adding model

dim(p.fac1) # 3000, 4, 5 -> 3000 iterations of 4 seasons and 5 sources
median(p.fac1[,1,1]) #median of season 1, source 1
p.fac1[,2,1]

#Making data frame for graph
shin.fall <- data.frame(Site = "Shinaku", Season = "Fall",Clam = p.fac1[,1,1], Crab = p.fac1[,1,2], 
                        Cucumber.Snail = p.fac1[,1,3], Mussel = p.fac1[,1,4], Urchin = p.fac1[,1,5])
shin.spring <- data.frame(Site = "Shinaku", Season = "Spring",Clam = p.fac1[,2,1], Crab = p.fac1[,2,2], 
                          Cucumber.Snail = p.fac1[,2,3], Mussel = p.fac1[,2,4], Urchin = p.fac1[,2,5])
shin.summer <- data.frame(Site = "Shinaku", Season = "Summer",Clam = p.fac1[,3,1], Crab = p.fac1[,3,2], 
                          Cucumber.Snail = p.fac1[,3,3], Mussel = p.fac1[,3,4], Urchin = p.fac1[,3,5])
shin.winter <- data.frame(Site = "Shinaku", Season = "Winter",Clam = p.fac1[,4,1], Crab = p.fac1[,4,2], 
                          Cucumber.Snail = p.fac1[,4,3], Mussel = p.fac1[,4,4], Urchin = p.fac1[,4,5])
fall.shin <- shin.fall %>% gather(source,value,3:7)
spring.shin <- shin.spring %>% gather(source,value,3:7)
summer.shin <- shin.summer %>% gather(source,value,3:7)
winter.shin <- shin.winter %>% gather(source,value,3:7)

all.shin <- rbind(spring.shin, summer.shin, fall.shin, winter.shin)

ggplot(aes(y = value, x = source, fill = Season), data = all.shin) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  #scale_fill_manual(values=c("black","white"), name="") + # Seagrass is black, coral is white
  theme( axis.title=element_text(size=16), legend.position = "none")

detach.jags(jags.shinaku)

#SUKKWAN

#Load image Very long seasons (this is converged)
load(file="SI/sukkwan_long.RData")

attach.jags(jags.sukkwan) # adding model

median(p.fac1[,1,1]) #median of season 1, source 1

#Making data frame for graph
suk.fall <- data.frame(Site = "Sukkwan", Season = "Fall",Clam = p.fac1[,1,1], Crab = p.fac1[,1,2], 
                       Cucumber.Snail = p.fac1[,1,3], Mussel = p.fac1[,1,4], Urchin = p.fac1[,1,5])
suk.spring <- data.frame(Site = "Sukkwan", Season = "Spring",Clam = p.fac1[,2,1], Crab = p.fac1[,2,2], 
                         Cucumber.Snail = p.fac1[,2,3], Mussel = p.fac1[,2,4], Urchin = p.fac1[,2,5])
suk.summer <- data.frame(Site = "Sukkwan", Season = "Summer",Clam = p.fac1[,3,1], Crab = p.fac1[,3,2], 
                         Cucumber.Snail = p.fac1[,3,3], Mussel = p.fac1[,3,4], Urchin = p.fac1[,3,5])
suk.winter <- data.frame(Site = "Sukkwan", Season = "Winter",Clam = p.fac1[,4,1], Crab = p.fac1[,4,2], 
                         Cucumber.Snail = p.fac1[,4,3], Mussel = p.fac1[,4,4], Urchin = p.fac1[,4,5])
fall.suk <- suk.fall %>% gather(source,value,3:7)
spring.suk <- suk.spring %>% gather(source,value,3:7)
summer.suk <- suk.summer %>% gather(source,value,3:7)
winter.suk <- suk.winter %>% gather(source,value,3:7)


all.suk <- rbind(spring.suk, summer.suk, fall.suk, winter.suk)

ggplot(aes(y = value, x = source, fill = Season), data = all.suk) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  #scale_fill_manual(values=c("black","white"), name="") + # Seagrass is black, coral is white
  theme( axis.title=element_text(size=16), legend.position = "none")

all <- bind_rows(all.ton, all.shin, all.suk)
write.csv(all, "SI/all_mixing_new.csv")

##READ
all <- read.csv("SI/all_mixing.csv")

#facet wrap by prey
ggplot(aes(y = value, x = Site, fill = Season), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  facet_wrap(vars(source), nrow = 2) +
  theme( axis.title=element_text(size=16))

#facet wrap by season
ggplot(aes(y = value, x = source, fill = Site), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  facet_wrap(vars(Season), nrow = 2) +
  theme( axis.title=element_text(size=16))


#facet wrap by site
ggplot(aes(y = value, x = source, fill = Season), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  facet_wrap(vars(Site), nrow = 2) +
  theme( axis.title=element_text(size=16))

ggsave("mixing_sites.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)


#grid - final plot
all$Season = factor(all$Season, levels=c('Spring','Summer','Fall','Winter'))
ggplot(aes(y = value, x = source, fill = source), data = all) + 
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

ggsave("mixing_sites_grid.png", device = "png", path = "SI/", width = 9, 
       height = 7, units = "in", dpi = 300)


