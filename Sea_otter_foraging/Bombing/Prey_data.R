#Kj Lipid Protein Moisture Ash
#calculate a dissimilarity matrix
#look at diffrerences between row 1
# this would show what is driving the differences
#Each sample is a row
#Each column is KJ Lipid Protein Moisture Ash 
#Then use species, site, season, size

#Size linear regression
#season anova


#New SOFA biomass sheet

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(ggformula) #only if using stat_lm


plab <- read.csv("Bombing/plab.csv")
summary <- read.csv("Bombing/Summary_PC.csv")

#add in weights for non-dissected things!
#Cant figure this out....
#replace_na(summary, replace = list(Dissected.Weight=summary$Frozen.Weight))

#first, make a model
power <- lm(data= summary, y~a*x^b)


#making the power analysis for size vs mass
ggplot(data= filter(summary, Tissue == "whole"), aes(x=Size.mm, y=Dissected.Weight)) + theme_few() +
  geom_point() +
  stat_smooth(method = 'nls', formula = 'y~a*x^b', se=FALSE) +
  stat_regline_equation(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = 'y~a*x^b') +
  facet_wrap(vars(Species), nrow = 6, scales = "free")

  
summary.short <- summary %>%
  filter(Tissue== "whole") %>%
  select(SIN, Species, Date.Collected, Size.mm, Dissected.Weight, SEX, Site.location, Season, PreyCat)

bomb<- full_join(plab, summary.short, by="SIN")
bomb <- bomb %>% drop_na(KJ)
bomb <- bomb %>% drop_na(PreyCat)
bomb$KJ.wetgram <-NA
bomb$KJ.wetgram <- (1-(bomb$moisture/100))*bomb$KJ
bomb$kcal <- NA
bomb$kcal <- bomb$KJ.wetgram*0.2390057361

write.csv(bomb, "Bombing/NLL_data.csv")

# kcal means for each species
bomb.mean <- bomb %>%
  group_by(Species) %>% 
  summarise(kcal.mean=mean(kcal), kcal.sd=sd(kcal))

#make csv
write.csv(bomb.mean, "Bombing/kcal.mean.csv")

# kcal means for each species
bomb.PC.mean <- bomb %>%
  group_by(PreyCat) %>% 
  summarise(kcal.mean=mean(kcal), kcal.sd=sd(kcal))

#make csv
write.csv(bomb.PC.mean, "Bombing/kcal_PC_mean.csv")
