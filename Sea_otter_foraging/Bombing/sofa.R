## Looking at sofa values before and after my prey switches 

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)


#old <- read.csv("Bombing/mass_old.csv")
#new <- read.csv("Bombing/mass_new.csv")

#Join the two 
#old.mass <- left_join(new,old, by= "Prey")
#old.mass <- old.mass[,-1]
#old.mass <- old.mass[,-c(2:11)]
#write.csv(old.mass, "Bombing/old.mass.csv")

#old <- read.csv("Bombing/energy_old.csv")
#new <- read.csv("Bombing/energy_new.csv")

#Join the two
#old.energy <- left_join(new, old, by = "Prey")
#write.csv(old.energy, "Bombing/old.energy.csv")

#Load converged files
mass <- read.csv("Bombing/mass.csv")
energy <- read.csv("Bombing/energy.csv")

#reshape the mass data
#pivot_longer()
mass <- pivot_longer(data = mass, cols = starts_with("X"), names_to = "Size", names_prefix = "X", values_to = "mass")

ggplot(data= mass) + theme_few() +
  geom_col(aes(x=Size, y=mass, fill=Study), position = "dodge") +
  facet_wrap(vars(Prey), scales= "free")

ggsave("mass.png", device = "png", path = "Bombing/", width = 8, 
       height = 7, units = "in", dpi = 300)

ggplot(data= energy, aes(x=Prey, y=energy, fill=Study)) + 
  theme_few() +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin=energy-sd, ymax=energy+sd), position = position_dodge(width=0.9), width=.2) +
  facet_wrap(vars(Prey), scales= "free")

ggsave("energy.png", device = "png", path = "Bombing/", width = 8, 
       height = 7, units = "in", dpi = 300)


#Proportional data from New sofa output
prop <- read.csv("Bombing/test_prop_new.csv")

ggplot(data= prop, aes(x=Diet, y=Proportion, fill=Method)) + 
  theme_few() +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin=Proportion-SD, ymax=Proportion+SD), position = position_dodge(width=0.9), width=.2)

ggsave("sofa_prop.png", device = "png", path = "Bombing/", width = 8, 
       height = 7, units = "in", dpi = 300) 


####################### Creating power functions for SOFA #####################
###############################################################################

library(tidyverse)
library(broom)
mtcars %>% 
  nest(-am) %>% 
  mutate(am = factor(am, levels = c(0, 1), labels = c("automatic", "manual")),
         fit = map(data, ~ lm(mpg ~ hp + wt + disp, data = .)),
         results = map(fit, augment)) %>% 
  unnest(results) %>% 
  ggplot(aes(x = mpg, y = .fitted)) +
  geom_abline(intercept = 0, slope = 1, alpha = .2) +  # Line of perfect fit
  geom_point() +
  facet_grid(am ~ .) +
  labs(x = "Miles Per Gallon", y = "Predicted Value") +
  theme_bw()

#Make sure to run after combining PLAB and SUMMARY_PC 

# count the number of samples for each species and filter out <5 count
sp.count <- plab %>%
  count(Species)

#join filtered species to plab
trial <- left_join(sp.count, plab)

#running with >=5 samples
power <- plab %>%
  nest(-Species) %>%
  mutate(fit=map(data, ~nlsLM(Dissected.Weight ~ a*Size.mm^b, 
                            start = list(a=1, b=3),data=.)),
         results = map(fit, tidy)) %>%
  unnest(results) %>%
  
#testing on single species 
power$fit[power$Species=="cao"]

# count of observations added to df
power.wide <- left_join(sp.count,power)

#add in standard dev
#st.err = std.dev/ sqr rt of n
#st.err*sqr rt of n = std.dev
power.wide$std.dev <- NA
power.wide$std.dev <- power.wide$std.error*sqrt(power.wide$n)

#making an exportable file
power.print <- select(power.wide, Species, n, term, estimate, std.dev)
#Exporting a/b values for use in spreadsheet
write_csv(power.print, "Bombing/power.csv")

#make means of cal/gram
means <-
ggplot(data=plab, aes(y=log(Dissected.Weight), x=log(Size.mm))) +
  geom_point()+
  facet_wrap(vars(Species))
  
apc <- filter(plab, Species == "apc") 
n <- nls(Dissected.Weight ~ a*Size.mm^b, start = list(a=1, b=2), data=apc)
