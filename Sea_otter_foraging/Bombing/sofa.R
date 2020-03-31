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
mass <- read.csv("Bombing/mass_combine.csv")
energy <- read.csv("Bombing/energy.csv")

#reshape the mass data
#pivot_longer()
mass <- pivot_longer(data = mass, cols = starts_with("x."), names_to = "Size", names_prefix = "x.", values_to = "mass")

ggplot(data= mass) + theme_few() +
  geom_col(aes(x=Size, y=mass, fill=Study), position = "dodge") +
  facet_wrap(vars(Species), scales= "free")

ggsave("mass.png", device = "png", path = "Bombing/", width = 8, 
       height = 7, units = "in", dpi = 300)

#using the data from below calculations
old.mass <- filter(mass, Study== "OLD_AK")
new.mass <- pivot_longer(power.values, cols = starts_with("x."), 
                         names_to="Size", names_prefix = "x.", 
                         values_to = "mass")
new.mass <- new.mass[,-2]
new.mass <- new.mass[,-2]
new.mass <- new.mass[,-2]
colnames(new.mass) <- c("Prey", "Size", "mass")
new.mass$Study <- NA 
new.mass <- replace_na(new.mass, list(Study = "NLL"))
mass <- bind_rows(old.mass, new.mass)

#same graph again - missing a bunch of comparable values
ggplot(data= mass) + theme_few() +
  geom_col(aes(x=Size, y=mass, fill=Study), position = "dodge") +
  facet_wrap(vars(Prey), scales= "free")


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
library(minpack.lm)

#EXAMPLE
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
power <- drop_na(summary.w, Size.mm)
power <- drop_na(power, Dissected.Weight)
power <- select(power, ID, Size.mm, Species, Dissected.Weight, PreyCat)
power <- filter(power, Species != "stf")

# count the number of samples for each species and filter out <5 count
sp.count <- power %>%
  count(Species) %>%
  filter(n>=5)

power.sp <- left_join(sp.count, power)

#making model for each species
pow.mod <- power.sp %>%
  nest(-Species) %>%
  mutate(fit=map(data, ~nlsLM(Dissected.Weight ~ a*Size.mm^b, 
                            start = list(a=0, b=3),data=.)),
         results = map(fit, tidy)) %>%
  unnest(results)
  
#testing on single species that has low samples
#pow.mod$fit[pow.mod$Species=="cao"]

# count of observations added to df
pow.mod.n <- left_join(sp.count,pow.mod)

#add in urchins from BC ---------
#bc stf
urchins <- read_csv("Bombing/urchin_bc.csv")
#transform to kcal
urchins$kcal <- urchins$cal_g/1000
#nls model
urch.mod <- nlsLM(edible_wet_mass_g ~ a*daimeter_mm^b, start = list(a=1, b=3), data=urchins)
##a         b 
##0.0004273 2.6483119 
##0.000559  0.277
# add to power.mod.n
x <- data.frame("Species" = c("stf", "stf"), "n" = c(74,74), "term" = c("a", "b"), 
                "estimate" = c(0.0004273, 2.6483119), "std.error" = c(0.000559, 0.277))
pow.mod.n <- bind_rows(pow.mod.n, x)

#making the combined species lists ----- 
power.cat <- drop_na(power, PreyCat)
# count the number of samples for each species and filter out <5 count
cat.count <- power.cat %>%
  count(PreyCat) %>%
  filter(n>=5)
power.cat <- left_join(cat.count, power.cat, by= "PreyCat")
#making model for each prey cat
cat.mod <- power.cat %>%
  nest(-PreyCat) %>%
  mutate(fit=map(data, ~nlsLM(Dissected.Weight ~ a*Size.mm^b, 
                              start = list(a=0, b=3),data=.)),
         results = map(fit, tidy)) %>%
  unnest(results)
#testing on single species that has low samples
cat.mod$fit[cat.mod$PreyCat=="Clam"]
# count of observations added to df
cat.mod.n <- left_join(cat.count,cat.mod)
#change preycat title to species
cat.mod.n <- cat.mod.n %>% rename(Species = PreyCat)
#add to pow.mod.n
pow.mod.n <- bind_rows(pow.mod.n, cat.mod.n)

# making can 
power.can <- filter(power, Species == "cam" | Species == "cap" | Species == "cao")
can.mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, start = list(a=1, b=3), data=power.can)
##a         b 
##0.0001873 2.7399639
###0.000137 0.145
z <- data.frame("Species" = c("can", "can"), "n" = c(52,52), "term" = c("a", "b"), 
                "estimate" = c(0.0001873, 2.7399639), "std.error" = c(0.000137, 0.145))
pow.mod.n <- bind_rows(pow.mod.n, z)

#add in standard dev
#st.err = std.dev/ sqr rt of n
#st.err*sqr rt of n = std.dev
pow.mod.n$std.dev <- NA
pow.mod.n$std.dev <- pow.mod.n$std.error*sqrt(pow.mod.n$n)

#making values in a /b form
power.values <- pow.mod.n %>%
  select(Species, n, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

#adding sizes
power.values$x.1a <- (power.values$a*(13^power.values$b))
power.values$x.1b <- (power.values$a*(26^power.values$b))
power.values$x.1c <- (power.values$a*(43.3^power.values$b))
power.values$x.2a <- (power.values$a*(60.7^power.values$b))
power.values$x.2b <- (power.values$a*(78^power.values$b))
power.values$x.2c <- (power.values$a*(95.3^power.values$b))
power.values$x.3a <- (power.values$a*(112.7^power.values$b))
power.values$x.3b <- (power.values$a*(130^power.values$b))
power.values$x.3c <- (power.values$a*(147.3^power.values$b))
power.values$x.4 <- (power.values$a*(170^power.values$b))
# print csv
write_csv(power.values, "Bombing/power.csv")


#making values in a /b form
power.v <- pow.mod.n %>%
  select(Species, n, term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate)

#adding sizes
power.v$x.13 <- (power.values$a*(13^power.values$b))
power.v$x.26 <- (power.values$a*(26^power.values$b))
power.v$x.43 <- (power.values$a*(43^power.values$b))
power.v$x.61 <- (power.values$a*(61^power.values$b))
power.v$x.78 <- (power.values$a*(78^power.values$b))
power.v$x.95 <- (power.values$a*(95^power.values$b))
power.v$x.113 <- (power.values$a*(113^power.values$b))
power.v$x.130 <- (power.values$a*(130^power.values$b))
power.v$x.147 <- (power.values$a*(147^power.values$b))
power.v$x.170 <- (power.values$a*(170^power.values$b))

power.v <- pivot_longer(power.v, cols = starts_with("x."), 
                        names_to="Fitted_Size", names_prefix = "x.", 
                        values_to = "Fitted_Weight") 
power.v$Fitted_Size <- as.numeric(power.v$Fitted_Size)

#graph with predicted values as red line
ggplot() +
  geom_point(data=power, aes(x=Size.mm, y=Dissected.Weight)) +
  geom_line(data=power.v, aes(x=Fitted_Size, y=Fitted_Weight), color= "Red") +
  facet_wrap(vars(Species), scales = "free") +
  labs(x = "Size (mm)", y = "Weight (g)") + theme_bw()


ggplot(data=power, aes(x=Size.mm, y=Dissected.Weight)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(vars(Species), scales = "free") +
  labs(x = "Size (mm)", y = "Weight (g)") + theme_bw()




#make means of kcal/gram STF
urch.mean <- urchins %>%
  summarise(mean=mean(kcal), sd(kcal))
urch.mean
##mean    sd
##2.97   1.22

# checking which species are missing
forage$PreyItem <- tolower(forage$PreyItem)
check <- forage %>%
  count(PreyItem) %>%
  rename(Species = PreyItem)
check <- check[,1]
calc <- power.values[,1]
missing <- setdiff(check, calc)
write_csv(missing, "Bombing/missing.csv")
