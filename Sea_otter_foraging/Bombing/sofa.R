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


#3                     Creating power functions for SOFA                      #
###############################################################################

library(tidyverse)
library(broom)
library(minpack.lm)
library(propagate)

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

#Make sure to run after combining PLAB and SUMMARY_PC from macronutrients
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
ggplot(data=power, aes(x=Size.mm, y=Dissected.Weight)) +
  geom_point() +
  geom_line(data=power.v, aes(x=Fitted_Size, y=Fitted_Weight), color= "Red") +
  facet_wrap(vars(Species), scales = "free") +
  labs(x = "Size (mm)", y = "Weight (g)") + theme_bw()


  ggplot(data= power, aes(x=Size.mm, y=Dissected.Weight)) +
  geom_point() +
  facet_wrap(vars(PreyCat), scales = "free") +
  geom_smooth(method = "nls", formula = (y~a*x^b), start = list(a=0,b=3), se = TRUE)+
  labs(x = "Size (mm)", y = "Weight (g)") + theme_bw()

ggplot(data=power, aes(x=Size.mm, y=Dissected.Weight)) +
  geom_point() +
  geom_abline(slope= power.values$a*x^power.values$b) +
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


## Cen AK mussels
cak.muss <- read.csv("Bombing/CAK_Mussel.csv")
summary(cak.muss)

# mass = wetTotalMass
#length = length
# moisture = 
cak.muss$moisture <- (cak.muss$wetTotalMass-cak.muss$DryTotalMass)/cak.muss$wetTotalMass
# cal dry = CalGram
cak.muss$kcal.dry <-cak.muss$CalGram/1000
#cal wet
cak.muss$kcal.wet <- (1-(cak.muss$moisture))*cak.muss$kcal.dry
cak.muss$percent.edible <- cak.muss$wetTotalMass/cak.muss$TOTALWET

cak.mean <- cak.muss %>%
  drop_na(kcal.wet) %>%
  summarise(mean = mean(kcal.wet), sd = sd(kcal.wet), 
            edible = mean(percent.edible, na.rm = TRUE), sd.ed = sd(percent.edible, na.rm = TRUE))

cak.n <- cak.muss %>%
  drop_na(kcal.wet) %>%
  count() #334


cak.mean
#	       mean        sd    edible      sd.ed
#   0.7379483 0.2067092 0.2972606 0.08937244

#Sizes
mussel.mod <- nlsLM( wetTotalMass~ a*Length^b, start = list(a=1, b=3), data=cak.muss)
summary(mussel.mod)
#            se
#a .0001197  6.518e-05 
#b 2.467     0.1530

mus <- data.frame(Speices = "mtr", a = .0001197, b = 2.467)
mus$x.1a <- (mus$a*(13^mus$b))
mus$x.1b <- (mus$a*(26^mus$b))
mus$x.1c <- (mus$a*(43.3^mus$b))
mus$x.2a <- (mus$a*(60.7^mus$b))
mus$x.2b <- (mus$a*(78^mus$b))
mus$x.2c <- (mus$a*(95.3^mus$b))
mus$x.3a <- (mus$a*(112.7^mus$b))
mus$x.3b <- (mus$a*(130^mus$b))
mus$x.3c <- (mus$a*(147.3^mus$b))
mus$x.4 <- (mus$a*(170^mus$b))
mus

###    SHRIMP     ###
shrimp <- summary.w %>%
  filter(Species == "pas")

shrimp <- rename(shrimp, "weight.g"="Frozen.Weight")

shrimp$weight.g <- as.numeric(as.character(shrimp$weight.g))

shrimp <- dplyr::select(shrimp, Species, weight.g, Size.mm)

shrimp.new <- read.csv("Bombing/shrimp.csv")
shrimp.new$weight.g <- as.numeric(as.character(shrimp.new$weight.g))
shrimp.new$Size.mm <- as.numeric(as.character(shrimp.new$Size.mm))

shrimp <- bind_rows(shrimp, shrimp.new)

shrimp.mod <- nlsLM(weight.g ~ a*Size.mm^b, start = list(a=1, b=3), data=shrimp)
summary(shrimp.mod)

#a         b
#3.163e-06 3.272
#4.116e-06 0.2544

#std.dev a
0.000004116*sqrt(30)
#2.254426e-05

#std.dev b
0.2544*sqrt(30)
#1.393406

#shrimp sizes
#adding sizes
#new data frame
df <- data.frame(Speices = "pas", n = 30, a = 3.163e-06, b = 3.272)
df$x.1a <- (df$a*(13^df$b))
df$x.1b <- (df$a*(26^df$b))
df$x.1c <- (df$a*(43.3^df$b))
df$x.2a <- (df$a*(60.7^df$b))
df$x.2b <- (df$a*(78^df$b))
df$x.2c <- (df$a*(95.3^df$b))
df$x.3a <- (df$a*(112.7^df$b))
df$x.3b <- (df$a*(130^df$b))
df$x.3c <- (df$a*(147.3^df$b))
df$x.4 <- (df$a*(170^df$b))

#save df
write.csv(df, "Bombing/shrimp_ab.csv")





#Making a table by season and species
table <- plab %>%
  group_by(Species, Season, PreyCat) %>%
  summarise(mean.moist = mean(moisture, na.rm = TRUE), sd.moist = sd(moisture, na.rm = TRUE),
            mean.kcald = mean(kcal.dry, na.rm = TRUE), sd.kcald = sd(kcal.dry, na.rm = TRUE),
            mean.kcalw = mean(kcal.wet, na.rm = TRUE), sd.kcalw = sd(kcal.wet, na.rm = TRUE),
            mean.lipid = mean(lipid_dry, na.rm = TRUE), sd.lipid = sd(lipid_dry, na.rm = TRUE),
            mean.protein = mean(protein_dry, na.rm = TRUE), sd.protein = sd(protein_dry, na.rm = TRUE))

table.ne <- plab %>%
  drop_na(kcal.dry) %>%
  group_by(Species, Season) %>%
  count() %>%
  rename(ne = n)

table.np <- plab %>%
  drop_na(protein_dry) %>%
  group_by(Species, Season) %>%
  count() %>%
  rename(np = n)

table.nl <- plab %>%
  drop_na(lipid_dry) %>%
  group_by(Species, Season) %>%
  count() %>%
  rename(nl = n)

table.nm <- plab %>%
  drop_na(moisture) %>%
  group_by(Species, Season) %>%
  count() %>%
  rename(nm = n)

table.na <- plab %>%
  drop_na(ash) %>%
  group_by(Species, Season) %>%
  count() %>%
  rename(nash = n)

table <- left_join(table, table.ne)
table <- left_join(table, table.nl)
table <- left_join(table, table.np)
table <- left_join(table, table.nm)
table <- left_join(table, table.na)

write.csv(table, "Bombing/table.csv")

table2 <- plab %>%
  group_by(Season, PreyCat) %>%
  summarise(mean.moist = mean(moisture, na.rm = TRUE), sd.moist = sd(moisture, na.rm = TRUE),
            mean.kcald = mean(kcal.dry, na.rm = TRUE), sd.kcald = sd(kcal.dry, na.rm = TRUE),
            mean.lipid = mean(lipid_dry, na.rm = TRUE), sd.lipid = sd(lipid_dry, na.rm = TRUE),
            mean.protein = mean(protein_dry, na.rm = TRUE), sd.protein = sd(protein_dry, na.rm = TRUE))

table.ne2 <- plab %>%
  drop_na(kcal.dry) %>%
  group_by(Season, PreyCat) %>%
  count() %>%
  rename(ne = n)

table.nl2 <- plab %>%
  drop_na(lipid_dry) %>%
  group_by(Season, PreyCat) %>%
  count() %>%
  rename(nl = n)

table.np2 <- plab %>%
  drop_na(protein_dry) %>%
  group_by(Season, PreyCat) %>%
  count() %>%
  rename(np = n)

table.nm2 <- plab %>%
  drop_na(moisture) %>%
  group_by(Season, PreyCat) %>%
  count() %>%
  rename(nm = n)

table2 <- left_join(table2, table.ne2)
table2 <- left_join(table2, table.nl2)
table2 <- left_join(table2, table.np2)
table2 <- left_join(table2, table.nm2)

write.csv(table2, "Bombing/table2.csv")

# New table - wet energy, %edible
plab$Frozen.Weight <- as.numeric(as.character(plab$Frozen.Weight))
plab$percent.edible <- plab$Dissected.Weight/plab$Frozen.Weight
table3 <- plab %>%
  group_by(Species) %>%
  summarise(mean.kcalw = mean(kcal.wet, na.rm = TRUE), sd.kcalw = sd(kcal.wet, na.rm = TRUE),
            mean.edible = mean(percent.edible, na.rm = TRUE), sd.edible = sd(percent.edible, na.rm = TRUE))

table3 <- left_join(table.n2, table3)

write.csv(table3, "Bombing/table3.csv")


#Calculating energy means and sds

#First read plab from macronutrients.rmd

means.energy <- plab %>% 
  group_by(Species) %>%
  summarise(mean=mean(kcal.wet, na.rm = T), sd=sd(kcal.wet, na.rm = T))

means.energy.group <- plab %>%
  group_by(PreyCat) %>%
  summarise(mean=mean(kcal.wet, na.rm = T), sd=sd(kcal.wet, na.rm = T))

means.energy <- bind_rows(means.energy, means.energy.group)

can <- plab %>%
  filter(Species == "cam" | Species == "cap" | Species == "cao") %>%
  summarise(mean=mean(kcal.wet, na.rm = T), sd=sd(kcal.wet, na.rm = T))

write_csv(means.energy, "Bombing/new.means.csv")


## Extracting fit for models

apc <- filter(power.sp, Species == "apc")

apc.mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
        start = list(a=0, b=3), data = apc)
newdata <- data.frame(Size.mm=c(13, 26, 43.3, 60.7, 78, 95.3, 112.7, 130, 147.3, 170))
predict.se <- predictNLS(apc.mod, newdata, interval= "confidence")

sd.low <- predict.se$summary[,3]
sd.high <-predict.se$summary[,4]

apc.sd <- data.frame(species = "apc", newdata, sd=sd.high-sd.low)

cln <- filter(power.sp, Species == "cln")

cln.mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
                 start = list(a=0, b=3), data = cln)
predict.se <- predictNLS(cln.mod, newdata, interval= "confidence")
sd.low <- predict.se$summary[,3]
sd.high <-predict.se$summary[,4]

cln.sd <- data.frame(Species = "cln", newdata, sd=sd.high-sd.low)


#now see if I can do for all???

pow.mod <- power.sp %>%
  nest(-Species) %>%
  mutate(fit=map(data, ~nlsLM(Dissected.Weight ~ a*Size.mm^b, 
                              start = list(a=0, b=3),data=.)))

#this doesn't work....
predict <- pow.mod %>%
  mutate(sd=map(fit, ~predictNLS(., newdata, interval= "confidence")))
  
##################################################################
##################################################################
#one by one
apc <- filter(power.sp, Species == "apc")
apc.mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
                 start = list(a=0, b=3), data = apc)
newdata <- data.frame(Size.mm=c(13, 26, 43.3, 60.7, 78, 95.3, 112.7, 130, 147.3, 170))
predict.se <- predictNLS(apc.mod, newdata)
apc.sd <- data.frame(species = "apc", newdata, sd = predict.se$summary[,3])

cam <- filter(power.sp, Species == "cam")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
                 start = list(a=0, b=3), data = cam)
predict.se <- predictNLS(mod, newdata)
cam.sd <- data.frame(species = "cam", newdata, sd = predict.se$summary[,3])

cao <- filter(power.sp, Species == "cao")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = cao)
predict.se <- predictNLS(mod, newdata)
cao.sd <- data.frame(species = "cao", newdata, sd = predict.se$summary[,3])

cap <- filter(power.sp, Species == "cap")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = cap)
predict.se <- predictNLS(mod, newdata)
cap.sd <- data.frame(species = "cap", newdata, sd = predict.se$summary[,3])

chr <- filter(power.sp, Species == "chr")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = chr)
predict.se <- predictNLS(mod, newdata)
chr.sd <- data.frame(species = "chr", newdata, sd = predict.se$summary[,3])

cln <- filter(power.sp, Species == "cln")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = cln)
predict.se <- predictNLS(mod, newdata)
cln.sd <- data.frame(species = "cln", newdata, sd = predict.se$summary[,3])

cum <- filter(power.sp, Species == "cum")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = cum)
predict.se <- predictNLS(mod, newdata)
cum.sd <- data.frame(species = "cum", newdata, sd = predict.se$summary[,3])

eul <- filter(power.sp, Species == "eul")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = eul)
predict.se <- predictNLS(mod, newdata)
eul.sd <- data.frame(species = "eul", newdata, sd = predict.se$summary[,3])

evt <- filter(power.sp, Species == "evt")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = evt)
predict.se <- predictNLS(mod, newdata)
evt.sd <- data.frame(species = "evt", newdata, sd = predict.se$summary[,3])

hak <- filter(power.sp, Species == "hak")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=1, b=3), data = hak)
predict.se <- predictNLS(mod, newdata)
hak.sd <- data.frame(species = "hak", newdata, sd = predict.se$summary[,3])

mtr <- filter(power.sp, Species == "mtr")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = mtr)
predict.se <- predictNLS(mod, newdata)
mtr.sd <- data.frame(species = "mtr", newdata, sd = predict.se$summary[,3])

pio <- filter(power.sp, Species == "pio")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = pio)
predict.se <- predictNLS(mod, newdata)
pio.sd <- data.frame(species = "pio", newdata, sd = predict.se$summary[,3])

pom <- filter(power.sp, Species == "pom")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = pom)
predict.se <- predictNLS(mod, newdata)
pom.sd <- data.frame(species = "pom", newdata, sd = predict.se$summary[,3])

prs <- filter(power.sp, Species == "prs")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = prs)
predict.se <- predictNLS(mod, newdata)
prs.sd <- data.frame(species = "prs", newdata, sd = predict.se$summary[,3])

pup <- filter(power.sp, Species == "pup")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = pup)
predict.se <- predictNLS(mod, newdata)
pup.sd <- data.frame(species = "pup", newdata, sd = predict.se$summary[,3])

sag <- filter(power.sp, Species == "sag")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = sag)
predict.se <- predictNLS(mod, newdata)
sag.sd <- data.frame(species = "sag", newdata, sd = predict.se$summary[,3])

std <- filter(power.sp, Species == "std")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = std)
predict.se <- predictNLS(mod, newdata)
std.sd <- data.frame(species = "std", newdata, sd = predict.se$summary[,3])

tec <- filter(power.sp, Species == "tec")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = tec)
predict.se <- predictNLS(mod, newdata)
tec.sd <- data.frame(species = "tec", newdata, sd = predict.se$summary[,3])

sd <- bind_rows(apc.sd, cam.sd, cao.sd, cap.sd, chr.sd, cln.sd, cum.sd, eul.sd, 
                evt.sd, hak.sd, mtr.sd, pio.sd, pom.sd, prs.sd, pup.sd, sag.sd, std.sd, tec.sd)

#prey cats
cla <- filter(power.sp, PreyCat == "Clam")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = cla)
predict.se <- predictNLS(mod, newdata)
cla.sd <- data.frame(species = "cla", newdata, sd = predict.se$summary[,3])

cra <- filter(power.sp, PreyCat == "Crab")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = cra)
predict.se <- predictNLS(mod, newdata)
cra.sd <- data.frame(species = "cra", newdata, sd = predict.se$summary[,3])

sna <- filter(power.sp, PreyCat == "Herb Snail")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = sna)
predict.se <- predictNLS(mod, newdata)
sna.sd <- data.frame(species = "sna", newdata, sd = predict.se$summary[,3])

sta <- filter(power.sp, PreyCat == "Star")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = sta)
predict.se <- predictNLS(mod, newdata)
sta.sd <- data.frame(species = "sta", newdata, sd = predict.se$summary[,3])

cuc <- filter(power.sp, PreyCat == "Cucumber")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = cuc)
predict.se <- predictNLS(mod, newdata)
cuc.sd <- data.frame(species = "cuc", newdata, sd = predict.se$summary[,3])

sd <- bind_rows(sd, cla.sd, cra.sd, sna.sd, sta.sd, cuc.sd)


#mods
predict.se <- predictNLS(can.mod, newdata)
can.sd <- data.frame(species = "can", newdata, sd = predict.se$summary[,3])

predict.se <- predictNLS(shrimp.mod, newdata)
pas.sd <- data.frame(species = "pas", newdata, sd = predict.se$summary[,3])

sca <- filter(power.sp, PreyCat == "Scallop")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
             start = list(a=0, b=3), data = sca)
newdata <- data.frame(Size.mm=c(13, 26, 43.3, 60.7, 78, 95.3, 112.7, 130, 147.3, 170))
predict.se <- predictNLS(mod, newdata)
sca.sd <- data.frame(species = "sca", newdata, sd = predict.se$summary[,3])

newdata <- data.frame(daimeter_mm=c(13, 26, 43.3, 60.7, 78, 95.3, 112.7, 130, 147.3, 170))
predict.se <- predictNLS(urch.mod, newdata)
stf.sd <- data.frame(species = "stf", newdata, sd = predict.se$summary[,3])
stf.sd <- rename(stf.sd, "Size.mm" = "daimeter_mm")


sd <- bind_rows(sd, can.sd, pas.sd, stf.sd, sca.sd)
sd <- pivot_wider(sd, names_from = Size.mm, values_from = sd)
write_csv(sd, "Bombing/sd.csv")


sca <- filter(power.sp, PreyCat == "Scallop")
mod <- nlsLM(Dissected.Weight ~ a*Size.mm^b, 
                 start = list(a=0, b=3), data = sca)
newdata <- data.frame(Size.mm=c(13, 26, 43.3, 60.7, 78, 95.3, 112.7, 130, 147.3, 170))
predict.se <- predictNLS(mod, newdata)
sca.sd <- data.frame(species = "sca", newdata, sd = predict.se$summary[,3])
