#####----- Eelgrass Analyses -----#####
## Originally Created 10.25.2017 - Wendel Raymond ##

#####----- Libraries -----####
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

theme_set(theme_classic())

#####----- Functions -----#####
st.er <- function(x, na.rm = TRUE) {
  stopifnot(is.numeric(x))
  return(sd(x, na.rm = na.rm)/sqrt(length(x)))
}

#####----- Load Data -----#####
# Eelgrass Data
dat <- read.csv("Data/Eelgrass_and_Grazer_Data_2017.csv", header = TRUE)

# Crab Data
crab <- read.csv("Data/Crab_summaries_site.csv", header = TRUE)
crab$string <- as.factor(crab$string)

# Nutrient Data
nut <- read.csv("Data/alaska_2017_nutrient_data_WR_10.28.2017.csv", header = TRUE)

#####----- Merging data -----#####
## Crab Data to master data ##
dat <- merge(dat, crab[,1:8], by = "site")

# clean up some names
colnames(dat)[3] <- "date_eelgrass_tran"
colnames(dat)[86] <- "date_crab_pulled"
colnames(dat)[87] <- "date_crab_pulled_julian"
colnames(dat)[88] <- "count_crabs"

#####----- Eelgrass Data Primary Analyses -----#####
### Eelgrass ###
## Shoot Density ##
plot(dat$PCA2, dat$shoot_dens, pch = 16)

# SOI
SD.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = shoot_dens), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = shoot_dens - shoot_dens_se, ymax = shoot_dens + shoot_dens_se), size = 1) +
  labs(x = "Sea otter Index", y = bquote('Shoot density +/- se' ~(m^-2))) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

cor(dat$PCA2, dat$shoot_dens)

# Jday
SD.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = shoot_dens, color = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = shoot_dens - shoot_dens_se, ymax = shoot_dens + shoot_dens_se), size = 1) +
  labs(x = "Date", y = bquote('Shoot density +/- se' ~(m^-2))) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = c(0.04, 0.8))

## Above Ground Mass ##
# SOI
plot(dat$PCA2, dat$abvgnd_mass, pch = 16)

AB.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = abvgnd_mass), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = abvgnd_mass - abvgnd_mass_se, ymax = abvgnd_mass + abvgnd_mass_se), size = 1) +
  labs(x = "Sea otter Index", y = bquote('Above ground mass +/- se' ~(g/m^-2))) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

summary(lm(dat$abvgnd_mass ~ dat$PCA2))

# Jday
AB.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = abvgnd_mass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = abvgnd_mass - abvgnd_mass_se, ymax = abvgnd_mass + abvgnd_mass_se), size = 1) +
  labs(x = "Date", y = bquote('Above ground mass +/- se' ~(g/m^-2))) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Below Ground Mass ##
# SOI
plot(dat$PCA2, dat$blwgnd_mass, pch = 16)

BB.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = blwgnd_mass), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = blwgnd_mass - blwgnd_mass_se, ymax = blwgnd_mass + blwgnd_mass_se), size = 1) +
  labs(x = "Sea otter Index", y = bquote('Below ground mass +/- se' ~(g/m^-2))) +
  lims(y = c(0, 80)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Jday
BB.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = blwgnd_mass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = blwgnd_mass - blwgnd_mass_se, ymax = blwgnd_mass + blwgnd_mass_se), size = 1) +
  labs(x = "Date", y = bquote('Below ground mass +/- se' ~(g/m^-2))) +
  lims(y = c(0, 80)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Node 2 ##
# SOI
N2.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = node2), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = node2 - node2_se, ymax = node2 + node2_se), size = 1) +
  labs(x = "Sea otter Index", y = "Node #2 (cm)") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Jday
N2.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = node2, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = node2 - node2_se, ymax = node2 + node2_se), size = 1) +
  labs(x = "Date", y = "Node #2 (cm)") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))
  
## Epiphytes ##
# SOI
EP.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = epi_mass), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = epi_mass - epi_mass_se, ymax = epi_mass + epi_mass_se), size = 1) +
  labs(x = "Sea otter Index", y = bquote('Epiphyte mass  +/- se (g)')) +
  lims(y = c(0, 0.04)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Jday
EP.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = epi_mass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = epi_mass - epi_mass_se, ymax = epi_mass + epi_mass_se), size = 1) +
  labs(x = "Date", y = bquote('Epiphyte mass  +/- se (g)')) +
  lims(y = c(0, 0.04)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = c(0.07, 0.8))

## Epiphyte Load ##
# SOI
plot(dat$date_julian, dat$abvgnd_mass, pch = 16)

EL.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = epiphmass_shootmass), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = epiphmass_shootmass - epiphmass_shootmass_se, ymax = epiphmass_shootmass + epiphmass_shootmass_se), size = 1) +
  labs(x = "Sea otter Index", y = bquote('Epiphyte mass / Shoot mass +/- se' ~(g/g^-2))) +
  lims(y = c(0, 0.25)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Jday
EL.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = epiphmass_shootmass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = epiphmass_shootmass - epiphmass_shootmass_se, ymax = epiphmass_shootmass + epiphmass_shootmass_se), size = 1) +
  labs(x = "Date", y = bquote('Epiphyte mass / Shoot mass +/- se' ~(g/g^-2))) +
  lims(y = c(0, 0.25)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Grazer Mass ##
# SOI
GM.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = grazer_mass), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = grazer_mass - grazer_mass_se, ymax = grazer_mass + grazer_mass_se), size = 1) +
  labs(x = "Sea otter Index", y = bquote('Grazer mass +/- se (g)')) +
  lims(y = c(0, 0.25)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Jday
GM.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = grazer_mass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = grazer_mass - grazer_mass_se, ymax = grazer_mass + grazer_mass_se), size = 1) +
  labs(x = "Date", y = bquote('Grazer mass +/- se (g)')) +
  lims(y = c(0, 0.25)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Grazer Load ##
# SOI
plot(dat$PCA2, dat$grazer_mass, pch = 16)

GL.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = grazermass_shootmass), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = grazermass_shootmass - grazermass_shootmass_se, ymax = grazermass_shootmass + grazermass_shootmass_se), size = 1) +
  labs(x = "Sea otter Index", y = bquote('Grazer mass +/- se' ~(g/g^-2))) +
  lims(y = c(0, 0.25)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Jday
GL.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = grazermass_shootmass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = grazermass_shootmass - grazermass_shootmass_se, ymax = grazermass_shootmass + grazermass_shootmass_se), size = 1) +
  labs(x = "Date", y = bquote('Grazer mass +/- se' ~(g/g^-2))) +
  lims(y = c(0, 0.25)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

#####----- Eelgrass Data Panel Figure -----#####
theme_set(theme_cowplot(font_size = 12))
plot_grid(SD.SOI, SD.JDY, AB.SOI, AB.JDY, BB.SOI, BB.JDY, nrow = 3, ncol = 2)
plot_grid(EP.SOI, EP.JDY, EL.SOI, EL.JDY, GL.SOI, GL.JDY, nrow = 3, ncol = 2)
plot_grid(N2.SOI, N2.JDY, nrow = 2, ncol = 1)

#####----- Eelgrass Data Secondary Analyses -----#####
## Total Grazers vs. epiphytes ##
TG.EP <- ggplot(dat) +
  geom_point(aes(x = grazer_mass, y = epi_mass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = grazer_mass, ymin = epi_mass - epi_mass_se, ymax = epi_mass + epi_mass_se)) +
  geom_errorbarh(aes(y = epi_mass, x = grazer_mass, xmin = grazer_mass - grazer_mass_se, xmax = grazer_mass + grazer_mass_se)) +
  labs(x = "Grazer mass +/- se (g)", y = "Epiphyte mass +/- se (g)") +
  lims(y = c(0, 0.04), x = c(0, 0.25)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = c(0.5, 0.85))

## Total Grazers vs. Shoot density ##
TG.SD <- ggplot(dat) +
  geom_point(aes(x = shoot_dens, y = grazer_mass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = shoot_dens, ymin = grazer_mass - grazer_mass_se, ymax = grazer_mass + grazer_mass_se)) +
  geom_errorbarh(aes(y = grazer_mass, x = shoot_dens, xmin = shoot_dens - shoot_dens_se, xmax = shoot_dens + shoot_dens_se)) +
  labs(x = "Shoot density +/- se", y = "Grazer mass +/- se (g)") +
  lims(y = c(0, 0.25), x = c(0, 525)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Total Grazers vs. Aboveground biomass ##
TG.AGB <- ggplot(dat) +
  geom_point(aes(x = abvgnd_mass, y = grazer_mass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = abvgnd_mass, ymin = grazer_mass - grazer_mass_se, ymax = grazer_mass + grazer_mass_se)) +
  geom_errorbarh(aes(y = grazer_mass, x = abvgnd_mass, xmin = abvgnd_mass - abvgnd_mass_se, xmax = abvgnd_mass + abvgnd_mass_se)) +
  labs(x = "Aboveground biomass +/- se (g/m2)", y = "Grazer mass +/- se (g)") +
  lims(y = c(0, 0.25), x = c(0, 200)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Total Grazers vs. Belowground biomass ##
TG.BGB <- ggplot(dat) +
  geom_point(aes(x = blwgnd_mass, y = grazer_mass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = blwgnd_mass, ymin = grazer_mass - grazer_mass_se, ymax = grazer_mass + grazer_mass_se)) +
  geom_errorbarh(aes(y = grazer_mass, x = blwgnd_mass, xmin = blwgnd_mass - blwgnd_mass_se, xmax = blwgnd_mass + blwgnd_mass_se)) +
  labs(x = "Belowground biomass +/- se (g/m2)", y = "Grazer mass +/- se (g)") +
  lims(y = c(0, 0.25), x = c(0, 80)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Crustacean Grazers ##
# SOI
CR.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = crusmass_shootmass), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = crusmass_shootmass - crusmass_shootmass_se, ymax = crusmass_shootmass + crusmass_shootmass_se)) +
  labs(x = "Sea otter index", y = "Crustmass/shootmass") +
  lims(y = c(0, 0.15)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))

# Jday
CR.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = crusmass_shootmass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = crusmass_shootmass - crusmass_shootmass_se, ymax = crusmass_shootmass + crusmass_shootmass_se)) +
  labs(x = "Date", y = "Crustmass/shootmass") +
  lims(y = c(0, 0.15)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Gastropod Grazers ##
# SOI
GA.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = gastmass_shootmass), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = gastmass_shootmass - gastmass_shootmass_se, ymax = gastmass_shootmass + gastmass_shootmass_se)) +
  labs(x = "Sea otter index", y = "Gastromass/shootmass") +
  lims(y = c(0, 0.10)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Jday
GA.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = gastmass_shootmass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = gastmass_shootmass - gastmass_shootmass_se, ymax = gastmass_shootmass + gastmass_shootmass_se)) +
  labs(x = "Date", y = "Gastromass/shootmass") +
  lims(y = c(0, 0.10)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

#####----- Grazer Panel Figure -----#####
theme_set(theme_cowplot(font_size = 12))
plot_grid(TG.EP, TG.SD, TG.AGB, TG.BGB, CR.SOI, CR.JDY, GA.SOI, GA.JDY, nrow = 4, ncol = 2)

## Cover ##
# Macroalgae vs. SOI
MC.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = macro_dens), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = macro_dens - macro_dens_se, ymax = macro_dens + macro_dens_se)) +
  labs(x = "Sea otter index", y = "Macroalgae cover (%)") +
  lims(y = c(0,75)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))

# Macroalgae vs. Jday
MC.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = macro_dens, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = macro_dens - macro_dens_se, ymax = macro_dens + macro_dens_se)) +
  labs(x = "Date", y = "Macroalgae cover (%)") +
  lims(y = c(0,75)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = c(0.1, 0.85))

# Diatoms vs. SOI
DC.SOI <- ggplot(dat) +
  geom_point(aes(x = PCA2, y = diatom_dens), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = diatom_dens - diatom_dens_se, ymax = diatom_dens + diatom_dens_se)) +
  labs(x = "Sea otter index", y = "Diatom cover (%)") +
  lims(y = c(0,125)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Diatoms vs. Jday
DC.JDY <- ggplot(dat) +
  geom_point(aes(x = date_julian, y = diatom_dens, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = diatom_dens - diatom_dens_se, ymax = diatom_dens + diatom_dens_se)) +
  labs(x = "Date", y = "Diatom cover (%)") +
  lims(y = c(0,125)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Macroalage vs. Grazers
MC.GRZ <- ggplot(dat) +
  geom_point(aes(x = macro_dens, y = grazer_mass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = macro_dens, ymin = grazer_mass - grazer_mass_se, ymax = grazer_mass + grazer_mass_se)) +
  geom_errorbarh(aes(x = macro_dens, y = grazer_mass, xmin = macro_dens - macro_dens_se, xmax = macro_dens + macro_dens_se)) +
  labs(x = "Macroalge Cover (%) +/- se", y = "Grazer mass +/- se (g)") +
  lims(y = c(0, 0.2)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Diatoms vs. grazers
DC.GRZ <- ggplot(dat) +
  geom_point(aes(x = diatom_dens, y = grazer_mass, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = diatom_dens, ymin = grazer_mass - grazer_mass_se, ymax = grazer_mass + grazer_mass_se)) +
  geom_errorbarh(aes(x = diatom_dens, y = grazer_mass, xmin = diatom_dens - diatom_dens_se, xmax = diatom_dens + diatom_dens_se)) +
  labs(x = "Diatom Cover (%) +/- se", y = "Grazer mass +/- se (g)") +
  lims(y = c(0, 0.2)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

#####----- Cover Panel Figure -----#####
plot_grid(MC.SOI, DC.SOI, MC.JDY, DC.JDY, MC.GRZ, DC.GRZ, nrow = 3, ncol = 2)

## Crabs and Grazers ##
# All grazers
GR.CM <- ggplot(dat, aes(mass, grazer_mass)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = grazer_mass - grazer_mass_se, ymax = grazer_mass + grazer_mass_se), size = 1, width = 0) +
  geom_errorbarh(aes(xmin = mass - mass_se, xmax = mass + mass_se), size = 1, width = 0) +
  lims(x = c(0, 275), y = c(0, 0.22)) +
  labs(x = "Crab mass +/- se (g)", y = "Grazer mass +/- se (g)") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))

# Crustacean Grazers
CG.CM <- ggplot(dat, aes(mass, crus_mass)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = crus_mass - crus_mass_se, ymax = crus_mass + crus_mass_se), size = 1, width = 0) +
  geom_errorbarh(aes(xmin = mass - mass_se, xmax = mass + mass_se), size = 1, width = 0) +
  lims(x = c(0, 275), y = c(0, 0.125)) +
  labs(x = "Crab mass +/- se (g)", y = "Crustacean grazer mass +/- se (g)") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))

# Gastropod Grazers
GG.CM <- ggplot(dat, aes(mass, gast_mass)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = gast_mass - gast_mass_se, ymax = gast_mass +gast_mass_se), size = 1, width = 0) +
  geom_errorbarh(aes(xmin = mass - mass_se, xmax = mass + mass_se), size = 1) +
  lims(x = c(0, 275), y = c(0, 0.175)) +
  labs(x = "Crab mass +/- se (g)", y = "Gastropod grazer mass +/- se (g)") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))

## Crabs and Grazers Panel Figure ##
plot_grid(GR.CM, CG.CM, GG.CM, nrow = 3, ncol = 1)

## Crabs and Eelgrass ##
# Aboveground biomass 
AG.CM <- ggplot(dat, aes(abvgnd_mass, mass)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = abvgnd_mass - abvgnd_mass_se, xmax = abvgnd_mass + abvgnd_mass_se), size = 1) +
  geom_errorbar(aes(ymin = mass - mass_se, ymax = mass + mass_se), size = 1) +
  lims(x = c(0, 200), y = c(0, 225)) +
  labs(x = "Aboveground mass +/- se (g)", y = "Crab mass +/- se (g)") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))

# Belowground biomass
BG.CM <- ggplot(dat, aes(blwgnd_mass, mass)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = blwgnd_mass - blwgnd_mass_se, xmax = blwgnd_mass + blwgnd_mass_se), size = 1) +
  geom_errorbar(aes(ymin = mass - mass_se, ymax = mass + mass_se), size = 1) +
  lims(x = c(0, 75), y = c(0, 225)) +
  labs(x = "Belowground mass +/- se (g)", y = "Crab mass +/- se (g)") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))

# Shoot Density
SD.CM <- ggplot(dat, aes(shoot_dens, mass)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = shoot_dens - shoot_dens_se, xmax = shoot_dens + shoot_dens_se), size = 1) +
  geom_errorbar(aes(ymin = mass - mass_se, ymax = mass + mass_se), size = 1) +
  lims(x = c(0, 500), y = c(0, 225)) +
  labs(x = "Shoot density +/- se (g)", y = "Crab mass +/- se (g)") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))

## Crabs and Eelgrass Panel figure ##
plot_grid(SD.CM, AG.CM, BG.CM, nrow = 3, ncol = 1)

#####----- Nutrient Data -----#####
# Add data
nut$date_corr <- as.Date(nut$date_corr, format = "%m/%d/%Y")
nut$date_julian <- format(nut$date_corr, "%j")

## NOx ##
# Site
NO.SIT <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "eelgrass",], aes(x = site_full, y = nox_umol),  size = 2) +
  labs(x = "Site", y = "NOx umol") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

# Jday 
NO.JDY <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "eelgrass",], aes(x = date_julian, y = nox_umol),  size = 2) +
  labs(x = "Date", y = "NOx umol") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

## Nutirent Day ##
NO.NDY <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "nutday",], aes(x = site_full, y = nox_umol, col = depth_m), size = 2) +
  labs(x = "Site", y = "NOx umol") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = c(0.05, 0.8))

## NH2 ##
# Site
NH.SIT <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "eelgrass",], aes(x = site_full, y = nh4_umol),  size = 2) +
  labs(x = "Site", y = "NH4 umol") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

# Jday 
NH.JDY <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "eelgrass",], aes(x = date_julian, y = nh4_umol),  size = 2) +
  labs(x = "Date", y = "NH4 umol") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

## Nutirent Day ##
NH.NDY <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "nutday",], aes(x = site_full, y = nh4_umol, col = depth_m), size = 2) +
  labs(x = "Site", y = "NH4 umol") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = "none")

## PO2 ##
# Site
PO.SIT <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "eelgrass",], aes(x = site_full, y = po4_umol),  size = 2) +
  labs(x = "Site", y = "PO4 umol") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

# Jday 
PO.JDY <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "eelgrass",], aes(x = date_julian, y = po4_umol),  size = 2) +
  labs(x = "Date", y = "PO4 umol") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

## Nutirent Day ##
PO.NDY <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "nutday",], aes(x = site_full, y = po4_umol, col = depth_m), size = 2) +
  labs(x = "Site", y = "PO4 umol") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), legend.position = "none")

## N to P ##
# Site
NP.SIT <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "eelgrass",], aes(x = site_full, y = N.P),  size = 2) +
  labs(x = "Site", y = "N:P") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), axis.text.x = element_text(angle = 25, hjust = 1))

# Jday 
NP.JDY <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "eelgrass",], aes(x = date_julian, y = N.P),  size = 2) +
  labs(x = "Date", y = "N:P") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Nutirent Day ##
NP.NDY <- ggplot() +
  geom_point(data = nut[nut$sampling_type == "nutday",], aes(x = site_full, y = N.P, col = depth_m), size = 2) +
  labs(x = "Site", y = "N:P") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

#####----- Nutrient Panel Figure -----#####
theme_set(theme_cowplot(font_size = 12))
plot_grid(NO.SIT, NO.JDY, NO.NDY, NH.SIT, NH.JDY, NH.NDY, PO.SIT, PO.JDY, PO.NDY, NP.SIT, NP.JDY, NP.NDY, nrow = 4, ncol = 3)

## All Nutrients ## - this may not actually be helpful
nut.region <- data.frame(
  nut %>%
    filter(sampling_type == "eelgrass") %>% 
    group_by(otter_region) %>% 
    summarise(NOx = mean(nox_umol),
              NOx_se = st.er(nox_umol),
              NH4 = mean(nh4_umol),
              NH4_se = st.er(nh4_umol),
              PO4 = mean(po4_umol),
              PO4_se = st.er(po4_umol))
)



