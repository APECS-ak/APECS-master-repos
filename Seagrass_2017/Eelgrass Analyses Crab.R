#####----- Eelgrass Crab Analyses -----#####
## Originally created 11.21.2017 - Wendel Raymond ##

#####----- Libraries -----####
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(vegan)

theme_set(theme_classic())

#####----- Functions -----#####
st.er <- function(x, na.rm = TRUE) {
  stopifnot(is.numeric(x))
  return(sd(x, na.rm = na.rm)/sqrt(length(x)))
}

#####----- Load Data -----#####

# Crab Data
crab <- read.csv("Data/Eelgrass_Crab_Data_2017.csv", header = TRUE)
crab$string <- as.factor(crab$string)

# Crab length to weight conversion
crabLW <- read.csv("Data/Eelgrass_crab_length_weight_27Nov17_WR.csv", header = TRUE)

# Site Data
dat <- read.csv("Data/Eelgrass_and_Grazer_Data_2017.csv", header = TRUE)

# Adding additional data
crab$date_pulled <- as.Date(crab$date_pulled , format = "%m/%d/%Y")
crab$date_julian <- format(crab$date_pulled, "%j")

#####----- Convert Carapase Width to Mass -----#####
## Add conversion values to data ##
crab <- merge(crab, crabLW, by = "sp_code")
crab <- crab[,c(1:21, 23:29)]
colnames(crab)[14] <- "taxon"

## Calculate Mass ##
crab$mass_g <- (crab$a_mean * (crab$carapace_width_mm)^crab$b_mean)
range(crab$mass_g, na.rm = TRUE)
hist(crab$mass_g)

#####----- Summaries -----#####
## Species by Site and species ##
crab.sp <- data.frame(
  crab %>% 
    filter(taxon == "Decapoda") %>% 
    group_by(site, sp_code) %>% 
    summarise(count = n(), 
              width = mean(carapace_width_mm), 
              width_se = st.er(carapace_width_mm),
              mass = mean(mass_g),
              mass_se = st.er(mass_g))
)

## Summaries by Site ##
crab.site <- crab %>% 
  filter(taxon == "Decapoda") %>% 
  group_by(site) %>% 
  summarise(date = unique(date_pulled),
            date_j = unique(date_julian),
            count = n(),
            width = mean(carapace_width_mm), 
            width_se = st.er(carapace_width_mm),
            mass = mean(mass_g),
            mass_se = st.er(mass_g))

# Add sea otter intex
crab.site$SOI <- dat$PCA2

# Write for use elsewhere
write.csv(crab.site, "Data/Crab_summaries_site.csv", row.names = FALSE)

#####----- Plots -----#####
## Mass of species at each site ##
ggplot(crab.sp, aes(sp_code, mass, fill = sp_code))  +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = sp_code, ymin = mass - mass_se, ymax = mass +mass_se), width = 0) +
  scale_fill_brewer(palette = "Dark2", labels = c("Decorator", "Dungeness", "Graceful", "Helmet", "Red rock", "Unknown rock crab")) +
  labs(x = "Species", y = "Mass +/- se (g)", fill = "Species") +
  scale_x_discrete(labels = c("Decorator", "Dungeness", "Graceful", "Helmet", "Red rock", "Unknown rock crab")) +
  lims(y = c(0, 400)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~site)

## Mass of all crabs by SOI ##
CM.SOI <- ggplot(crab.site, aes(SOI, mass)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mass - mass_se, ymax = mass +mass_se), size = 1, width = 0) +
  labs(x = "Sea otter index", y = "Crab mass +/- se (g)") +
  lims(y = c(0, 300)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))

## Mass by Date ##
CM.JDY <- ggplot(crab.site, aes(date_j, mass)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = mass - mass_se, ymax = mass +mass_se), size = 1, width = 0) +
  labs(x = "Date", y = "Crab mass +/- se (g)") +
  lims(y = c(0, 300)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2))

## Abundance by species ##
CA.SIT <- ggplot(crab.sp) +
  geom_bar(aes(fill = sp_code, y = count, x = site), position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Dark2", labels = c("Decorator", "Dungeness", "Graceful", "Helmet", "Red rock", "Unknown rock crab")) +
  labs(x = "Site", y = "Crab abunance", fill = "Species") +
  lims(y = c(0, 25)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = c(0.1, 0.8))

## Size by site ##
CS.SIT <- ggplot(crab.sp) +
  geom_bar(aes(fill = sp_code, y = width, x = site), position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Dark2", labels = c("Decorator", "Dungeness", "Graceful", "Helmet", "Red rock", "Unknown rock crab")) +
  labs(x = "Site", y = "Carapace width (mm)", fill = "Speceis") +
  lims(y = c(0, 175)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

#####----- Panel crab species and sizes -----#####
theme_set(theme_cowplot(font_size = 12))
plot_grid(CM.SOI, CM.JDY, CA.SIT, CS.SIT, nrow = 2, ncol = 2)

## Abundance ##
# SOI
CA.SOI <- ggplot(crab.site) +
  geom_point(aes(x = SOI, y = count), size = 4) +
  geom_errorbar(aes(x = SOI, ymin = count - count_se, ymax = count + count_se), size = 1) +
  labs(x = "Sea otter Index", y = "Crab abunance per string") +
  lims(y = c(0, 15)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Jday
CA.JDY <- ggplot(crab.site) +
  geom_point(aes(x = date_julian, y = count, col = SOI), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = count - count_se, ymax = count + count_se), size = 1, width = 0) +
  labs(x = "Date", y = "Crab abunance per string") +
  lims(y = c(0, 15)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = c(0.75, 0.8))

## Carapase width ##
# SOI
CW.SOI <- ggplot(crab.site) +
  geom_point(aes(x = PCA2, y = carapase), size = 4) +
  geom_errorbar(aes(x = PCA2, ymin = carapase - carapase_se, ymax = carapase + carapase_se), size = 1) +
  labs(x = "Sea otter Index", y = "Carapase width per string (mm)") +
  lims(y = c(0, 150)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

# Jday
CW.JDY <- ggplot(crab.site) +
  geom_point(aes(x = date_julian, y = carapase, col = PCA2), size = 4) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_errorbar(aes(x = date_julian, ymin = carapase - carapase_se, ymax = carapase + carapase_se), size = 1, width = 0) +
  labs(x = "Date", y = "Carapase width per string (mm)") +
  lims(y = c(0, 150)) +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

#####----- Crab Data Panel Figure -----#####
theme_set(theme_cowplot(font_size = 12))
plot_grid(CA.SOI, CA.JDY, CW.SOI, CW.JDY, nrow = 2, ncol = 2)

#####----- Multivariate Analyses -----#####


