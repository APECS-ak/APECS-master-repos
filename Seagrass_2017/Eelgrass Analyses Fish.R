#####----- Eelgrass Fish Analyses -----#####
## Originally created 11.17.2017 - Wendel Raymond ##

#####----- Libraries -----#####
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(corrgram)
library(corrr)
library(cowplot)

theme_set(theme_classic())

#####----- Load Data ------#####
fish <- read.csv("Eelgrass_Fish_Species_Counts_2017.csv", header = TRUE)

eel <- read.csv("Eelgrass_and_Grazer_Data_2017.csv", header = TRUE)

#####----- Add Data and Summaries -----#####
## Sea otter index ##
fish$PCA2 <- eel$PCA2

## Julian Day ##
fish$date <- as.Date(fish$date, format = "%m/%d/%Y")
fish$date_julian <- format(fish$date, "%j")

## Total Fishes ##
fish$total <- rowSums(fish[,9:64])

## Species Richness ##
fish$shannon <- diversity(fish[,9:64], index = "shannon")

#####----- Exploratory Univariate Analyses -----#####
## Total Fishes SOI ##
plot(fish$PCA2, fish$total)

FT.SOI <- ggplot(fish) +
  geom_point(aes(x = PCA2, y = log(total)), size = 4) +
  labs(x = "Sea otter Index", y = "Log total fishes") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Total Fishes Date ##
plot(fish$date_julian, log(fish$total))

FT.JDY <- ggplot(fish) +
  geom_point(aes(x = date_julian, y = log(total)), size = 4) +
  labs(x = "Date", y = "Log total fishes") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Richness SOI ##
plot(fish$PCA2, fish$shannon)

SD.SOI <- ggplot(fish) +
  geom_point(aes(x = PCA2, y = shannon), size = 4) +
  labs(x = "Sea otter Index", y = "Shannon diversity") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Richness date ##
plot(fish$date_julian, fish$shannon)

SD.JDY <- ggplot(fish) +
  geom_point(aes(x = date_julian, y = shannon), size = 4) +
  labs(x = "Date", y = "Shannon diversity") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

## Corelations ##
x <- fish[,9:65] %>% 
  correlate() %>% 
  focus(PCA2)

COR <- x %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(PCA2)])) %>% 
  ggplot(aes(x = rowname, y = PCA2)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with PCA2") +
  xlab("Species") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Staghorn Sculpins ##
ST.SOI <- ggplot(fish) +
  geom_point(aes(x = PCA2, y = log(SCULPSTG)), size = 4) +
  labs(x = "Sea Otter Index", y = "Log staghorn abundance") +
  theme(text = element_text(size = 12), axis.ticks = element_line(size = 2), legend.position = "none")

#####----- Fish panel figure 1 -----#####
plot_grid(FT.SOI, FT.JDY, SD.SOI, SD.JDY, ST.SOI, nrow = 3, ncol = 2)
