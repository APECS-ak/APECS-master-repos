#####----- Salmon Nursery Habitat -----#####
## Models ##
## Originaly Created 5.17.2017

#####----- Libraries -----#####
library(dplyr)
library(ggplot2)
theme_set(theme_classic())

#####----- Load Data -----#####
raw <- read.csv("SEAK_Fish_data_5.16.2017.csv", header = TRUE)

#####----- General Data Clean up -----#####
raw$SpCode <- as.character(raw$SpCode)
unique(raw$SpCode)

unique(raw$Habitat)
levels(raw$Habitat)[levels(raw$Habitat)=="Sand-gravel"] <- "Sand-Gravel"
levels(raw$Habitat)[levels(raw$Habitat)=="Surfgrass"] <- "Eelgrass"

# Change all eelgrass to Seagrass because we know that its not all eelgrass
levels(raw$Habitat)[levels(raw$Habitat)=="Eelgrass"] <- "Seagrass"

# Convert Unmeasuted NAs to 0
raw$Unmeasured[is.na(raw$Unmeasured)] <- 0

# Format Date
raw$Date<- as.Date(raw$Date, "%m/%d/%Y")

#####----- Salmonids -----#####
# Subset Cohos, Pinks, and Chums, Chinooks and Sockeye are just too rare
dat.sal <- subset(raw, SpCode == "SALCOHO" | SpCode == "SALPINK" | SpCode == "SALCHUM")


#####----- Data Exploration -----#####
# Course Summary
summary <- data.frame(
  dat.sal %>% 
    group_by(SpCode, Habitat) %>% 
    summarize(N = n(), minimum = min(Length, na.rm = T), maximum = max(Length, na.rm = T), mean = mean(Length, na.rm = T), median = median(Length, na.rm = T), sd = sd(Length, na.rm = T))
)

# Plot of Counts of speices by habitat
ggplot(dat.sal, aes(SpCode)) +
  geom_bar(aes(fill = Habitat), position = "dodge") +
  labs(x = "Species", y = "Counts")

#####----- Models -----#####