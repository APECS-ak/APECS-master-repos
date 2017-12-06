#####----- Data Preparation Beach Seine Fish Data -----#####
## Originally created 11.17.2017 - Wendel Raymond ##

#####----- Libraries -----#####
library(dplyr)
library(tidyr)
library(ggplot2)

#####----- Load Data ------#####
fish <- read.csv("Eelgrass_Fish_Data_2017.csv", header = TRUE)


#####----- Checking -----#####
names(fish)
levels(fish$sp_code)
levels(fish$taxon)
levels(fish$fork_length)

#####----- Toggle Fish Only -----#####
fish <- filter(fish, taxon == "Vertebrata")

#####----- Subsetting data to Unmeasued (counted) fishes only and measured fishes only -----#####
## Convert instances of no 'unmeasured' fish to 0 ##
fish$unmeasured[is.na(fish$unmeasured)] <- 0

## Unmeasured fish only data frame ##
fish.un <- data.frame(
  fish %>% 
    filter(unmeasured > 0)
)

## Measured fish only data frame ##
fish.m <- data.frame(
  fish %>% 
    filter(unmeasured == 0)
)

#####----- Summaries of subsetted data -----#####
## Summarise unmeasured data ##
Nsp.un <- cbind.data.frame(fish.un[,1], fish.un[,11], fish.un[,14])
colnames(Nsp.un) <- c("site", "sp_code", "N")

## Summarise measured data to counts ##
Nsp.m <- data.frame(
  fish.m %>% 
    group_by(site, sp_code) %>% 
    summarise(N = n())
)

#####----- Merge measuered and unmeasured summaries -----#####
## Merge site by species counts ##
Nsp.t <- merge(Nsp.m, Nsp.un, by = c("site", "sp_code"), all = TRUE)
Nsp.t$N.y[is.na(Nsp.t$N.y)] <- 0  
Nsp.t$N <- (Nsp.t$N.x + Nsp.t$N.y)
Nsp.t <- Nsp.t[,c(1,2,5)]

## Spread ##
Nsp.wide <- spread(Nsp.t, key = sp_code, value = N)
Nsp.wide[is.na(Nsp.wide)] <- 0

#####----- Master species counts data -----#####
dat <- unique(fish[,c(1:8)])

dat <- merge(dat, Nsp.wide, by = "site")

## Export ##
write.csv(dat, "Eelgrass_Fish_Species_Counts_2017.csv", row.names = FALSE)
