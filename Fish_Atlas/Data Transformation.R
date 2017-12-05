#####----- Salmonid Nursery Habitat -----#####
### Data Manipulation and Preparation ###
# Originally Created 5.7.2017

#####----- Libraries -----#####
library(dplyr)
library(ggplot2)
theme_set(theme_classic())
library(tidyr)

#####----- Load Data -----#####
raw <- read.csv("SEAK_Fish_data_5.16.2017.csv", header = TRUE)

#####----- Exctract Species List -----#####
splist <- data.frame(
  raw %>% 
  distinct(SpCode, Sp_CommonName, Sp_ScientificName)
)

write.csv(splist, "NOAA_Beach_Seine_Sp_list.csv")

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

#####----- Salmonids THIS WILL NOT INCLUDE ZEROS-----#####
# Subset Cohos, Pinks, and Chums, Chinooks and Sockeye are just too rare
dat.sal <- subset(raw, SpCode == "SALCOHO" | SpCode == "SALPINK" | SpCode == "SALCHUM")

#####----- Convert to Counts -----#####
# Subset of unmeasured counts
dat.un <- data.frame(
  raw %>%
    filter(Unmeasured > 0)
)

# subset of all measured data
dat.m <- data.frame(
  raw %>%
    filter(Unmeasured == 0)
)

# Nsp
Nsp <- data.frame(
  raw %>% 
    group_by(EventID, SpCode) %>% 
    summarise(N = n())
)

# Summarize unmeasured counts
Nsp.un <- cbind.data.frame(dat.un[,2], dat.un[,19], dat.un[,23])
colnames(Nsp.un) <- c("EventID", "SpCode", "N")

# Add unmeasured counts to Nsp
Nsp$EventSp <- paste(Nsp$EventID, Nsp$SpCode, sep = ".")
Nsp.un$EventSp <- paste(Nsp.un$EventID, Nsp.un$SpCode, sep = ".")
Nsp.total <- merge(Nsp, Nsp.un, by = "EventSp", all = TRUE)
Nsp.total$N.x[is.na(Nsp.total$N.x)] <- 0
Nsp.total$N.y[is.na(Nsp.total$N.y)] <- 0

Nsp.total$EventID <- ifelse(is.na(Nsp.total$EventID.x), Nsp.total$EventID.y, Nsp.total$EventID.x)
Nsp.total$SpCode <- ifelse(is.na(Nsp.total$SpCode.x), as.character(Nsp.total$SpCode.y), as.character(Nsp.total$SpCode.x))
Nsp.total$N <- (Nsp.total$N.x + Nsp.total$N.y)

# Add Zeros of fish not present
unique(raw$EventID)
nulls <- data.frame(rep(unique(raw$EventID), 224))
nulls$SpCode <- rep(splist$SpCode, 919)
colnames(nulls) <- c("EventID", "SpCode")
alldat <- merge(nulls, Nsp.total, all = TRUE)

# Remove extra columns
alldat <- alldat[,c(1,2,10)]
alldat[is.na(alldat)] <- 0

# Add Site Data to alldat
site <- unique(raw[,1:18])
alldat.site <- merge(site, alldat, by = "EventID")
write.csv(alldat.site, "SEAK_by_Haul_Counts.csv", row.names = FALSE)


# Other Transformations
Nsp.final <- Nsp.total[,8:10]
Nsp.final$EventSp <- paste(Nsp.final$EventID, Nsp.final$SpCode, sep = ".")
Nsp.ag <- aggregate(Nsp.final['N'], by=Nsp.final['EventSp'], sum)

Nsp.t <- merge(Nsp.final, Nsp.ag, by = "EventSp", all = TRUE)
Nsp.t <- Nsp.t[,c(1,2,3,5)]
Nsp.tall <- unique(Nsp.t[,2:4])

# Spread
Nsp.wide <- spread(Nsp.tall, key = SpCode , value = N.y)
Nsp.wide[is.na(Nsp.wide)] <- 0

# append site level data
site <- unique(raw[,1:18])
dat.sal.wide <- merge(site, Nsp.wide, by = "EventID")
write.csv(dat.sal.sub, "Salmon_Habitat_Wide.csv")

# Trun Species into columns
Nsp.sal <- merge(site, Nsp.tall, by = "EventID")
colnames(Nsp.sal)[20] <- "count"
write.csv(Nsp.sal, "SEAK_Salmon_by_Haul_Counts.csv", row.names = FALSE)

avg.seine <- data.frame(
  Nsp.sal %>% 
    group_by(SpCode, Habitat) %>% 
    summarise(N.seines = n(), mean = mean(count), sd = sd(count))
)

ggplot(avg.seine, aes(SpCode)) +
  geom_bar(aes(y = mean, fill = Habitat), position = "dodge", stat = "identity") +
  labs(y = "Mean per haul", x = "Species")
