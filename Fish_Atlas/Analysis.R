#####----- Salmon Nursery Habitat -----#####
## Analysis ##
## Originaly Created 5.17.2017

#####----- Libraries -----#####
library(dplyr)
library(ggplot2)
theme_set(theme_classic())
library(tidyr)
library(rgdal)

#####----- Load Data -----#####
raw <- read.csv("SEAK_Fish_data_5.16.2017.csv", header = TRUE)

# Counts only
counts <- read.csv("SEAK_by_Haul_Counts.csv", header = TRUE)

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
counts$Date<- as.Date(counts$Date, "%Y-%m-%d")

#####----- Salmonids -----#####
# Subset Cohos, Pinks, and Chums, Chinooks and Sockeye are just too rare
# This will have columns of lengths AND N unmeasured
dat.sal <- subset(raw, SpCode == "SALCOHO" | SpCode == "SALPINK" | SpCode == "SALCHUM")


#####----- Data Exploration -----#####
## Temporal Range ##
range(counts$Date)

# Convert Julian Day
counts$jday <- format(counts$Date, "%j")
counts$jday <- as.numeric(counts$jday)

# Temporal Summary
site <- unique(counts[,c(1:18, 21)])
hist(site$jday) # probably cut off seines <jday 91 (April 1st) and <jday 245 (August 31st)
hist(site$jday[site$jday > 91 & site$jday < 245], xlab = "Julian Day", main = NA)

# Temporal Subset Data Frame
count.temp <- subset(counts, counts$jday > 91 & counts$jday < 245)
count.temp$SpCode <- as.character(count.temp$SpCode)
site.temp <- subset(site, site$jday > 91 & site$jday < 245)

## Temperature ##
tempsal <- data.frame(
  site.temp %>% 
    group_by(Year, Mon) %>% 
    summarise(mean.temp = mean(Temp, na.rm = TRUE), sd.temp = sd(Temp, na.rm = TRUE),
              mean.sal = mean(Salinity, na.rm = TRUE), sd.sal = sd(Salinity, na.rm = TRUE),
              N = n())
)

write.csv(tempsal, "Year_Month_TempSal.csv", row.names = FALSE)

## Spatial Range ##
# Map
SEAK <- readOGR(dsn="C:/Users/Wendubs/Documents/Graduate School/Sea Otter Harvest/GIS Files/ALASKA_PY_UTM_SEAK_redu", layer="ALASKA_PY_UTM_8N_SEAK3")
SEAK <- spTransform(SEAK, CRS("+proj=longlat +datum=WGS84"))
SEAK.f <- fortify(SEAK)

ggplot() +
  geom_polygon(data = SEAK.f, aes(x = long, y = lat, group = group)) +
  geom_point(data = site.temp, aes(x = Long1, y = Lat1, group = Habitat, col = Habitat)) +
  scale_color_manual(values=c("grey","brown","khaki", "green")) 

## Habitat ###
# Habitat Summary
hab <- data.frame(
  site.temp %>%
    group_by(Habitat, Mon, Year) %>% 
    summarize(N = n())
)

hab.wide <- spread(hab, key = Mon, value = N)
hab.wide <- hab.wide[,c(1,2,3,7,6,5,4)]
hab.wide[is.na(hab.wide)] <- 0
hab.wide$Total <- rowSums(hab.wide[,3:7])
hab.wide$Year <- sort(hab.wide$Year)
write.csv(hab.wide, "Sampling Efffort Summary.csv")

ggplot(site.temp) +
  geom_bar(aes(Habitat, fill = Habitat)) +
  scale_fill_manual(values=c("grey","brown","khaki", "green")) +
  ylim(c(0,300)) +
  labs(y = "N Hauls")

# Counts by Habitat per unit effors (catch / n hauls)
count.hab <- data.frame(
  count.temp %>% 
    filter(SpCode == "SALCOHO" | SpCode == "SALPINK" | SpCode == "SALCHUM") %>% 
    group_by(Habitat, SpCode) %>% 
    summarise(N.hauls = n(), total = sum(N), CPUE = total/N.hauls)
)

ggplot(count.hab) +
  geom_bar(aes(SpCode, CPUE, fill = Habitat), stat = "identity", position = "dodge") +
  scale_fill_manual(values=c("grey","brown","khaki", "green")) +
  labs(x = "Species")

## Time as a function of presence ##
# Chum
plot(count.temp$jday[count.temp$SpCode == "SALCHUM"], log(count.temp$N[count.temp$SpCode == "SALCHUM"] +1))

ggplot(subset(count.temp, SpCode == "SALCHUM")) +
  geom_point(aes(x = jday, y = log(N), color = Habitat), size = 2, pch = 16) +
  scale_x_continuous(limits = c(90, 250)) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(values=c("grey","brown","khaki", "green")) +
  labs(x = "Julian Day", y = "log chum abundance")

# Coho
plot(count.temp$jday[count.temp$SpCode == "SALCOHO"], log(count.temp$N[count.temp$SpCode == "SALCOHO"] +1))

ggplot(subset(count.temp, SpCode == "SALCOHO")) +
  geom_point(aes(x = jday, y = log(N), color = Habitat), size = 2, pch = 16) +
  scale_x_continuous(limits = c(90, 250)) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(values=c("grey","brown","khaki", "green")) +
  labs(x = "Julian Day", y = "log coho abundance")

# Pink
plot(count.temp$jday[count.temp$SpCode == "SALPINK"], log(count.temp$N[count.temp$SpCode == "SALPINK"] +1))

ggplot(subset(count.temp, SpCode == "SALPINK")) +
  geom_point(aes(x = jday, y = log(N), color = Habitat), size = 2, pch = 16) +
  scale_x_continuous(limits = c(90, 250)) +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(values = c("grey", "brown", "khaki", "green")) +
  labs(x = "Julian Day", y = "log pink abundance")

#####----- Specices Specific -----#####
## Chums ##
par(mfrow = c(2,2))
hist(raw$Length[raw$SpCode == "SALCHUM" & raw$Habitat == "Bedrock"], xlim = c(0, 130), ylim = c(0, 600), xlab = "FL (mm)", main = "Bedrock", breaks = 10)
hist(raw$Length[raw$SpCode == "SALCHUM" & raw$Habitat == "Kelp"], xlim = c(0, 130), ylim = c(0, 600), xlab = "FL (mm)", main = "Kelp", breaks = 10)
hist(raw$Length[raw$SpCode == "SALCHUM" & raw$Habitat == "Seagrass"], xlim = c(0, 130), ylim = c(0, 600), xlab = "FL (mm)", main = "Seagrass", breaks = 10)
hist(raw$Length[raw$SpCode == "SALCHUM" & raw$Habitat == "Sand-Gravel"], xlim = c(0, 130), ylim = c(0, 600), xlab = "FL (mm)", main = "Sand-Gravel", breaks = 10)

## Coho ##
par(mfrow = c(2,2))
hist(raw$Length[raw$SpCode == "SALCOHO" & raw$Habitat == "Bedrock"], xlim = c(0, 200), ylim = c(0, 350), xlab = "FL (mm)", main = "Bedrock",  breaks = 10)
hist(raw$Length[raw$SpCode == "SALCOHO" & raw$Habitat == "Kelp"], xlim = c(0, 200), ylim = c(0, 350), xlab = "FL (mm)", main = "Kelp",  breaks = 10)
hist(raw$Length[raw$SpCode == "SALCOHO" & raw$Habitat == "Seagrass"], xlim = c(0, 200), ylim = c(0, 350), xlab = "FL (mm)", main = "Seagrass",  breaks = 10)
hist(raw$Length[raw$SpCode == "SALCOHO" & raw$Habitat == "Sand-Gravel"], xlim = c(0, 200), ylim = c(0, 350), xlab = "FL (mm)", main = "Sand-Gravel",  breaks = 10)

## Pink ##
par(mfrow = c(2,2))
hist(raw$Length[raw$SpCode == "SALPINK" & raw$Habitat == "Bedrock"], xlim = c(0, 120), ylim = c(0, 300), xlab = "FL (mm)", main = "Bedrock", breaks = 10)
hist(raw$Length[raw$SpCode == "SALPINK" & raw$Habitat == "Kelp"], xlim = c(0, 120), ylim = c(0, 300), xlab = "FL (mm)", main = "Kelp", breaks = 10)
hist(raw$Length[raw$SpCode == "SALPINK" & raw$Habitat == "Seagrass"], xlim = c(0, 120), ylim = c(0, 300), xlab = "FL (mm)", main = "Seagrass", breaks = 10)
hist(raw$Length[raw$SpCode == "SALPINK" & raw$Habitat == "Sand-Gravel"], xlim = c(0, 120), ylim = c(0, 300), xlab = "FL (mm)", main = "Sand-Gravel", breaks = 10)

#####----- Models -----#####