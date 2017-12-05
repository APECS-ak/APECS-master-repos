#####----- Pit and Shell Summaries -----#####
## Calculation of proportion of sea otter pits by site ##
## Originally created 10.9.2017 WR ##

#####----- Libraries -----#####
library(dplyr)
library(ggplot2)
theme_set(theme_classic())

#####----- Load Data -----#####
pit <- read.csv("E:/wraymond2/My Documents/Graduate School/Eelgrass/Data/2017 Field Season/Pit and Shell/Pit_Sediment_09Oct2017_WR.csv", header = TRUE)
shell <- read.csv("E:/wraymond2/My Documents/Graduate School/Eelgrass/Data/2017 Field Season/Pit and Shell/Clam_09Oct2017_WR.csv", header = TRUE)

#####----- Number of Pits by Site -----#####
# Sum pits by site
pit.site <- data.frame(
  pit %>%
    group_by(Site) %>% 
    summarise(n_pits = sum(PitBin))
)

pit.site <- pit.site[2:22,]

# Simple plot
ggplot() +
  geom_bar(data = pit.site, aes(x = Site, y = n_pits), stat  = "identity") +
  theme(axis.text.x = element_text(angle = 90))

#####----- Proportion of Sea otter cracked shells by site -----#####
# Sum of shell death type by site
shell.site <- data.frame(
  shell %>%
    group_by(Site, Death) %>% 
    summarise(n_shells = n())
)

# Simple plot
ggplot() +
  geom_bar(data = shell.site, aes(x = Site, y = n_shells, fill = Death), stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

#####----- Proportion Sea otter pits -----#####
# Proportion of shells that where sea otter cracked by site
prop.shell <- data.frame(
shell.site %>% 
  group_by(Site) %>% 
  summarise(n_SO_shells = n_shells[Death == "Sea Otter"], n_Total_shells = sum(n_shells))
)

prop.shell$prop_SO_shells <- (prop.shell$n_SO/prop.shell$n_Total)

# Proportion sea otter pit attributed to sea otters
seaotter.pits <- merge(pit.site, prop.shell)
seaotter.pits$SO_pits <- seaotter.pits$n_pits * seaotter.pits$prop_SO

# Simple plot
ggplot() +
  geom_bar(data = seaotter.pits, aes(x = Site, y = SO_pits), stat  = "identity") +
  theme(axis.text.x = element_text(angle = 90))