#' This is a R script that cleans the fish/hab/env data from 2019

# Cleaning
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(readxl)
library(reshape2)

rm(list = ls())
## Data input
seine <- read.csv("../ALL_DATA/Lia_fish/FishSeine_2019_RAW_10-4-19.csv")
name <- read.csv("site_code_names.csv", header = TRUE)
hab_qrt <- read_excel("../ALL_DATA/Lia_fish/Habitat_2019_RAW_10-23-19.xlsx", sheet = 1)
hab_lng <- read_excel("../ALL_DATA/Lia_fish/Habitat_2019_RAW_10-23-19.xlsx", sheet = 2)
hab_wgt <- read_excel("../ALL_DATA/Lia_fish/Habitat_2019_RAW_10-23-19.xlsx", sheet = 3)
env19 <- read.csv("../ALL_DATA/Lia_fish/Environmental_2019_RAW_10-4-19.csv")
otts <- read.csv("../ALL_DATA/Lia_fish/Otter_density_site_2019.csv")
biom <- read.csv("../ALL_DATA/seagrass_biomass_conversions.csv")



## Data clean up 
### Fish
# Look at structure of data and determine number of sites and combine fish datasets
seine$sp_code <- as.character(seine$sp_code)
unique(seine$sp_code) # 70 species

# include only fish species
seine$taxon <- as.character(seine$taxon)
fish <- filter(seine, taxon == "fish")
unique(fish$species_common)

# convert unmeasured NAs to 0
fish$unmeasured[is.na(fish$unmeasured)] <- 0

# number of unique sites
unique(fish$site) #25

# filter by habitat to see site number breakdown by habitat in 2019
# 20 eelgrass sites
fish %>%
  filter(habitat == "eelgrass") %>%
  distinct(site) 
# 5 kelp sites
fish %>%
  filter(habitat == "kelp") %>%
  distinct(site)

# add appropriate habitat and year data
fish$year <- "2019"

# adjust capitals/symantics for 2019 and 2017 sitenames to match
names2 <- dplyr::select(name, site, names_2019)
names2 <- data.frame(lapply(names2, as.character), stringsAsFactors = FALSE)
colnames(names2) <- c("place", "site")
fish2 <- left_join(fish, names2, by = "site")
fish19 <- fish2 %>%
  select(-site) %>%
  dplyr::rename(site = place)


# extract a complete list of all sites seines on a julian date
sites <- fish19 %>%
  filter(habitat == "eelgrass") %>%
  dplyr::select(site, year, date) %>%
  distinct() %>%
  mutate(date = mdy(date)) %>%
  mutate(juli_date = yday(date))
sites <- sites[-16,] # remove the salt lake bay seine that happened early season that didn't work

### Sea otter survey
# Sea otters are hard to count and understanding their impact on nearshore ecosystem is 
# dependent not only on their presence but also on their use of the environment we are 
# interested in. We collected multiple (like correlated) measures of sea otter impact on 
# a nearshore eelgrass community. With this we can create a PCA index of sea otter impact. 
# HOWEVER for simplicity sake for the time being we are only going to consider the average 
# number of sea otters per site based on surveys. These were replicate (except for Shakan - eel) 
# boat based surveys that with a minimum of 3 observers (?) that drove approximately 2 nm 
# around the site going in inlets and bays and marking the approximate location of all sea 
# otters within the survey area.  

# See R Markdown script : Sea otter impact index allR vLia2019 for getting raw waypoint data 
# to shapefile to then calculating number of otters per site. 

otts19 <- otts %>%
  group_by(site) %>%
  rowwise() %>%
  dplyr::mutate(avg_density = mean(c(dens_surv1, dens_surv2))) %>%
  dplyr::mutate(sd_density = sd(c(dens_surv1, dens_surv2))) %>%
  dplyr::select(-survey_dat_1, -survey_dat_2, -area)
otts19$year <- "2019"
names(otts19)[3:4] <- c("n_surv1" , "n_surv2")

# change site names for 2019 data
otts19$site <- as.character(otts19$site)
otts19[c(2,5,8,11,20), 2] <- c("Chusini Cove", "Goats mouth inlet", "Kaguk Cove", "Naukati Bay", 
                               "South Wadleigh Island")
otts19$site <- as.factor(otts19$site)


# graph the density of otters by site
ggplot(data = otts19, aes(x = reorder(site, -avg_density), y = avg_density, fill = year)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90))
# note that shakan - eel has low density, but was only sampled once instead of twice. 

### Habitat - 2019
# The data for 2019: is set up in a 4 sheet excel spreadsheets. quadrat_ID in hab_qrt is 
# unique for *every* quadrat that was done in summer 2019. Use the quadrat_ID to connect 
# to the other sheets (hab_lng and hab_wgt). Hab_lng has all the individual lengths for 
# all measured plants (in mm) (for eelgrass its 15 blades from each quadrat/site. For kelp 
# its up to 3 of each species collected from the site). Hab_wgt has the biomass weights for 
# individual species biomass by bag
# Has the quadrat level measurements... includes: density (eelgrass) and total_biomass (kelp)
hab_qrt$habitat <- as.factor(hab_qrt$habitat) 
levels(hab_qrt$habitat)[levels(hab_qrt$habitat)=="seagrass"] <- "eelgrass" # actual is eelgrass

hab_eel <- hab_qrt %>% # create dataframe for just eelgrass sites
  filter(habitat == "eelgrass") %>%
  mutate(eelgrass_shoots_msq = (as.numeric(density))*4) %>%
  mutate(flowering_shoots_msq = (as.numeric(flowering_shoots))*4) %>%
  dplyr::select(-date, -total_biomass, -density, -flowering_shoots) %>%
  mutate(date = ymd(YYYYMMDD)) %>%
  mutate(date_julian = yday(date)) %>%
  select(-YYYYMMDD)
hab_eel$year <- "2019"



# hab_lng (all measured in mm)
# use quadrat number info + habitat info to extract *just* the lengths for eelgrass sites
eel_qrts <- na.omit(ifelse(hab_qrt$habitat == "eelgrass", paste(hab_qrt$quadrat_ID), NA))
# subset the data by quadrat at an eelgrass site
lng_sub <- subset(hab_lng, quadrat_ID %in% eel_qrts)
levels(as.factor(lng_sub$species)) # only Z. marina, awesome! 

# want to average by quadrat and join avg/sd with hab_eel
lng_sub$length <- as.integer(lng_sub$length)
is.numeric(lng_sub$length)

lng_avg <- lng_sub %>% # calculate average by quadrat
  group_by(quadrat_ID) %>%
  summarise(length_avg = mean(length, na.rm = TRUE)) 
lng_sd <- lng_sub %>% # calculate sd by quadrat
  group_by(quadrat_ID) %>%
  summarise(length_sd = sd(length, na.rm = TRUE)) 
lng_sum <- full_join(lng_avg, lng_sd, by = "quadrat_ID")

# add canopy information to the habitat data
hab_eel2 <- full_join(hab_eel, lng_sum, by = "quadrat_ID")

# Combine with biomass conversions g/m2
hab_eel2 <- left_join(hab_eel2, biom[,2:3], by="quadrat_ID")


### Environmental - 2019
# The dates the environmental are going to be different from the seine
# dates sometimes by a few days, others by months... 
# rename the columns for env at surface
env19_srf <- env19 %>%
  filter(habitat == "eelgrass") %>%
  select(-time, -weather, -tide, -notes) %>%
  filter(depth == 1) %>%
  rename(dissolved_02_percent_surface = dissolved_O2_., 
         specific_conductivity_surface = Spec_cond, 
         temperature_C_surface = temperature, 
         salinity_ppt_surface = salinity, 
         dissolved_02_mg.l_surface = dissolved_O2_mgL)

# rename the columns for env at the bed level 
env19_bed <- env19 %>%
  filter(habitat == "eelgrass") %>%
  select(-time, -weather, -tide, -notes) %>%
  filter(depth == "bed") %>%
  rename(dissolved_02_percent_transect = dissolved_O2_., 
         specific_conductivity_transect = Spec_cond, 
         temperature_C_transect = temperature, 
         salinity_ppt_transect = salinity, 
         dissolved_02_mg.l_transect = dissolved_O2_mgL)


# combine into one dataframe for 2019 and clean up columns
env19_all <- full_join(env19_srf, env19_bed, by = c("site", "date", "YYYYMMDD")) %>%
  mutate(date = ymd(YYYYMMDD)) %>%
  mutate(date_julian = yday (date)) %>%
  select(-YYYYMMDD, -depth.x, -depth.y, -habitat.x, -habitat.y)
env19_all$year <- "2019"
env19_all$site <- as.character(env19_all$site)
env19_all$date <- as.character(env19_all$date)
env19_all[1,1] <- "Shakan - eel"
env19_all <- rbind(env19_all, c("North Pass", "NA", "NA", "NA", "NA", "NA", "NA", 
                                "NA", "NA", "NA", "NA", "NA", "NA", "2019"))
env19_all$site <- as.factor(env19_all$site)
env19_all$date <- as.Date(env19_all$date)
env19_all$year <- as.character(env19_all$year)
env19_all[,3:13] <- sapply(env19_all[,3:13], as.numeric)


# add 0 for where we seined but did not catch all salmon
sal19.0 <- sal19 %>%
  complete(site, sp_code, fill = list(abundance = 0)) %>%
  group_by(site, sp_code, YYYYMMDD) %>%
  summarise(count = sum(abundance)) 

date19 <- sal19 %>%
  select(site, YYYYMMDD) %>%
  distinct()

salmon19 <- sal19.0 %>%
  left_join(date19, by = "site") %>%
  select(-YYYYMMDD.x) %>%
  mutate(date = ymd(YYYYMMDD.y)) %>%
  mutate(date_julian = yday(date))
salmon19$year <- "2019"





# Dataframes ready for analysis:
# Habitat information (shoot/flowering density m2 by *quadrat*)
hab_red <- hab_all %>%
  select(-notes) %>%
  group_by(site, year) %>%
  mutate(avg_shoot = mean(eelgrass_shoots_msq)) %>%
  mutate(sd_shoot = sd(eelgrass_shoots_msq)) %>%
  mutate(avg_flowering = mean(flowering_shoots_msq)) %>%
  mutate(sd_flowering = sd(flowering_shoots_msq)) %>%
  select(site, year, date_julian, date, avg_shoot, sd_shoot, avg_flowering, sd_flowering) %>%
  distinct()

# SUMMED abundance by different species of salmon at all sites from 2017 and 2019 including zeros for all species found that year. (zero - inflated)
str(salmon)
salmon.wd <- dcast(data = salmon, site + year + date_julian  ~ sp_code, value.var = "count", fill = "0")
sal_all <- salmon.wd[-c(2,5,12,17,27),] # remove kelp sites
sal <- left_join(sites, sal_all, by = c("site", "year"))

sal[,6:10][is.na(sal[6:10])] <- 0
sal <- sal[,-5]

# environmental data from both 2017 and 2019 at two depths: surface and transect
# some missing data points (i.e. North Pass  2019)
env_all <- env_all %>%
  select(-date)

# otter average and sd density by site for all years
str(otts_all)

dat <- full_join(otts_all, env_all)
dat2 <- full_join(dat, hab_red, by = c("site", "year"))
dat2 <- dat2[,-c(19:21)] # drop dates associated with hab and environmental
dat3 <- left_join(dat2, sal, by = c("site", "year")) 
# dat3 <- dat3[-42,] # remove mistake in data

# this is a *complete* dataset of salmon abundance and explanatory variables. 
str(dat3)





# Maps! 
library(dplyr)
library(tidyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(raster)
library(gdistance)
library(spatstat)
library(leaflet)
library(DT)
library(sf)


# for 2019
sites19 <- readOGR(dsn = "../APECS Master repo/ALL_DATA/spatial_data", layer = "Otter_survey_sites_2019_UTM_10-2-19")
# for 2017
sites17 <- readOGR(dsn = "../APECS Master repo/ALL_DATA/spatial_data", layer = "Otter_Survey_Sites_2017_All_UTM")

sites19.eg <- subset(sites19, site_type %in% "eelgrass") 
sites17.eg <- subset(sites17, site_type %in% "Eelgrass")

## Sea otters that where observed ##
# for 2019
otts19 <- readOGR(dsn = "../APECS Master repo/ALL_DATA/spatial_data", 
                  layer = "so_survey_2019_UTM_10-2-19")

# for 2017: 
otts17 <- readOGR(dsn = "../APECS Master repo/ALL_DATA/spatial_data", layer = "Otter_Survey_All_10Jan18_UTM")

otts17@data$n_otters <- as.numeric(as.character(otts17@data$n_otters)) # data is imported as a factor
otts19@data$n_otters <- as.numeric(as.character(otts19@data$n_otters))
## Prince of Wales Water Polygon
h2o.utm <- readOGR(dsn = "../APECS Master repo/ALL_DATA/spatial_data", layer = "POW_water_UTM")
h2o.latlong <- spTransform(h2o.utm, CRS("+init=epsg:4326"))

map <- leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = h2o.latlong, stroke = NA, fillColor = "blue", fillOpacity = 0.8, group = "POW") %>% 
  addMarkers(data = sites19.eg, ~longitude, ~latitude, label = ~site) %>% 
  addMarkers(data = sites17.eg, ~longitude, ~latitude, label = ~place_name) %>%
  addCircleMarkers(data = otts19, ~longitude, ~latitude, color = "red", stroke = FALSE, radius = 5, fillOpacity = ~(n_otters/max(n_otters))) %>%
  addCircleMarkers(data = otts17, ~longitude, ~latitude, color = "red", stroke = FALSE, radius = 5, fillOpacity = ~(n_otters/max(n_otters)))


saveWidget(widget=map,
           file="Sites_otter_survey.html", selfcontained = TRUE)


