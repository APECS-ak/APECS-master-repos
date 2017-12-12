#####----- Data Exploration, Prep, and Summaries -----#####
## Originally created 10.19.2017 - Wendel Raymond ##

#####----- Libraries -----#####
library(dplyr)
library(ggplot2)

theme_set(theme_classic())

#####----- Load Data -----######
# Eelgrass Transect
eg.tran <- read.csv("E:/wraymond2/My Documents/Graduate School/Eelgrass/Data/2017 Field Season/Eelgrass Transect/Eelgrass_Transect_data_2017_19Oct2017_WR.csv", header = TRUE)

# Eelgrass Biometrics (lab data)
eg.bio <- read.csv("E:/wraymond2/My Documents/Graduate School/Eelgrass/Data/2017 Field Season/Eelgrass Biometrics/Eelgrass_Biometrics_data_2017_25Oct2017_WR.csv", header = TRUE)

# Eelgrass Site
eg.site <- read.csv("E:/wraymond2/My Documents/Graduate School/Eelgrass/Data/2017 Field Season/Site/Eelgrass_Site_data_2017_20Oct2017_WR.csv", header = TRUE)

# Sea Otter Impact Index
so.index <- read.csv("E:/wraymond2/My Documents/Graduate School/Eelgrass/Data/2017 Field Season/Sea Otter Impact Index/Sea_Otter_Impact_Index_2017_WR.csv", header = TRUE)

#####----- Data Checking -----#####
### Eelgrass Transect ###
str(eg.tran)
levels(eg.tran$site)
levels(eg.tran$primary_obeserver)
levels(eg.tran$secondary_observer)
range(eg.tran$quadrat)
range(eg.tran$macroalgae_cover_0.25msq)
range(eg.tran$eelgrass_shoots_0.25msq)
hist(eg.tran$eelgrass_shoots_0.25msq)
hist(eg.tran$macroalgae_cover_0.25msq)
hist(eg.tran$diatom_cover_0.25msq)

### Eelgrass Biometrics ###
str(eg.bio)
range(eg.bio$rhi_length, na.rm = TRUE)
range(eg.bio$node1, na.rm = TRUE)
hist(eg.bio$node1)
hist(eg.bio$leaf_width10)

#####----- Data Clean up and the like -----#####
### Julian Day ###
## Biometrics ##
eg.bio$collection_date <- as.Date(eg.bio$collection_date, format = "%m/%d/%Y")
eg.bio$collection_julian <- format(eg.bio$collection_date, "%j")

## Transect ##
eg.tran$date <- as.Date(eg.tran$date, format = "%m/%d/%Y")
eg.tran$date_julian <- format(eg.tran$date, "%j")

## Site ##
eg.site$date <- as.Date(eg.site$date, format = "%m/%d/%Y")
eg.site$date_julian <- format(eg.site$date, "%j")

### Numeric to Factors ###
## Biometrics ##
eg.bio$quadrat <- as.factor(eg.bio$quadrat)
eg.bio$plant <- as.factor(eg.bio$plant)

## Transect ##
eg.tran$quadrat <- as.factor(eg.tran$quadrat)

### Changing a few column names so its not confusing ###
## Start and End times ##
# Site
colnames(eg.site)[6] <- "site_start_time"
colnames(eg.site)[7] <- "site_end_time"

# Transect
colnames(eg.tran)[6] <- "transect_start_time"
colnames(eg.tran)[7] <- "transect_end_time"

#####----- Calculations -----#####
### Effective Leaf Area ###
eg.bio$leaf_area1 <- (eg.bio$leaf_length1 * eg.bio$leaf_width1)
eg.bio$leaf_area2 <- (eg.bio$leaf_length2 * eg.bio$leaf_width2)
eg.bio$leaf_area3 <- (eg.bio$leaf_length3 * eg.bio$leaf_width3)
eg.bio$leaf_area4 <- (eg.bio$leaf_length4 * eg.bio$leaf_width4)
eg.bio$leaf_area5 <- (eg.bio$leaf_length5 * eg.bio$leaf_width5)
eg.bio$leaf_area6 <- (eg.bio$leaf_length6 * eg.bio$leaf_width6)
eg.bio$leaf_area7 <- (eg.bio$leaf_length7 * eg.bio$leaf_width7)
eg.bio$leaf_area8 <- (eg.bio$leaf_length8 * eg.bio$leaf_width8)
eg.bio$leaf_area9 <- (eg.bio$leaf_length9 * eg.bio$leaf_width9)
eg.bio$leaf_area10 <- (eg.bio$leaf_length10 * eg.bio$leaf_width10)

eg.bio$leaf_areaT <- rowSums(eg.bio[, 73:82], na.rm = TRUE)

### Epiphytes, Rhizomes, and Shoots Mass ###
## Epiphyte Mass ##
eg.bio$epi_mass <- (eg.bio$pad_epihpyte_mass_g - eg.bio$pad_mass_g)

## Rhizome Mass ##
eg.bio$rhi_mass <- ((eg.bio$rhi_foil_dw_g - eg.bio$rhi_foil) * (5 / eg.bio$rhi_length))

## Shoot Mass ##
eg.bio$shoot_mass <- (eg.bio$shoot_foil_dw - eg.bio$shoot_foil)

## Exess Shoot Mass ##
eg.bio$exshoot_mass <- (eg.bio$xs_shoot_foil_dw - eg.bio$xs_shoot_foil)

## Exess Epiphyte Mass ##
eg.bio$exepi_mass <- (eg.bio$xs_epiphyte_pad_mass_g - eg.bio$xs_pad_mass_g)

### Grazer Mass ###
## Isopod (Idothea rascata) Mass - bulk ##
eg.bio$iso_mass <- (eg.bio$iso_foil_dw - eg.bio$iso_foil)

## Gammarid Amphipod Mass - bulk ##
eg.bio$gamm_mass <- (eg.bio$gamm_amph_foil_dw - eg.bio$gamm_amph_foil)

## Caprellid Amphipod Mass - bulk ##
eg.bio$caprel_mass <- (eg.bio$caprel_foil_dw - eg.bio$caprel_foil)

## Limpet Mass - bulk ##
eg.bio$limp_mass <- (eg.bio$limpet_foil_dw - eg.bio$limpet_foil)

## Other Gastropod Mass - bulk ##
eg.bio$gast_mass <- (eg.bio$gastropod_foil_dw - eg.bio$gastropod_foil)

## Crab Mass ##
eg.bio$crab_mass <- (eg.bio$crab_foil_dw - eg.bio$crab_foil)

## Fill absent grazers with 0s so that cacluations below will work ##
eg.bio[,89:94][is.na(eg.bio[,89:94])] <- 0

## Crustacean Grazers Mass ##
eg.bio$crust_mass <- (eg.bio$iso_mass + eg.bio$gamm_mass +  eg.bio$caprel_mass + eg.bio$crab_mass)

## Gastropod Grazers Mass ##
eg.bio$gastro_mass <- (eg.bio$limp_mass + eg.bio$gast_mass)

## Total Grazer Mass ##
eg.bio$graz_massT <- rowSums(eg.bio[, 89:94], na.rm = TRUE)

#####----- Quadrat level Summaries -----#####
## standard error of the mean fucntion to use later ##
st.er <- function(x, na.rm = TRUE) {
  stopifnot(is.numeric(x))
  return(sd(x, na.rm = na.rm)/sqrt(length(x)))
}

## Convert Transect data to per meter ##
eg.tran$eelgrass_shoots_msq <- (4 * eg.tran$eelgrass_shoots_0.25msq)
eg.tran$flowering_shoots_msq <- (4 * eg.tran$flowering_shoots_0.25msq)
eg.tran$macroalgae_cover_msq <- (1 * eg.tran$macroalgae_cover_0.25msq)
eg.tran$diatom_cover_msq <- (1 * eg.tran$diatom_cover_0.25msq)

## Biometrics data summarized by site and quadrat - shoots and rhizomes ##
quad.bio <- data.frame(
  eg.bio %>% 
    group_by(site, quadrat) %>% 
    summarise(s_shoot_area = mean(leaf_areaT, na.rm = TRUE),
              s_epi_mass = mean(epi_mass, na.rm = TRUE),
              s_shoot_mass = mean(shoot_mass, na.rm = TRUE),
              s_rhi_mass = mean(rhi_mass, na.rm = TRUE),
              s_node1 = mean(node1, na.rm = TRUE),
              s_node2 = mean(node2, na.rm = TRUE),
              s_node3 = mean(node3, na.rm = TRUE),
              s_node4 = mean(node4, na.rm = TRUE),
              s_node5 = mean(node5, na.rm = TRUE),
              s_exshoot_mass = mean(exshoot_mass, na.rm = TRUE),
              s_exepi_mass = mean(exepi_mass, na.rm = TRUE))
)

# Extract Grazer biomasses
quad.graz <- data.frame(eg.bio[eg.bio$plant == 1,]) # extract just plant 1 which has all the grazer data
quad.graz <- quad.graz[order(quad.graz[,1], quad.graz[,4]),] # order so that it matches everything else
quad.graz <- data.frame(quad.graz[,c(1, 4, 89:97)]) # reduce to get grazer data only
quad.graz[is.na(quad.graz)] <- 0 # turn NAs to 0s becasue if we sampled everything so if it was not there its a 0

## Combined Transect and Biometrics data to give site summaries ##
site.quad <- merge(eg.site[,c(1, 2)], eg.tran, by = "site")
site.quad <- merge(site.quad, quad.bio, by = c("site", "quadrat"))
site.quad <- merge(site.quad, quad.graz, by = c("site", "quadrat"))

### Quadrat level caclulations ###
## Aboveground Biomass ##
site.quad$ag_mass <- (site.quad$eelgrass_shoots_msq * site.quad$s_shoot_mass)

## Below Ground Biomass ##
site.quad$bg_mass <- (site.quad$eelgrass_shoots_msq * site.quad$s_rhi_mass)

## Epiphytes per eeglrass area ##
site.quad$epimass_shootarea <- (site.quad$s_epi_mass / site.quad$s_shoot_area)

## Epiphytes per eelgrass mass ##
site.quad$epimass_shootmass <- (site.quad$s_epi_mass / site.quad$s_shoot_mass)

## Grazer Mass per Quadrat ##
# this is just equal to graz_massT

## Grazer mass per eelgrass mass ##
site.quad$grazmass_shootmass <- (site.quad$graz_massT / (site.quad$s_shoot_mass + site.quad$s_exshoot_mass))

## Crustacean mass per eelgrass mass ##
site.quad$crustmass_shootmass <- (site.quad$crust_mass / (site.quad$s_shoot_mass + site.quad$s_exshoot_mass))

## Gastropod mass per eelgrass mass ##
site.quad$gastromass_shootmass <- (site.quad$gastro_mass / (site.quad$s_shoot_mass + site.quad$s_exshoot_mass)) 

#####----- Master Compile and Calculations -----#####
### Site Level Data ###
tran.dat <- data.frame(rbind(eg.tran[1, c(1, 4:8)], eg.tran[9, c(1, 4:8)], eg.tran[17, c(1, 4:8)], eg.tran[25, c(1, 4:8)], eg.tran[33, c(1, 4:8)], eg.tran[41, c(1, 4:8)], 
                             eg.tran[49, c(1, 4:8)],eg.tran[57, c(1, 4:8)], eg.tran[65, c(1, 4:8)], eg.tran[73, c(1, 4:8)], eg.tran[81, c(1, 4:8)], eg.tran[89, c(1, 4:8)], 
                             eg.tran[97, c(1, 4:8)], eg.tran[105, c(1, 4:8)], eg.tran[113, c(1, 4:8)], eg.tran[121, c(1, 4:8)], eg.tran[129, c(1, 4:8)], eg.tran[137, c(1, 4:8)], 
                             eg.tran[145, c(1, 4:8)], eg.tran[153, c(1, 4:8)], eg.tran[161, c(1, 4:8)]))

### Master ###
dat <- data.frame(cbind(eg.site[, c(1:3, 32, 4:9)], tran.dat[, 2:5], so.index[, 6:13], eg.site[, 14:26]))

## Transect data summarized by site ##
site.tran <- data.frame(
  eg.tran %>% 
    group_by(site) %>% 
    summarise(shoot_dens = (mean(eelgrass_shoots_0.25msq * 4)),
              shoot_dens_se = (st.er(eelgrass_shoots_0.25msq * 4)),
              macro_dens = (mean(macroalgae_cover_0.25msq * 1)),
              macro_dens_se = (st.er(macroalgae_cover_0.25msq * 1)),
              diatom_dens = (mean(diatom_cover_0.25msq * 1)),
              diatom_dens_se = (st.er(diatom_cover_0.25msq * 1)),
              flower_dens = (mean(flowering_shoots_0.25msq * 4)),
              flower_dens_se = (st.er(flowering_shoots_0.25msq * 4)))
)

## Biometrics data summarized by site ##
site.bio <- data.frame(
  site.quad %>% 
    group_by(site) %>% 
    summarise(shoot_area = (mean(s_shoot_area, na.rm = TRUE)),
              shoot_area_se = (st.er(s_shoot_area)),
              epi_mass = (mean(s_epi_mass, na.rm = TRUE)),
              epi_mass_se = (st.er(s_epi_mass)),
              shoot_mass = (mean(s_shoot_mass, na.rm = TRUE)),
              shoot_mass_se = (st.er(s_shoot_mass)),
              rhi_mass = (mean(s_rhi_mass, na.rm = TRUE)),
              rhi_mass_se = (st.er(s_rhi_mass)),
              node1 = (mean(s_node1, na.rm = TRUE)),
              node1_se = (st.er(s_node1)),
              node2 = (mean(s_node2, na.rm = TRUE)),
              node2_se = (st.er(s_node2)),
              node3 = (mean(s_node3, na.rm = TRUE)),
              node3_se = (st.er(s_node3)),
              node4 = (mean(s_node4, na.rm = TRUE)),
              node4_se = (st.er(s_node4)),
              node5 = (mean(s_node5, na.rm = TRUE)),
              node5_se = (st.er(s_node5)),
              exshoot_mass = (mean(s_exshoot_mass, na.rm = TRUE)),
              exshoot_mass_se = (st.er(s_exshoot_mass)),
              exepi_mass = (mean(s_exepi_mass, na.rm = TRUE)),
              exepi_mass_se = (st.er(s_exepi_mass)),
              abvgnd_mass = (mean(ag_mass, na.rm = TRUE)),
              abvgnd_mass_se = (st.er(ag_mass)),
              blwgnd_mass = (mean(bg_mass, na.rm = TRUE)),
              blwgnd_mass_se = (st.er(bg_mass)),
              epiphmass_shootarea = (mean(epimass_shootarea, na.rm = TRUE)),
              epiphmass_shootarea_se = (st.er(epimass_shootarea)),
              epiphmass_shootmass = (mean(epimass_shootmass, na.rm = TRUE)),
              epiphmass_shootmass_se = (st.er(epimass_shootmass)),
              grazer_mass = (mean(graz_massT, na.rm = TRUE)),
              grazer_mass_se = (st.er(graz_massT)),
              crus_mass = (mean(crust_mass, na.rm = TRUE)),
              crus_mass_se = (st.er(crust_mass)),
              gast_mass = (mean(gastro_mass, na.rm = TRUE)),
              gast_mass_se = (st.er(gastro_mass)),
              grazermass_shootmass = (mean(grazmass_shootmass, na.rm = TRUE)),
              grazermass_shootmass_se = (st.er(grazmass_shootmass)),
              crusmass_shootmass = (mean(crustmass_shootmass, na.rm = TRUE)),
              crusmass_shootmass_se = (st.er(crustmass_shootmass)),
              gastmass_shootmass = (mean(gastromass_shootmass, na.rm = TRUE)),
              gastmass_shootmass_se = (st.er(gastromass_shootmass)))
)

### Merge Transect and Biometrics data to Master dat ###
## Transect ##
dat <- merge(dat, site.tran, by = "site")

## Biometrics ##
dat <- merge(dat, site.bio, by = "site")

### Clean up ###
dat$specific_conductivity_transect <- as.numeric(dat$specific_conductivity_transect)

#####----- Export -----#####
write.csv(dat, "Eelgrass_and_Grazer_Data_2017.csv", row.names = FALSE)
