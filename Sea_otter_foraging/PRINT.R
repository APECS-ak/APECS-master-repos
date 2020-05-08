#### FILE FOR PRINTING THINGS ####

library(ggplot2)
library(dplyr) 
library(ggthemes)
library(ggpubr)
library(cowplot)
library(gridExtra) # making multi pane graphs
library(grid) # making multi pane graphs

#Chapter 2 - FIGURE xx - Seasonal Enegery
#data from macronutrients.rmd
a <- ggplot(data= means , aes(y=kcal.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = kcal.m-kcal.sd, ymax = kcal.m+kcal.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20")) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18)) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="Kcal (per dry gram)", tag = "A") + theme_few() +
  theme(legend.position = "none")

b <- ggplot(data= means , aes(y=lipid.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = lipid.m-lipid.sd, ymax = lipid.m+lipid.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20")) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18)) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="% Lipid (per dry gram)", tag = "B") + theme_few() +
  theme(legend.position = "none")

c <- ggplot(data= means , aes(y=protein.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = protein.m-protein.sd, ymax = protein.m+protein.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20")) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18)) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="% Protein (per dry gram)", tag = "C") + theme_few()

#function for legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#create legend
legend <- get_legend(c)

#remove legend from c
c <- c + theme(legend.position = "none")

#create a blank plot
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

#make grid
fig2 <- grid.arrange(a, blankPlot, b, legend, c, blankPlot, ncol=2, nrow=3, widths=c(4, 1), heights=c(6,6,6))

save_plot("Figure2.png", fig2, base_height = 10, base_width = 6)

#Moisture and Ash - not using
d <- ggplot(data= means , aes(y=moisture.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = moisture.m-moisture.sd, ymax = moisture.m+moisture.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20")) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18)) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="% Moisture") + theme_few() 

e <- ggplot(data= means , aes(y=ash.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = ash.m-ash.sd, ymax = ash.m+ash.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20")) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18)) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="% Ash") + theme_few()




#Figure xx, Proportion of lipid and protein
#data from macronutrients.rmd

ggplot(data = graph.all, aes(x=PreyCat, y=mean, fill = macro)) + theme_few() +
  geom_bar(stat="identity", position = "fill", color = "black") +
  geom_errorbar(data = filter(graph.all, macro == "protein"), 
                aes(ymin = mean-sd, ymax = mean+sd), width=.2, color = "white") +
  scale_fill_manual(values= c("black", "gray"), name="Macronutrient") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x= "", y="Percent of total energy") +
  theme(axis.text = element_text(size=14), legend.text = element_text(size = 12))

ggsave("Figure3.png", device = "png", path = "Bombing/", width = 9, 
       height = 6, units = "in", dpi = 300)

#Figure 2, Proportions for year and zone
#load data from biomass.rmd

a <- biomass %>%
  drop_na(Year.since, Proportion) %>%
  filter(Prey.Cat == "Clam" | Prey.Cat == "Crab" | Prey.Cat == "Cucumber" | Prey.Cat == "Snail" | 
           Prey.Cat == "Urchin") %>%
  ggplot(aes(x=Year.since, y=Proportion, fill = Prey.Cat)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "Proportion of diet (by biomass)", x=NULL, tag = "A") +
  scale_fill_brewer(palette = "RdYlBu", name= "Prey Catagory", direction = -1) +
  theme_few()+
  theme(axis.text = element_text(size=14), legend.position = "right", axis.title = element_text(size = 14))


b <- biomass %>%
  drop_na(Zone, Proportion) %>%
  filter(Prey.Cat == "Clam" | Prey.Cat == "Crab" | Prey.Cat == "Cucumber" | Prey.Cat == "Snail" | 
           Prey.Cat == "Urchin") %>%
  ggplot(aes(x=Zone, y=Proportion, fill = Prey.Cat)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "Proportion of diet (by biomass)", x=NULL, tag = "B") +
  scale_fill_brewer(palette = "RdYlBu", name= "Prey Catagory", direction = -1) +
  scale_x_discrete(labels=c("ZONE 1" = "1", "ZONE 2" = "2", "ZONE 3" = "3", "ZONE 4" = "4", 
                            "ZONE 5" = "5", "ZONE 6" = "6", "ZONE 7" = "7")) +
  theme_few() +
  theme(axis.text = element_text(size=14), legend.position = "none", 
        axis.title = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 1))

g <- grid.arrange(a, b, nrow = 2, widths= 7, heights = c(2, 2) )

ggsave("Year_and_zone.png", g, device = "png", path = "Visual/", width = 7, 
       height = 9, units = "in", dpi = 300)

#FIGURE 3
#Sex and status
#Same as 2. 

b <- biomass %>%
  drop_na(Status, Proportion) %>%
  filter(Prey.Cat == "Clam" | Prey.Cat == "Crab" | Prey.Cat == "Cucumber" | Prey.Cat == "Snail" | 
           Prey.Cat == "Urchin") %>%
  ggplot(aes(x=Status, y=Proportion, fill = Prey.Cat)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "Proportion of diet (by biomass)", x=NULL, tag= "B") +
  scale_fill_brewer(palette = "RdYlBu", name= "Prey Catagory", direction = -1) +
  scale_x_discrete(labels=c("Female_no_pup" = "Female, no pup", 
                            "Female_w_pup" = "Female, with pup", "Male" = "Male"))+
  theme_few() +
  theme(legend.position="none", axis.text = element_text(size=14), axis.title = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 1))

a <- biomass %>%
  drop_na(Age, Proportion) %>%
  filter(Prey.Cat == "Clam" | Prey.Cat == "Crab" | Prey.Cat == "Cucumber" | Prey.Cat == "Snail" | 
           Prey.Cat == "Urchin") %>%
  ggplot(aes(x=Age, y=Proportion, fill = Prey.Cat)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "Proportion of diet (by biomass)", x=NULL, tag= "A") +
  scale_fill_brewer(palette = "RdYlBu", name= "Prey Catagory", direction = -1) +
  theme_few() +
  theme(legend.position="right", axis.text = element_text(size=14), axis.title = element_text(size = 14))

g <- grid.arrange(a, b, nrow = 2, heights = c(2, 2), widths = 7)

ggsave("sex_status.png", g, device = "png", path = "Visual/", width = 7, 
       height = 9, units = "in", dpi = 300)
#########################################################################################
#########################################################################################

#Chapter 1 - FIGURE 4
#Carbon and nitrogen for site and season
#data from LME.Rmd
A <- ggplot(data=whisker) +
  geom_boxplot(aes(x=Season, y=C, fill=Site)) +
  labs(y=expression(paste(delta^13, "C" )), x = NULL, tag = "A") +
  scale_fill_brewer(palette = "Greys") +
  theme_few() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.title.y = element_text(size = 16), 
        axis.text.y = element_text(size = 14))

B <- ggplot(data=whisker) +
  geom_boxplot(aes(x=Season, y=N, fill=Site)) +
  labs(y=expression(paste(delta^15, "N" )), tag = "B") +
  scale_fill_brewer(palette = "Greys") +
  theme_few() +
  theme(legend.position="bottom", axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16))

g <- grid.arrange(A, B, nrow = 2, heights = c(1.5, 2))
#g <- arrangeGrob(A, B, nrow=2) #generates g
ggsave("CN_season_site.png", g, device = "png", path = "SI/", width = 9, 
       height = 6, units = "in", dpi = 300)



# Capter 1 - FIGURE 2
#Diet Proportion
# data comes from biomass.Rmd

biomass %>%
  drop_na(Season, Proportion) %>%
  filter(Prey.Cat == "Clam" | Prey.Cat == "Crab" | Prey.Cat == "Cucumber" | Prey.Cat == "Snail" | 
           Prey.Cat == "Mussel" | Prey.Cat == "Urchin") %>%
  ggplot(aes(x=Prey.Cat, y=Proportion, fill = Season)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "Proportion of diet (by biomass)", x=NULL) +
  scale_fill_grey() +
  theme_few() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16))


ggsave("prop_season_dive.png", device = "png", path = "Visual/", width = 8, 
       height = 7, units = "in", dpi = 300)


## Chapter 1 - FIGURE 3
#BIPLOT
#data from SI.R
si.mean <-read.csv("SI/si.mean_TDF.csv")
#whisker from above

ggplot() +
  geom_point(data=si.mean, aes(x=Cmean, y=Nmean), size = 3) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= si.mean, aes(x= Cmean, y= Nmean, ymin = Nmean-Nsd, ymax = Nmean+Nsd, linetype= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x= Cmean, y=Nmean, xmin = Cmean-Csd,xmax = Cmean+Csd, linetype= PreyCat), height=0) +
  geom_point(data=whisker, aes(x=C, y=N, shape=Site, color=Season), size=2)+
  geom_text(data= si.mean, aes(x= Cmean, y=Nmean, label=PreyCat),hjust=-0.2, vjust=1.8, size = 6) +
  theme_few() +
  labs(colour = "Season", shape = "Site") +
  guides(linetype=FALSE) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16, face = "bold"))

ggsave("biplot.png", device = "png", path = "SI/", width = 9, 
       height = 6, units = "in", dpi = 300)


# Chapter 1 - FIGURE 5
#Mixing model
## data from SI_Mixing.R
all <- read.csv("SI/all_mixing.csv")
ggplot(aes(y = value, x = source, fill = source), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  facet_grid(Season~Site) +
  theme( axis.title=element_text(size=16), legend.position = "none", 
         axis.text.x = element_text(angle = -45, hjust=0, size =12), 
         axis.text.y = element_text(size = 12),
         strip.text=element_text(size = 12, face="bold"))

ggsave("mixing_sites_grid.png", device = "png", path = "SI/", width = 9, 
       height = 7, units = "in", dpi = 300)

#Chapter 1 - FIGURE 6
#Individual otters
## data from SI.R

a <- ggplot(data=filter(whisker, OtterID=="163520")) +   theme_few() +
  geom_line(aes(x=distance, y=N)) +
  geom_point(aes(x=distance, y=N)) +
  geom_line(aes(x=distance, y=C+25), linetype = "dashed") +
  geom_point(aes(x=distance, y=C+25)) +
  labs(x= NULL, y=expression(paste(delta^15, "N (\u2030)" )), tag = "A")  +
  scale_y_continuous(sec.axis = sec_axis(~.-25, name = NULL), 
                     breaks= c(9, 11, 13, 15), limits = c(8.5,15)) +
  theme(axis.title = element_text(size=18, face = "bold"), 
        axis.text= element_text(size = 14, face = "bold"), 
        legend.position = "none", legend.title = element_blank(), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  ggtitle("520")

b <- ggplot(data=filter(whisker, OtterID=="163521")) +   theme_few() +
  geom_line(aes(x=distance, y=N)) + 
  geom_point(aes(x=distance, y=N)) +
  geom_line(aes(x=distance, y=C+25), linetype = "dashed") +
  geom_point(aes(x=distance, y=C+25)) +
  labs(x= NULL, y= NULL, tag = "B")  +
  scale_y_continuous(sec.axis = sec_axis(~.-25,
                                         name = expression(paste(delta^13, "C (\u2030)" ))), 
                     breaks= c(9, 11, 13, 15), limits = c(8.5,15)) +
  theme(axis.title = element_text(size=18, face = "bold"), 
        axis.text= element_text(size = 14, face = "bold"),
        legend.position = "none", legend.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  ggtitle("521")

c <- ggplot(data=filter(whisker,OtterID=="200752")) +   theme_few() +
  geom_line(aes(x=distance, y=N)) + 
  geom_point(aes(x=distance, y=N)) +
  geom_line(aes(x=distance, y=C+25), linetype = "dashed") +
  geom_point(aes(x=distance, y=C+25)) +
  labs(x= "Distance from root (cm)", y=expression(paste(delta^15, "N (\u2030)" )), tag = "C")  +
  scale_y_continuous(sec.axis = sec_axis(~.-25, name = NULL), 
                     breaks= c(9, 11, 13, 15), limits = c(8.5,15)) +
  xlim(0,6) +
  theme(axis.title = element_text(size=18, face = "bold"), 
        axis.text= element_text(size = 14, face = "bold"), 
        legend.position = "none", legend.title = element_blank(), 
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  ggtitle("524")

d <-ggplot(data=filter(whisker, OtterID=="77287")) +   theme_few() +
  geom_line(aes(x=distance, y=N)) + 
  geom_point(aes(x=distance, y=N)) + 
  geom_line(aes(x=distance, y=C+25), linetype = "dashed") +
  geom_point(aes(x=distance, y=C+25)) +
  labs(x= "Distance from root (cm)", y=NULL, tag = "D")  +
  xlim(0,6) +
  scale_y_continuous(sec.axis = sec_axis(~.-25, 
                                         name = expression(paste(delta^13, "C (\u2030)" ))), 
                     breaks= c(9, 11, 13, 15), limits = c(8.5,15)) +
  theme(axis.title = element_text(size=18, face = "bold"), 
        axis.text= element_text(size = 14, face = "bold"), 
        legend.position = "none", legend.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  ggtitle("287")

gr <- arrangeGrob(a, b, c, d, nrow=2) 
ggsave("INDIV.png", gr, device = "png", path = "SI/", width = 9, 
       height = 7.5, units = "in", dpi = 300)


# Chapter 1 - figure 8 
# Prey Seasonal changes 
# Data from LME_prey.Rmd

A <- si.prey %>%
  filter(Site == "Craig" | Site == "Soda Bay") %>%
  ggplot(aes(x=Season, y=Cnorm, fill=Site)) +
  geom_boxplot() +   theme_few() +
  labs(y=expression(paste(delta^13, "C" )), x= NULL, tag = "A") +
  scale_fill_grey() +
  facet_wrap(vars(PreyCat), labeller = labeller(PreyCat = c("Clam" = "Clam", "Crab" = "Crab", 
                                                            "Cucumber.Snail" = "Cucumber and Snail", 
                                                            "Mussel" = "Mussel", "Urchin" = "Urchin"))) +
  theme(legend.position = "none", axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), strip.text = element_text(size = 14))

B <- si.prey %>%
  filter(Site == "Craig" | Site == "Soda Bay") %>%
  ggplot(aes(x=Season, y=N, fill=Site)) +
  geom_boxplot() +   theme_few() +
  labs(y=expression(paste(delta^15, "N" )), x=NULL, tag = "B") +
  scale_fill_grey() +
  facet_wrap(vars(PreyCat),labeller = labeller(PreyCat = c("Clam" = "Clam", "Crab" = "Crab", 
                                                           "Cucumber.Snail" = "Cucumber and Snail", 
                                                           "Mussel" = "Mussel", "Urchin" = "Urchin"))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), strip.text = element_text(size = 14), 
        legend.title = element_text(size = 16), legend.text = element_text(size = 14))

g <- grid.arrange(A, B, nrow = 2, heights = c(1.5, 2))
ggsave("CN_Prey_season_site.png", g, device = "png", path = "SI/", width = 9, 
       height = 8, units = "in", dpi = 300)


