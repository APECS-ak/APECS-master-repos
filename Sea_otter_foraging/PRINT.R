#### FILE FOR PRINTING THINGS ####

library(ggplot2)
library(dplyr) 
library(ggthemes)
library(ggpubr)
library(cowplot)
library(scales)
library(gridExtra) # making multi pane graphs
library(grid) # making multi pane graphs

#Run these first!
prey <- c("Clam", "Crab", "Cucumber/Snail", "Mussel", "Urchin")
sites <- c("Shiaku Inlet", "Sukkwan Strait", "Tonowek Narrows")
labels <- c("Clam", "Crab", "Cucumber", "Snail", "Urchin")

#Chapter 2 - FIGURE xx - Seasonal Enegery of prey catagories
#data from macronutrients.rmd
a <- ggplot(data= means, aes(y=kcal.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = kcal.m-kcal.sd, ymax = kcal.m+kcal.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20"), labels = labels) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18), labels = labels) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash"), labels = labels) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="Kcal (dry gram)", tag = "A") + theme_few() +
  theme(legend.position = "none", axis.title = element_text(size = 18), axis.text = element_text(size = 16))

b <- ggplot(data= means , aes(y=lipid.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = lipid.m-lipid.sd, ymax = lipid.m+lipid.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20"), labels = labels) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18), labels = labels) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash"), labels = labels) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="% Lipid (dry gram)", tag = "B") + theme_few() +
  theme(legend.position = "none", axis.title = element_text(size = 18), axis.text = element_text(size = 16))

c <- ggplot(data= means , aes(y=protein.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = protein.m-protein.sd, ymax = protein.m+protein.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20"), labels = labels) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18), labels = labels) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash"), labels = labels) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="% Protein (dry gram)", tag = "C") + theme_few() +
  theme(legend.text = element_text(size=18), legend.position = "bottom", legend.title = element_text(size = 18),
        axis.title = element_text(size = 20), axis.text = element_text(size = 16))


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
prey.plm <- grid.arrange(a, b, c, blankPlot, legend, blankPlot, ncol=3, nrow=2, widths=c(4, 4, 4), heights=c(4,1))

save_plot("prey_plm.png", prey.plm, base_height = 5, base_width = 12)

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



#Figure - Proportion of lipid and protein
#data from macronutrients.rmd

ggplot(data = graph.all, aes(x=PreyCat, y=mean, fill = macro)) + theme_few() +
  geom_bar(stat="identity",position = position_dodge(), color = "black") +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=0.2, 
                position = position_dodge(.9)) +
  scale_fill_grey(start = .3, end = .9, name="Macronutrient") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels = labels) +
  labs(x= "", y="Percent of total energy") +
  theme(axis.text = element_text(size=14), legend.text = 
          element_text(size=16), legend.title = element_text(size= 16), axis.title = element_text(size=16))

ggsave("percent_lp.png", device = "png",  width = 9, 
       height = 6, units = "in", dpi = 300)



## Figure 2 ## 
#Visual obs, Sex and Status
#load data from biomass.rmd

A <- biomass %>%
  drop_na(Year.since, Proportion) %>%
  filter(Prey.Cat == "Clam" | Prey.Cat == "Crab" | Prey.Cat == "Cucumber" | 
           Prey.Cat == "Snail" | Prey.Cat == "Urchin") %>%
  ggplot(aes(x=Year.since, y=Proportion, fill = Prey.Cat)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "Proportion of diet", x=NULL, tag = "A") +
  scale_fill_brewer(palette = "RdYlBu", name= "Prey Catagory", direction = -1) +
  theme_few()+
  theme(axis.text = element_text(size=14), legend.position = "none", axis.title = element_text(size = 14))

B <- biomass %>%
  drop_na(Age, Proportion) %>%
  filter(Prey.Cat == "Clam" | Prey.Cat == "Crab" | Prey.Cat == "Cucumber" | 
           Prey.Cat == "Snail" | Prey.Cat == "Urchin") %>%
  ggplot(aes(x=Age, y=Proportion, fill = Prey.Cat)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "Proportion of diet", x=NULL, tag= "B") +
  scale_fill_brewer(palette = "RdYlBu", name= "Prey Catagory", direction = -1) +
  theme_few() +
  theme(legend.position="right", axis.text = element_text(size=14), 
        axis.title = element_text(size = 14))

C <- biomass %>%
  drop_na(Status, Proportion) %>%
  filter(Prey.Cat == "Clam" | Prey.Cat == "Crab" | Prey.Cat == "Cucumber" | 
           Prey.Cat == "Snail" | Prey.Cat == "Urchin") %>%
  ggplot(aes(x=Status, y=Proportion, fill = Prey.Cat)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y= "Proportion of diet", x=NULL, tag= "C") +
  scale_fill_brewer(palette = "RdYlBu", name= "Prey Catagory", direction = -1) +
  scale_x_discrete(labels=c("Female_no_pup" = "Female w/o pup", 
                            "Female_w_pup" = "Female, w/ pup", "Male" = "Male"))+
  theme_few() +
  theme(legend.position="none", axis.text = element_text(size=14), 
        axis.title = element_text(size = 14)) +
  guides(fill = guide_legend(nrow = 1))

g <- grid.arrange(A,B,C, nrow = 3)

ggsave("visual_prop.png", g, device = "png", width = 7, 
       height = 9, units = "in", dpi = 300)


#Figure xx - Kcal/ wetmass by season with percent diet overlaid
#clam and cuc only
#data from Macronutrients.Rmd

a <- ggplot(data=clam.energy, aes(x=month, y=kcal.wet)) +
  geom_point(shape = 1) +
  labs(x= NULL, y="Kcal/gram (wet mass)", tag = "A") +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", 
               limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  scale_y_continuous(sec.axis = sec_axis(~ ./1.5, name = "Proportion of diet", 
                                         labels = percent_format(accuracy = 1))) +
  geom_smooth(color="black", linetype = 2, se = F) +
  geom_smooth(data=filter(si.mean, PreyCat =="Clam"), aes(x=month, y=mean*1.5), 
              color="red") +
  theme_few()+
  theme(legend.position = "none")

b <- ggplot(data=filter(c.energy, PreyCat == "Cucumber"), aes(x=month, y=kcal.wet)) +
  geom_point(shape = 1) +
  labs(x= NULL, y="Kcal/gram (wet mass)", tag= "B") +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", 
               limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  scale_y_continuous(sec.axis = sec_axis(~ ./1.8, name = "Proportion of diet", 
                                         labels = percent_format(accuracy = 1))) +
  geom_smooth(color="black", linetype = 2, se = F) +
  geom_smooth(data=filter(c.mean, PreyCat == "Cucumber"), aes(x=month, y=mean*1.8), 
              color="red") +
  theme_few()+
  theme(legend.position = "none")

c <- ggplot(data=filter(c.energy, PreyCat == "Cucumber"), aes(x=month, y=lipid_wet)) +
  geom_point(shape = 1) +
  labs(x= NULL, y="% Lipid (wet mass)") +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), 
               expand = c(0.1,0))+
  scale_y_continuous(sec.axis = sec_axis(~ ./3, name = "Proportion of diet", labels = percent_format(accuracy = 1))) +
  geom_smooth(color="black", linetype = 2, se = F) +
  geom_smooth(data=filter(c.mean, PreyCat == "Cucumber"), aes(x=month, y=mean*3), color="red") +
  theme_few()+
  theme(legend.position = "none")

g <- grid.arrange(a, b, c, nrow = 3, heights = c(3,3,3), ncol = 1, widths = 3)

ggsave("kcal_biomass.png", g, device = "png", width = 5, 
       height = 9, units = "in", dpi = 300)


## SUPER LONG CODE WARNING ##
# Figure 3 - Sea otter macronutrient consumption
#data from Macronutrients.Rmd

a <- ggplot(data = year_all, aes(x= Year.since, y=kcal_dry, fill = Year.since)) +
  geom_bar(stat="identity", color= "black") +
  theme_few() +
  scale_fill_brewer(palette = "Greens") +
  labs(y = "Kcal/g", x=NULL, tag= "A") +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

b <- ggplot(data = year_all, aes(x= Year.since, y=lip_dry, fill = Year.since)) +
  geom_bar(stat="identity", color= "black") +
  theme_few() +
  scale_fill_brewer(palette = "Greens") +
  labs(y = "Lipid", x=NULL, tag= " ") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

c <- ggplot(data = year_all, aes(x= Year.since, y=prot_dry, fill = Year.since)) +
  geom_bar(stat="identity", color= "black") +
  theme_few() +
  scale_fill_brewer(palette = "Greens") +
  labs(y = "Protein", x=NULL, tag= " ") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

recol <- grid.arrange(a,b,c,nrow=1, widths = c(2.9,3,3.1))

status_all$Status <- factor(status_all$Status,levels = c("Female_w_pup", "Female_no_pup", 
                                                  "Male"))
status.labels <- c("Female \nw/ pup", "Female \nw/o pup", "Male")

d <- ggplot(data = status_all, aes(x= Status, y=kcal_dry, fill = Status)) +
  geom_bar(stat="identity", color = "black") +
  theme_few() +
  scale_fill_brewer(palette = "Purples") +
  labs(y = "Kcal/g", x=NULL, tag= "C") +
  scale_x_discrete(labels= status.labels) +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

e <- ggplot(data = status_all, aes(x= Status, y=lip_dry, fill = Status)) +
  geom_bar(stat="identity", color = "black") +
  theme_few() +
  scale_fill_brewer(palette = "Purples") +
  labs(y = "Lipid", x=NULL, tag= " ") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels= status.labels) +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

f <- ggplot(data = status_all, aes(x= Status, y=prot_dry, fill = Status)) +
  geom_bar(stat="identity", color = "black") +
  theme_few() +
  scale_fill_brewer(palette = "Purples") +
  labs(y = "Protein", x=NULL, tag= " ") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels= status.labels) +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

status <- grid.arrange(d,e,f,nrow=1, widths = c(2.9,3,3.1))

age_all$Age <- factor(age_all$Age,levels = c("Juvenile", "Adult"))

g <- ggplot(data = age_all, aes(x= Age, y=kcal_dry, fill = Age)) +
  geom_bar(stat="identity", color = "black") +
  theme_few() +
  scale_fill_brewer(palette = "Oranges") +
  labs(y = "Kcal/g", x=NULL, tag= "B") +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

h <- ggplot(data = age_all, aes(x= Age, y=lip_dry, fill = Age)) +
  geom_bar(stat="identity", color = "black") +
  theme_few() +
  scale_fill_brewer(palette = "Oranges") +
  labs(y = "Lipid", x=NULL, tag= " ") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

i <- ggplot(data = age_all, aes(x= Age, y=prot_dry, fill = Age)) +
  geom_bar(stat="identity", color = "black") +
  theme_few() +
  scale_fill_brewer(palette = "Oranges") +
  labs(y = "Protein", x=NULL, tag= " ") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())


sex <- grid.arrange(g,h,i,nrow=1, widths = c(2.9,3,3.1))

j <- ggplot(data = season_all, aes(x= Season, y=kcal_dry, fill = Season)) +
  geom_bar(stat="identity", color = "black") +
  theme_few() +
  scale_fill_brewer(palette = "Blues") +
  labs(y = "Kcal/g", x=NULL, tag= "D") +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

k <- ggplot(data = season_all, aes(x= Season, y=lip_dry, fill = Season)) +
  geom_bar(stat="identity", color = "black") +
  theme_few() +
  scale_fill_brewer(palette = "Blues") +
  labs(y = "Lipid", x=NULL, tag= " ") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

l <- ggplot(data = season_all, aes(x= Season, y=prot_dry, fill = Season)) +
  geom_bar(stat="identity", color = "black") +
  theme_few() +
  scale_fill_brewer(palette = "Blues") +
  labs(y = "Protein", x=NULL, tag= " ") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(legend.position = "none", axis.title = element_text(size=16), 
        axis.text.y = element_text(size = 14), 
        axis.text.x = element_text(size = 14, angle= 45, hjust = 1),
        axis.ticks.x = element_blank())

season <- grid.arrange(j,k,l,nrow=1, widths = c(2.9,3,3.1))

macros <- grid.arrange(recol, sex, status, season, nrow=4)
ggsave("macros.png", plot = macros, device = "png", width = 9, 
       height = 12, dpi = 300 )

#########################################################################################
#########################################################################################

#Chapter 1 

#########################################################################################
#########################################################################################


#Carbon and nitrogen for site and season - Sea otter whiskers
#data from LME.Rmd
A <- ggplot(data=whisker) +
  geom_boxplot(aes(x=Season, y=C, fill=Site), color = "black") +
  labs(y=expression(paste(delta^13, "C (\u2030)" )), x = NULL, tag = "A") +
  scale_fill_grey(start=1, end=0.3) +
  theme_few() +
  theme(legend.position="none", axis.text.x = element_blank(), axis.title.y = element_text(size = 16), 
        axis.text.y = element_text(size = 14))

B <- ggplot(data=whisker) +
  geom_boxplot(aes(x=Season, y=N, fill=Site), color ="black") +
  labs(y=expression(paste(delta^15, "N (\u2030)" )), tag = "B") +
  scale_fill_grey(start=1, end=0.3, labels = sites) +
  theme_few() +
  theme(legend.position="bottom", axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16))

g <- grid.arrange(A, B, nrow = 2, heights = c(1.5, 2))
#g <- arrangeGrob(A, B, nrow=2) #generates g
ggsave("CN_season_site.png", g, device = "png", path = "SI/", width = 9, 
       height = 6, units = "in", dpi = 300)



## FIGURE 2 ##
#Diet Proportion bar graph 
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



# BIPLOT #
#Data from SI.R, whisker, and si.mean (346)
#Adding TDFs in graph (new as of 5/19/20)
#Make sure to run labels at top of script

ggplot() +   theme_few() +
  geom_point(data=whisker, aes(x=C, y=N, shape=Site, color=Season), size=2) +
  geom_point(data=si.mean, aes(x=Cmean+2, y=Nmean+2.8), size = 3) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= si.mean, aes(x= Cmean+2, y= Nmean+2.8, 
                                   ymin = Nmean-Nsd+2.8, ymax = Nmean+Nsd+2.8, 
                                   linetype= PreyCat), width=0) + 
  geom_errorbarh(data= si.mean, aes(x= Cmean+2, y=Nmean+2.8, 
                                    xmin = Cmean-Csd+2,xmax = Cmean+Csd+2, 
                                    linetype= PreyCat), height=0) +
  geom_text(data= si.mean, aes(x= Cmean+2, y=Nmean+2.8, 
                               label= prey),hjust=-0.2, vjust=1.8, size = 6) +
  labs(colour = "Season", shape = "Site") +
  guides(linetype=FALSE) +
  scale_shape_discrete(name = "Site", labels = sites) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), 
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16, face = "bold"))

ggsave("biplot.png", device = "png", path = "SI/", width = 9, 
       height = 6, units = "in", dpi = 300)



# Mixing model #
## data from SI_Mixing.R
all <- read.csv("SI/all_mixing.csv")
all$Season<-factor(all$Season , levels=c("Spring", "Summer", "Fall", "Winter"))

ggplot(aes(y = value, x = source, fill = source), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  scale_x_discrete(labels=c("Clam" = "Clam", "Crab" = "Crab", 
                            "Cucumber.Snail" = "Cucumber/Snail",
                            "Mussel" = "Mussel", "Urchin" = "Urchin")) +
  facet_grid(Season~Site, labeller = labeller(Site = c("Shinaku" = "Shinaku Inlet",
                                                       "Sukkwan" = "Sukkwan Strait",
                                                       "Tonowek" = "Tonowek Narrows"))) +
  theme( axis.title=element_text(size=16), legend.position = "none", 
         axis.text.x = element_text(angle = -45, hjust=0, size =12), 
         axis.text.y = element_text(size = 12),
         strip.text=element_text(size = 12, face="bold"))

ggsave("mixing_sites_grid.png", device = "png", path = "SI/", width = 9, 
       height = 7, units = "in", dpi = 300)


# Individual otters #
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


# Chapter 1 - figure xx 
# Prey Seasonal changes 

A <- si.prey %>%
  filter(Site == "Craig" | Site == "Soda Bay") %>%
  drop_na(PreyCat) %>%
  ggplot(aes(x=Season, y=Cnorm, fill=Site)) +
  geom_boxplot() +   theme_few() +
  labs(y=expression(paste(delta^13, "C (\u2030)" )), x= NULL, tag = "A") +
  scale_fill_grey(start=0.3, end=1) +
  ylim(-20,-9) +
  facet_wrap(vars(PreyCat), labeller = labeller(PreyCat = c("Clam" = "Clam", "Crab" = "Crab", 
                                                            "Cucumber.Snail" = "Cucumber/Snail", 
                                                            "Mussel" = "Mussel", "Urchin" = "Urchin"))) +
  theme(legend.position = "none", axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), strip.text = element_text(size = 14))

B <- si.prey %>%
  filter(Site == "Craig" | Site == "Soda Bay") %>%
  drop_na(PreyCat) %>%
  ggplot(aes(x=Season, y=N, fill=Site)) +
  geom_boxplot() +   theme_few() +
  labs(y=expression(paste(delta^15, "N (\u2030)" )), x=NULL, tag = "B") +
  scale_fill_grey(start=0.3, end=1) +
  facet_wrap(vars(PreyCat),labeller = labeller(PreyCat = c("Clam" = "Clam", "Crab" = "Crab", 
                                                           "Cucumber.Snail" = "Cucumber/Snail", 
                                                           "Mussel" = "Mussel", "Urchin" = "Urchin"))) +
  theme(legend.position = "bottom", axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), strip.text = element_text(size = 14), 
        legend.title = element_text(size = 16), legend.text = element_text(size = 14))

g <- grid.arrange(A, B, nrow = 2, heights = c(1.75, 2))
ggsave("CN_Prey_season_site.png", g, device = "png", path = "SI/", width = 9, 
       height = 8, units = "in", dpi = 300)



## Correlations ##
#data in Whisker_correlations.rmd
ggplot(data=corr.2, aes(x= OtterID, y = correlation, fill = Site)) +
  geom_bar(stat="identity", color = "black") +
  scale_fill_grey(start=1, end=0) +
  labs(x= "Individual sea otter", y = "C:N correlation") +
  geom_abline(slope = 0, intercept = 0) +
  facet_wrap(vars(Site), nrow = 3, 
             labeller = labeller(Site = c("Shinaku" = "Shinaku Inlet",
                                          "Sukkwan Strait" = "Sukkwan Strait",
                                          "Tonowek" = "Tonowek Narrows"))) +
  theme_few() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none", 
        strip.text = element_text(size=16))


ggsave("correlation.png", device = "png", path = "SI/", width = 9, 
       height = 7, units = "in", dpi = 300)
