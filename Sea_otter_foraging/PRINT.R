#### FILE FOR PRINTING THINGS ####

library(ggplot2)
library(dplyr) 
library(ggthemes)
library(ggpubr)
library(cowplot)
library(gridExtra) # making multi pane graphs
library(grid) # making multi pane graphs

#Chapter 2 - FIGURE 1 - Seasonal Enegery
a <- ggplot(data= means , aes(y=kcal.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = kcal.m-kcal.sd, ymax = kcal.m+kcal.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20")) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18)) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="Kcal (per dry gram)") + theme_few() +
  theme(legend.position = "none")

b <- ggplot(data= means , aes(y=lipid.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = lipid.m-lipid.sd, ymax = lipid.m+lipid.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20")) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18)) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="% Lipid (per dry gram)") + theme_few() +
  theme(legend.position = "none")

c <- ggplot(data= means , aes(y=protein.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = protein.m-protein.sd, ymax = protein.m+protein.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20")) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18)) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="% Protein (per dry gram)") + theme_few() +
  theme(legend.position = "none")

d <- ggplot(data= means , aes(y=moisture.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = moisture.m-moisture.sd, ymax = moisture.m+moisture.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20")) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18)) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="% Moisture") + theme_few() +
  theme(legend.position = "none")

e <- ggplot(data= means , aes(y=ash.m, x=month, color=PreyCat), na.rm=TRUE) + 
  geom_line(aes(linetype=PreyCat), position=position_dodge(width=25)) + 
  geom_point(aes(shape= PreyCat), size =4, position=position_dodge(width=25)) +
  geom_errorbar(aes(ymin = ash.m-ash.sd, ymax = ash.m+ash.sd), position=position_dodge(width=25), width = 25) + 
  scale_color_manual(name="Prey Catagory", values= c("Black", "gray30", "gray40", "gray60", "gray20")) +
  scale_shape_manual(name="Prey Catagory", values = c(1,15,16,17,18)) +
  scale_linetype_manual(name ="Prey Catagory", values=c("solid", "dashed", "dotted", "dotdash", "longdash")) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b", limits = as.Date(c("2018-04-01","2019-03-01")), expand = c(0.1,0))+
  labs(x="", y="% Ash") + theme_few()


ggdraw() +
  draw_plot(a, x = 0, y = .5, width = .333, height = .5) +
  draw_plot(b, x = .333, y = .5, width = .333, height = .5) +
  draw_plot(c, x = .667, y = .5, width = .333, height = 0.5) +
  draw_plot(d, x = 0, y = 0, width = .333, height = 0.5) +
  draw_plot(e, x = .333, y = 0, width = .475, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D", "E"), size = 15,
                  x = c(0, 0.333, .667, 0, .333), y = c(1, 1, 1, .5, .5))

ggsave("Figure1.png", device = "png", path = "Bombing/", width = 10, 
       height = 6, units = "in", dpi = 300)




#########################################################################################
#########################################################################################

#Chapter 1 - Carbon and nitrogen for site and season

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
