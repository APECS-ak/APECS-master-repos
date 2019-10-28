#### SI NOT USING ###

#Biplot with catagories seperated by species
#kind of confusing with a lot of overlap
ggplot(data= si.test, aes(x=C, y=N)) +
  geom_point(aes(color=Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=PreyCat)) +
  theme_classic()

#separated by site and catagory
ggplot(data= si.test, aes(x=C, y=N)) +
  geom_point(aes(color=PreyCat, shape= Site)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" ))) +
  stat_ellipse(mapping=aes(color=PreyCat)) +
  theme_classic()



#################################################################################
#####                            CARBON:NITROGEN                            #####
#################################################################################


#d13C vs CN ratio
ggplot(data= si.star, aes(x=C, y=CN)) +
  geom_point(aes(color=Tissue, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

# C:N ratio for all species except stars  - Looks like Urchins also are very high
xstar<- filter(si.test, PreyCat != "Star" & PreyCat != "Urchin")
ggplot(data= xstar, aes(x=C, y=CN)) +
  geom_point(aes(color=Species, shape= PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

# C:N ratio of just urchins
ggplot(data= si.urch, aes(x=C, y=CN)) +
  geom_point(aes(color=Species, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

# C:N ratio of just clams
ggplot(data= si.clam, aes(x=C, y=CN)) +
  geom_point(aes(color=Species, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y="Carbon:Nitrogen") +
  theme_classic()

ggplot(data= si.crab, aes(x=C, y=N)) +
  geom_point(aes(color=Site, shape= Species)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  theme_classic()



#### Carbon Normalization

#Prey only plots, normalized
#plot with prey min/max error bars
ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  theme_classic()

#prey with SD as errorbars
ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmean-Nsd, ymax = Nmean+Nsd, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmean-Csd,xmax = Cmean+Csd, color= PreyCat), height=0) +
  theme_classic()

##Now I want to look at the non-normalized carbon data
#Making mean and min/max for each prey type using C
sic.mean <-si.test %>%
  group_by(PreyCat) %>% 
  summarise(Cmean=mean(C), Nmean=mean(N), Cmin=min(C), Cmax= max(C), Nmin=min(N), Nmax=max(N))
sic.mean<-sic.mean[-1,]

ggplot() +
  geom_point(data= sic.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= sic.mean, aes(x=Cmean, y=Nmean, ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(data= sic.mean, aes(x=Cmean, y=Nmean, xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  geom_point(data=whis.mean, aes(x=TDFC, y=TDFN))+
  geom_errorbar(data=whis.mean, aes(x=TDFC, y=TDFN, ymin = TDFN-Nsd, ymax = TDFN+Nsd), width=0) + 
  geom_errorbarh(data=whis.mean, aes(x=TDFC, y=TDFN, xmin = TDFC-Csd, xmax = TDFC+Csd), height=0) +
  theme_classic()

ggplot() +
  geom_point(data= sic.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(data= sic.mean, aes(x=Cmean, y=Nmean, ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(data= sic.mean, aes(x=Cmean, y=Nmean, xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  geom_point(data=whisker, aes(x=C-2.5, y=N-3, color=OtterID))+
  theme_classic()

ggsave("prey_nocarbonnorm.png", device = "png", path = "SI/", width = 6, 
       height = 8, units = "in", dpi = 300)

#no error bars
ggplot() +
  geom_point(data= si.mean, aes(x=Cmean, y=Nmean, color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_point(data=whis.mean, aes(x=TDFC, y=TDFN))+
  theme_classic()


#without TDFs
ggplot(data= si.mean, aes(x=Cmean, y=Nmean)) +
  geom_point(aes(color=PreyCat)) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  geom_errorbar(aes(ymin = Nmin,ymax = Nmax, color= PreyCat), width=0) + 
  geom_errorbarh(aes(xmin = Cmin,xmax = Cmax, color= PreyCat), height=0) +
  geom_point(data=whis.mean) +
  geom_errorbar(data=whis.mean, aes(ymin = Nmean-Nsd, ymax = Nmean+Nsd), width=0) + 
  geom_errorbarh(data=whis.mean, aes(xmin = Cmean-Csd, xmax = Cmean+Csd), height=0) +
  theme_classic()
