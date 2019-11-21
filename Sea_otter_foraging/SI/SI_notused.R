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


#variability

#For individual otters?
#Carbon
Cdist<-filter(whisker, OtterID == "163520")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 6.251 on 1 and 10 DF,  p-value: 0.03143
Cdist<-filter(whisker, OtterID == "163521")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.1071 on 1 and 10 DF,  p-value: 0.7502
Cdist<-filter(whisker, OtterID == "163522")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 14.04 on 1 and 9 DF,  p-value: 0.004578
Cdist<-filter(whisker, OtterID == "160523")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.1362 on 1 and 8 DF,  p-value: 0.7217
Cdist<-filter(whisker, OtterID == "163524")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.9172 on 1 and 7 DF,  p-value: 0.3701
Cdist<-filter(whisker, OtterID == "163533")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.009771 on 1 and 6 DF,  p-value: 0.9245
Cdist<-filter(whisker, OtterID == "163534")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 1.741 on 1 and 7 DF,  p-value: 0.2286
Cdist<-filter(whisker, OtterID == "163535")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.5315 on 1 and 9 DF,  p-value: 0.4845
Cdist<-filter(whisker, OtterID == "160479")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 13.54 on 1 and 7 DF,  p-value: 0.007857
Cdist<-filter(whisker, OtterID == "160480")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 16.19 on 1 and 7 DF,  p-value: 0.005038
Cdist<-filter(whisker, OtterID == "163525")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 22.44 on 1 and 9 DF,  p-value: 0.001064
Cdist<-filter(whisker, OtterID == "163526")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 2.185 on 1 and 8 DF,  p-value: 0.1776
Cdist<-filter(whisker, OtterID == "163527")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 1.623 on 1 and 7 DF,  p-value: 0.2434
Cdist<-filter(whisker, OtterID == "163528")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 1.841 on 1 and 8 DF,  p-value: 0.2118
Cdist<-filter(whisker, OtterID == "163529")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 17.11 on 1 and 6 DF,  p-value: 0.006105
Cdist<-filter(whisker, OtterID == "163530")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.4251 on 1 and 7 DF,  p-value: 0.5352
Cdist<-filter(whisker, OtterID == "77280")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.2358 on 1 and 12 DF,  p-value: 0.636
Cdist<-filter(whisker, OtterID == "77281")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 0.3106 on 1 and 7 DF,  p-value: 0.5947
Cdist<-filter(whisker, OtterID == "77284")
Cdist.lm<-lm(C~distance, data = Cdist)
summary(Cdist.lm)
#F-statistic: 1.061 on 1 and 10 DF,  p-value: 0.3273

#Nitrogen
Ndist<-filter(whisker, OtterID == "163520")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 16.67 on 1 and 10 DF,  p-value: 0.002205
Ndist<-filter(whisker, OtterID == "163521")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.1572 on 1 and 10 DF,  p-value: 0.7001
Ndist<-filter(whisker, OtterID == "163522")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.415 on 1 and 9 DF,  p-value: 0.5355
Ndist<-filter(whisker, OtterID == "160523")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 6.886 on 1 and 8 DF,  p-value: 0.03045
Ndist<-filter(whisker, OtterID == "163524")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 7.703 on 1 and 7 DF,  p-value: 0.02748
Ndist<-filter(whisker, OtterID == "163533")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.9326 on 1 and 6 DF,  p-value: 0.3715
Ndist<-filter(whisker, OtterID == "163534")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.7286 on 1 and 7 DF,  p-value: 0.4216
Ndist<-filter(whisker, OtterID == "163535")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 26.84 on 1 and 9 DF,  p-value: 0.0005785
Ndist<-filter(whisker, OtterID == "160479")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 38.57 on 1 and 7 DF,  p-value: 0.0004407
Ndist<-filter(whisker, OtterID == "160480")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.3462 on 1 and 7 DF,  p-value: 0.5748
Ndist<-filter(whisker, OtterID == "163525")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 1.64 on 1 and 9 DF,  p-value: 0.2323
Ndist<-filter(whisker, OtterID == "163526")
Ndist.lm<-lm(N~distance, data = Cdist)
summary(Ndist.lm)
#F-statistic: 7.703 on 1 and 7 DF,  p-value: 0.02748
Ndist<-filter(whisker, OtterID == "163527")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.08946 on 1 and 7 DF,  p-value: 0.7736
Ndist<-filter(whisker, OtterID == "163528")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 66.41 on 1 and 8 DF,  p-value: 3.821e-05
Ndist<-filter(whisker, OtterID == "163529")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 5.212 on 1 and 6 DF,  p-value: 0.06255
Ndist<-filter(whisker, OtterID == "163530")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 2.245 on 1 and 7 DF,  p-value: 0.1778
Ndist<-filter(whisker, OtterID == "77280")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.3143 on 1 and 12 DF,  p-value: 0.5854
Ndist<-filter(whisker, OtterID == "77281")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 19.58 on 1 and 7 DF,  p-value: 0.003061
Ndist<-filter(whisker, OtterID == "77284")
Ndist.lm<-lm(N~distance, data = Ndist)
summary(Ndist.lm)
#F-statistic: 0.005928 on 1 and 10 DF,  p-value: 0.9401


Cnotsig<-filter(whisker, OtterID != "163520" & OtterID != "163522" & OtterID != "160479" & 
                  OtterID != "160480" & OtterID != "163525" & OtterID != "163529")

Csig<-filter(whisker, OtterID == "163520" | OtterID == "163522" | OtterID == "160479" | 
               OtterID == "160480" | OtterID == "163525" | OtterID == "163529")

Nnotsig<-filter(whisker, OtterID != "163520" & OtterID != "163523" & OtterID != "160479" & 
                  OtterID != "163535" & OtterID != "163526" & OtterID != "163528" & OtterID != "77281")

Nsig<-filter(whisker, OtterID == "163520" | OtterID == "163523" | OtterID == "160479" | 
               OtterID == "163535" | OtterID == "163526" | OtterID == "163528" | OtterID == "77281")



season.aov <- aov(C~Season + OtterID + Season:OtterID, data = whisker)
summary(season.aov)

#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#  Season          3  11.88   3.959   8.985 2.72e-05 ***
#  OtterID        16 115.30   7.206  16.353  < 2e-16 ***
#  Season:OtterID 45  48.20   1.071   2.431 0.000151 *** 
#  Residuals      94  41.42   0.441 

season.aov <- aov(N~Season + OtterID + Season:OtterID, data = whisker)
summary(season.aov)

#                 Df Sum Sq Mean Sq F value  Pr(>F)    
#  Season          3   3.40   1.133   5.347 0.00191 ** 
#  OtterID        16  54.71   3.419  16.139 < 2e-16 ***
#  Season:OtterID 45  16.41   0.365   1.721 0.01402 *  
#  Residuals      94  19.92   0.212 

Cdistance.aov <-aov(C~factor(OtterID), data = whisker)
sum<-summary(Cdistance.lm)
Cdistance.aov <- aov(Cdistance.lm)
summary(Cdistance.aov)
TukeyHSD(Cdistance.aov)

#                   Df Sum Sq Mean Sq F value   Pr(>F)    
#  distance          1   0.21   0.214   0.371 0.54364     
#  OtterID          16 121.05   7.566  13.102 < 2e-16 ***
#  distance:OtterID 16  23.35   1.459   2.527 0.00214 **   
#  Residuals       125  72.18   0.577 

Anova(distance.aov, type="III") 
posth=glht(Cdistance.aov, linfct=mcp(factorvariable="Tukey"))  ##gives the post-hoc Tukey analysis
summary(posth)

ancova(C~OtterID*distance, whisker)
y.hat<-fitted.values(Cdistance.lm)
e<-residuals(Cdistance.lm)
h<-hatvalues(Cdistance.lm)
r<-e/(sum$sigma*sqrt(1-h))
d<-rstudent(Cdistance.lm)

plot(e~y.hat,xlab="Fitted Values", ylab="Residuals")
stripchart(data= whisker, e~OtterID,vertical=T, method="jitter", xlab="treatment", ylab="Residuals")
plot(e~jitter(y.hat))
qqnorm(r,main=NULL); abline(a=0,b=1,lty=3)
qq.cortest(r,0.05) #not normal
shapiro.test(r) #not normal

#Mood's test allows for an ANOVA like analysis for non-normal data
mood.medtest(C ~ OtterID,
             data  = whisker,
             exact = FALSE)

### Order groups by median
#?? whisker$OtterID = factor(whisker$OtterID)
### Pairwise median tests
PT = pairwiseMedianTest(C ~ OtterID,
                        data   = whisker,
                        exact  = NULL,
                        method = "fdr")
# Adjusts p-values for multiple comparisons;
# See ?p.adjust for options

#Letter version to show who is different
cldList(p.adjust ~ Comparison,
        data = PT,
        threshold = 0.05)

Sig<-filter(PT,p.adjust<=.05)

PT

library(multcompView)

PT = pairwiseMedianMatrix(C ~ OtterID,
                          data   = whisker,
                          exact  = NULL,
                          method = "fdr")

PT

library(multcompView)

multcompLetters(PT$Adjusted,
                compare="<",
                threshold=0.05,
                Letters=letters)

write.csv(PT$Adjusted, "Significant.csv")

Ndistance.lm <-lm(N~factor(OtterID)*distance, data = whisker)
summary(Ndistance.lm)
Ndistance.aov <- aov(Ndistance.lm)
summary(Ndistance.aov)

#                   Df Sum Sq Mean Sq F value   Pr(>F)    
#  distance          1   5.29   5.285  21.374 9.28e-06 ***  
#  OtterID          16  53.03   3.314  13.403  < 2e-16 ***
#  distance:OtterID 16   5.22   0.326   1.318    0.196    
#  Residuals       125  30.91   0.247    

ancova(N~OtterID*distance, whisker)
y.hat<-fitted.values(Ndistance.lm)
e<-residuals(Ndistance.lm)
h<-hatvalues(Ndistance.lm)
r<-e/(sum$sigma*sqrt(1-h))
d<-rstudent(Ndistance.lm)

plot(e~y.hat,xlab="Fitted Values", ylab="Residuals")
stripchart(data= whisker, e~OtterID,vertical=T, method="jitter", xlab="treatment", ylab="Residuals")
plot(e~jitter(y.hat))
qqnorm(r,main=NULL); abline(a=0,b=1,lty=3)
qq.cortest(r,0.05) #normal
shapiro.test(r) #normal


#LS means package
#install.packages("lsmeans")
library(lsmeans)
leastsquare = lsmeans(Cdistance.lm,
                      C ~ OtterID:distance,
                      adjust = "tukey")
leastsquare$contrasts
cld(leastsquare)


ggplot(data=whisker) +
  geom_boxplot(aes(x=Season, y=N)) +
  labs(x= "Season", 
       y=expression(paste(delta^15, "N (\u2030)")))  +
  theme_classic()

ggplot(data=whisker) +
  geom_boxplot(aes(x=Season, y=C+28)) +
  labs(x= "Season", 
       y=expression(paste(delta^13, "C (\u2030)" )))  +
  theme_classic()

whisker$distance<-as.character(whisker$distance)
whis.mean2<-whisker %>%
  group_by(distance) %>%
  summarise(Cmean=mean(C), Nmean=mean(N), Csd=sd(C), Nsd=sd(N))

ggplot(data= whis.mean2, aes(x=distance, y=Cmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Cmean-Csd,ymax = Cmean+Csd), width=.2) + 
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^13, "C (\u2030)" )))  +
  theme_classic()

ggplot(data= whis.mean2, aes(x=distance, y=Nmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Nmean-Nsd,ymax = Nmean+Nsd), width=.2) + 
  labs(x= "Distance from root (cm)", 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  theme_classic()
