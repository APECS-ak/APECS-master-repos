## Mixing model with stars added

load(file="SI/season_verylong_star.RData")

attach.jags(jags.1) # adding model

dim(p.fac1) # 3000, 4, 8 -> 3000 iterations of 4 seasons and 8 sources


#Making data frame for graph
post.fall <- data.frame(Season = "Fall",Clam = p.fac1[,1,1], Crab = p.fac1[,1,2], 
                        Cucumber = p.fac1[,1,3], Mussel = p.fac1[,1,4], 
                        Snail = p.fac1[,1,5], Star = p.fac1[,1,6], Tegula = p.fac1[,1,7], Urchin = p.fac1[,1,8])
post.spring <- data.frame(Season = "Spring",Clam = p.fac1[,2,1], Crab = p.fac1[,2,2], 
                          Cucumber = p.fac1[,2,3], Mussel = p.fac1[,2,4], 
                          Snail = p.fac1[,2,5], Tegula = p.fac1[,2,7], Urchin = p.fac1[,2,8], Star = p.fac1[,2,6])
post.summer <- data.frame(Season = "Summer",Clam = p.fac1[,3,1], Crab = p.fac1[,3,2], 
                          Cucumber = p.fac1[,3,3], Mussel = p.fac1[,3,4], 
                          Snail = p.fac1[,3,5], Tegula = p.fac1[,3,7], Urchin = p.fac1[,3,8], Star = p.fac1[,3,6])
post.winter <- data.frame(Season = "Winter",Clam = p.fac1[,4,1], Crab = p.fac1[,4,2], 
                          Cucumber = p.fac1[,4,3], Mussel = p.fac1[,4,4], 
                          Snail = p.fac1[,4,5], Tegula = p.fac1[,4,7], Urchin = p.fac1[,4,8], Star = p.fac1[,3,6])
fall <- post.fall %>% gather(source,value,2:9)
spring <- post.spring %>% gather(source,value,2:9)
summer <- post.summer %>% gather(source,value,2:9)
winter<- post.winter %>% gather(source,value,2:9)
all <- rbind(spring, summer, fall, winter)

ggplot(aes(y = value, x = source, fill = Season), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  theme( axis.title=element_text(size=16))

ggsave("mixing_mixing.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)



#making my own biplot for 8 prey (with whisker csv already loaded)

source <- read.csv("SI/prey_sources.csv")

ggplot() +
  geom_point(data= source, aes(x=MeanC+2, y=MeanN+2.8, color=X), size=3) +
  labs(x=expression(paste(delta^13, "C (\u2030)")), 
       y=expression(paste(delta^15, "N (\u2030)" )))  +
  scale_color_discrete(name  ="Prey Group") +
  geom_errorbar(data= source, aes(x=MeanC+2, y=MeanN+2.8, ymin = MeanN+2.8-SDN, ymax = MeanN+2.8+SDN,
                                   color= X), width=0) + 
  geom_errorbarh(data= source, aes(x=MeanC+2, y=MeanN+2.8, xmin = MeanC+2-SDC,xmax = MeanC+2+SDC,
                                    color= X), height=0) +
  geom_point(data=whisker, aes(x=C, y=N, shape=Season), size=2)+
  theme_few()


#How simialar are clams and stars?
prey.si <- read.csv("SI/Prey.csv")

# independent t-test  - testing two that are very different: Clams and Crabs

crab.c <- prey.si %>% 
  filter(PreyCat == "Crab") %>%
  select(C)

clam.c <- prey.si %>% 
  filter(PreyCat == "Clam") %>%
  select(C)
t.test(clam.c,crab.c) # where y1 & y2 are numeric

#data:  clam.c and crab.c
#t = -6.0922, df = 60.806, p-value = 8.259e-08
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -1.9163142 -0.9691606
#sample estimates:
#  mean of x mean of y 
#-17.19128 -15.74854 

star.c <- prey.si %>% 
  filter(PreyCat == "Star") %>%
  select(C)

clam.c <- prey.si %>% 
  filter(PreyCat == "Clam") %>%
  select(C)
t.test(clam.c,star.c) # where y1 & y2 are numeric

#Welch Two Sample t-test
#data:  clam.c and star.c
#t = 2.0226, df = 23.525, p-value = 0.05463
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.0201014  1.8875130
#sample estimates:
#  mean of x mean of y 
#-17.19128 -18.12498

star.n <- prey.si %>% 
  filter(PreyCat == "Star") %>%
  select(N)

clam.n <- prey.si %>% 
  filter(PreyCat == "Clam") %>%
  select(N)
t.test(clam.n,star.n) # where y1 & y2 are numeric

#Welch Two Sample t-test
#data:  clam.n and star.n
#t = 1.5137, df = 36.122, p-value = 0.1388
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.1183356  0.8151163
#sample estimates:
#  mean of x mean of y 
#9.848140  9.499749

