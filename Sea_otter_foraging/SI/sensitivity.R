## Sensitivity analysis

sensitivity <- read.csv("SI/sensitivity.csv")

sensitivity$Season<-factor(sensitivity$Season , levels=c("Spring", "Summer", "Fall", "Winter"))

c.labs <- c("C: 1", "C: 1.5", "C: 1.8", "C: 2", "C: 2.3")
names(c.labs) <- c("1", "1.5", "1.8", "2", "2.3")
n.labs <- c("N: 2.8", "N: 3.1", "N: 3.4/5")
names(n.labs) <- c("2.8", "3.1", "3.4")

ggplot(sensitivity, aes(fill=forcats::fct_rev(PreyCat), y=Mean, x=Season)) + 
  theme_few() +
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Blues", name= "Prey Group") +
  facet_grid(N~C, labeller = labeller(C=c.labs, N=n.labs)) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="Proportion of Diet") +
  theme(legend.position="bottom")
  
ggsave("sensitivity.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)


## Missing protein values - find the tray numbers!
file <- read.csv("SI/Need_Protein.csv")
file.2 <- read.csv("SI/tray.csv")

file <- left_join(file, file.2, by = "SIN")
write.csv(file, "protein.csv")
