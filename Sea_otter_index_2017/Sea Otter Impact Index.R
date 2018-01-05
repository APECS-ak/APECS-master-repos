#####----- Sea Otter Impact Index -----######
## PCA of sea otter measures ##
## Originally created 10.9.2017 WR ##

#####----- Libraries -----#####
library(corrgram)
library(lattice)
library(vegan)
library(ggplot2)
theme_set(theme_classic())

#####----- Load Data -----#####
seaotter <- read.csv("Otter_Index_data_2017_19Oct2017.csv") #BH: I can't find this file and assume that it is the index file in the folder.


#####----- Data prep and exploration -----#####
# Correlations
corrgram(seaotter[,6:11])
pairs(seaotter[,6:11], pch = 16)

# Checking data shape --- all appear right skewed
hist(seaotter$density1_2017)
hist(seaotter$density2_2017)
hist(seaotter$pits)
hist(seaotter$prop_so_shells)
hist(seaotter$so_duration)
hist(seaotter$density_pop_surv)

# log transformation
hist(log(seaotter$density1_2017 + 1))
hist(log(seaotter$density2_2017 + 1))
hist(log(seaotter$pits + 1))
hist(log(seaotter$prop_so_shells + 1))

# Separate values only
dat <- seaotter[, 6:11]

#####----- Transformations -----#####
# log seems the best...
dat.l <- cbind(log(dat[,1:4] + 1), dat[,5:6]) 
pairs(dat.l, pch = 16)

#####----- Standaradizations -----#####
# Standardize to column (sea otter measure) maximum
dat.std.l <- scale(dat.l, center = FALSE, scale = apply(dat.l, 2, max))
dat.std <- scale(dat, center = FALSE, scale = apply(dat, 2, max))

#####----- PCA -----#####
# PCA on transformed AND standarized data only 2017 data
pca1 <- princomp(dat.std.l[,1:4]) 
summary(pca1)
loadings(pca1)
biplot(pca1, cex = 0.6)
screeplot(pca1)
scores(pca1)

# PCA on transformed AND standarized data with duration and Tims pop est.
pca2 <- princomp(dat.std.l)
summary(pca2)
loadings(pca2)
biplot(pca2, cex = 1)
screeplot(pca2)
scores(pca2) 

PCAbi <- data.frame(cbind(seaotter$site, (pca2$scores[,1] * -1), (pca2$scores[,2] * -1)))

ggplot() +
  geom_point(data = PCAbi, aes(x = PCAbi[,2], y = PCAbi[,3])) + 
  geom_text(size=8, aes(x = PCAbi[,2], y = PCAbi[,3], label=PCAbi[,1]))

# Transformed and standardized version is preferred because it separated the data a bit better. Just standardize groups many
# of the sites together. Both show that PC1 is explains 80% and 70% respectivley. 

# Extract scores - also mult by -1 to switch sign
seaotter$PCA1 <- (scores(pca1)[,1] * -1)
seaotter$PCA2 <- (scores(pca2)[,1] * -1)

# Data Export
write.csv(seaotter, "Sea_Otter_Impact_Index_2017_WR.csv", row.names = FALSE)

# plots
ggplot() +
  geom_point(data = seaotter, aes(x = site, y = PCA2), size = 4) +
  ylab("PC1 Scores") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(size = 17), axis.ticks = element_line(size = 2))
  
ggplot() +
  geom_point(data = seaotter, aes(x = so_duration, y = PCA1, label = site)) +
  geom_text(data = seaotter, aes(x = so_duration, y = PCA1, label = site), hjust = -0.15) +
  xlim(c(7, 15))

ggplot() +
  geom_point(data = seaotter, aes(x = density_pop_surv, y = PCA1, label = site)) +
  geom_text(data = seaotter, aes(x = density_pop_surv, y = PCA1, label = site), hjust = -0.15) +
  xlim(c(0,4))

seaotter.s1 <- factor(seaotter$site, levels = seaotter$site[order(seaotter$PCA1)])
seaotter.o1 <- seaotter[order(seaotter$PCA1),]
seaotter.o1$index <- 1:21

seaotter.s2 <- factor(seaotter$site, levels = seaotter$site[order(seaotter$PCA2)])
seaotter.o2 <- seaotter[order(seaotter$PCA2),]
seaotter.o2$index <- 1:21

sitesPCA1 <- factor(seaotter.o1$site, levels=unique(seaotter.o1$site[order(seaotter.o1$index)]), ordered=TRUE)
sitesPCA2 <- factor(seaotter.o2$site, levels=unique(seaotter.o2$site[order(seaotter.o2$index)]), ordered=TRUE)

ord <- data.frame(1:21)
ord$PCA1 <- sitesPCA1
ord$PCA2 <- sitesPCA2
colnames(ord) <- c("Order", "PCA1", "PCA2")

ggplot() +
  geom_point(data = seaotter.o1, aes(x = index, y = PCA1), size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Site", y = "PC1 Scores") +
  theme(text = element_text(size = 17), axis.ticks = element_line(size = 2))

ggplot() +
  geom_point(data = seaotter.o2, aes(x = ord$PCA2, y = PCA2), size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Site", y = "PC1 Scores") +
  theme(text = element_text(size = 17), axis.ticks = element_line(size = 2))

