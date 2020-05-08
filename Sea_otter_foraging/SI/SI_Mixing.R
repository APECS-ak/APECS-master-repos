
########################################################################
##                         Mixing Models                              ##
##                             SIBER                                  ##
########################################################################
#install.packages("SIBER")
library(SIBER)

# load in the included demonstration dataset
demo<-read.csv("SI/demo.siber.data.csv")
#
# create the siber object
siber.example <- createSiberObject(demo)

# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

plotSiberObject(siber.example,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030'))

#Summary statistics and custom graphic additions
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.95, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")

# this time we will make the points a bit smaller by 
# cex = 0.5
plotSiberObject(siber.example,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab=expression({delta}^13*C~'\u2030'),
                ylab=expression({delta}^15*N~'\u2030'),
                cex = 0.5)

# Calculate summary statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siber.example)
print(group.ML)
#>            1.1       1.2       1.3       2.1       2.2       2.3
#> TA   21.924922 10.917715 17.945127 3.0714363 11.476354 1.4818061
#> SEA   5.783417  3.254484  5.131601 0.8623300  3.458824 0.4430053
#> SEAc  5.989967  3.370715  5.314872 0.8931275  3.582354 0.4588269


# You can add more ellipses by directly calling plot.group.ellipses()
# Add an additional p.interval % prediction ellilpse
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                  lty = 1, lwd = 2)

# or you can add the XX% confidence interval around the bivariate means
# by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95, ci.mean = T,
                  lty = 1, lwd = 2)

# A second plot provides information more suitable to comparing
# the two communities based on the community-level Layman metrics

# this time we will make the points a bit smaller by 
# cex = 0.5
plotSiberObject(siber.example,
                ax.pad = 2, 
                hulls = T, community.hulls.args, 
                ellipses = F, group.ellipses.args,
                group.hulls = F, group.hull.args,
                bty = "L",
                iso.order = c(1,2),
                xlab=expression({delta}^13*C~'\u2030'),
                ylab=expression({delta}^15*N~'\u2030'),
                cex = 0.5)

# or you can add the XX% confidence interval around the bivariate means
# by specifying ci.mean = T along with whatever p.interval you want.
plotGroupEllipses(siber.example, n = 100, p.interval = 0.95,
                  ci.mean = T, lty = 1, lwd = 2) 

# Calculate the various Layman metrics on each of the communities.
community.ML <- communityMetricsML(siber.example) 
print(community.ML)
#>                  1         2
#> dY_range  6.526633  4.617564
#> dX_range  8.184069 10.184171
#> TA       13.079701 12.799232
#> CD        4.235518  4.809830
#> MNND      4.944052  5.111483
#> SDNND     3.526088  4.598012

#Fitting the baysian model to the data
# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the 
# means. Fitting is via the JAGS method.
ellipses.posterior <- siberMVN(siber.example, parms, priors)

# The posterior estimates of the ellipses for each group can be used to
# calculate the SEA.B for each group.
SEA.B <- siberEllipses(ellipses.posterior)

siberDensityPlot(SEA.B, xticklabels = colnames(group.ML), 
                 xlab = c("Community | Group"),
                 ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 main = "SIBER ellipses on each group")

# Add red x's for the ML estimated SEA-c
points(1:ncol(SEA.B), group.ML[3,], col="red", pch = "x", lwd = 2)

# Calculate some credible intervals 
cr.p <- c(0.95, 0.99) # vector of quantiles

# call to hdrcde:hdr using lapply()
SEA.B.credibles <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$hdr},
  prob = cr.p)

# do similar to get the modes, taking care to pick up multimodal posterior
# distributions if present
SEA.B.modes <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$mode},
  prob = cr.p, all.modes=T)

#Comparison of entire communities using the Layman metrics
# extract the posterior means
mu.post <- extractPosteriorMeans(siber.example, ellipses.posterior)

# calculate the corresponding distribution of layman metrics
layman.B <- bayesianLayman(mu.post)


# --------------------------------------
# Visualise the first community
# --------------------------------------
siberDensityPlot(layman.B[[1]], xticklabels = colnames(layman.B[[1]]), 
                 bty="L", ylim = c(0,20))

# add the ML estimates (if you want). Extract the correct means 
# from the appropriate array held within the overall array of means.
comm1.layman.ml <- laymanMetrics(siber.example$ML.mu[[1]][1,1,],
                                 siber.example$ML.mu[[1]][1,2,]
)
points(1:6, comm1.layman.ml$metrics, col = "red", pch = "x", lwd = 2)

# --------------------------------------
# Visualise the second community
# --------------------------------------
siberDensityPlot(layman.B[[2]], xticklabels = colnames(layman.B[[2]]), 
                 bty="L", ylim = c(0,20))

# add the ML estimates. (if you want) Extract the correct means 
# from the appropriate array held within the overall array of means.
comm2.layman.ml <- laymanMetrics(siber.example$ML.mu[[2]][1,1,],
                                 siber.example$ML.mu[[2]][1,2,])
points(1:6, comm2.layman.ml$metrics, col = "red", pch = "x", lwd = 2)

# --------------------------------------
# Alternatively, pull out TA from both and aggregate them into a 
# single matrix using cbind() and plot them together on one graph.
# --------------------------------------

siberDensityPlot(cbind(layman.B[[1]][,"TA"], layman.B[[2]][,"TA"]),
                 xticklabels = c("Community 1", "Community 2"), 
                 bty="L", ylim = c(0,20),
                 las = 1,
                 ylab = "TA - Convex Hull Area",
                 xlab = "")
########################################################################
##                         Mixing Models                              ##
##                            MixSIAR                                 ##
########################################################################
library(tidyr)
library(dplyr)
library(ggplot2)
library(MixSIAR)

mixsiar.dir <- find.package("MixSIAR")

#make consumer file
whisker <- read.csv("SI/whiskers.csv")
whisker$OtterID <- as.character(whisker$OtterID)
whisker <- filter(whisker, OtterID != "163532")
whisker.consumer <- whisker %>%
  select(C, N, Season, Site)
write.csv(whisker.consumer, "SI/whisker_consumer.csv")

#make source file with cucs and snails combined
si.test <- read.csv("SI/SI.csv")
si.test$PreyCat <- ifelse(si.test$Species == "apc" | si.test$Species == "tes", "Cucumber_Snail", 
                          ifelse(si.test$Species == "cln" | si.test$Species == "prs" | si.test$Species == "sag", "Clam", 
                                 ifelse(si.test$Species == "cam" | si.test$Species == "cap" | si.test$Species == "cao" | 
                                          si.test$Species == "tec" | si.test$Species == "pup",  "Crab", 
                                        ifelse(si.test$Species == "std" | si.test$Species == "stf", "Urchin",""))))
### Fixing C for high fat critters. Anything above 3.5:1 ratio
si.test$Cnorm <- NA
si.test$Cnorm<- ifelse(si.test$CN >= 3.5, si.test$C-3.32+(0.99*si.test$CN), si.test$C)
si.test <- filter(si.test, PreyCat != is.na(PreyCat))

si.mean <-si.test %>%
  group_by(PreyCat) %>% 
  summarise(MeanC=mean(Cnorm), SDC=sd(Cnorm), MeanN=mean(N), SDN=sd(N))
si.mean<-si.mean[-1,] #removing blank

si.count <- si.test %>%
  group_by(PreyCat) %>%
  count()
si.count<-si.count[-1,] #removing blank

#join count and mean
si.mean <- left_join(si.mean, si.count)

write.csv(si.mean, "SI/prey_sources.csv")

#working dir for consumer (whisker)
mix.filename <- "SI/whisker_consumer.csv"

# Load the mixture/consumer data
mix <- load_mix_data(filename=mix.filename, 
                     iso_names=c("C","N"), 
                     factors=c("Season", "Site"), 
                     fac_random=c(FALSE,FALSE),
                     fac_nested=c(FALSE,FALSE), 
                     cont_effects=NULL)

# working dir for source (prey)
source.filename <- "SI/prey_sources.csv"

# Load the source data
source <- load_source_data(filename=source.filename,
                           source_factors=NULL, 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

# working dir for discrimination factors (prey)
discr.filename <- "SI/prey_discrimination.csv"

# Load the discrimination/TDF data
discr <- load_discr_data(filename=discr.filename, mix)

# Make an isospace plot
plot_data(filename="isospace_plot", 
          plot_save_pdf=TRUE, 
          plot_save_png=FALSE, 
          mix,source,discr)

# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)


#Run with an informative primer
# Our 14 fecal samples were 10, 1, 0, 0, 3
mix.alpha <- c(72,13,5,3)


# Plot your informative prior
plot_prior(alpha.prior=mix.alpha,
           source=source,
           plot_save_pdf=TRUE,
           plot_save_png=FALSE,
           filename="prior_plot_inf")

# Write the JAGS model file
model_filename <- "MixSIAR_model_2.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)


run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)


jags.2 <- run_model(run="test", mix, source, discr, model_filename,
                    alpha.prior = mix.alpha, resid_err, process_err)

jags.2 <- run_model(run="normal", mix, source, discr, model_filename,
                    alpha.prior = mix.alpha, resid_err, process_err)

output_options <- list(summary_save = TRUE,
                       summary_name = "summary_statistics",
                       sup_post = FALSE,
                       plot_post_save_pdf = TRUE,
                       plot_post_name = "posterior_density",
                       sup_pairs = FALSE,
                       plot_pairs_save_pdf = TRUE,
                       plot_pairs_name = "pairs_plot",
                       sup_xy = TRUE,
                       plot_xy_save_pdf = FALSE,
                       plot_xy_name = "xy_plot",
                       gelman = TRUE,
                       heidel = FALSE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "diagnostics",
                       indiv_effect = FALSE,
                       plot_post_save_png = FALSE,
                       plot_pairs_save_png = FALSE,
                       plot_xy_save_png = FALSE)

output_JAGS(jags.2, mix, source, output_options)


########################################################################
##                     Mixing Model - Season                          ##
##                            MixSIAR                                 ##
########################################################################
library(tidyr)
library(ggplot2)
library(MixSIAR)
#browseVignettes("MixSIAR")
#mixsiar_gui() # this won't work and I cannot figure out why
mixsiar.dir <- find.package("MixSIAR")
#paste0(mixsiar.dir,"/example_scripts")
#source(paste0(mixsiar.dir,"/example_scripts/mixsiar_script_wolves.R"))

#working dir for consumer (whisker)
mix.filename <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/season_consumer.csv"

# Load the mixture/consumer data
mix <- load_mix_data(filename=mix.filename, 
                     iso_names=c("C","N"), 
                     factors=c("Season"), 
                     fac_random=FALSE, 
                     fac_nested=FALSE, 
                     cont_effects=NULL)

# working dir for source (prey)
source.filename <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/prey_sources.csv"

# Load the source data
source <- load_source_data(filename=source.filename,
                           source_factors=NULL, 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

# working dir for discrimination factors (prey)
discr.filename <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/prey_discrimination.csv"

# Load the discrimination/TDF data
discr <- load_discr_data(filename=discr.filename, mix)

# Make an isospace plot
plot_data(filename="isospace_plot", 
          plot_save_pdf=TRUE, 
          plot_save_png=FALSE, 
          mix,source,discr)

# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

# default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#plot_prior(alpha.prior=1,source)



#Run with an informative primer
# Our 14 fecal samples were 10, 1, 0, 0, 3
mix.alpha <- c(81,32,14,2,3,5,3,7)


# Plot your informative prior
plot_prior(alpha.prior=mix.alpha,
           source=source,
           plot_save_pdf=TRUE,
           plot_save_png=FALSE,
           filename="prior_plot_inf")

# Write the JAGS model file
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)


#run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)


jags.1 <- run_model(run="test", mix, source, discr, model_filename,
                    alpha.prior = mix.alpha, resid_err, process_err)

jags.1 <- run_model(run="very long", mix, source, discr, model_filename,
                    alpha.prior = mix.alpha, resid_err, process_err)

output_options <- list(summary_save = TRUE,
                       summary_name = "summary_statistics",
                       sup_post = FALSE,
                       plot_post_save_pdf = TRUE,
                       plot_post_name = "posterior_density",
                       sup_pairs = FALSE,
                       plot_pairs_save_pdf = TRUE,
                       plot_pairs_name = "pairs_plot",
                       sup_xy = TRUE,
                       plot_xy_save_pdf = FALSE,
                       plot_xy_name = "xy_plot",
                       gelman = TRUE,
                       heidel = FALSE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "diagnostics",
                       indiv_effect = FALSE,
                       plot_post_save_png = FALSE,
                       plot_pairs_save_png = FALSE,
                       plot_xy_save_png = FALSE)

output_JAGS(jags.1, mix, source, output_options)
#########################################################################
####                    Looking at the outputs                       ####  
#########################################################################
#using code from the mantis shrimp example

library(MixSIAR)
library(ggplot2)
library(R2jags)
library(tidyr)
library(ggthemes)

#Load image Very long seasons (this is converged)
load(file="SI/Old Model Runs/season_verylong.RData")

attach.jags(jags.1) # adding model

dim(p.fac1) # 3000, 4, 7 -> 3000 iterations of 4 seasons and 7 sources
median(p.fac1[,1,1]) #median of season 1, source 1
p.fac1[,2,1]


#Making data frame for graph
post.fall <- data.frame(Season = "Fall",Clam = p.fac1[,1,1], Crab = p.fac1[,1,2], 
                          Cucumber = p.fac1[,1,3], Mussel = p.fac1[,1,4], 
                          Snail = p.fac1[,1,5], Tegula = p.fac1[,1,6], Urchin = p.fac1[,1,7])
post.spring <- data.frame(Season = "Spring",Clam = p.fac1[,2,1], Crab = p.fac1[,2,2], 
                         Cucumber = p.fac1[,2,3], Mussel = p.fac1[,2,4], 
                         Snail = p.fac1[,2,5], Tegula = p.fac1[,2,6], Urchin = p.fac1[,2,7])
post.summer <- data.frame(Season = "Summer",Clam = p.fac1[,3,1], Crab = p.fac1[,3,2], 
                        Cucumber = p.fac1[,3,3], Mussel = p.fac1[,3,4], 
                        Snail = p.fac1[,3,5], Tegula = p.fac1[,3,6], Urchin = p.fac1[,3,7])
post.winter <- data.frame(Season = "Winter",Clam = p.fac1[,4,1], Crab = p.fac1[,4,2], 
                          Cucumber = p.fac1[,4,3], Mussel = p.fac1[,4,4], 
                          Snail = p.fac1[,4,5], Tegula = p.fac1[,4,6], Urchin = p.fac1[,4,7])
fall <- post.fall %>% gather(source,value,2:8)
spring <- post.spring %>% gather(source,value,2:8)
summer <- post.summer %>% gather(source,value,2:8)
winter<- post.winter %>% gather(source,value,2:8)
all <- rbind(spring, summer, fall, winter)

ggplot(aes(y = value, x = source, fill = Season), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  #scale_fill_manual(values=c("black","white"), name="") + # Seagrass is black, coral is white
  theme( axis.title=element_text(size=16), legend.position = "none")

ggsave("mixing_mixing.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)


### Looking at only clams and crabs

cc.all <- filter(all, source == "Clam" | source == "Crab")
ggplot(aes(y = value, x = source, fill = Season), data = cc.all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  theme( axis.title=element_text(size=16), legend.position = "none")

ggsave("cc_mixing.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)


#anova for mixing model by species

#clam
clam<-filter(all, source == "Clam")
clam.aov <- aov(value~Season, data = clam)
summary(clam.aov)
pairwise.t.test(x=clam$value, g=clam$Season, p.adj="bonferroni")

#crab
crab<-filter(all, source == "Crab")
crab.aov <- aov(value~Season, data = crab)
summary(crab.aov)
pairwise.t.test(x=crab$value, g=crab$Season, p.adj="bonferroni")

#cucumber
cucumber<-filter(all, source == "Cucumber")
cucumber.aov <- aov(value~Season, data = cucumber)
summary(cucumber.aov)
pairwise.t.test(x=cucumber$value, g=cucumber$Season, p.adj="bonferroni")

#mussel
mussel<-filter(all, source == "Mussel")
mussel.aov <- aov(value~Season, data = mussel)
summary(mussel.aov)
pairwise.t.test(x=mussel$value, g=mussel$Season, p.adj="bonferroni")

#this is a graph of just the summer values 
ggplot(aes(y = value, x = source, fill = source), data = summer) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  xlab(NULL) +
  ylab("Diet proportion") +
  theme(axis.title=element_text(size=16), legend.position = "none")

ggsave("mixing_summer.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)

##############################################################################
## With Stars Added - note this model didn't converge
##############################################################################

dim(p.fac1) # 3000, 4, 8 -> 3000 iterations of 4 seasons and 7 sources
median(p.fac1[,1,1]) #median of season 1, source 1
p.fac1[,2,1]


#Making data frame for graph
post.fall <- data.frame(Season = "Fall",Clam = p.fac1[,1,1], Crab = p.fac1[,1,2], 
                        Cucumber = p.fac1[,1,3], Mussel = p.fac1[,1,4], 
                        Snail = p.fac1[,1,5], Star = p.fac1[,1,6], 
                        Tegula = p.fac1[,1,7], Urchin = p.fac1[,1,8])
post.spring <- data.frame(Season = "Spring",Clam = p.fac1[,2,1], Crab = p.fac1[,2,2], 
                          Cucumber = p.fac1[,2,3], Mussel = p.fac1[,2,4], 
                          Snail = p.fac1[,2,5], Tegula = p.fac1[,2,7], Urchin = p.fac1[,2,8],
                          Star = p.fac1[,2,6])
post.summer <- data.frame(Season = "Summer",Clam = p.fac1[,3,1], Crab = p.fac1[,3,2], 
                          Cucumber = p.fac1[,3,3], Mussel = p.fac1[,3,4], 
                          Snail = p.fac1[,3,5], Tegula = p.fac1[,3,7], Urchin = p.fac1[,3,8],
                          Star = p.fac1[,3,6])
post.winter <- data.frame(Season = "Winter",Clam = p.fac1[,4,1], Crab = p.fac1[,4,2], 
                          Cucumber = p.fac1[,4,3], Mussel = p.fac1[,4,4], 
                          Snail = p.fac1[,4,5], Tegula = p.fac1[,4,7], Urchin = p.fac1[,4,8],
                          Star = p.fac1[,4,6])
fall <- post.fall %>% gather(source,value,2:9)
spring <- post.spring %>% gather(source,value,2:9)
summer <- post.summer %>% gather(source,value,2:9)
winter<- post.winter %>% gather(source,value,2:9)
all <- rbind(spring, summer, fall, winter)

ggplot(aes(y = value, x = source, fill = Season), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_bw() +
  xlab("Prey Catagory") +
  ylab("Diet proportion") +
  #scale_fill_manual(values=c("black","white"), name="") + # Seagrass is black, coral is white
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title=element_text(size=16))

ggsave("mixing2.png", device = "png", path = "SI/", width = 9, 
       height = 6, units = "in", dpi = 300)

#anova for mixing model by species

#clam
clam<-filter(all, source == "Clam")
clam.aov <- aov(value~Season, data = clam)
summary(clam.aov)
pairwise.t.test(x=clam$value, g=clam$Season, p.adj="bonferroni")

#crab
crab<-filter(all, source == "Crab")
crab.aov <- aov(value~Season, data = crab)
summary(crab.aov)
pairwise.t.test(x=crab$value, g=crab$Season, p.adj="bonferroni")

#cucumber
cucumber<-filter(all, source == "Cucumber")
cucumber.aov <- aov(value~Season, data = cucumber)
summary(cucumber.aov)
pairwise.t.test(x=cucumber$value, g=cucumber$Season, p.adj="bonferroni")

#mussel
mussel<-filter(all, source == "Mussel")
mussel.aov <- aov(value~Season, data = mussel)
summary(mussel.aov)
pairwise.t.test(x=mussel$value, g=mussel$Season, p.adj="bonferroni")


##############################################################################
## 45 samples, 7 sources with season only very long run
##############################################################################

load(file="SI/Old Model Runs/season/Run_0403-2020/season_verylong_45.RData")
attach.jags(jags.1)

jags.1[["parameters.to.save"]]

##############################################################################
## 45 samples, 5 sources
##############################################################################

library(MixSIAR)
library(ggplot2)
library(R2jags)
library(tidyr)

#TONOWEK - very long

#Load image Very long seasons (this is converged)
load(file="SI/tonowek_verylong.RData")

attach.jags(jags.tonowek) # adding model

dim(p.fac1) # 3000, 4, 5 -> 3000 iterations of 4 seasons and 5 sources
mean(p.fac1[,1,1]) #median of season 1, source 1
p.fac1[,2,1]


#Making data frame for graph
ton.fall <- data.frame(Site = "Tonowek", Season = "Fall",Clam = p.fac1[,1,1], Crab = p.fac1[,1,2], 
                        Cucumber.Snail = p.fac1[,1,3], Mussel = p.fac1[,1,4], Urchin = p.fac1[,1,5])
ton.spring <- data.frame(Site = "Tonowek", Season = "Spring",Clam = p.fac1[,2,1], Crab = p.fac1[,2,2], 
                         Cucumber.Snail = p.fac1[,2,3], Mussel = p.fac1[,2,4], Urchin = p.fac1[,2,5])
ton.summer <- data.frame(Site = "Tonowek", Season = "Summer",Clam = p.fac1[,3,1], Crab = p.fac1[,3,2], 
                         Cucumber.Snail = p.fac1[,3,3], Mussel = p.fac1[,3,4], Urchin = p.fac1[,3,5])
ton.winter <- data.frame(Site = "Tonowek", Season = "Winter",Clam = p.fac1[,4,1], Crab = p.fac1[,4,2], 
                         Cucumber.Snail = p.fac1[,4,3], Mussel = p.fac1[,4,4], Urchin = p.fac1[,4,5])
fall.ton <- ton.fall %>% gather(source,value,3:7)
spring.ton <- ton.spring %>% gather(source,value,3:7)
summer.ton <- ton.summer %>% gather(source,value,3:7)
winter.ton <- ton.winter %>% gather(source,value,3:7)
all.ton <- rbind(spring.ton, summer.ton, fall.ton, winter.ton)

ggplot(aes(y = value, x = source, fill = Season), data = all.ton) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  #scale_fill_manual(values=c("black","white"), name="") + # Seagrass is black, coral is white
  theme( axis.title=element_text(size=16), legend.position = "none")

detach.jags(jags.tonowek)

#Load image 
load(file="SI/tonowek_extreme.RData")

attach.jags(jags.tonowek) # adding model

dim(p.fac1) # 3000, 4, 5 -> 3000 iterations of 4 seasons and 5 sources
mean(p.fac1[,1,1]) #median of season 1, source 1
p.fac1[,2,1]


#Making data frame for graph
ton.fall <- data.frame(Site = "Tonowek", Season = "Fall",Clam = p.fac1[,1,1], Crab = p.fac1[,1,2], 
                       Cucumber.Snail = p.fac1[,1,3], Mussel = p.fac1[,1,4], Urchin = p.fac1[,1,5])
ton.spring <- data.frame(Site = "Tonowek", Season = "Spring",Clam = p.fac1[,2,1], Crab = p.fac1[,2,2], 
                         Cucumber.Snail = p.fac1[,2,3], Mussel = p.fac1[,2,4], Urchin = p.fac1[,2,5])
ton.summer <- data.frame(Site = "Tonowek", Season = "Summer",Clam = p.fac1[,3,1], Crab = p.fac1[,3,2], 
                         Cucumber.Snail = p.fac1[,3,3], Mussel = p.fac1[,3,4], Urchin = p.fac1[,3,5])
ton.winter <- data.frame(Site = "Tonowek", Season = "Winter",Clam = p.fac1[,4,1], Crab = p.fac1[,4,2], 
                         Cucumber.Snail = p.fac1[,4,3], Mussel = p.fac1[,4,4], Urchin = p.fac1[,4,5])
fall.ton <- ton.fall %>% gather(source,value,3:7)
spring.ton <- ton.spring %>% gather(source,value,3:7)
summer.ton <- ton.summer %>% gather(source,value,3:7)
winter.ton <- ton.winter %>% gather(source,value,3:7)
all.ton <- rbind(spring.ton, summer.ton, fall.ton, winter.ton)

write.csv(all.ton, "SI/tonowek_extreme.csv")

ggplot(aes(y = value, x = source, fill = Season), data = all.ton) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  #scale_fill_manual(values=c("black","white"), name="") + # Seagrass is black, coral is white
  theme( axis.title=element_text(size=16), legend.position = "none")


detach.jags(jags.tonowek)

#SHINKAKU

#Load image Very long seasons (this is converged)
load(file="SI/shinaku_verylong.RData") 

attach.jags(jags.shinaku) # adding model

dim(p.fac1) # 3000, 4, 5 -> 3000 iterations of 4 seasons and 5 sources
median(p.fac1[,1,1]) #median of season 1, source 1
p.fac1[,2,1]

#Making data frame for graph
shin.fall <- data.frame(Site = "Shinaku", Season = "Fall",Clam = p.fac1[,1,1], Crab = p.fac1[,1,2], 
                       Cucumber.Snail = p.fac1[,1,3], Mussel = p.fac1[,1,4], Urchin = p.fac1[,1,5])
shin.spring <- data.frame(Site = "Shinaku", Season = "Spring",Clam = p.fac1[,2,1], Crab = p.fac1[,2,2], 
                         Cucumber.Snail = p.fac1[,2,3], Mussel = p.fac1[,2,4], Urchin = p.fac1[,2,5])
shin.summer <- data.frame(Site = "Shinaku", Season = "Summer",Clam = p.fac1[,3,1], Crab = p.fac1[,3,2], 
                         Cucumber.Snail = p.fac1[,3,3], Mussel = p.fac1[,3,4], Urchin = p.fac1[,3,5])
shin.winter <- data.frame(Site = "Shinaku", Season = "Winter",Clam = p.fac1[,4,1], Crab = p.fac1[,4,2], 
                         Cucumber.Snail = p.fac1[,4,3], Mussel = p.fac1[,4,4], Urchin = p.fac1[,4,5])
fall.shin <- shin.fall %>% gather(source,value,3:7)
spring.shin <- shin.spring %>% gather(source,value,3:7)
summer.shin <- shin.summer %>% gather(source,value,3:7)
winter.shin <- shin.winter %>% gather(source,value,3:7)


all.shin <- rbind(spring.shin, summer.shin, fall.shin, winter.shin)

ggplot(aes(y = value, x = source, fill = Season), data = all.shin) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  #scale_fill_manual(values=c("black","white"), name="") + # Seagrass is black, coral is white
  theme( axis.title=element_text(size=16), legend.position = "none")

detach.jags(jags.shinaku)

#SUKKWAN

#Load image Very long seasons (this is converged)
load(file="SI/sukkwan_verylong.RData")

attach.jags(jags.sukkwan) # adding model

median(p.fac1[,1,1]) #median of season 1, source 1

#Making data frame for graph
suk.fall <- data.frame(Site = "Sukkwan", Season = "Fall",Clam = p.fac1[,1,1], Crab = p.fac1[,1,2], 
                        Cucumber.Snail = p.fac1[,1,3], Mussel = p.fac1[,1,4], Urchin = p.fac1[,1,5])
suk.spring <- data.frame(Site = "Sukkwan", Season = "Spring",Clam = p.fac1[,2,1], Crab = p.fac1[,2,2], 
                          Cucumber.Snail = p.fac1[,2,3], Mussel = p.fac1[,2,4], Urchin = p.fac1[,2,5])
suk.summer <- data.frame(Site = "Sukkwan", Season = "Summer",Clam = p.fac1[,3,1], Crab = p.fac1[,3,2], 
                          Cucumber.Snail = p.fac1[,3,3], Mussel = p.fac1[,3,4], Urchin = p.fac1[,3,5])
suk.winter <- data.frame(Site = "Sukkwan", Season = "Winter",Clam = p.fac1[,4,1], Crab = p.fac1[,4,2], 
                          Cucumber.Snail = p.fac1[,4,3], Mussel = p.fac1[,4,4], Urchin = p.fac1[,4,5])
fall.suk <- suk.fall %>% gather(source,value,3:7)
spring.suk <- suk.spring %>% gather(source,value,3:7)
summer.suk <- suk.summer %>% gather(source,value,3:7)
winter.suk <- suk.winter %>% gather(source,value,3:7)


all.suk <- rbind(spring.suk, summer.suk, fall.suk, winter.suk)

ggplot(aes(y = value, x = source, fill = Season), data = all.suk) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  #scale_fill_manual(values=c("black","white"), name="") + # Seagrass is black, coral is white
  theme( axis.title=element_text(size=16), legend.position = "none")

all <- bind_rows(all.ton, all.shin, all.suk)
write.csv(all, "SI/all_mixing.csv")

##READ
all <- read.csv("SI/all_mixing.csv")

#facet wrap by prey
ggplot(aes(y = value, x = Site, fill = Season), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  facet_wrap(vars(source), nrow = 2) +
  theme( axis.title=element_text(size=16))

#facet wrap by season
ggplot(aes(y = value, x = source, fill = Site), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  facet_wrap(vars(Season), nrow = 2) +
  theme( axis.title=element_text(size=16))


#facet wrap by site
ggplot(aes(y = value, x = source, fill = Season), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  facet_wrap(vars(Site), nrow = 2) +
  theme( axis.title=element_text(size=16))

ggsave("mixing_sites.png", device = "png", path = "SI/", width = 9, 
       height = 5, units = "in", dpi = 300)


#trying to get them all on the same graph
ggplot(aes(y = value, x = source, fill = Season, color = Site), data = all) + 
  geom_boxplot(outlier.colour = NA) +
  theme_few() +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  theme( axis.title=element_text(size=16))


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


##############################################################
## Checking Tonowek very long vs extreme

# Load files
all <- read.csv("SI/all_mixing.csv")
tonowek <- read.csv("SI/tonowek_extreme.csv")

long <- all %>%
  filter(Site == "Tonowek") %>%
  group_by(source, Season) %>%
  summarise(mean=mean(value), sd=(sd(value)))
long$model <- "long"

extreme <- tonowek %>%
  group_by(source, Season) %>%
  summarise(mean=mean(value), sd=(sd(value)))
extreme$model <- "extreme"

all.mean <- bind_rows(long, extreme)

all.mean <- pivot_longer(all.mean, cols = c(starts_with("mean."), starts_with("sd.")), names_to = "model", 
                         names_prefix = c("mean.", "sd."), values_to = c("mean","sd"))

ggplot(data=all.mean, aes(x=source, y=mean, color=model)) +
  theme_few() +
  geom_point(position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=0, position = position_dodge(width=0.5)) +
  xlab(NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Diet proportion") +
  facet_wrap(vars(Season))


## looking at means for paper
all <- read.csv("SI/all_mixing.csv")

all.mean <- all%>%
  group_by(source, Season, Site) %>%
  summarise(mean=mean(value), sd=(sd(value)))


all.lm <- lm(value~Season*source, data=all)
summary(all.lm)
