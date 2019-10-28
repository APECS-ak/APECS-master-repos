
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

library(MixSIAR)
#browseVignettes("MixSIAR")
#mixsiar_gui() # this won't work and I cannot figure out why
mixsiar.dir <- find.package("MixSIAR")
#paste0(mixsiar.dir,"/example_scripts")
#source(paste0(mixsiar.dir,"/example_scripts/mixsiar_script_wolves.R"))

#working dir for consumer (whisker)
mix.filename <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/whis_consumer.csv"

# Load the mixture/consumer data
mix <- load_mix_data(filename=mix.filename, 
                     iso_names=c("C","N"), 
                     factors=c("OtterID"), 
                     fac_random=TRUE, 
                     fac_nested=NULL, 
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
plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

# default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#plot_prior(alpha.prior=1,source)

#Run with an informative primer
# Our 14 fecal samples were 10, 1, 0, 0, 3
mix.alpha <- c(69.2,14.0,3.4,1.1,7.9,3.3)


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

run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)


jags.1 <- run_model(run="test", mix, source, discr, model_filename,
                    alpha.prior = mix.alpha, resid_err, process_err)

jags.1 <- run_model(run="normal", mix, source, discr, model_filename,
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