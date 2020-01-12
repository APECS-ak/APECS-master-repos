########################################################################
##                     Mixing Model - Season                          ##
##                     MixSIAR       1.5/2.8                          ##
########################################################################
library(tidyr)
library(ggplot2)
library(MixSIAR)

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
discr.1528 <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/1.52.8_prey_discrimination.csv"

# Load the discrimination/TDF data
discr.1528 <- load_discr_data(filename=discr.1528, mix)

# Make an isospace plot
plot_data(filename="isospace_plot", 
          plot_save_pdf=TRUE, 
          plot_save_png=FALSE, 
          mix,source,discr.1528)

# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr.1528)

#Run with an informative primer
mix.alpha <- c(69.2,14.0,3.4,1.1,7.9,3.3)

# Plot your informative prior
plot_prior(alpha.prior=mix.alpha,
           source=source,
           plot_save_pdf=TRUE,
           plot_save_png=FALSE,
           filename="prior_plot_inf")

# Write the JAGS model file
model_filename.1528 <- "season.1528.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename.1528, resid_err, process_err, mix, source)

jags.1528 <- run_model(run="normal", mix, source, discr.1528, model_filename.1528,
                    alpha.prior = mix.alpha, resid_err, process_err)

output_options.1528 <- list(summary_save = TRUE,
                       summary_name = "summary_statistics.1528",
                       sup_post = FALSE,
                       plot_post_save_pdf = TRUE,
                       plot_post_name = "posterior_density.1528",
                       sup_pairs = FALSE,
                       plot_pairs_save_pdf = TRUE,
                       plot_pairs_name = "pairs_plot.1528",
                       sup_xy = TRUE,
                       plot_xy_save_pdf = FALSE,
                       plot_xy_name = "xy_plot.1528",
                       gelman = TRUE,
                       heidel = FALSE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "diagnostics.1528",
                       indiv_effect = FALSE,
                       plot_post_save_png = FALSE,
                       plot_pairs_save_png = FALSE,
                       plot_xy_save_png = FALSE)

output_JAGS(jags.1528, mix, source, output_options.1528)
