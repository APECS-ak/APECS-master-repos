########################################################################
##                     Mixing Model - Season                          ##
##                     MixSIAR       1.8/2.8                          ##
########################################################################
library(tidyr)
library(ggplot2)
library(MixSIAR)


discr.1828 <- "SI/1.82.8_prey_discrimination.csv"

# Load the discrimination/TDF data
discr.1828 <- load_discr_data(filename=discr.1828, mix)

# Make an isospace plot
plot_data(filename="isospace_plot", 
          plot_save_pdf=TRUE, 
          plot_save_png=FALSE, 
          mix,source,discr.1828)

# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr.1828)


# Write the JAGS model file
model_filename.1828 <- "season.1828.txt"   # Name of the JAGS model file

write_JAGS_model(model_filename.1828, resid_err, process_err, mix, source)

jags.1828 <- run_model(run="normal", mix, source, discr.1828, model_filename.1828,
                       alpha.prior = mix.alpha, resid_err, process_err)

output_options.1828 <- list(summary_save = TRUE,
                            summary_name = "summary_statistics.1828",
                            sup_post = FALSE,
                            plot_post_save_pdf = TRUE,
                            plot_post_name = "posterior_density.1828",
                            sup_pairs = FALSE,
                            plot_pairs_save_pdf = TRUE,
                            plot_pairs_name = "pairs_plot.1828",
                            sup_xy = TRUE,
                            plot_xy_save_pdf = FALSE,
                            plot_xy_name = "xy_plot.1828",
                            gelman = TRUE,
                            heidel = FALSE,
                            geweke = TRUE,
                            diag_save = TRUE,
                            diag_name = "diagnostics.1828",
                            indiv_effect = FALSE,
                            plot_post_save_png = FALSE,
                            plot_pairs_save_png = FALSE,
                            plot_xy_save_png = FALSE)

output_JAGS(jags.1828, mix, source, output_options.1828)
