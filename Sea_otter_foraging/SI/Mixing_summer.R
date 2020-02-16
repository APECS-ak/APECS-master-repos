########################################################################
##                Mixing Model - Season: Summer                       ##
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
mix.summer <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/season_consumer.csv"

# Load the mixture/consumer data
mix <- load_mix_data(filename=mix.summer, 
                     iso_names=c("C","N"), 
                     factors=NULL,
                     fac_random=NULL,
                     fac_nested=NULL,
                     cont_effects=NULL)

# working dir for source (prey)
source.summer <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/summer_sources.csv"

# Load the source data
source <- load_source_data(filename=source.summer,
                           source_factors=NULL, 
                           conc_dep=FALSE, 
                           data_type="means", 
                           mix)

# working dir for discrimination factors (prey)
discr.filename <- "/Users/nila/Documents/UAF/RStudio/APECS/Sea_otter_foraging/SI/prey_discrimination.csv"

# Load the discrimination/TDF data
discr <- load_discr_data(filename=discr.filename, mix)

# Make an isospace plot
plot_data(filename="isospace_plot_summer", 
          plot_save_pdf=TRUE, 
          plot_save_png=FALSE, 
          mix,source,discr)

# Calculate the convex hull area, standardized by source variance
calc_area(source=source,mix=mix,discr=discr)

# default "UNINFORMATIVE" / GENERALIST prior (alpha = 1)
#plot_prior(alpha.prior=1,source)



#Run with an informative primer
# Our 14 fecal samples were 10, 1, 0, 0, 3
mix.alpha <- c(80,39,18,1,3,5,3,10)


# Plot your informative prior
plot_prior(alpha.prior=mix.alpha,
           source=source,
           plot_save_pdf=TRUE,
           plot_save_png=FALSE,
           filename="prior_plot_inf")

# Write the JAGS model file
model_summer <- "MixSIAR_model_summer.txt"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_summer, resid_err, process_err, mix, source)


#run <- list(chainLength=200000, burn=150000, thin=50, chains=3, calcDIC=TRUE)


jags.summer <- run_model(run="test", mix, source, discr, model_summer,
                    alpha.prior = mix.alpha, resid_err, process_err)

jags.summer <- run_model(run="normal", mix, source, discr, model_summer,
                    alpha.prior = mix.alpha, resid_err, process_err)

output_options <- list(summary_save = TRUE,
                       summary_name = "summary_statistics_summer",
                       sup_post = FALSE,
                       plot_post_save_pdf = TRUE,
                       plot_post_name = "posterior_density_summer",
                       sup_pairs = FALSE,
                       plot_pairs_save_pdf = TRUE,
                       plot_pairs_name = "pairs_plot_summer",
                       sup_xy = TRUE,
                       plot_xy_save_pdf = FALSE,
                       plot_xy_name = "xy_plot_summer",
                       gelman = TRUE,
                       heidel = FALSE,
                       geweke = TRUE,
                       diag_save = TRUE,
                       diag_name = "diagnostics_summer",
                       indiv_effect = FALSE,
                       plot_post_save_png = FALSE,
                       plot_pairs_save_png = FALSE,
                       plot_xy_save_png = FALSE)

output_JAGS(jags.summer, mix, source, output_options)

save.image("SI/summer_normal.RData")

#Load image 
load(file="SI/summer_normal.RData")

library(R2jags)
attach.jags(jags.summer) # adding model

dim(p.global) # 3000, 4, 7 -> 3000 iterations of 4 seasons and 7 sources
median(p.global[,1]) #median of source 1 (clams)


#Making data frame for graph
post.summer <- data.frame(Clam = p.global[,1], Crab = p.global[,2], 
                          Cucumber = p.global[,3], Mussel = p.global[,4], 
                          Snail = p.global[,5], Star = p.global[,6], 
                          Tegula = p.global[,7], Urchin = p.global[,8])
summer <- post.summer %>% gather(source,value,1:8)


ggplot(aes(y = value, x = source), data = summer) + 
  geom_boxplot(outlier.colour = NA) +
  theme_bw() +
  xlab("Prey Catagory") +
  ylab("Diet proportion") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title=element_text(size=16))

ggsave("summer_mix.png", device = "png", path = "SI/", width = 9, 
       height = 6, units = "in", dpi = 300)
