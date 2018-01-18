# Model 1: Effect of time, sea otter index, and sediment type
#  Models 1a - 1g: evaluate combimnations of linear and quadratic effects for time, 
#  otter index and sediment type (and models without the latter 2 vars)
#
# Set current directory
dirname = "G:/My Drive/Papers/SEAK_eelgrass" # Set this to desired directory
setwd(dirname)
## -------- Load packages --------
library(lattice)
library(coda)
library(boot)
library(parallel)
library(rjags)
library(runjags)
library(loo)
library(readxl)
## -------- Read and prepare data for Bayesian modeling --------
Dat <- read_excel("eelgrass_data.xlsx")
attach(Dat)
#
# Dependent observed variable: this should be whatever numeric variable you 
# wish to analyze (e.g. above ground biomass, shoot biomass, etc.), as raw data
Obs = as.numeric(ag_mass)
# select only oberved variables that are not NA
ii = which(!is.na(Obs))
Obs = Obs[ii]
# Variables to use as independent or identifiwer variables
Ottindx = as.numeric(sea_otter_index)[ii]
Sedtype = as.numeric(sed1_no)[ii]
Julianday = as.numeric(date_julian)[ii]
Sitetxt = site[ii]
Quad = as.numeric(quadrat)[ii]
detach(Dat) 
#
Sitelist = unique(Sitetxt)
Sitenum = as.numeric(as.factor(Sitetxt))
Sites = seq(1,max(Sitenum))
Nsites = max(Sitenum)
Nquads = max(Quad)
Nobs = length(Obs)
Minday = min(Julianday)
Dayindx = Julianday - Minday + 1
MeanDay = round(mean(Dayindx),0)
# Model comparison data frame
dfWIAC = data.frame(Model = c("Model_1a","Model_1b","Model_1c","Model_1d","Model_1e",
                              "Model_1f","Model_1g"), WAIC = numeric(length=7))
cores = detectCores()
ncore = min(20,cores-1)

# -- Model 1a - linear time ------------------------
jagsfile = 'Eelgrass_1a.jags'
# savename='EelgrassM1a_Results.Rdata'
jags.data <- list(Nsites = Nsites, Nobs = Nobs, Obs = Obs, Site = Sitenum,
                  Day = Dayindx) #, Ott = Ottindx, Sed = Sedtype Nquads = Nquads, Quad = Quad
#
inits <- function() list(sigS = runif(1, .1, 1), sigO = runif(1, .1, 1)) 
#
params <- c("sigS","sigO","beta0","beta1","eps","loglik") 
#
nsamples <- 500
nt <- 5
nb <- 5000
nc <- ncore
# For parallel (comment out for serial)
cl <- makeCluster(ncore)
# Call JAGS from R 
out <- run.jags(data = jags.data, 
                inits = inits, 
                monitor = params, 
                model = jagsfile, 
                n.chains = nc, 
                thin = nt, 
                sample = nsamples, 
                burnin = nb,
                method="rjparallel", cl=cl)
#
post = rbind(out$mcmc[[1]], out$mcmc[[2]])
for (i in 3:nc){
  post = rbind(post, out$mcmc[[i]])
}
# Calculate WAIC
mc_ll <- post[,paste0("loglik[",1:Nobs,"]")]
WAIC1 = waic(mc_ll)
dfWIAC$WAIC[1] = WAIC1$waic
out1 = out

# -- Model 1b - linear time + linear ott------------------------
jagsfile = 'Eelgrass_1b.jags'
# savename='EelgrassM1b_Results.Rdata'
jags.data <- list(Nsites = Nsites, Nobs = Nobs, Obs = Obs, Site = Sitenum,
                  Day = Dayindx, Ott = Ottindx) #, Sed = Sedtype Nquads = Nquads, Quad = Quad
#
inits <- function() list(sigS = runif(1, .1, 1), sigO = runif(1, .1, 1)) 
#
params <- c("sigS","sigO","beta0","beta1","beta2","eps","loglik") 
#
nsamples <- 500
nt <- 5
nb <- 5000
nc <- ncore
# For parallel (comment out for serial)
# cl <- makeCluster(ncore)
# Call JAGS from R 
out <- run.jags(data = jags.data, 
                inits = inits, 
                monitor = params, 
                model = jagsfile, 
                n.chains = nc, 
                thin = nt, 
                sample = nsamples, 
                burnin = nb,
                method="rjparallel", cl=cl)
#
post = rbind(out$mcmc[[1]], out$mcmc[[2]])
for (i in 3:nc){
  post = rbind(post, out$mcmc[[i]])
}
# Calculate WAIC
mc_ll <- post[,paste0("loglik[",1:Nobs,"]")]
WAIC2 = waic(mc_ll)
dfWIAC$WAIC[2] = WAIC2$waic
out2 = out

# -- Model 1c - linear time + linear ott + linear sed------------------------
jagsfile = 'Eelgrass_1c.jags'
# savename='EelgrassM1c_Results.Rdata'
jags.data <- list(Nsites = Nsites, Nobs = Nobs, Obs = Obs, Site = Sitenum,
                  Day = Dayindx, Ott = Ottindx, Sed = Sedtype) #, Sed = Sedtype Nquads = Nquads, Quad = Quad
#
inits <- function() list(sigS = runif(1, .1, 1), sigO = runif(1, .1, 1)) 
#
params <- c("sigS","sigO","beta0","beta1","beta2","beta3","eps","loglik") 
#
nsamples <- 500
nt <- 5
nb <- 5000
nc <- ncore
# For parallel (comment out for serial)
# cl <- makeCluster(ncore)
# Call JAGS from R 
out <- run.jags(data = jags.data, 
                inits = inits, 
                monitor = params, 
                model = jagsfile, 
                n.chains = nc, 
                thin = nt, 
                sample = nsamples, 
                burnin = nb,
                method="rjparallel", cl=cl)
#
post = rbind(out$mcmc[[1]], out$mcmc[[2]])
for (i in 3:nc){
  post = rbind(post, out$mcmc[[i]])
}
# Calculate WAIC
mc_ll <- post[,paste0("loglik[",1:Nobs,"]")]
WAIC3 = waic(mc_ll)
dfWIAC$WAIC[3] = WAIC3$waic
out3 = out

# -- Model 1d - linear time + quadratic ott ------------------------
jagsfile = 'Eelgrass_1d.jags'
# savename='EelgrassM1d_Results.Rdata'
jags.data <- list(Nsites = Nsites, Nobs = Nobs, Obs = Obs, Site = Sitenum,
                  Day = Dayindx, Ott = Ottindx) #, Sed = Sedtype Nquads = Nquads, Quad = Quad
#
inits <- function() list(sigS = runif(1, .1, 1), sigO = runif(1, .1, 1)) 
#
params <- c("sigS","sigO","beta0","beta1","beta2","beta2b","eps","loglik") 
#
nsamples <- 500
nt <- 5
nb <- 5000
nc <- ncore
# For parallel (comment out for serial)
# cl <- makeCluster(ncore)
# Call JAGS from R 
out <- run.jags(data = jags.data, 
                inits = inits, 
                monitor = params, 
                model = jagsfile, 
                n.chains = nc, 
                thin = nt, 
                sample = nsamples, 
                burnin = nb,
                method="rjparallel", cl=cl)
#
post = rbind(out$mcmc[[1]], out$mcmc[[2]])
for (i in 3:nc){
  post = rbind(post, out$mcmc[[i]])
}
# Calculate WAIC
mc_ll <- post[,paste0("loglik[",1:Nobs,"]")]
WAIC4 = waic(mc_ll)
dfWIAC$WAIC[4] = WAIC4$waic
out4 = out

# -- Model 1e - linear time + quadratic otter + linear sediment type ------------------------
jagsfile = 'Eelgrass_1e.jags'
# savename='EelgrassM1e_Results.Rdata'
jags.data <- list(Nsites = Nsites, Nobs = Nobs, Obs = Obs, Site = Sitenum,
                  Day = Dayindx, Ott = Ottindx, Sed = Sedtype) # Nquads = Nquads, Quad = Quad
#
inits <- function() list(sigS = runif(1, .1, 1), sigO = runif(1, .1, 1)) 
#
params <- c("sigS","sigO","beta0","beta1","beta2","beta2b","beta3","eps","loglik") 
#
nsamples <- 500
nt <- 5
nb <- 5000
nc <- ncore
# For parallel (comment out for serial)
# cl <- makeCluster(ncore)
# Call JAGS from R
out <- run.jags(data = jags.data, 
                inits = inits, 
                monitor = params, 
                model = jagsfile, 
                n.chains = nc, 
                thin = nt, 
                sample = nsamples, 
                burnin = nb,
                method="rjparallel", cl=cl)
#
post = rbind(out$mcmc[[1]], out$mcmc[[2]])
for (i in 3:nc){
  post = rbind(post, out$mcmc[[i]])
}
# Calculate WAIC
mc_ll <- post[,paste0("loglik[",1:Nobs,"]")]
WAIC5 = waic(mc_ll)
dfWIAC$WAIC[5] = WAIC5$waic
out5 = out

# -- Model 1f - quadratic time + linear otter ------------------------
jagsfile = 'Eelgrass_1f.jags'
# savename='EelgrassM1f_Results.Rdata'
jags.data <- list(Nsites = Nsites, Nobs = Nobs, Obs = Obs, Site = Sitenum,
                  Day = Dayindx, Ott = Ottindx) # , Sed = Sedtype, Nquads = Nquads, Quad = Quad
#
inits <- function() list(sigS = runif(1, .1, 1), sigO = runif(1, .1, 1)) 
#
params <- c("sigS","sigO","beta0","beta1","beta1b","beta2","eps","loglik") 
#
nsamples <- 500
nt <- 5
nb <- 5000
nc <- ncore
# For parallel (comment out for serial)
#cl <- makeCluster(ncore)
# Call JAGS from R
out <- run.jags(data = jags.data, 
                inits = inits, 
                monitor = params, 
                model = jagsfile, 
                n.chains = nc, 
                thin = nt, 
                sample = nsamples, 
                burnin = nb,
                method="rjparallel", cl=cl)
#
post = rbind(out$mcmc[[1]], out$mcmc[[2]])
for (i in 3:nc){
  post = rbind(post, out$mcmc[[i]])
}
# Calculate WAIC
mc_ll <- post[,paste0("loglik[",1:Nobs,"]")]
WAIC6 = waic(mc_ll)
dfWIAC$WAIC[6] = WAIC6$waic
out6 = out

# -- Model 1g - quadratic time + quadratic otter ------------------------
jagsfile = 'Eelgrass_1g.jags'
# savename='EelgrassM1g_Results.Rdata'
jags.data <- list(Nsites = Nsites, Nobs = Nobs, Obs = Obs, Site = Sitenum,
                  Day = Dayindx, Ott = Ottindx) # , Sed = Sedtype, Nquads = Nquads, Quad = Quad
#
inits <- function() list(sigS = runif(1, .1, 1), sigO = runif(1, .1, 1)) 
#
params <- c("sigS","sigO","beta0","beta1","beta1b","beta2","beta2b","eps","loglik") 
#
nsamples <- 500
nt <- 5
nb <- 5000
nc <- ncore
# For parallel (comment out for serial)
#cl <- makeCluster(ncore)
# Call JAGS from R
out <- run.jags(data = jags.data, 
                inits = inits, 
                monitor = params, 
                model = jagsfile, 
                n.chains = nc, 
                thin = nt, 
                sample = nsamples, 
                burnin = nb,
                method="rjparallel", cl=cl)
#
post = rbind(out$mcmc[[1]], out$mcmc[[2]])
for (i in 3:nc){
  post = rbind(post, out$mcmc[[i]])
}
# Calculate WAIC
mc_ll <- post[,paste0("loglik[",1:Nobs,"]")]
WAIC7 = waic(mc_ll)
dfWIAC$WAIC[7] = WAIC7$waic
out7 = out

## --- Compare Models --------------------------------------------
stopCluster(cl = cl)
rm(out)

modranks = order(dfWIAC$WAIC)
comptab = print(compare(WAIC1, WAIC2, WAIC3, WAIC4, WAIC5, WAIC6, WAIC7), digits = 3)
# Select best model
eval(parse(text=paste0("out = out",modranks[1]))) 
post = rbind(out$mcmc[[1]], out$mcmc[[2]])
for (i in 3:nc){
  post = rbind(post, out$mcmc[[i]])
}
reps = dim(post)[1]
sumout = add.summary(out, c("sig","beta"))
sumstats = sumout$summaries
vn = row.names(sumstats)
#
# Diagnostic plots
plot(out, c('trace','histogram'),
     vars=c("sig","beta"))

# Create dataframes for plotting fixed effects: assume Model1g

library(ggplot2)

mind = min(Dayindx)
maxd = max(Dayindx)
Julday = numeric(length=length(seq(mind:maxd)))
Predict = numeric(length=length(seq(mind:maxd)))
Lwr = numeric(length=length(seq(mind:maxd)))
Upr = numeric(length=length(seq(mind:maxd)))
sig = post[,which(vn=='sigO')]
bt0 = post[,which(vn=='beta0')]
bt1 = post[,which(vn=='beta1')]
bt1b = post[,which(vn=='beta1b')]
bt2 = post[,which(vn=='beta2')]
bt2b = post[,which(vn=='beta2b')]
for (d in mind:maxd){
  Julday[d] = d-1+Minday
  Mu = bt0 + bt1*d + bt1b*d^2
  Predict[d] = mean(exp(Mu + sig^2/2))
  Lwr[d] = as.numeric(quantile(exp(Mu + sig^2/2),.025))
  Upr[d] = as.numeric(quantile(exp(Mu + sig^2/2),.975))
} 
dfDay = data.frame(Julianday=Julday,Mean=Predict,Lwr=Lwr,Upr=Upr)
dfData = data.frame(x=Julianday,y=Obs)

p1 <- ggplot(dfDay, aes(x = Julianday, y = Mean)) + 
  geom_ribbon(aes(ymin = Lwr, ymax = Upr, fill = 1), alpha = 0.2) + 
  geom_line(aes(colour = 1),size = 1) +
  # geom_point(data = dfData, aes(x=x, y=y))+
  xlab('Julian Day') + 
  ylab('Mean Predicted Biomass') +
  theme(legend.position="none")
print(p1)

Ottvals = seq(min(Ottindx), max(Ottindx), length.out = 100)
Predict = numeric(length=100)
Lwr = numeric(length=100)
Upr = numeric(length=100)
for (o in 1:100){
  Mu = bt0 + bt1*MeanDay + bt1b*MeanDay^2 + bt2*Ottvals[o] + bt2b*Ottvals[o]^2
  Predict[o] = mean(exp(Mu + sig^2/2))
  Lwr[o] = as.numeric(quantile(exp(Mu + sig^2/2),.025))
  Upr[o] = as.numeric(quantile(exp(Mu + sig^2/2),.975))
} 
dfOtt = data.frame(OtterIndex=Ottvals,Mean=Predict,Lwr=Lwr,Upr=Upr)
p2 <- ggplot(dfOtt, aes(x = OtterIndex, y = Mean)) + 
  geom_ribbon(aes(ymin = Lwr, ymax = Upr, fill = 1), alpha = 0.2) + 
  geom_line(aes(colour = 1),size = 1) +
  xlab('Sea Otter Index') + 
  ylab('Mean Predicted Biomass') +
  theme(legend.position="none")
print(p2)

save(list = ls(all.names = TRUE),file='Model1_Results.rdata')
