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
# Set up for parralel computing
cores = detectCores()
ncore = min(20,cores-1)
# Set JAGS params for running
nsamples <- 1000
nt <- 20
nb <- 7000
nc <- ncore
# For parallel (comment out for serial)
cl <- makeCluster(ncore)

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
WAIC = waic(mc_ll)
WAIC1a = WAIC
dfWIAC$WAIC[1] = WAIC$waic
T1 = data.frame(modID = c('WAIC1a'), model = c('1a'), WAIC = WAIC$waic, WAIC_se = WAIC$se_waic,
                deltaWAIC = c(0), deltaWAIC_se = c(0), P_comp = c(0))
# LOOIC = LOO$looic, LOOIC_se = LOO$se_looic, deltaLOOIC = c(0), deltaLOOIC_se = c(0))
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
WAIC = waic(mc_ll)
WAIC1b = WAIC
dfWIAC$WAIC[2] = WAIC$waic
T2 = data.frame(modID = c('WAIC1b'), model = c('1b'), WAIC = WAIC$waic, WAIC_se = WAIC$se_waic,
                deltaWAIC = c(0), deltaWAIC_se = c(0), P_comp = c(0))
# LOOIC = LOO$looic, LOOIC_se = LOO$se_looic, deltaLOOIC = c(0), deltaLOOIC_se = c(0))
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
WAIC = waic(mc_ll)
WAIC1c = WAIC
dfWIAC$WAIC[3] = WAIC$waic
T3 = data.frame(modID = c('WAIC1c'), model = c('1c'), WAIC = WAIC$waic, WAIC_se = WAIC$se_waic,
                deltaWAIC = c(0), deltaWAIC_se = c(0), P_comp = c(0))
# LOOIC = LOO$looic, LOOIC_se = LOO$se_looic, deltaLOOIC = c(0), deltaLOOIC_se = c(0))
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
WAIC = waic(mc_ll)
WAIC1d = WAIC
dfWIAC$WAIC[4] = WAIC$waic
T4 = data.frame(modID = c('WAIC1d'), model = c('1d'), WAIC = WAIC$waic, WAIC_se = WAIC$se_waic,
                deltaWAIC = c(0), deltaWAIC_se = c(0), P_comp = c(0))
# LOOIC = LOO$looic, LOOIC_se = LOO$se_looic, deltaLOOIC = c(0), deltaLOOIC_se = c(0))
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
WAIC = waic(mc_ll)
WAIC1e = WAIC
dfWIAC$WAIC[5] = WAIC$waic
T5 = data.frame(modID = c('WAIC1e'), model = c('1e'), WAIC = WAIC$waic, WAIC_se = WAIC$se_waic,
                deltaWAIC = c(0), deltaWAIC_se = c(0), P_comp = c(0))
# LOOIC = LOO$looic, LOOIC_se = LOO$se_looic, deltaLOOIC = c(0), deltaLOOIC_se = c(0))
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
WAIC = waic(mc_ll)
WAIC1f = WAIC
dfWIAC$WAIC[6] = WAIC$waic
T6 = data.frame(modID = c('WAIC1f'), model = c('1f'), WAIC = WAIC$waic, WAIC_se = WAIC$se_waic,
                deltaWAIC = c(0), deltaWAIC_se = c(0), P_comp = c(0))
# LOOIC = LOO$looic, LOOIC_se = LOO$se_looic, deltaLOOIC = c(0), deltaLOOIC_se = c(0))
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
WAIC = waic(mc_ll)
WAIC1g = WAIC
dfWIAC$WAIC[7] = WAIC$waic
T7 = data.frame(modID = c('WAIC1g'), model = c('1g'), WAIC = WAIC$waic, WAIC_se = WAIC$se_waic,
                deltaWAIC = c(0), deltaWAIC_se = c(0), P_comp = c(0))
# LOOIC = LOO$looic, LOOIC_se = LOO$se_looic, deltaLOOIC = c(0), deltaLOOIC_se = c(0))
out7 = out

## --- Compare Models --------------------------------------------
stopCluster(cl = cl)
rm(out)
modranks = order(dfWIAC$WAIC)
T = rbind(T1,T2,T3,T4,T5,T6,T7)
rm(T1,T2,T3,T4,T5,T6,T7)
comptab = compare(WAIC1a, WAIC1b, WAIC1c, WAIC1d, WAIC1e, WAIC1f, WAIC1g)
filelist = row.names(comptab)
for (i in 2:7){
  row = which(T$modID == filelist[i])
  comp = compare(eval(as.name(filelist[i])) , eval(as.name(filelist[1])) )
  Z_score = comp[1]/comp[2]
  P_val = 1-pnorm(Z_score)
  T$deltaWAIC[row] = comp[1]
  T$deltaWAIC_se[row] = comp[2]
  T$P_comp[row] = P_val
}
T = T[modranks,]
T$P_comp[1] = 1
T$Likelihood = exp(-0.5*T$deltaWAIC)
T$Likelihood[T$P_comp<0.1]=0 
sumlik = sum(T$Likelihood)
T$WAICwt = T$Likelihood/sumlik

# --Select best model for plots ------------------------------------------
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
