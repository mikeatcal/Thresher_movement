library(rjags)
library(ggmcmc)
library(beepr)

setwd("C:\\R_work\\Bayesian movement model\\Thresher_movement\\data\\Results\\Bayes model")
load("model.results.3720.rdata")

# Get the basic summary data
summary(draws1.0)

# Plot trace and density plots of all betas
plot(draws1.0); beep(sound = 4, expr = NULL) #Sound for fun

# Plot just the desnity plots without the trace plots
plot(draws1.0, trace = FALSE, density = TRUE)


# Just a quick look to see if parameters are correlated
chains <- ggs(draws1.0)
ggs_crosscorrelation(chains)

