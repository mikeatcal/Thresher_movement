  # Load required libraries
  library(rjags)
  library(MASS)
  library(lme4)
  library(dplyr)
  library(gamm4)
  library(parallel)
  
  # define directories
  setwd("C:/R_work/Bayesian movement model/Thresher_movement/data/Worked_data")
  resultsDir <- "C:/R_work/Bayesian movement model/Thresher_movement/data/Results/Bayes model"

  # Set model defaults
  Nchains <- 4
  Nadapt <- 100000
  Nupdate <- 10000
  Ncoda <- 500000
  Nthin <- 10
  
  ## Define model ##
  model_string1.0 <- "model{
    B0 ~ dnorm(0, 0.1)
    B1 ~ dnorm(0, 0.1)
    B2 ~ dnorm(0, 0.1)
    B3 ~ dnorm(0, 0.1)
    B4 ~ dnorm(0, 0.1)
    B5 ~ dnorm(0, 0.1)
    B6 ~ dnorm(0, 0.1)
    tau ~ dgamma(0.1, 0.01)
    s <- 1/sqrt(tau)
      for(j in 1:N){ u[j] ~ dnorm(0, tau)
        for(i in cumul_tracks[j]:(cumul_tracks[j+1]-1)) {
          logit(mu[i]) <- B0 + (B1 * Spring[i]) + (B2 * Summer[i]) + (B3 * Winter[i]) + (B4 * FL[i]) + (B5 * Sex[i]) + (B6 * NPGO[i]) + u[j]
          z[i] ~ dbern(mu[i])
        }
      }
  }"
####----------------------------Run the model using all the different data sets-------------------------------------------####
    # Get thresher data
    load("Thresher_data_Season_ElNino_Moon_normalized_FL_z_sst_chl.rdata")
  
    #Define track lengths and number of tracks for hierarchical model structure
    # Track lengths
    tracks <- table(Original$ptt)
    tracks_2 <- c(1,tracks)
    cumul_tracks <- cumsum(tracks_2)
    
    # Define data size for title of saved file
    data.size <- length(Original$z)
    
    # Number of tracks/animals
    N <- length(unique(Original$ptt))
    
    # JAGS model
    # Bring in model and run in jags
    model1.0 <- jags.model(textConnection(model_string1.0),
                           n.chains=Nchains,
                           n.adapt=Nadapt,
                           data=list(Spring=Original$Spring,
                                     Summer=Original$Summer,
                                     Winter=Original$Winter,
                                     FL=Original$FL,
                                     Sex=Original$sex,
                                     MEI=Original$MEI_Index,
                                     z=Original$z,
                                     N=N,
                                     cumul_tracks=as.vector(cumul_tracks)),
                           inits = function ()
                           {
                             list('B0' = runif(1, 0, 1),
                                  'B1' = runif(1, 0, 1),
                                  'B2' = runif(1, 0, 1),
                                  'B3' = runif(1, 0, 1),
                                  'B4' = runif(1, 0, 1),
                                  'B5' = runif(1, 0, 1),
                                  'B6' = runif(1, 0, 1))
                           },)
    update(model1.0, Nupdate)
    draws1.0 <- coda.samples(model = model1.0,
                             n.iter = Ncoda,
                             variable.names=c("B0", "B1", "B2",
                                              "B3", "B4", "B5",
                                              "B6", "s"),
                             thin = Nthin)
    
    # Save the model results to an rdata file
    outfile <- paste("model.results", data.size, "rdata", sep = ".")
    save(draws1.0, data.size, file = file.path(resultsDir, outfile))
    

