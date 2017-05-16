library(gamm4)

# Get thresher data
setwd("C:/R_work/Bayesian movement model/Thresher_movement/data/Worked_data")
load("Thresher_data_Season_ElNino_Moon_normalized_FL_z_sst_chl.rdata")

# Split all the seasons into their own columns

#### GAMM ####
gamm1.0 <- gamm4(z ~ chl + sst + FL + NPGO_Index + Spring + Summer + Winter + Sex + Moon, 
                 random = ~(1|ptt), 
                 data = Original, 
                 family = binomial(link = "logit"))

# Basic model summary to check if coefficient estimates are simalar to my JAGS and the other mixing models (glmmer, PQL)
summary(gamm1.0$gam)
gamm1.0$gam$coefficients

# DF in the anova test help to evaluate weather terms can be treated as linear (i.e. if DF = 1)
anova(gamm1.0$gam)

