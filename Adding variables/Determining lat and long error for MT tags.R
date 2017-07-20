library(plyr)

setwd("C:\\R_work\\Bayesian movement model\\Thresher_movement\\data\\Raw_data\\Tim and Dan thresher data limited")
dans <- read.csv("CorrectedTracks.csv")


# Find avg lat and long error for each individual
Basic <- ddply(dans,.(TagID),summarise,
               N = length(PointID),
               Lat.error = mean(LatVariance),
               Long.error = mean(LonVariance))

# Across individual lat and long error
All.lat.error <- mean(Basic$Lat.error)
All.long.error <- mean(Basic$Long.error)

# Convert errors from degrees to meters to compare with WC errors
dans$Lat.error.m <- All.lat.error * 110887
dans$Long.error.m <- All.long.error * 94493

