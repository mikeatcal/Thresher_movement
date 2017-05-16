rm(list = ls())
library(rpart)
library(randomForest)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(rfPermute)
library(cluster)

# Get thresher data
setwd("C:/R_work/Bayesian movement model/Thresher_movement/data/Worked_data")
load("Thresher_data_Season_ElNino_Moon_normalized_FL_z_sst_chl.rdata")

# Change colnames to matach with thresher data
#colnames(Original)[18] <- "NPGO_Index"
#colnames(Original)[16] <- "MEI_Index"

# Plot data to see what it looks like
ggplot()+
  geom_point(data=Original, aes(x=Lon, y=Lat, color=as.factor(z)))

# Remove cols that should not be in random forrest
Original$ptt <- Original$Lon <- Original$Lon_Ori <- Original$Lat <- Original$dt <- Original$date <- Original$Spring <- Original$Summer <- Original$Fall <- Original$Winter <- Original$Month <- Original$Day <- Original$Year <- Original$lc <- Original$FL_real <- Original$Lat.error.m <- Original$Lat.error.deg <- Original$Lon.error.m <- Original$Lon.error.deg <- Original$missing.chl <- Original$missing.sst <- NULL

# Fill na vlaues in sst and chl from last observation
library(zoo)
Original$chl <- na.locf(Original$chl, fromLast = TRUE)
Original$sst <- na.locf(Original$sst, fromLast = TRUE)

# Remove other two El Nino indices
#Original$NPGO_Index <- Original$PDO_Index <- NULL

# Remove chl becuase it has perfect correlation
#Original$chl <- NULL

####----------------------------------------------Clara Data--------------------------------------------------####
# Use this code for data compression, i.e. finding representive data centrodies and then run random forest on them
#clara example (in 'cluster' package)
#x <- Original
#n <- 1000
#i <- clara(x, k = n, stand = TRUE, samples = 200,
#           sampsize = min(n + 2, nrow(x)), medoids.x = FALSE,
#           rngR = TRUE, pamLike = TRUE)$i.med
#orig.x[i, ]
#rep.data <- Original[i, ]

#table(rep.data$z)
#setwd("C:\\R_work\\Thresher_behavior\\Random forrest\\clara.data")
#rep.data <- read.csv("Calara.picked.data.k.1000.csv", header=T)
#rep.data$X <- NULL  #Remove the X col
#rep.data$SeasonNum <- as.factor(rep.data$SeasonNum)
#rep.data$Res.Tran.Fixed <- as.factor(rep.data$Res.Tran.Fixed)

####------------------------------------------Basic purmuted model--------------------------------------------####
    Start <- Sys.time()
rp <- rfPermute(z ~ ., Original, sampsize = c(500,500), replace = FALSE, ntree = 10000, nrep = 100, a=0.1)
    End <- Sys.time() # Just so I know how long the model was run
    Start - End
plot(rp.importance(rp))
print(rp)

# With only highest enviornmental index
Original_only1 <- Original
Original_only1$MEI_Index <- Original_only1$PDO_Index <- NULL
Start <- Sys.time()
rp2 <- rfPermute(z ~ ., Original_only1, sampsize = c(500,500), replace = FALSE, ntree = 10000, nrep = 100, a=0.1)
End <- Sys.time() # Just so I know how long the model was run
Start - End
plot(rp.importance(rp2))
print(rp2)

setwd("C:\\R_work\\Bayesian movement model\\Thresher_movement\\data\\Results\\Random Forest")
#save.image(file = "Threshers_Random_Forrest_with_chl_sst_Results.RData")
#save.image(file = "Mako_Random_Forrest_with_chl_sst_proper_error_Results_OnlyMEI_no_chl.RData")
#load(file = "Threshers_Random_Forrest_with_chl_sst_Results_OnlyNPGO.RData")
# Load the below file to get all El Nino indices
#save.image(file = "Mako_Random_Forrest_with_chl_sst_proper_error_Results.RData")
#load(file = "Mako_Random_Forrest_with_chl_sst_proper_error_Results.RData")
#save.image(file = "Mako_Random_Forrest_with_sst_proper_error_Results_no_chl.RData")
load(file = "Threshers_Random_Forrest_with_chl_sst_Results.RData")

#### Plot with just one El nino index####
# Plot just the Gini Importance Scores
Gini_only <- as.data.frame(rp$importance[,4])
Gini_only[,2] <- rp.importance(rp)[,8]
Gini_only[,3] <- c("Sex", "Season", "FL", "MEI_Index",  "Moon", "SST")
Gini_only[,4] <- c("Red", "Red", "Red", "Red", "Black", "Black")
colnames(Gini_only) <- c("Importance", "p_val", "Parameter", "Color")


Gini_only <- within(Gini_only, 
                    Parameter <- factor(Parameter, 
                                      levels=c("Sex", "Season", "Moon","MEI_Index", "SST", "FL")))

levels(Gini_only$Parameter)

ggplot(Gini_only, aes(Parameter, Importance, fill=Gini_only$Color))+
  geom_bar(stat = "identity")+
  coord_flip()+ 
  guides(fill=FALSE)+
  theme_bw()+ 
  scale_fill_manual(values=c("Black", "Red"))+
  theme(text = element_text(size=30), axis.title.y = element_blank())
#ggsave("RF_with_sst_1_index.tiff", width = 7, height = 5)

#### Plot with all the El nino indices####
# Plot just the Gini Importance Scores
Gini_only <- as.data.frame(rp$importance[,4])
Gini_only[,2] <- rp.importance(rp)[,8]
Gini_only[,3] <- c("Sex", "Season", "FL", "NPGO_Index", "PDO_Index", "MEI_Index",  "Moon", "SST")
Gini_only[,4] <- c("Red", "Red", "Red", "Red", "Red", "Red", "Black", "Black")
colnames(Gini_only) <- c("Importance", "p_val", "Parameter", "Color")


Gini_only <- within(Gini_only, 
                    Parameter <- factor(Parameter, 
                                        levels=c("Sex", "Season", "Moon", "PDO_Index", "NPGO_Index", "MEI_Index",  "SST", "FL")))

levels(Gini_only$Parameter)

ggplot(Gini_only, aes(Parameter, Importance, fill=Gini_only$Color))+
  geom_bar(stat = "identity")+
  coord_flip()+ 
  guides(fill=FALSE)+
  theme_bw()+ 
  scale_fill_manual(values=c("Black", "Red"))+
  theme(text = element_text(size=30), axis.title.y = element_blank())
#ggsave("RF_with_sst_chl.tiff", width = 6, height = 5)

# oob trace plot
#err <- melt(rp$err.rate)
#colnames(err) <- c("tree", "type", "error")
#ggplot(err, aes(tree, error, colour = type)) + geom_line()
