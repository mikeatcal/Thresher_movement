# Load needed libraries
library(SDMTools)
library(plotKML)
library(rgdal)
library(ggmap)
library(ggplot2)

# Set wd and load data from properly formatted rdata file
setwd("C:\\R_work\\Data\\Movement_data\\Thresher_data\\Worked_data")

load("Thresher_data_Season_Moon_and_normalized_FL.rdata")

#####----------------------------------------Read in shapefile-------------------------------------------#####
# Bring in thresher 50% kernel density Shapefile
core <- readOGR("C:\\R_work\\Bayesian movement model\\Threshers\\Kernel densities\\50 kernel",
                "50_kernel",p4s = "+proj=longlat +ellps=WGS84 +datum=WGS84")

####Convert Longs into the right format####
# Lon's are in worng format, need neg numbers!
negs <- as.matrix(coordinates(core[1,])[[1]][[1]])
negs[,1] <- (negs[,1])-360
core@lines[[1]]@Lines[[1]]@coords[] <- negs

####------------------------------ID points within polygon for Res.Tran. split----------------------------####
#define the points and polygon
pnts = rbind(cbind(x=Original$Lon, y=Original$Lat))
poly.pnts = negs

within <- pnt.in.poly(pnts, poly.pnts)

within$pip <- as.factor(ifelse(within$pip == "1", 0, 1))

Original$z <- within$pip

#write.csv(Original, "MASTER_Horizontal_Movement fixed Res.Tran.csv")

# Makre sure everything is in order
Original <- Original[order(Original$Year,Original$ptt, Original$Month, Original$Day),]

#####----------------------------------------Plot Res and Tran-------------------------------------------#####
# Define map location
myLocation <- c(min(Original$Lon), min(Original$Lat), max(Original$Lon), max(Original$Lat))
Cal_map <- get_map(location=myLocation, source="stamen", maptype="terrain-lines", crop=F)

outside <- within[which(within$pip==1),1:2]

Th_map <- ggmap(Cal_map)+
  geom_point(aes(x=Lon, y=Lat),size = 0.40 , data = Original)+
  geom_point(aes(x=x, y=y), size = 0.40, colour="purple", data = outside)+
  geom_path(aes(long,lat,group=group), data=(core), color="red", alpha=1, size=0.5)+ #Plot as polyline
  #geom_polygon(aes(long,lat,group=group), data=(core), color="red", alpha=1, size=0.5) #Plot as polygon
  labs(x = "Longitude (°)",y="Latitude (°)")
Th_map
#ggsave(Th_map, file =paste("C:/R_work/Thresher_behavior/Kernel_density/Plot core SDSU kernel with ggplot/Completed plots/Points with 50 Kernel split Res from Tran.pdf"))

setwd("C:\\R_work\\Bayesian movement model\\Threshers\\Adding variables to data")
#save(Original, file="Thresher_data_Season_Moon_normalized_FL_and_z.rdata")




