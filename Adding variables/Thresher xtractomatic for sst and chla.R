# Load libraries
library(ncdf4)
library(curl)
library("xtractomatic")
require(ggplot2)
require(mapdata)

# Set the wd to wherever these files are
setwd("C:/R_work/Bayesian movement model/Thresher_movement/data/Worked_data")
#load("Thresher_data_Season_ElNino_Moon_normalized_FL_and_z.RData")
load("Thresher_data_Season_ElNino_Moon_normalized_FL_and_z.RData")

# Average error anound points assumed for ERDDAP data pulls
Original$lc <- as.factor(2)

# Calculate error based on Vincent et al. 2002 for each location quality
# Original$Lat.error.m <- ordered(Original$lc,
#                                 levels = c("D", 3, 2, 1, 0, "A", "B"),
#                                 labels = c(10, 326,511,1265,4618,5373,15535))
# Original$Lon.error.m <- ordered(Original$lc,
#                                 levels = c("D", 3, 2, 1, 0, "A", "B"),
#                                 labels = c(10, 742,1355,3498,10551,10393,41219))
# Fixed error around each point as mean of lat and long error from Vincent et al. 2002, not including deployment error
 Original$Lat.error.m <- ordered(Original$lc,
                                 levels = c("D", 3, 2, 1, 0, "A", "B"),
                                 labels = c(4605,4605,4605,4605,4605,4605,4605))
 Original$Lon.error.m <- ordered(Original$lc,
                                 levels = c("D", 3, 2, 1, 0, "A", "B"),
                                 labels = c(11293,11293,11293,11293,11293,11293,11293))
Original$Lat.error.m <- as.numeric(as.character(Original$Lat.error.m))
Original$Lon.error.m <- as.numeric(as.character(Original$Lon.error.m))
# Convert meters of error into degrees of lat and long to put into xtraction code
Original$Lat.error.deg <- Original$Lat.error.m/110887
Original$Lon.error.deg <- Original$Lon.error.m/94493

# Put data in the proper format
Original$date <- as.Date(Original$dt, format='%Y/%m/%d')

#Original <- Original[0:100,]  # for running a test on a smaller dataset 

# Set up everything for xtraction
xpos <- Original$Lon
ypos <- Original$Lat
tpos <- Original$date
xlen <- Original$Lat.error.deg
ylen <- Original$Lon.error.deg

################################################ Get SST values ########################################################
# Run extraction for sst (agsstamday=Global monthly composite, gassta1day=GOES Daily Satellite SST values, jplMURSST=JPL MUR Daily Satellite SST values)
#agmsst <- xtracto(xpos,ypos,tpos,"agsstamday",xlen=xlen,ylen=ylen,verbose=TRUE)
#goes <- xtracto(xpos,ypos,tpos,"gassta1day",xlen=xlen,ylen=ylen,verbose=TRUE)
#mur <- xtracto(xpos,ypos,tpos,"jplMURSST",xlen=xlen,ylen=ylen,verbose=TRUE)  #listed as DEPRECATED now!

# Save the extracted data
#setwd("C:\\R_work\\Bayesian movement model\\Thresher_movement\\data\\ERDDAP_data")
#save(agmsst, file="SST_xtract_1m_agmsst.RData")
#save(goes, file="SST_xtract_1d_goes.RData")
#save(mur, file="SST_xtract_1d_mur.RData")

# Load the already extracted data
setwd("C:\\R_work\\Bayesian movement model\\Mako\\Adding variables to data\\ERDDAP data")
load("SST_xtract.RData")

# First combine the two dataframes (the input and the output) into one,
# so it will be easy to take into account the locations that didn’t
# retrieve a value.
#Original$sst <- mur$mean
#Original$sst <- agmsst$mean
Original$sst <- goes$mean
gtitle <- "GOES Daily Satellite SST values"
#gtitle <- "Monthy Satellite SST values"
#gtitle <- "GOES Daily Satellite SST values"

# Create a variable that shows if satellite data is missing
Original$missing.sst<- is.na(Original$sst)*1

# set limits of the map
ylim<-c(0,50)
xlim<-c(-156,-100)

# get outline data for map
w <- map_data("worldHires",ylim = ylim, xlim = xlim)

# plot sst using ggplot
sst.plot <- ggplot(Original,aes(x=Lon,y=Lat)) +
  geom_point(aes(colour=sst,shape=factor(missing.sst)),size=2.) +
  scale_shape_manual(values=c(19,1)) +
  geom_polygon(data= w,aes(x=long,y=lat,group=group), fill = "grey80") +
  theme_bw() +
  scale_colour_gradientn(colours = rev(rainbow(5)),limits=c(10,30),"SST") +
  coord_fixed(1.3,xlim = xlim, ylim = ylim) +
  ggtitle(gtitle)
sst.plot 

################################################ Get Chl-a values ########################################################
# Run extraction for Chlorophyll-a (mhchlamday=Global monthly composite, erdMWchla3day=3 day composite)
# All years
GMC <- xtracto(xpos,ypos,tpos,"mhchlamday",xlen=xlen,ylen=ylen,verbose=TRUE)

####################### Within bounds of  MODIS ############################

# Split data into old and new data since not all chl datasets cover the whole span of the tagging data
Original_boxed <- subset(Original, (Original$long)+360 >= 205 )
Original_boxed <- subset(Original_boxed, Original_boxed$lat >= 22.5 )
# For ploting replace Original with Original_boxed
#Original <- Original_boxed

# Set up everything for xtraction with new data
xpos_new <- Original_boxed$long
ypos_new <- Original_boxed$lat
tpos_new <- Original_boxed$date
xlen_new <- Original_boxed$Lat.error.deg
ylen_new <- Original_boxed$Lon.error.deg

# Run extraction for Chlorophyll-a (erdMWchla3day=3 day composite)
#modischl3d <- xtracto(xpos=xpos_new, ypos=ypos_new, tpos=tpos_new,"erdMWchla3day",xlen=xlen_new,ylen=ylen_new,verbose=TRUE)



# Save chl-a data sets
setwd("C:\\R_work\\Bayesian movement model\\Thresher_movement\\data\\ERDDAP_data")
#save(GMC, file="mhchlamday_1m.RData")
#save(viirschl, file="viirschl_new.RData")
#save(modischl3d, file="modischl3d.RData")

# Load the already extracted data
setwd("C:\\R_work\\Bayesian movement model\\Thresher_movement\\data\\ERDDAP_data")
#load("SST_xtract.RData")
load("GMC.RData")
#load("mhchlamday_1m.RData")

# First combine the two dataframes (the input and the output) into one,
# so it will be easy to take into account the locations that didn’t
# retrieve a value.
Original$chl <- GMC$mean
#Original$chl <- modischl3d$mean
gtitle <- "Monthy Satellite Chl-a values"

# Create a variable that shows if satellite data is missing
Original$missing.chl<- is.na(Original$chl)*1

# set limits of the map
ylim<-c(0,50)
xlim<-c(-156,-100)

# get outline data for map
w <- map_data("worldHires",ylim = ylim, xlim = xlim)

# plot chl using ggplot
chl.plot <- ggplot(Original,aes(x=Lon,y=Lat)) +
  geom_point(aes(colour=chl,shape=factor(missing.chl)),size=2.) +
  scale_shape_manual(values=c(19,1)) +
  geom_polygon(data= w,aes(x=long,y=lat,group=group), fill = "grey80") +
  theme_bw() +
  scale_colour_gradientn(colours = rev(rainbow(5)),limits=c(0,3),"Chl") +
  coord_fixed(1.3,xlim = xlim, ylim = ylim) +
  ggtitle(gtitle)
chl.plot 


# Save the newly created dataset with sst included to the MASTER data file
setwd("C:/R_work/Bayesian movement model/Thresher_movement/data/Worked_data")
#save(Original, file="Thresher_data_Season_ElNino_Moon_normalized_FL_z_sst_chl_larger_error.RData")
#write.csv(Original, file="Mako_data_Season_El_Nino_Moon_z_sst_chl_and_normalized.csv")
