# Load libraries
library(oce)

# Set wd for thresher data to master data file in R_work
setwd("C:\\R_work\\Data\Movement_data\\Thresher_data\\Raw_data")
# Read in mako tagging data
Original <- read.csv("MASTER_Horizontal_Movement - RAW with Cartamil.csv", header=T)

################################################## Add Season to Data ##################################################
# Add a correctly formatted date and time column to the data
Original$dt <- ISOdate(year=Original$Year, month=Original$Month, day=Original$Day,
                       tz='UTC')
# This makes data redundant so remove date
Original$date <- NULL

Original$ptt <- as.factor(Original$ptt)

# Get Seasons by defineing a function based around the Solstices and Equinoxs
getSeason <- function(DATES) {
  WS <- as.Date("2008-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2008-3-20",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2008-6-20",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2008-9-22",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2008 dates (2008 used because it's a leap year, deals with Feb 29th)
  d <- as.Date(strftime(DATES, format="2008-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
Original$Season <- getSeason(Original$dt)

# Seperating Season into different factors
Original$Spring <- as.factor(as.numeric(Original$Season =="Spring"))
Original$Summer <- as.factor(as.numeric(Original$Season =="Summer"))
Original$Fall <- as.factor(as.numeric(Original$Season =="Fall"))
Original$Winter <- as.factor(as.numeric(Original$Season =="Winter"))

# Normilize fork length with z transformation
Original$FL_real <- Original$FL
Original$FL <- ((Original$FL_real - mean(Original$FL_real))/sd(Original$FL_real))
#hist(Original$FL_real)

########################################### Add Moon Phase ##################################################
# Loop assigns moon phase to each thresher location (phase given as 1=full, 0=new, 0.5=half)
for(i in 1:length(Original$dt)){
  Original$Moon[i] <- moonAngle(t=Original$dt[i], longitude=Original$long[i], 
                                latitude=Original$lat[i])$illuminatedFraction
}

setwd("C:\\R_work\\Bayesian movement model\\Threshers\\Adding variables to data")
save(Original, file="Thresher_data_Season_Moon_and_normalized_FL.RData")
#write.csv(Original, file="Mako_data_Season_El_Nino_Moon_z_and_normalized.csv")

# Find total number of unique data points, only 1 point per day
Original_1pd <-  Original[!duplicated(Original[,c('ptt','dt')]),]

