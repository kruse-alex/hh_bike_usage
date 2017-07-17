#############################################################################################################################################
# PACKAGES
#############################################################################################################################################

# setwd
setwd("C:/Users/Alex/Documents/R/bike_sharing-master/fahrradpegel/")

# load packages
require(dplyr)
require(leaflet)
require(rgdal)
require(RColorBrewer)
require(ggplot2)
require(reshape2)

# scaling function
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#############################################################################################################################################
# GET DATA
#############################################################################################################################################

# import data
pegel = read.csv("messung_2011-2016.csv", header = T, sep = ",", encoding = "UTF-8")
colnames(pegel) = gsub("X","",colnames(pegel))

# load hamburg shape for map
hhshape = readOGR(dsn = ".", layer = "HH_ALKIS_Landesgrenze")
    
#############################################################################################################################################
# MAP MEASURE POINTS WITH DATA FROM 2016
#############################################################################################################################################

# create map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 9.992924, lat = 53.55100, zoom = 12) %>%
           addPolygons(data = hhshape, stroke = T, smoothFactor = 0.05, fillOpacity = 0.05, color = "red", weight = 1, layerId = "notfoo") %>%
           addCircleMarkers(lng = pegel$lon, lat = pegel$lat, fillOpacity = 100, color = "red", stroke = F, radius = 3)

#############################################################################################################################################
# PLOT SUMMARIZED BIKE RIDE COUNTS OVER ALL MEASURING STATIONS FOR EACH YEAR
#############################################################################################################################################

# replace NAs with mean values
pegel_long = pegel[3:8]
pegel_long[1][is.na(pegel_long[1])] = mean(pegel_long[1][!is.na(pegel_long[1])])
pegel_long[2][is.na(pegel_long[2])] = mean(pegel_long[2][!is.na(pegel_long[2])])

# process data for plot
pegel_long1 = as.data.frame(colSums(pegel_long))
pegel_long1$year = 2011:2016
colnames(pegel_long1) = c("Gezählte Fahrräder","Jahr")

# plot data
ggplot(data=pegel_long1, aes(x=Jahr, y=`Gezählte Fahrräder`)) +
  geom_line()

#############################################################################################################################################
# PLOT BIKE RIDE COUNTS FROM ALL MEASUREING STATIONS FOR EACH YEAR
#############################################################################################################################################

# add station id
pegel_long$station = 1:nrow(pegel_long)

# prepare plot
pegel_long2 = melt(pegel_long, id="station")
colnames(pegel_long2) = c("Station","Jahr","Gezählte Fahrräder")

# plot
ggplot(data=pegel_long2, aes(x=Jahr, y=`Gezählte Fahrräder`, group = 1)) +
  geom_line() +
  facet_wrap(~as.factor(pegel_long2$Station), scales = "free_y", ncol = 5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#############################################################################################################################################
# OVERLAY IN ONE PLOT WITH MEAN LINE
#############################################################################################################################################

# scale data
pegel_long$station = NULL
for(i in 1:nrow(pegel_long)){
  pegel_long[i,] = range01(pegel_long[i,])
}

# add station id
pegel_long$station = 1:nrow(pegel_long)

# prepare plot
pegel_long3 = melt(pegel_long, id="station")
colnames(pegel_long3) = c("Station","Year","BikeCount")

# plot
ggplot(data=pegel_long3, aes(x=Year, y=BikeCount, group=as.factor(Station))) +
  geom_line() +
  geom_smooth(aes(x=Year, y=BikeCount, group=1), se = F) +
  ggtitle("38 Official Measuring Stations in Hamburg (Overlayed with blue Meanline)") +
  ylab("BikeCount (Scaled)")
