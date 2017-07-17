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

# black theme ggplot
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#############################################################################################################################################
# GET DATA
#############################################################################################################################################

# load coords from fahrradpegel
pegel = read.csv("messung_2011-2016.csv", header = T, sep = ",", encoding = "UTF-8")
colnames(pegel) = gsub("X","",colnames(pegel))

# load hamburg shape for map
hhshape = readOGR(dsn = ".", layer = "HH_ALKIS_Landesgrenze")
    
#############################################################################################################################################
# MAP MEASURE POINTS WITH DATA FROM 2016
#############################################################################################################################################

# color palette
qpal = colorQuantile(rev(brewer.pal(4, "YlGnBu")), NULL, n = 4)
    
# create map
leaflet() %>% 
  setView(lng = 9.992924, lat = 53.55100, zoom = 12) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
           addPolygons(data = hhshape, stroke = T, smoothFactor = 0.05, fillOpacity = 0.05, color = "red", weight = 1, layerId = "notfoo") %>%
           addCircleMarkers(lng = pegel$lon, lat = pegel$lat, popup = paste("BikeCount:",pegel$`2016`), fillOpacity = 100, color = rev(brewer.pal(5, "YlGnBu")), stroke = F, radius = 3) %>%
           addLegend(position = 'bottomleft',colors = rev(brewer.pal(5, "YlGnBu")),labels = c("Very low","Low","Average","High","Very high"),title = 'Frequency')

#############################################################################################################################################
# PLOT SUMMARIZED BIKE RIDE COUNTS OVER ALL MEASUREING STATIONS FOR EACH YEAR
#############################################################################################################################################

# process missing values
pegel_long = pegel[3:8]
pegel_long[1][is.na(pegel_long[1])] = mean(pegel_long[1][!is.na(pegel_long[1])])
pegel_long[2][is.na(pegel_long[2])] = mean(pegel_long[2][!is.na(pegel_long[2])])

# aggregate stations
pegel_long1 = as.data.frame(colSums(pegel_long))
pegel_long1$year = 2011:2016
colnames(pegel_long1) = c("BikeCount","Year")

# plot data
ggplot(data=pegel_long1, aes(x=Year, y=BikeCount)) +
  geom_line() +
  ggtitle("Sum of 38 Official Measuring Stations in Hamburg")

#############################################################################################################################################
# PLOT BIKE RIDE COUNTS FROM ALL MEASUREING STATIONS FOR EACH YEAR
#############################################################################################################################################

# add station id
pegel_long$station = 1:nrow(pegel_long)

# prepare plot
pegel_long2 = melt(pegel_long, id="station")
colnames(pegel_long2) = c("Station","Year","BikeCount")

# plot
ggplot(data=pegel_long2, aes(x=Year, y=BikeCount, group = 1)) +
  geom_line() +
  facet_wrap(~as.factor(pegel_long2$Station), scales = "free_y", ncol = 5) +
  ggtitle("38 Official Measuring Stations in Hamburg") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#############################################################################################################################################
# OVERLAY IN ONE PLOT AND MEAN LINE
#############################################################################################################################################

# Scale data
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
