#require(devtools)
#install_github('ggsubplot',"garrettgman")
#on cran so can
install.packages("ggsubplot")

require(ggplot2)
require(ggsubplot)
require(gridSVG)
require(RColorBrewer)

data(nasa)
data(casualties)
load(system.file("/extdata/map_layers.Rdata",package="ggsubplot"))

ggplot(nasa) +
  map_americas +
  geom_subplot(aes(long, lat, group = id,
                   subplot = geom_point(aes(surftemp, temperature), size = 1/4))) +
  coord_map()

## Afghan War Journal raw data
ggplot(casualties) +
  map_afghanistan +
  geom_point(aes(lon, lat, color = victim)) +
  coord_map()
## 2d bin with bar chart subplots displaying data in each region
ggplot(casualties) +
  map_afghanistan +
  geom_subplot2d(aes(lon, lat,
                     subplot = geom_bar(aes(victim, ..count.., fill = victim))),
                 bins = c(15,12), ref = NULL, width = rel(0.8)) +
  coord_map()


## A single star
one_nasa <- nasa[nasa$id == "1-1", ]
ggplot(one_nasa) +
  geom_star(aes(x = 0, y = 0, r = surftemp, angle = date,
                fill = mean(temperature)), r.zero = FALSE)
## r.zero determines whether the origin of the star should
## correspond to r = 0? If FALSE, origin corresponds to the
## lowest value of r.
## Stars in an embedded plot
ggplot(nasa) +
  map_americas +
  geom_subplot(aes(long, lat, group = id,
                   subplot = geom_star(aes(x = 0, y = 0, r = surftemp,
                                           angle = date, fill = mean(surftemp)), r.zero = FALSE))) +
  coord_map()



ggplot(casualties) +
  geom_coxcomb(aes(angle=month, fill = month))
## Coxcombs in an embedded plot
ggplot(casualties) +
  map_afghanistan +
  geom_subplot2d(aes(lon, lat,
                     subplot = geom_coxcomb(aes(angle = month, fill = month))),
                 bins = c(15, 12), ref = NULL) +
  coord_map()



## Using reference object aesthetics to display additional information
ggplot(casualties) +
  map_afghanistan +
  geom_subplot2d(aes(lon, lat,
                     subplot = geom_freqpoly(aes(date), binwidth = 13151560)),
                 bins = c(18, 14),
                 ref = ref_box(aes(fill = length(date)), alpha =0.75)) +
  coord_map()
## length(date) is a proxy for how many casualties
## occured in each region


#####from issues############################
ggplot(casualties) + 
  map_afghanistan +
  annotate("text", label = "Kabul", x = 69.1667, y = 34.453,
           color = "white", size = 3.5) +
  annotate("text", label = "Kandahar", x = 65.7053, y = 31.4978,
           color = "white", size = 3.4) +
  geom_subplot2d(aes(lon, lat,
                     subplot = geom_bar(aes(victim, ..count.., fill = victim), 
                                        color = rev(brewer.pal(5,"Blues"))[1], size = 1/4)),
                 bins = c(15,12), ref = NULL, width = rel(0.8), height = rel(1)) + 
  ggtitle("Afghan War Diary - Casualty counts by type and location") +
  coord_map() +
  scale_fill_manual(values = rev(brewer.pal(5,"Blues"))[c(1,4,2,3)]) 


ggplot(diamonds) +
  geom_subplot2d(aes(log10(carat), log10(price), 
                     subplot = geom_bar(aes(color, fill = color), position = "dodge")), 
                 bins = c(10, 14), y_scale = free, height.adjust = 0.8,
                 width.adjust = 0.8, ref = ref_box(aes(color = length(color)))) +
  scale_color_gradient("Total\ncount", low = "grey70", high = "black") +
  ggtitle("Diamonds, carat vs. price")

ggplot(diamonds) +
  geom_subplot2d(aes(log10(carat), log10(price), 
                     subplot = geom_bar(aes(color, fill = color), position = "dodge")), 
                 bins = c(10, 14), height.adjust = 0.8, #y_scale = free, 
                 width.adjust = 0.8, ref = ref_box(aes(color = length(color)))) +
  scale_color_gradient("Total\ncount", low = "grey70", high = "black") +
  ggtitle("Diamonds, carat vs. price")

ggplot(nasa) + 
  map_americas + 
  geom_subplot(aes(long, lat, group = id, 
                   subplot = geom_star(aes(r = surftemp, angle = date, 
                                           fill = mean(surftemp)), r.zero = FALSE))) + 
  scale_fill_gradient("Average\nTemperature (F)   ", 
                      breaks = c(55,60,65,70,75,80), guide = guide_colorbar()) +
  ggtitle("Surface temperature fluctuations 1995 - 2001") +
  coord_map()


ggplot(nasa) + 
  geom_subplot(aes(min(surftemp), max(surftemp), group = id, 
                   subplot = geom_star(aes(r = surftemp, angle = date, 
                                           fill = mean(surftemp)), r.zero = FALSE)), width = 2, height = 2) + 
  scale_fill_gradient("Average\nTemperature (F)   ", 
                      breaks = c(55,60,65,70,75,80), guide = guide_colorbar()) +
  scale_x_continuous("Minimum temperature") +
  scale_y_continuous("Maximum temperature") +
  ggtitle("Temperature extremes and seasonality\nby region 1995 - 2001") +
  coord_fixed()




####from hadley paper ####################################

ggplot(diamonds) +
  geom_subplot2d(aes(carat, price,
                     subplot = geom_bar(aes(color, fill = color),
                                        position = "dodge")), bins = c(10, 14), y_scale = rescale_01,
                 height.adjust = 0.8, width.adjust = 0.8,
                 ref = ref_box(aes(color = length(color)))) +
  scale_color_gradient("Total\ncount", low = "grey70",
                       high = "black")


grid.export("diamonds.svg")





############new CRAN package###################################
one_nasa <- nasa[nasa$id == "1-1", ]
ggplot(one_nasa) +
  geom_star(aes(x = 0, y = 0, r = surftemp, angle = date,
                fill = mean(temperature)), r.zero = FALSE)
## r.zero determines whether the origin of the star should
## correspond to r = 0? If FALSE, origin corresponds to the
## lowest value of r.
## Stars in an embedded plot
ggplot(nasa) +
  map_americas +
  geom_subplot(aes(long, lat, group = id,
                   subplot = geom_star(aes(x = 0, y = 0, r = surftemp,
                                           angle = date, fill = mean(surftemp)), r.zero = FALSE))) +
  coord_map()
## A single coxcomb
ggplot(casualties) +
  geom_coxcomb(aes(angle = month, fill = month))
## Coxcombs in an embedded plot
ggplot(casualties) +
  map_afghanistan +
  geom_subplot2d(aes(lon, lat,
                     subplot = geom_coxcomb(aes(angle = month, fill = month))),
                 bins = c(15, 12), ref = NULL) +
  coord_map()



library(ggsubplot)
library(ggplot2)
library(maps)
library(plyr)

#Get world map info
world_map <- map_data("world")

#Create a base plot
p <- ggplot()  + geom_polygon(data=world_map,aes(x=long, y=lat,group=group))

# Calculate the mean longitude and latitude per region, these will be the coördinates where the plots will be placed, so you can tweak them where needed.
# Create simulation data of the age distribution per region and merge the two.

centres <- ddply(world_map,.(region),summarize,long=mean(long),lat=mean(lat))
mycat <- cut(runif(1000), c(0, 0.1, 0.3, 0.6, 1), labels=FALSE) 
mycat <- as.factor(mycat)
age <- factor(mycat,labels=c("<15","15-30","20-60",">60"))
simdat <- merge(centres ,age)
colnames(simdat) <- c( "region","long","lat","Age" )

# Select the countries where you want a subplot for and plot
simdat2 <- subset(simdat, region %in% c("USA","China","USSR","Brazil", "Australia"))
(testplot <- p+geom_subplot2d(aes(long, lat, subplot = geom_bar(aes(Age, ..count.., fill = Age))), bins = c(15,12), ref = NULL, width = rel(0.8), data = simdat2))
p