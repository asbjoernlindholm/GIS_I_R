
###########################################################################
### installing and downloading relevant libraries 
###########################################################################

#removing ervery ting in R 

rm(list=ls())

## setting path to library 
.libPaths("D:/Hedonice Dropbox/toke panduro/R_packges")
.libPaths()

# list of relevant libraries 
pack<-c("rgdal", "classInt", "sf", "tidyverse", 
        "ggplot2", "ggspatial", "ggmap")


# installing relavnt libraries from list 
# for (i in pack){
#   install.packages(pkgs=i, dependencies=TRUE)
# }

# update.packages()


# calling libraries 
lapply(pack, require, character.only=T)


###########################################################################
### setting working directory 
###########################################################################

# defining location
data_path<-"D:/Hedonice Dropbox/toke panduro/senior/course/GIS_workshop/data"
result_path<-"D:/Hedonice Dropbox/toke panduro/senior/course/GIS_workshop/result"

# changing  directory
setwd(data_path)

# what is my working directory
getwd()

# looking in the directory folder 
dir()

## Reading & managing spatial data in R 

# reading in shapefile as an sf object 
Nord_jylland <- st_read("Nord_jylland.shp")

# checking the class of the Nord_jylland object
class(Nord_jylland)

# checking the projection 
st_crs(Nord_jylland)

# let's remove the projection 
Nord_jylland <- st_set_crs(Nord_jylland, NA)

# setting the projection using the EPSG code 
Nord_jylland <- st_set_crs(Nord_jylland, 25832) # Example with EPSG:25832 

# setting the projection using the proj4string 
proj_string <- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
Nord_jylland <- st_set_crs(Nord_jylland, proj_string)

# plotting the Nord_jylland 
plot(Nord_jylland["ID_NR"])

#A key thing to note about sf objects is their versatility: they can be manipulated much like conventional data frames or tibbles in R. This makes the transition from standard data wrangling to spatial data manipulation rather seamless.

# creating a summary of the attribute table of the shapefile 
summary(Nord_jylland)

# looking into the column names of the dataframe 
names(Nord_jylland)

## looking at a specific column 
Nord_jylland$ID_NR

## looking into column type 
str(Nord_jylland$ID_NR)

## changning the column type 
str(Nord_jylland$ID_NR)


## Exercise 1 

shape_file_names<-c("coast","forest","harbour", "highway","lake","large_roads",
"municipality", "Nord_jylland","parish", "postal","postal_codes",
"railway", "sea","windturbines","Aalborg", "urban_area")   


# The solution could be something like this:

i<-"urban_area"

for(i in shape_file_names){
    shape_file_name<-paste(i,".shp", sep="")
    print(shape_file_name)
    shape_file <- st_read(shape_file_name)
    assign(i, shape_file)
  }

## Writing spatial data to hardisk 

sea_2d <- st_zm(sea, drop = TRUE, what = "ZM")
st_write(sea_2d, paste(data_path, "/test_sea.shp", sep=""))


## Exercise 2 
#try to write a wind turbine shapefile to harddisk where you have excluded all turbines lower than 46 meters. 

names(windturbines)

# Check structure of 'height' column.
str(windturbines$height)

# Filter turbines with height greater than 46.
windturbines_large <- windturbines %>% filter(height > 46)

# Summary of heights for the filtered turbines.
summary(windturbines_large$height)

# Convert spatial data to 2D.
windturbines_large_2d <- st_zm(windturbines_large, drop = TRUE, what = "ZM")

# Save 2D data to shapefile.
st_write(windturbines_large_2d, paste(data_path, "/windturbines_large.shp", sep=""))


```

## Colors in R 

#In R, visual appeal can significantly enhance the effectiveness of our plots. Colors play a crucial role in this. The code snippets below guide you through exploring different color options in R:
#- Available Colors in R: R provides a variety of in-built colors. To list them all, use colors(). This function returns all available named colors.

colors(distinct = FALSE)

#- Searching for Specific Colors: Looking for shades of a particular color, say 'red'? Use grep to search within colors().

colors()[grep("red",colors())]

#Color Schemes: R comes with predefined color schemes. Here are some of them:
  
#- rainbow(): Creates a rainbow of colors.
#- heat.colors(): Generates a heat color map.
#- terrain.colors(): Gives colors based on terrain elevation.
#- topo.colors(): Returns colors of topographic elevation.
#- cm.colors(): Creates cyan-magenta color map.

n <- 5
rainbow(n)
heat.colors(n)
terrain.colors(n)
topo.colors(n)
cm.colors(n)

#Color Wheel with Pie Chart: Visualize the color distribution using a pie chart. For instance, with the heat.colors scheme:
  
pie(rep(1, 5), col = heat.colors(5))

#Further Reading: For an in-depth exploration of graphical parameters, refer to the provided link:
#  http://www.statmethods.net/advgraphs/parameters.html 

#Remember, the choice of color can greatly affect the readability and interpretation of your plot, so choose wisely!
  
## Introduction to Plotting Spatial Data in R
#Spatial data visualization is a core aspect of geospatial data analysis. It provides insights into spatial patterns, relationships, and anomalies. In R, there are primarily two ways to visualize spatial data: using base R's plotting functions and leveraging the ggplot2 package. We will explore both methods with the example provided.

### Base R Plotting
#R's base plotting system is simple and straightforward. With spatial data, the plot() function combined with st_geometry() allows you to visualize the geometries of spatial datasets. Here's how you can use it:

plot(st_geometry(Nord_jylland), col=colors()[c(18)])
plot(st_geometry(Aalborg), col=colors()[c(18)], add=TRUE)
plot(st_geometry(sea), col=colors()[c(26)], add=TRUE)
plot(st_geometry(urban_area), col="skyblue1", add=TRUE, border=NA) 
plot(st_geometry(highway), col="gray40", add=TRUE, lwd=1)
plot(st_geometry(large_roads), col="gray60", add=TRUE, lwd=1)
plot(st_geometry(railway), col="gray40", add=TRUE, lwd=1)

#Key things to note:

#- add=TRUE ensures subsequent layers are added onto the same plot.
#- col specifies the color of the geometry.
#- border=NA can be used to remove borders from geometries.
#- lwd controls the line width for line geometries.

### ggplot2: A Layered Approach
#The ggplot2 package offers a more flexible, layered approach to data visualization. With geom_sf(), spatial data visualization becomes seamless:

bb <- st_bbox(Nord_jylland)

ggplot() +
  geom_sf(data = Nord_jylland, fill = "lightyellow", color = "black") +
  geom_sf(data = Aalborg, fill = "lightyellow", color = "black") +
  geom_sf(data = sea, fill = "blue", color = NA) +
  geom_sf(data = urban_area, fill="skyblue1") +
  geom_sf(data = highway, color="gray40", size=1) +
  geom_sf(data = large_roads, color="gray60", size=1) +
  geom_sf(data = railway, color="gray40", size=1) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]), expand = FALSE) + # Setting the extent
  theme_minimal() +
  theme(
    axis.text = element_blank(),  # Removing degree annotations
    axis.ticks = element_blank(), # Removing ticks from the axes
    axis.title = element_blank(),  # Removing axis titles
    legend.position = "none"      # Removing the legend
  )

#* Each geom_sf() represents a layer, and multiple layers are added to create a composite map.
#* fill = ... allows you to set fill colors.
#* color and size adjust the color and size of the line geometries respectively.
#* coord_sf() is crucial for setting the spatial extent of the plot.
#* theme() and its elements provide customization of the plot's aesthetics.


## Spatial visualization using color schemes 

# setting the extend of the map
bb <- st_bbox(Nord_jylland)

# creating height intervals
windturbines_large$sf_height_intervals <- cut(windturbines_large$height, 
                                       breaks = classIntervals(windturbines_large$height, 
                                                               n=5, style="quantile")$brks,
                                       labels = c("1","2","3","4","5"),
                                       include.lowest = TRUE)

# Create labels based on the intervals
breaks <- classIntervals(windturbines_large$height, n=5, style="quantile")$brks
labels <- sprintf("(%s, %s]", head(breaks, -1), tail(breaks, -1))

# correcting labels
labels<-gsub("\\[|\\]", "", labels)            
labels<-gsub("\\(|\\(", "", labels)            
labels<-gsub(", ", "-", labels)            

# creating map object 
windturbine_map<-ggplot() +
  geom_sf(data = Nord_jylland, fill = "grey90") + 
  # ... (other layers) 
  geom_sf(data = windturbines_large, aes(color = sf_height_intervals)) +
  scale_color_manual(values = terrain.colors(5),
                     name = "Hight",
                     labels = labels,
                     drop = FALSE) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]), 
           ylim = c(bb["ymin"], bb["ymax"]), 
           expand = FALSE) + # Setting the extent
  annotation_north_arrow(location = "tr") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "lightblue"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
  )

windturbine_map

## Saving maps to hardisk

#After creating compelling maps in R, there often arises the need to save them to files. This can be useful for sharing, embedding in presentations, or for publishing. R provides various ways to achieve this, both with packages like `ggplot2` and with base R functions. In this chapter, we will explore how to save your maps as image files.

#### Saving Maps with ggplot2

#The `ggsave` function from the `ggplot2` package is a versatile tool to save the plots created with `ggplot` or `geom_sf`.

#1. **Setting the Working Directory**:

setwd(result_path)


#2. **Saving as PNG**:

ggsave(filename = "wind_turbine_Aalborg.png", 
       plot = windturbine_map, 
       dpi = 200, device = "png")

#3. **Saving as JPEG**:
ggsave(filename = "wind_turbine_Aalborg.jpg", 
       plot = windturbine_map, 
       dpi = 200, device = "jpeg")

### Saving Maps with Base R
  
Base R provides a straightforward method to save plots, using a series of commands to initiate the file saving, plot the map, and then finalize the saving process.

#1. **Defining the Map Output**:
png(filename="example_2.png", width=1000, height=1000, bg=FALSE, res=200)

#2. **Creating the Map**:
plot(st_geometry(Nord_jylland), col=colors()[c(18)])

#3. **Finalizing the Saving Process**:
dev.off()

#Whether you're using `ggplot2` or base R functions, saving your spatial visualizations as image files is a straightforward process. Remember to always verify your saved images to ensure they have captured all the necessary details and aesthetics of your original plots.

## Exercise 3 
#1. change the location of the north arrow

annotation_north_arrow(location = "tl")+

#2. change the colors of the map 

geom_sf(data = Nord_jylland, fill = "grey90") + 

#3. remove the border of the sea 

geom_sf(data = urban_area, fill="skyblue1", color = NA ) +

#4. add habour to the map 

#geom_sf(data = harbour, fill="black") +

#5. increase the size of the wind turbines dots 

  geom_sf(data = windturbines, aes(color = sf_hight_intervals, size = 2)) +

#6. change the dots to squares 

geom_sf(data = windturbines, aes(color = sf_hight_intervals, shape = 22)) +
   
#7. save the map in 300 dpi to the result folder folder 

ggsave(filename = "wind_turbine_Aalborg.jpg", 
       plot = windturbine_map, 
       dpi = 300, device = "jpeg")

## Exercise 4 
# Create a map of the single male/female across the municipalities in Nordjylland.
# You get the single male and female data from the single.csv file 

setwd(data_path)

# reading in data 
singles<-read.table("singles.csv", header=T , sep=";", encoding="Latin-1")

#### Base R Solution

# looking at joining variables
str(municipality$ADM_KODE)
str(singles$kommune_kode)

# sub - str_replace (stringr) to changes

# merging data 
municipality<-merge(municipality, singles, 
                    by.x=c("ADM_KODE"), 
                    by.y=("kommune_kode"), all=FALSE)


# Assuming your 'municipality' and 'sea' are already read in and are sf objects

municipality$ratio <- municipality$male / municipality$female

# creating intervals 
breaks <- classIntervals(municipality$ratio, 
                         n=4, 
                         style="equal", 
                         digits=2)$brks
# removing too many digits
breaks <- round(breaks, 2)

# Create a factor variable based on the breaks
municipality$ratio_interval <- cut(municipality$ratio, breaks = breaks, 
                                   labels = c(1:(length(breaks)-1)), include.lowest = TRUE)

# 2. Assign colors based on those intervals
color_pal <- heat.colors(5)
colors_for_plot <- color_pal[as.numeric(municipality$ratio_interval)]

# 3. Plot your data
# Add the municipalities with their colors
plot(st_geometry(municipality), col = colors_for_plot,  main="Map of Municipality")
# add sea
plot(st_geometry(sea), col = colors()[c(26)], add=T)
# Add legend manually
legend("topright", fill=color_pal, 
       legend = sprintf("%s - %s", head(breaks, -1), tail(breaks, -1)), 
       title=" Ratio: male/female")


#### Tidyverse Solution  

# joining data 
municipality <- municipality %>%
  left_join(singles, by = c("ADM_KODE" = "kommune_kode"))

# creating ratio
municipality$ratio<-municipality$single_male/municipality$single_female

# making breaks 
breaks <- classIntervals(municipality$ratio, 
                         n=4, 
                         style="equal", 
                         digits=2)$brks
# removing too many digits 
breaks<-round(breaks, 2)

# Create a factor variable based on the breaks
municipality$ratio_interval <- cut(municipality$ratio, breaks = breaks, 
                                   labels = c(1:(length(breaks)-1)), include.lowest = TRUE)

# Plot the data using ggplot2

# removing some complexity in data 
municipality <- st_simplify(municipality, dTolerance=0.01)
# setting the extend of the data 
bb <- st_bbox(municipality)
# plotting data 
ggplot() +
  geom_sf(data = sea, fill = colors()[c(26)]) +
  geom_sf(data = municipality, aes(fill = ratio_interval))+
  scale_fill_manual(values = heat.colors(5),
                    name = "Ratio: male/female",
                    labels = sprintf("%s - %s", head(breaks, -1), tail(breaks, -1))) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]), expand = FALSE) + # Setting the extent
  theme_minimal()




