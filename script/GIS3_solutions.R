###########################################################################
### calling  relevant libraries 
###########################################################################

## setting path to library 
.libPaths("D:/Hedonice Dropbox/toke panduro/R_packges")
.libPaths()

# list of relevant libraries 
pack<-c("rgdal", "sf", "dplyr","classInt", "tmap", "ggplot2",
        "RColorBrewer","rgeos","spdep"
)

lapply(pack, require, character.only=T)

###########################################################################
### setting working directory 
###########################################################################

rm(list=ls())

# defining location
data_path<-"D:/Hedonice Dropbox/toke panduro/senior/course/GIS_workshop/data"
result_path<-"D:/Hedonice Dropbox/toke panduro/senior/course/GIS_workshop/result"

# changing  directory
setwd(data_path)

### Exercise 
# Make a spatial point sf object from salg.RData
# Read in the following shapefiles "windturbines","Aalborg", "postal_codes", "parish"

#### Solution:

# Set the working directory
setwd(data_path)

# Load the saved R data
load("salg.RData")

# Examine the dataset
head(salg)

# Check the dimensions of the dataset
dim(salg)

# Check the class of the dataset
class(salg)

# Convert the data.frame to an sf object
salg <- st_as_sf(salg, coords = c("coord_x", "coord_y"), crs = 25832) # 25832 is the EPSG code for "ETRS89 / UTM zone 32N"

# Names of the shapefiles
shape_file_names <- c("windturbines", "Aalborg", "postal_codes", "parish")

# Reading in shapefiles from shape_file_names
for(i in shape_file_names) {
  shape_file <- st_read(dsn = data_path, layer = i)
  assign(i, shape_file)
}


### Exercise: Selection

#* Subset the postal_codes data based on these postal code numbers: 9000, 9200, 9210, 9220, 9230, 9270, 9280, 9310, 9362, 9400, 9430
#* Use the st_crop function to select postal codes within the Aalborg polygon 

#### Solutions:

# Solution 1

numbers <- c(9000, 9200, 9210, 9220, 9230, 9270, 9280, 9310, 9362, 9400, 9430)

# Selecting postal areas based on the given postal codes
postal <- postal_codes[postal_codes$POSTCLABEL %in% numbers, ]

# Visualizing the subset
plot(st_geometry(postal_codes))
plot(st_geometry(postal), col = "red", add = TRUE)


# Solution 2 

# looking at the projection of the data 
st_crs(Aalborg)
st_crs(postal_codes)

# transforming the projection
A_crs <- st_crs(Aalborg)
postal_codes <- st_transform(postal_codes, A_crs) # adding the lake 

# Now, we'll crop the postal_codes based on the Aalborg polygon
postal_crop <- st_crop(postal_codes, Aalborg)

# Visualizing the cropped version
plot(st_geometry(postal_crop), col = "red")

# Comparing the extent of the subset object and the crop object
plot(st_geometry(postal_crop), col = "lightblue")
plot(st_geometry(postal), col = "red", add = TRUE)
plot(st_geometry(postal_codes), add = TRUE)

plot(st_geometry(postal), col = "lightblue")
plot(st_geometry(postal_crop), col = "red", add = TRUE)
plot(st_geometry(postal_codes), add = TRUE)


#### Exercise: spatial concentration 
Calculate the number of windturbines within a 3000 meters of every sold property in the sales dataset that have a size below 60 m2(i.e. size).

#### Solution:

# subsetting salg 
salg_small<-salg[salg$size<60,]

# looking at the projection of the data 
st_crs(salg_small)
st_crs(windturbines)

# transforming the projection
D_crs <- st_crs(salg_small)
windturbines <- st_transform(windturbines, D_crs) #  

# Calculate distances 
dist_matrix <- st_distance(salg_small, windturbines)
dist_matrix <- matrix(as.numeric(dist_matrix ), nrow = nrow(dist_matrix ))

# The dimensions of the matrix
dim(dist_matrix)

# Initialize a new column in data.set
salg_small$number_of_windturbines <- 0

# Iterate through rows of the distance matrix and count windturbines within 3000 units
for (i in 1:nrow(dist_matrix)) {
  salg_small$number_of_windturbines[i] <- sum(dist_matrix[i, ] < 3000)
}

# Examine the distribution
hist(salg_small$number_of_windturbines, breaks = 30)
summary(salg_small$number_of_windturbines)


#### Exercise - buffer analysis 

#* Make a new polygon that buffers the Aalborg sf obejct by a 1000 meter  
#* Select all wind turbines within 1000 meter of the Aalborg polygon 

#### Solution:
# buffer 1000 meter
Aalborg_buffered <- st_buffer(Aalborg, dist = 1000)

# Dissolve the overlapping areas in the buffer
Aalborg_buffered_dissolved <- st_union(Aalborg_buffered)

# Plotting original and dissolved buffered objects
plot(st_geometry(Aalborg_buffered_dissolved), col = "lightyellow")
plot(st_geometry(Aalborg), main = "Dissolved Buffer for Aalborg", col = "lightblue", add = TRUE)

# correcting projections
A_crs <- st_crs(Aalborg_buffered_dissolved)
windturbines <- st_transform(windturbines, A_crs) #  

# Selecting windturbines located within Aalborg_buffered_dissolved
windturbines_within_buffer <- windturbines[st_intersects(windturbines, Aalborg_buffered_dissolved, sparse = FALSE), ]

# Plotting the results
plot(st_geometry(Aalborg_buffered_dissolved), col = "lightyellow", main = "Windturbines within Aalborg's Dissolved Buffer")
plot(st_geometry(Aalborg), col = "lightblue", add = TRUE)
plot(st_geometry(windturbines_within_buffer), col = "red", pch = 20, add = TRUE)


#### lets make interactive maps  

# Load the tmap package for thematic mapping
library(tmap)

salg$mill_kr<-salg$price/1000000

salg<-salg[salg$postal_code==9200,]

tm_shape(salg) + 
  tm_dots(col = "mill_kr", style = "quantile", scale = 2.5, palette = "Reds", 
          title = "House Prices (DDK)", border.col = "black", 
          border.lwd = 0.1, border.alpha = 0.4) + 
  tm_layout(legend.position = c(1, 0.2), legend.text.size = 1.1, legend.title.size = 1.4, 
            frame = FALSE, legend.bg.color = "white" ) +
  tm_shape(postal) + tm_borders() 

tmap_mode("view")

tm_shape(salg) + 
  tm_dots(col = "mill_kr", style = "quantile", scale = 2.5, palette = "Reds", 
          title = "House Prices (DDK)", border.col = "black", 
          border.lwd = 0.1, border.alpha = 0.4) + 
  tm_layout(legend.position = c(1, 0.2), legend.text.size = 1.1, legend.title.size = 1.4, 
            frame = FALSE, legend.bg.color = "white" ) +
  tm_shape(postal) + tm_borders() 



#### Exercise - buffer analysis 

#In this exercise, you will calculate the average property price for each parish based on property sales data and visualize it on an interactive map using the tmap package.

### solution

# Load the saved R data
load("salg.RData")

# Examine the dataset
head(salg)

# Check the dimensions of the dataset
dim(salg)

# Check the class of the dataset
class(salg)

# changing prices into millions 
salg$price<-salg$price/1000000

# Convert the data.frame to an sf object
salg <- st_as_sf(salg, coords = c("coord_x", "coord_y"), crs = 25832) # 25832 is the EPSG code for "ETRS89 / UTM zone 32N"

# check if the two data have the same projections
st_crs(salg)
st_crs(parish)

# they do not have the same projections. Therefor change the projection 
salg <- st_transform(salg, st_crs(parish)) # adding the lake projection to the data.set

# overlay analysis: using st_join to join the parish data to data.set based on their spatial relationship
joined_data <- st_join(salg, parish, left = TRUE)

joined_data<-joined_data[!is.na(joined_data$ADM_KODE),]

# Aggregate the average price for each parish
average_price_by_parish <- joined_data %>%
  group_by(ADM_KODE = ADM_KODE) %>% 
  summarize(avg_price = mean(price, na.rm = TRUE))

# create a dataframe 
average_price_by_parish<-data.frame(average_price_by_parish)[,1:2]

# Join the average price back to the parish polygons
parish_with_avg_price <- parish %>%
  left_join(average_price_by_parish, by = "ADM_KODE") 

# Plot the map in "view" mode
tmap_mode("view")
tm_shape(parish_with_avg_price) +
  tm_polygons(col = "avg_price", style = "quantile", palette = "Blues", 
              title = "Average Property Price (million DKK)") +
  tm_layout(title = "Average Property Price by Parish in million DDK")

