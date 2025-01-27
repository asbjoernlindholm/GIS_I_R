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


## Make a spatial point sf object from a dataframe 
# reading in data 
setwd(data_path)
load("salg.RData")

# Check the class of the data
class(salg)
str(salg)

# Convert data frame to sf object
salg <- st_as_sf(salg, coords = c("coord_x", "coord_y"), crs = 25832) # 25832 is the EPSG code for "ETRS89 / UTM zone 32N"

# Examine the new class and structure of the sf object
class(salg)
head(salg)

# Visualize the spatial distribution
plot(st_geometry(salg))

# Subset data based on a condition and visualize the subset in green
expensive_salg <- salg[salg$price > 10000000,]
plot(st_geometry(expensive_salg), col = "green", add = TRUE)

```

#### Exercise 1 

#Read in spatial data and check the projection of each sf object 

shape_file_names<-c("coast","forest","harbour", "highway","lake","large_roads",
                    "municipality", "Nord_jylland","parish", "postal","postal_codes",
                    "railway", "sea","windturbines","Aalborg", "urban_area")   


#### Solution 

setwd(data_path)

for(w in shape_file_names) {
  shape_file <- st_read(paste0(w, ".shp"))  # Read in the shapefile using st_read
  assign(w, shape_file) # This stores the sf object with the name specified in shape_file_names
  print(w)
  print(st_crs(shape_file)$proj4string) # Get the CRS string for the sf object
}

# second solution using two loops 

# reading in data using the assign function
for(w in shape_file_names) {
  assign(w, st_read(paste0(w, ".shp"))) # Read in and assign the shapefile to the global environment
}

# checking the projection using the get function  
for(w in shape_file_names) {
  print(w)
  print(st_crs(get(w))) # Get the CRS string for the sf object
}

## Calculate the size of sf polygons 

# Check if there's a size variable
head(as.data.frame(lake))

# Use this if you're working within RStudio for a more user-friendly view:
# View(as.data.frame(lake))

# Calculate the area in m^2
lake$area <- st_area(lake)

# Examine the distribution of the lake areas
summary(lake$area)

# you will have to make it into numerical value 
lake$area=as.numeric(lake$area)

# Check the dimensions of the lake object

# Filter lakes based on the area
lake_large <- lake[lake$area > 10000, ]

# Check the dimensions of the lake object again after filtering
dim(lake_large)

# checking the spatial location of large lakes 

plot(st_geometry(coast))
plot(st_geometry(lake_large), col="blue", add=T)




## Calculate distance matrix

# first make sure they have the same projection
st_crs(expensive_salg)
st_crs(lake_large)

# they do not have the same projections. Therefor change the projection 
# of one of the objects so that both sf objects have similar projections 
lake_crs <- st_crs(lake_large)
expensive_salg <- st_transform(expensive_salg, lake_crs) # adding the lake projection to the data.set

# Calculate the distance matrix
dim(expensive_salg)
dim(lake_large)

dist_units <- st_distance(expensive_salg, lake_large)

# Strip the units from the matrix
dist_matrix <- matrix(as.numeric(dist_units), nrow = nrow(dist_units))

dim(dist_units)
dim(dist_matrix)

# you can make the dist_matrix into a dataframe 

dist_df=as.data.frame(dist_matrix)

# Find the minimum distance for the first observation in the data.set
min(dist_df[1,])

# Count how many lakes are within 10 km of the first observation in the data.set
sum(dist_df[1,] < 10000)

# The minimum distance for each observation in data.set to any lake
min_distances <- apply(dist_matrix, 1, min)

# Adding the minimum distance to the data.set
expensive_salg$lake <- min_distances

# looking at the new variable
summary(expensive_salg$lake)

```

#### Exercise 2

Calculate the distance to large windturbines higher than 79 meters to all observation in the data.set.   

#### Solution

#Here is the code:

# Subset the windturbines dataset
windturbines <- windturbines[windturbines$height > 79, ]

# check if the two data have the same projections
st_crs(expensive_salg)
st_crs(windturbines)

# they do not have the same projections. Therefor change the projection 
windturbines <- st_transform(windturbines, lake_crs) # adding the lake projection to the data.set

# Calculate distances. This will give us a matrix where the entry [i,j] is the distance from data.set[i] to windturbines[j]
dist_units <- st_distance(expensive_salg, windturbines)

dist_matrix <- matrix(as.numeric(dist_units), nrow = nrow(dist_units))

dim(dist_matrix)

# Initialize a new windturbines column with zeros
expensive_salg$windturbines <- 0

# For each entry in windturbines, find the minimum distance to any wind turbine
for (i in 1:nrow(expensive_salg)) {
  expensive_salg$windturbines[i] <- min(dist_matrix[i, ])
}

dim(expensive_salg)

# Print the minimum distance across all entries in expensive_salg
print(min(expensive_salg$windturbines))



## Overlay analysis

### looking at the parish data 
plot(st_geometry(parish))
head(parish)

# check if the two data have the same projections
st_crs(salg)
st_crs(parish)

# they do not have the same projections. Therefor change the projection 
salg <- st_transform(salg, st_crs(parish)) # adding the lake projection to the data.set

# overlay analysis: using st_join to join the parish data to data.set based on their spatial relationship
joined_data <- st_join(salg, parish, left = TRUE)

# creating a new variable  
salg$parishes <- joined_data$ADM_KODE

## checking if some observation is located outside the parishes
summary(is.na(joined_data$ADM_KODE))

# you can also check using a map
plot(st_geometry(salg), col="red")
plot(st_geometry(parish), add=T)

# removing NA's

salg_parishes<-salg[is.na(salg$parishes)==FALSE,]

# checking the type of variable 
str(salg_parishes$parishes)

# how many obs in each parish 
salg_parishes %>%
  count(parishes) %>%
  arrange(desc(n))

#the same using base R 
summary(as.factor(salg_parishes$parishes))

## Advanced overlay analysis 

# Filter parishes based on unique ADM_KODEs in data.set. Now we only have the parishes where there is sales
parish <- parish %>% 
  filter(ADM_KODE %in% unique(salg_parishes$parishes))

#look
plot(st_geometry(parish))

# Aggregate number of sales per parish
sum_parish <- salg_parishes %>%
  count(parishes) %>%
  rename(sum = n, ADM_KODE = parishes)

# only one object can be an sf object in a join 
sum_parish=data.frame(sum_parish)

# Merge the data
parish_merge <- left_join(parish, sum_parish, by = "ADM_KODE")

# choosing platte 
display.brewer.all()
palette_name <- "YlOrRd"
num_colors <- 5
selected_palette <- brewer.pal(num_colors, palette_name)

# plot using tm_shape
tm_shape(parish_merge) + 
  tm_borders() + 
  tm_fill(col = "sum", style = "quantile", 
          palette = selected_palette, 
          title = "Number of sales") +
  tm_legend(position = c(1, 0.2)) # location of the legend

# alternatively plot using ggplot2

# note you often need to remove the z-dimension from the dataset
parish_merge <- st_zm(parish_merge, drop = TRUE)

ggplot(data = parish_merge) +
  geom_sf(aes(fill = sum), color = "black") + # `sum` is your variable for fill
  scale_fill_gradientn(colors = selected_palette, name = "Number of sales") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.2)) # Adjust legend position



#### Exercise 3
Figure out the number of wind turbines in each postal code in Nordjylland and make a map of what you find. 

#### Solution 

```{r,eval=F}

# Overlay analysis: Determines which postal_codes polygons the windturbines points fall within

# reading in a clean windturbine dataset
windturbines<- st_read("windturbines.shp")
dim(windturbines)

# checking projections
st_crs(windturbines)
st_crs(postal_codes)

# transformning projection of postal_codes
w_crs <- st_crs(windturbines)
postal_codes <- st_transform(postal_codes, w_crs) # adding the lake projection to the data.set

windturbines <- st_join(windturbines, postal_codes)

# Examine the results of the overlay
dim(windturbines)
head(windturbines)
class(windturbines)

# Check out the postal_codes dataset
dim(postal_codes)
head(postal_codes)

# Aggregate the number of wind turbines by postal code
sum_wind <- windturbines %>%
  group_by(POSTCLABEL) %>%
  tally() %>%
  rename(sum = n)

# Filtering out potential NA postal codes (shouldn't happen, but just in case)
sum_wind <- filter(sum_wind, !is.na(POSTCLABEL))

#transforming sum_wind into a dataframe 
sum_wind<-data.frame(sum_wind)

# Joining the count of wind turbines back to the postal codes spatial object
postal_merge <- left_join(postal_codes, sum_wind, by = "POSTCLABEL")

# Creating a map
# I assume `sea` and `Nord_jylland` are other spatial objects you have. 
# Their plots would still work with plot() if they are `sf` objects

plot(st_geometry(postal_merge))

# Assuming `sea` and `Nord_jylland` are also converted to sf objects
plot(st_geometry(sea), col=colors()[c(26)], add=TRUE)
plot(st_geometry(Nord_jylland), col=colors()[c(18)], add=TRUE)

# Divide values into intervals for coloring
q5 <- classIntervals(postal_merge$sum, n=5, style="quantile", dataPrecision=0)
q5Colours <- findColours(q5, rev(heat.colors(5)))

# Plotting municipalities with color indicating number of wind turbines
plot(st_geometry(postal_merge), col=q5Colours, add=TRUE, axes=TRUE)

# Add a legend
legend("topright", 
       fill = attr(q5Colours, "palette"), 
       border="black", 
       legend=c("<=2", "3", "4", "4-7", "8"), 
       bg ="white",
       title="Number of windturbines")

# alternatively ggplot2 map 

postal_merge<-postal_merge[!is.na(postal_merge$sum),]
postal_merge <- st_zm(postal_merge, drop = TRUE)

num_colors <- 5
intervals <- classIntervals(postal_merge$sum, n = num_colors, style = "quantile")
postal_merge$sum_class <- cut(postal_merge$sum, breaks = intervals$brks, include.lowest = TRUE)
palette <- rev(heat.colors(num_colors))

ggplot() +
  # defining zoom level basad on Nord_jylland layer 
  geom_sf(data = Nord_jylland, fill = colors()[18], color = NA)+
  # Add the Nord_jylland layer
  geom_sf(data = Nord_jylland, fill = colors()[18], color = NA) +
  # Add the postal_merge layer, colored by the "sum_class"
  geom_sf(data = postal_merge, aes(fill = sum_class), color = "black") +
  # Add a custom color scale for the fill
  scale_fill_manual(values = palette, name = "Number of wind turbines", 
                    labels = c("<=2", "3", "4", "4-7", "8")) +
  # Customize the theme
  labs(title = "Wind Turbines by Postal Area")

