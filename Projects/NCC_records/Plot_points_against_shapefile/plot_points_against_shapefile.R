# Finding points outside of Alberta
# EKB; Dec 21, 2020

# LIBRARIES #

library(raster)
library(rgdal)
library(rgeos)
library(tidyverse)

# DATA #

# import canada shapefile
# you only read in the .shp file but you need all the other files in the same place
# (.dbf, .prj, .shx) --  also, make sure file path is correct for you
canada <- readOGR('lpr_000b16a_e.shp', verbose = FALSE)

### MAKE DATAFRAME WITH NAME OF PROVINCE WHERE POINTS ARE LOCATED ###

# NOTES:
# 1. You will need to convert points in lat/long to UTM (which is preferred, right?)
# 2. You'll also need to do this separately for different UTM zones, I think
# 3. I have no idea how you find out what UTM zone corresponds to a given lat/long

## LAT-LONG TO UTM ##
# Process for creating a dataframe with lat/long points #

# make some lat/long points for testing
# 3 are in Alberta and 3 are right across the border in BC
test.points <- data.frame(lat = c(52.71, 52.71, 49.8, 54.51, 49.32, 49.8),
                          long = c(-117.9, -118.78, -114.56, -118.5, -115.34, -115.21))

# convert lat/long coordinates to "SpatialPoints"
coordinates(test.points) <- c("long", "lat")
proj4string(test.points) <- CRS("+proj=longlat +datum=WGS84")

# prepare coordinate reference system for UTM (I think you)
# I happen to know these points are all in zone 11, so I'll use that
# pretty sure you could use the CRS("+proj=utm +zone=11N...") structure above, too
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs11 <- CRS(sprintf(utmStr, 11))         # I honestly don't know what this line does except 11 = zone

# convert the SpatialPoints from lat/long to UTM
canUTM.11 <- spTransform(canada, crs11)      # transforms Canada shapefile to UTM zone 11
ptsUTM.11 <- spTransform(test.points, crs11) # transforms test points to UTM zone 11

# get the name of the province in which the point is located
province <- over(ptsUTM.11, canUTM.11)[3] #PRENAME means province English name, I think
province

## UTM ALREADY ##
# This is for if points are already in UTM #

# pull some UTM data from the query for testing -- make sure file path is correct for you
test.points2 <- readxl::read_excel("../LDP Internship/Data/2020-Dec_NCCSpeciesQuery-MASTER.xlsx") %>% 
  head(., 20) %>% 
  select(`UTM Easting`, `UTM Northing`, `UTM Zone`) %>% 
  drop_na() %>% 
  print()
# rename the columns
names(test.points2) <- c("easting", "northing", "zone")

# convert to spatial points
# the points I pulled happen to all be in zone 12N
# instead of these two lines, for proj4strings, you could use:
# CRS("+proj=utm +zone=12N +datum=NAD83 +units=m +no_defs +ellps=GRS80")
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs12 <- CRS(sprintf(utmStr, 12)) 

coordinates(test.points2) <- c("easting", "northing") # whatever the columns are named
proj4string(test.points2) <- CRS(sprintf(utmStr, 12))

# these are the points already in UTM
canUTM.12 <- spTransform(canada, crs12) #new canada map in 12 since these points are in 12N
pts2.UTM.12 <- spTransform(test.points2, crs12) # you don't technically have to run this
                                                # since the points are already in UTM zone 12
                                                # and, therefore, don't need to be transformed

# get the name of the province in which the point is located
# you get the same thing whether you run line 75 or not (spTranform)
province2 <- over(test.points2, canUTM.12[3]) %>% print()
province2 <- over(pts2.UTM.12, canUTM.12[3]) %>% print()


## Once you have the dataframe of province names, you can filter out points that
# aren't in Alberta.