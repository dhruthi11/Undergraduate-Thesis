
#setup and path
setwd("~/Desktop/eBird")
path <- ("~/Desktop/eBird")

#install and load packages
X <- c( "ggplot2", "dplyr", "tidyverse", "RColorBrewer", "reshape2", "cowplot", 
        "egg", "emmeans", "lme4", "performance", "segregation", "readr", 
        "stringi", "stringr", "sf", "tidycensus", "tigris", "units")

#(install.packages("ggpubr"))
library('ggpubr')

if (length(setdiff(X, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(X, rownames(installed.packages())))
  
}

invisible(lapply(X, library, character.only = TRUE))

#install.packages("ggthemes") # Install 

library(ggthemes) # Load

#install.packages("rgbif")
library(rgbif)
library(dplyr)
library(tidyverse)
library(tigris)

#make shapefiles

options(tigris_use_cache = TRUE)

#census shapefile

sf_census_shapefile <- tigris::tracts(state = "CA",
                                      county="San Francisco",
                                      year=2018,
                                      class = "sf")
sf_census_shapefile <- st_transform(sf_census_shapefile,  crs = 4326)
sf_census_shapefile <- sf_census_shapefile[,-c(1:3,5:8,10:12)]
colnames(sf_census_shapefile)[colnames(sf_census_shapefile) == 'GEOID'] <- 'census_block'
sf_census_shapefile$census_block <- as.numeric(sf_census_shapefile$census_block)


#ebird00_05
ebird00_05 <- occ_download_get(key = "0008225-240425142415019", overwrite = TRUE) %>% 
  occ_download_import(sf_gbif_download, na.strings = c("", NA))
#convert ebird to shapefile
sf_ebird_shapefile1 <- st_as_sf(ebird00_05, 
                               coords = c("decimalLongitude", "decimalLatitude"), 
                               crs = 4326)

#only in sf
sf_ebird_shapefile1 <- sf_ebird_shapefile1 %>%
  st_filter(y = sf_census_shapefile, .predicate = st_within)

sf_ebird_census1 <- st_join(sf_ebird_shapefile1, sf_census_shapefile, join = st_within)
regular_dataframe1 <- as.data.frame(sf_ebird_census1)
regular_dataframe12 <- as.data.frame(sf_census_shapefile)
colnames(regular_dataframe12)[colnames(regular_dataframe12) == "geometry"] <- "geom"
sf_ebird_census12 <- full_join(regular_dataframe12, regular_dataframe1, by = 'census_block')

#ebird06_09

ebird10_14

ebird15_18