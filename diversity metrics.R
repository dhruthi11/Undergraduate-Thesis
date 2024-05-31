install.packages("vegan")
install.packages("dplyr")
install.packages("auk")
install.packages("rebird")
library('auk')
library('rebird')
library('vegan')
library('tidycensus')
library('dplyr')
library('readxl')
library('writexl')

gentrification_tracts_inter <- read.csv('/Users/dhruthi11/Desktop/SanFrancisco_typology_output.csv') %>% 
  select('FIPS', 'typ_cat')
gentrification_tracts <- gentrification_tracts_inter %>% 
  filter(typ_cat %in% c("'AdvG'", "'SAE'", "'ARG'", "'LISD'", "'OD'", "'AdvG', 'BE'"))

gent_scores <- read_xlsx("/Users/dhruthi11/Desktop/Gentrification Scores and Percentiles.xlsx")
scores_and_typ <- merge(gentrification_tracts, gent_scores, by = 'FIPS') %>% select('FIPS', 'GEOID', 'typ_cat', 'Gentrification Score', 'NAME')

ebird <- read_tsv('/Users/dhruthi11/Desktop/0034512-240314170635999.csv')
f_in <- system.file("extdata/ebd-sample.txt", package = "auk")
# output text file
f_out <- "ebd_filtered_grja.txt"
ebird_data <- f_in %>% 
  # 1. reference file
  auk_ebd() %>% 
  # 2. define filters
  auk_county(county = "US-CA-038") %>% 
  auk_date(date = c("2016-01-01", "2019-12-31"))
  # 3. run filtering
  auk_filter(file = f_out) %>% 
  # 4. read text file into r data frame
  read_ebd()
  
  
library(auk)
install.packages('raster')
library(raster)
library(tidyverse)
install.packages('sf')
library(sf)
install.packages('rnaturalearth')
library(rnaturalearth)
install.packages('tigris')
library(tigris)
install.packages('viridisLite')
library(viridisLite)
# path to ebird data
# change this to the location where you saved the data
ebd_dir <- "/Users/dhruthi11/Downloads/eBird.csv"

# ebd
f <- file.path(ebd_dir, "ebd-2018.txt")
#f_clean <- file.path(ebd_dir, "ebd-2018_clean.txt")
#auk_clean(f, f_out = f_clean, remove_text = TRUE)
# sampling
f_sampling <- file.path(ebd_dir, "ebd_sampling_rel-2018.txt")
#f_sampling_clean <- file.path(ebd_dir, "ebd_sampling_rel-2018_clean.txt")
#auk_clean(f, f_out = f_sampling_clean, remove_text = TRUE)

f_in_ebd <- file.path(ebd_dir, "ebd-2018.txt")
#f_in_sampling <- file.path(ebd_dir, "ebd_sampling_rel-2018_clean.txt")
auk_ebd(file = f_in_ebd)
ebd_filters <- auk_ebd(f_in_ebd, f_in_sampling) %>% 
  auk_species(c("Bicknell's Thrush", "Swainson's Thrush")) %>% 
  auk_state("US-NH") %>% 
  auk_date(c("*-06-01", "*-06-30")) %>% 
  auk_complete()
ebd_filters







library(auk)
# path to the ebird data file, here a sample included in the package
# in practice, provide path to ebd, e.g. input_file <- "data/ebd_relFeb-2018.txt"
input_file <- system.file("/Users/dhruthi11/Downloads/eBird.csv", package = "auk")
# output text file
output_file <- "/Users/dhruthi11/Downloads/eBird_filtered.csv"
ebird_data <- f_in %>% 
  # 1. reference file
  auk_ebd() %>% 
  # 2. define filters
  auk_county(county = "US-CA-038") %>% 
  auk_date(date = c("2016-01-01", "2019-12-31"))
# 3. run filtering
auk_filter(file = f_out) %>% 
  # 4. read text file into r data frame
  read_ebd()




library(auk)
library(dplyr)

input_file <- "/Users/dhruthi11/Downloads/eBird.csv"
output_file <- "/Users/dhruthi11/Downloads/eBird_filtered.csv"

ebird_data <- input_file %>%
  # 1. reference file
  auk_ebd() %>% 
  # 2. define filters
  auk_county(county = "US-CA-038") %>% 
  auk_date(date = c("2016-01-01", "2019-12-31")) %>%
  # 3. run filtering
  auk_filter(file = output_file) %>%
  # 4. read text file into r data frame
  read_ebd()


input_file <- "/Users/dhruthi11/Downloads/0013449-231120084113126.csv"
output_file <- "/Users/dhruthi11/Downloads/ebird-filtered.csv"

ebird_data <- read_ebd(input_file, unique = FALSE) %>%
  # Filter data based on county and date
  auk_county(county = "US-CA-038") %>% 
  auk_date(date = c("2016-01-01", "2019-12-31"))

# Write filtered data to a new CSV file
write.csv(ebird_data, file = output_file, row.names = FALSE)




install.packages("rgbif")
library('rgbif')
library('readr')
ebird_data_gbif <- read_tsv('')