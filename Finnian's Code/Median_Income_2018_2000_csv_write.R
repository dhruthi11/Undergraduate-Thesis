install.packages("tidyverse")
install.packages("tidycensus")
library(tidyverse)
library(tidycensus)

# Specify API key 
census_api_key("eb9af99fe41e755d468d4cfed2d45e955c7bebbf", overwrite = TRUE, install = TRUE)

#Median Household Income in 2018
# Get median household income for census tracts in San Francisco County for 2018
median_income_sf_2018 <- get_acs(geography = "tract",
                            variables = "B19013_001",
                            year = 2018,
                            state = "CA",
                            county = "San Francisco")
# View the structure of the data
glimpse(median_income_sf_2018)

# View the first few rows of the data
head(median_income_sf_2018)
median_income_sf_2018

# Save the data as a CSV file
write.csv(median_income_sf_2018, "median_income_sf_2018.csv", row.names = FALSE)
read.csv("median_income_sf_2018.csv")
write.csv(median_income_sf_2018, file = "/Users/finnianwhelan/Desktop/Gentrification_Code/median_income_sf_2018.csv", row.names = FALSE)

#Median Household Income in 2000
# Get median household income for census tracts in San Francisco County for 2018
median_income_sf_2000 <- get_decennial(geography = "tract",
                            variables = "P053001",
                            year = 2000,
                            state = "CA",
                            county = "San Francisco")
# View the structure of the data
glimpse(median_income_sf_2000)
# Save the data as a CSV file
write.csv(median_income_sf_2000, "median_income_sf_2000.csv", row.names = FALSE)
read.csv("median_income_sf_2000.csv")
write.csv(median_income_sf_2000, file = "/Users/finnianwhelan/Desktop/Gentrification_Code/median_income_sf_2000.csv", row.names = FALSE)
