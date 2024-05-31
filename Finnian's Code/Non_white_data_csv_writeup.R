install.packages("tidyverse")
install.packages("tidycensus")
library(tidyverse)
library(tidycensus)

# Specify API key 
census_api_key("eb9af99fe41e755d468d4cfed2d45e955c7bebbf", overwrite = TRUE, install = TRUE)

#Racial Demographics for 2018
pop_2018 <- get_acs(geography = "tract",
                                variables = c("B02001_001E"), 
                                 year = 2018,
                                 state = "CA",
                                 county = "San Francisco")
white_2018 <- get_acs(geography = "tract",
                      variables = c("B02001_002E"), 
                      year = 2018,
                      state = "CA",
                      county = "San Francisco")
# View the structure of the data
glimpse(pop_2018)
glimpse(white_2018)

# View the first few rows of the data
head(pop_2018)
head(white_2018)

pop_2018
white_2018

# Save the data as a CSV file
write.csv(pop_2018, "pop_2018.csv", row.names = FALSE)
read.csv("pop_2018.csv")
write.csv(pop_2018, file = "/Users/finnianwhelan/Desktop/Gentrification_Code/pop_2018.csv", row.names = FALSE)

write.csv(white_2018, "white_2018.csv", row.names = FALSE)
read.csv("white_2018.csv")
write.csv(white_2018, file = "/Users/finnianwhelan/Desktop/Gentrification_Code/white_2018.csv", row.names = FALSE)

# Get population data for SF in 2000 (replace with your desired geography)
pop_2000 <- get_decennial(geography = "tract",
                                      variables = c("P001001"),
                                      year = 2000,
                                      state = "CA",
                                      county = "San Francisco")
# Get white data for SF in 2000 
white_2000 <- get_decennial(geography = "tract",
                          variables = c("P004002"),
                          year = 2000,
                          state = "CA",
                          county = "San Francisco")

# View the structure of the data
glimpse(pop_2000)
glimpse(white_2000)

# View the first few rows of the data
head(pop_2000)
head(white_2000)

# Save the data as a CSV file
write.csv(pop_2000, "pop_2000.csv", row.names = FALSE)
read.csv("pop_2000.csv")
write.csv(pop_2000, file = "/Users/finnianwhelan/Desktop/Gentrification_Code/pop_2018.csv", row.names = FALSE)

write.csv(white_2000, "white_2018.csv", row.names = FALSE)
read.csv("white_2000.csv")
write.csv(white_2000, file = "/Users/finnianwhelan/Desktop/Gentrification_Code/white_2018.csv", row.names = FALSE)