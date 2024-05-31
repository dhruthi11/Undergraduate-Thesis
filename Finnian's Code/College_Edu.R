install.packages("tidyverse")
install.packages("tidycensus")
library(tidyverse)
library(tidycensus)

# Specify API key 
census_api_key("eb9af99fe41e755d468d4cfed2d45e955c7bebbf", overwrite = TRUE, install = TRUE)

#College Educated 2018 Census Call
college_2018 <- get_acs(geography = "tract",
                          variables = c("B15003_022E"),
                          year = 2018,
                          state = "CA",
                          county = "San Francisco")
# View the structure of the data
glimpse(college_2018)

# View the first few rows of the data
head(college_2018)
college_2018 

# Save the data as a CSV file
write.csv(college_2018, "college_2018.csv", row.names = FALSE)
read.csv("college_2018.csv")
write.csv(college_2018, file = "/Users/finnianwhelan/Desktop/Gentrification_Code/college_2018.csv", row.names = FALSE)

#College Educated 2000 Census Call
college_2000 <- get_decennial(geography = "tract",
                              variables = c("PCT18"),
                              year = 2000,
                              state = "CA",
                              county = "San Francisco")
# View the structure of the data
glimpse(college_2000)

# View the first few rows of the data
head(college_2000)
college_2000 
