install.packages("tidyverse")
#Load Libraries 
library(tidyverse)

#Median Household Income in 2000
# Get median household income for the 2000 census at the census tract level for San Francisco city from csv
read.csv("/Users/finnianwhelan/Desktop/Gentrification_Code/median_income_sf_2018.csv")
mhinc_2018 <- read.csv("/Users/finnianwhelan/Desktop/Gentrification_Code/median_income_sf_2018.csv")
mhinc_2018


#calculate median of all median household income in SF
mSFmhinc18 <- median(mhinc_2018$estimate)
mSFmhinc

#Create new data frame 
print(mhinc_2018)
cat('\n\n')

mhinc_2018['comp_mean'] <- c(mhinc_2018$estimate)
print(mhinc_2018)

df <- mhinc_2018
value_to_subtract <- 58816.5

# Subtracting the specific value from the column
df$comp_mean <- df$comp_mean - value_to_subtract

# Viewing the result
print(df)

#Write csv
# Save the data as a CSV file
write.csv(df, "median_income_sf_2018_percentiles.csv", row.names = FALSE)
read.csv("median_income_sf_2018_percentiles.csv")
write.csv(df, file = "/Users/finnianwhelan/Desktop/Gentrification_Code/median_income_sf_2018_percentiles.csv", row.names = FALSE)


