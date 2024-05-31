#Load Libraries 
library(tidyverse)

#Call in datasheet
read.csv("/Users/finnianwhelan/Desktop/Cont_Code_Folder/Gent_Percentiles/SanFrancisco_database_2018.csv")
gent_data <- read.csv("/Users/finnianwhelan/Desktop/Cont_Code_Folder/Gent_Percentiles/SanFrancisco_database_2018.csv")
gent_data

#Select Needed Columns 
spec_gent_data <- subset(gent_data, select = c(FIPS, hinc_18, real_hinc_00, ch_per_col_00_18, per_nonwhite_00, per_nonwhite_18)) 
spec_gent_data

#Calculate Percentiles for Median Income in 2018
spec_gent_data$hinc_18_percentiles <- ecdf(spec_gent_data$hinc_18)(spec_gent_data$hinc_18)
spec_gent_data

#Calculate Percentiles for Change in Median Income between 2000 and 2018 
spec_gent_data$income18_00_dif <- spec_gent_data$hinc_18 - spec_gent_data$real_hinc_00
spec_gent_data$income_18_00_percentiles <- ecdf(spec_gent_data$income18_00_dif)(spec_gent_data$income18_00_dif)
spec_gent_data

#Calculate Percentiles for Change in Percent Non-White between 2000 and 2018 
spec_gent_data$nonwhite18_00_dif <- spec_gent_data$per_nonwhite_18 - spec_gent_data$per_nonwhite_00
spec_gent_data$nonwhite18_00_percentiles <- ecdf(spec_gent_data$nonwhite18_00_dif)(spec_gent_data$nonwhite18_00_dif)
spec_gent_data

#Calculate Percentiles for Change in College Educated People over 25 betwen 2000 and 2018 
spec_gent_data$col_ed_18_00_percentiles <- ecdf(spec_gent_data$ch_per_col_00_18)(spec_gent_data$ch_per_col_00_18)
spec_gent_data

#Save csv
write.csv(spec_gent_data, file = "/Users/finnianwhelan/Desktop/Cont_Code_Folder/Gent_Percentiles/gent_percentiles_output.csv", row.names = FALSE)
