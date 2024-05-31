library('tidycensus')
library('dplyr')
library('readxl')
#install.packages("writexl")
library('writexl')

sf_data <- read.csv('/Users/dhruthi11/Desktop/GitHub/displacement-typologies/data/outputs/databases/SanFrancisco_database_2018.csv')

# Change in Median Income 2000 - 2018 (Inflation-Adjusted) ----------------

household_incomes <- sf_data %>% select('FIPS', 'real_hinc_18', 'real_hinc_00')
household_incomes$change <- household_incomes$real_hinc_18 - household_incomes$real_hinc_00
household_incomes$change_hi_percentiles <- ecdf(household_incomes$change)(household_incomes$change) * 10
household_incomes <- household_incomes %>% select('FIPS', 'change_hi_percentiles')

# Median income (Individual) - 2018 ---------------------------------------------------

#HIGHER VALUES -> MORE GENTRIFICATION

median_income18 <- sf_data %>% select('FIPS', 'iinc_18')
median_income18$mii_percentiles <- ecdf(median_income18$iinc_18)(median_income18$iinc_18) * 10
median_income18 <- median_income18 %>% select('FIPS', 'mii_percentiles')



# Change in Prop of College Educated Residents ----------------------------

#HIGHER VALUES -> MORE GENTRIFICATION

college_ed <- sf_data %>% select('FIPS', 'ch_per_col_00_18')
college_ed$col_ed_percentiles <- ecdf(college_ed$ch_per_col_00_18)(college_ed$ch_per_col_00_18) * 10
college_ed <- college_ed %>% select('FIPS', 'col_ed_percentiles')



# Change in Prop Non-hispanic White Residents -----------------------------

#LOWER INCREASE IN % NON-WHITE OVER THE YEARS -> LESS GENTRIFICATION

non_white_ppl <- sf_data %>% select('FIPS', 'per_nonwhite_00', 'per_nonwhite_18')
non_white_ppl$change <- non_white_ppl$per_nonwhite_00 - non_white_ppl$per_nonwhite_18
non_white_ppl$white_percentiles <- ecdf(non_white_ppl$change)(non_white_ppl$change) * 10 
non_white_ppl <- non_white_ppl %>% select('FIPS', 'white_percentiles')




#EXPORT
finnian_percentiles <- full_join(household_incomes, median_income18,
                                 by = 'FIPS')

finnian_percentiles <- full_join(finnian_percentiles, college_ed,
                                 by = 'FIPS')

finnian_percentiles <- full_join(finnian_percentiles, non_white_ppl,
                                 by = 'FIPS')


write.csv(finnian_percentiles, '/Users/dhruthi11/Desktop/gent_percentiles_output.csv')


