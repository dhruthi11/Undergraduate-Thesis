library('tidycensus')
library('dplyr')
library('readxl')
#install.packages("writexl")
library('writexl')

#census_api_key("788ac724031497f9d680231a50b53b10279a29f5", install = TRUE)
readRenviron("~/.Renviron")
#census_data <- get_acs(geography = 'tract', table='DP02', cache_table = TRUE, year = 2018, state = 'California')
acs_18_vars = load_variables(
  year = 2018, 
  "acs5",
  cache = TRUE)

vars=list('B03002_001',
          'B03002_003',
          'B19001_001',
          'B19013_001',
          'B25077_001',
          'B25064_001')

census_data18 = get_acs(
  geography = "tract",
  state = "CA",
  county = 'San Francisco',
  variables = vars, 
  #summary_var = "P2_001N", 
  year = 2018,
  sumfile = "pl")

census_data12 = get_acs(
  geography = "tract",
  state = "CA",
  county = 'San Francisco',
  variables = vars, 
  #summary_var = "P2_001N", 
  year = 2012,
  sumfile = "pl")

census_data00 <- get_decennial(geography = "tract",
                                       variables = "P053001",
                                       year = 2000,
                                       state = "CA",
                                       county = "San Francisco")


# 4. Current housing or rent - 2018 ------------------------------------------

#HIGHER VALUES -> MORE GENTRIFICATION

#current rent cost
rent_values <- census_data18 %>% filter(variable == 'B25064_001') %>% select('GEOID', 'NAME', 'estimate')
rent_values$percentiles <- ecdf(rent_values$estimate)(rent_values$estimate)

#current housing cost
housing_values <- census_data18 %>% filter(variable == 'B25077_001') %>% select('GEOID', 'NAME', 'estimate')
housing_values$percentiles <- ecdf(housing_values$estimate)(housing_values$estimate)

#FINAL
rent_values$housing_and_rent <- ((rent_values$percentiles + housing_values$percentiles)/2)*10
housing_and_rent <- rent_values %>% select('GEOID', 'housing_and_rent')


# 3. Change in housing or rent from 2012-2018  ----------------------------

#HIGHER CHANGE IN HOUSING OR RENT -> HIGHER PERCENTILE -> MORE GENTRIFICATION

#rent
rent_values12 <- census_data12 %>% filter(variable == 'B25064_001') %>% select('GEOID', 'NAME', 'estimate')
rent_values$change <- rent_values$estimate - rent_values12$estimate
rent_values$change_percentiles <- ecdf(rent_values$change)(rent_values$change)
csv_rent12 <- census_data12 %>% filter(variable == 'B25064_001')

#housing
housing_values12 <- census_data12 %>% filter(variable == 'B25077_001') %>% select('GEOID', 'NAME', 'estimate')
housing_values$change <- housing_values$estimate - housing_values12$estimate
housing_values$change_percentiles <- ecdf(housing_values$change)(housing_values$change)

#FINAL
rent_values$change_housing_and_rent <- ((rent_values$change_percentiles + housing_values$change_percentiles)/2)*10
change_housing_and_rent <- rent_values %>% select('GEOID', 'NAME', 'change_housing_and_rent')


# 5. Change in low income households from 2000 to 2018 --------------------
## Percentage & total low-income households - under 80% AMI

#NEGATIVE CHANGE IN LI HOUSEHOLDS -> LOWER PERCENTILE -> MORE GENTRIFICATION -> DID -1

#2018
#percent_li <- read.csv('/Users/dhruthi11/Desktop/GitHub/displacement-typologies/data/outputs/databases/SanFrancisco_database_2018.csv') %>% select('FIPS', 'inc80_18')
fips_to_geoid <- read_excel('/Users/dhruthi11/Desktop/UDP Code/fips_conversion.xlsx') %>% select('GEOID', 'inc80_18')
fips_to_geoid$x <- duplicated(fips_to_geoid$GEOID)
merged_li <- merge(rent_values, fips_to_geoid, by = 'GEOID')

#2000
fips_to_geoid00 <- read_excel('/Users/dhruthi11/Desktop/UDP Code/fips_conversion.xlsx') %>% select('GEOID', 'inc80_00')
fips_to_geoid00$x <- duplicated(fips_to_geoid00$GEOID)
merged_li00 <- merge(rent_values, fips_to_geoid00, by = 'GEOID')

merged_li$change <- merged_li$inc80_18 - merged_li00$inc80_00
merged_li$change_real <- merged_li$change * -1

#FINAL
merged_li$percentiles <- ecdf(merged_li$change_real)(merged_li$change_real)
merged_li$percentiles10 <- merged_li$percentiles * 10
change_li_percentiles <- merged_li %>% select('GEOID', 'percentiles10')

# All Percentiles ---------------------------------------------------------
#change_housing_and_rent
#housing_and_rent
#change_li_percentiles

all_percentiles <- merge(change_housing_and_rent, housing_and_rent, by = 'GEOID')
all_percentiles <- merge(all_percentiles, change_li_percentiles, by = 'GEOID')

all_percentiles['FIPS'] <- as.numeric(unlist(all_percentiles['GEOID']))

finnian_data <- read.csv('/Users/dhruthi11/Desktop/gent_percentiles_output.csv')[, -1]
#finnian_data['hinc_18_percentiles'] <- finnian_data['hinc_18_percentiles'] * 10
#finnian_data['income_18_00_percentiles'] <- finnian_data['income_18_00_percentiles'] * 10
#finnian_data['nonwhite18_00_percentiles'] <- finnian_data['nonwhite18_00_percentiles'] * 10
#finnian_data['col_ed_18_00_percentiles'] <- finnian_data['col_ed_18_00_percentiles'] * 10

final_percentiles <- merge(all_percentiles, finnian_data, by = 'FIPS', type='outer')
final_percentiles['Gentrification Score'] <- (final_percentiles$change_housing_and_rent + 
                                                final_percentiles$housing_and_rent + 
                                                final_percentiles$percentiles10 + 
                                                final_percentiles$change_hi_percentiles + 
                                                final_percentiles$mii_percentiles + 
                                                final_percentiles$col_ed_percentiles + 
                                                final_percentiles$white_percentiles)
final_percentiles['Gentrification Score'] <- final_percentiles['Gentrification Score'] * (100/70)

write.csv(final_percentiles, "/Users/dhruthi11/Desktop/eBird/Gentrification Scores and Percentiles.csv")
write_xlsx(final_percentiles, "/Users/dhruthi11/Desktop/eBird/Gentrification Scores and Percentiles.xlsx")

hist(final_percentiles$`Gentrification Score`)
