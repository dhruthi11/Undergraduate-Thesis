# Import Data -------------------------------------------------------

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

#upload ebird data

#0013449-231120084113126
#0008302-240425142415019 - long

sf_gbif <- occ_download_get(key = "0013449-231120084113126", overwrite = TRUE) %>% 
  occ_download_import(sf_gbif_download, na.strings = c("", NA))
#sf_gbif <- read.csv('/Users/dhruthi11/Desktop/0008302-240425142415019.csv')

#convert ebird to shapefile
sf_ebird_shapefile <- st_as_sf(sf_gbif, 
                               coords = c("decimalLongitude", "decimalLatitude"), 
                               crs = 4326)

#only in sf
sf_ebird_shapefile<- sf_ebird_shapefile %>%
  st_filter(y = sf_census_shapefile, .predicate = st_within)

sf_ebird_census <- st_join(sf_ebird_shapefile, sf_census_shapefile, join = st_within)
regular_dataframe <- as.data.frame(sf_ebird_census)
regular_dataframe2 <- as.data.frame(sf_census_shapefile)
colnames(regular_dataframe2)[colnames(regular_dataframe2) == "geometry"] <- "geom"
sf_ebird_census2 <- full_join(regular_dataframe2, regular_dataframe, by = 'census_block')
#write.csv(sf_ebird_census2, file = '/Users/dhruthi11/Desktop/eBird/sf_ebird_census2')

#class(sf_ebird_census)
#sf_ebird_census$geometry <- st_geometry(sf_ebird_census$geometry)



#sf_ebird_census2 <- read_csv('/Users/dhruthi11/Desktop/eBird/sf_ebird_census2')
# PIVOT -------------------------------------------------------------------
sf_ebird_census2$date <- as.Date(paste(sf_ebird_census2$year, 
                                       sf_ebird_census2$month, 
                                       sf_ebird_census2$day, sep="-"), 
                                 "%Y-%m-%d")

#with date
sf_census_ebird_date <- sf_ebird_census2 %>%
  group_by(census_block, verbatimScientificName, date) %>%
  summarise(total_individuals = sum(individualCount))

pivot_species_date <- sf_census_ebird_date %>%
  pivot_wider(
    id_cols = c("census_block", "date"),
    names_from = "verbatimScientificName",
    values_from = "total_individuals"
  )

pivot_species_date <- pivot_species_date %>%
  mutate(across(-date, ~replace(., is.na(.), 0)))

#without date
sf_census_ebird_dataframe <- sf_ebird_census2 %>%
  group_by(census_block, verbatimScientificName) %>%
  summarise(total_individuals = sum(individualCount))

pivot_species_neighborhood_CA <- sf_census_ebird_dataframe %>%
  pivot_wider(
    id_cols = c("census_block"),
    names_from = "verbatimScientificName",
    values_from = "total_individuals"
  )

pivot_species_neighborhood_CA[is.na(pivot_species_neighborhood_CA)] <- 0

#now we'll calculate species richness, Shannon's diversity, Simpson's diversity, 
# and 1/Simpson. Diversity data will be using the vegan package.


# Biodiversity Metrics ----------------------------------------------------

jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

#with date
pivot_species_date$Richness <- apply(X = pivot_species_date[, c(3:378)], #check rows for columns
                                                MARGIN = 1,
                                                FUN = function(x) {
                                                  sum(x > 0)
                                                })

pivot_species_date$Diversity <- apply(X = pivot_species_date[, c(3:378)],
                                                 MARGIN = 1,
                                                 FUN = function(x) {
                                                   vegan::diversity(x = x, index = "shannon")
                                                 })

library(vegan) # Make sure the vegan package is installed and loaded

#pivot_species_date$Beta <- apply(X = pivot_species_date[, c(3:378)],
#                                     MARGIN = 1,
#                                      FUN = function(x) {
#                                        vegdist(x, method = "jaccard")
#                                      })

#pivot_species_date$Beta <- apply(pivot_species_date[, c(3:378)],
#                                            MARGIN = 1,
#                                            FUN = function(x) {
#                                              jaccard_sim <- 1 - sum(x) / length(x)
#                                              return(jaccard_sim)
#                                            })

#pivot_species_date$Beta2 <- apply(pivot_species_date[, c(3:378)],
#                                 MARGIN = 1,
#                                 FUN = function(x) {
#                                   vegan::vegdist(x = x, index = "jaccard")
#                                 })

#pivot_species_date$Beta3 <- apply(pivot_species_date[, c(3:378)],
#                                            MARGIN = 1,
#                                            FUN = function(x) {
#                                              jaccard()
#                                              })

#install.packages('betapart')
#library(betapart)

#pivot_species_date <- pivot_species_date %>%
#  mutate_at(vars(3:378), as.numeric)

#pivot_species_date[, 3:378] <- apply(pivot_species_date[, 3:378], 2, as.numeric)

#beta_diversity <- beta.pair(pivot_species_date)

#pivot_species_date$Beta4 <- apply(pivot_species_date[, c(3:378)],
#                                  MARGIN = 1,
#                                  FUN = function(x) {
#                                    jaccard_sim <- sum(x > 0) / length(x)
#                                    return(jaccard_sim)
#                                  })

pivot_species_date$Observations <- apply(X = pivot_species_date[, c(3:378)],
                                                    MARGIN = 1,
                                                    FUN = function(x) {
                                                      sum(x)
                                                    })

tract_biodiv_date <- pivot_species_date %>% select('census_block', 'date', 
                                                   'Richness', 'Diversity',
                                                   'Observations')


#without date
pivot_species_neighborhood_CA$Richness <- apply(X = pivot_species_neighborhood_CA[, c(2:378)], 
                                                #check rows for columns
                                                MARGIN = 1,
                                                FUN = function(x) {
                                                  sum(x > 0)
                                                })

pivot_species_neighborhood_CA$Diversity <- apply(X = pivot_species_neighborhood_CA[, c(2:378)],
                                                 MARGIN = 1,
                                                 FUN = function(x) {
                                                   vegan::diversity(x = x, index = "shannon")
                                                 })

#pivot_species_neighborhood_CA$Beta <- apply(pivot_species_neighborhood_CA[, c(2:378)],
#                                            MARGIN = 1,
#                                            FUN = function(x) {
#                                              jaccard_sim <- 1 - sum(x) / length(x)
#                                              return(jaccard_sim)
#                                            })

pivot_species_neighborhood_CA$Observations <- apply(X = pivot_species_neighborhood_CA[, c(2:378)],
                                                    MARGIN = 1,
                                                    FUN = function(x) {
                                                      sum(x)
                                                    })

tract_biodiv <- pivot_species_neighborhood_CA %>% select('census_block', 
                                                         'Richness', 'Diversity',
                                                         'Observations')


# Add Gentrification Scores/Levels ----------------------------------------

gentrification_scores <- read.csv('/Users/dhruthi11/Desktop/eBird/Gentrification Scores and Percentiles.csv')
gentrification_tract_type <- read.csv('/Users/dhruthi11/Desktop/SanFrancisco_typology_output.csv') %>% 
  select('FIPS', 'typ_cat')

#without date - binary
tract_biodiv_gent <- full_join(tract_biodiv, gentrification_tract_type, by = c('census_block' = 'FIPS')) %>%
  filter(!is.na(Richness))

tract_biodiv_gent_smaller <- tract_biodiv_gent %>% filter(!is.na(typ_cat)) %>%
  filter(typ_cat != "[]")
gent_biodiv <- tract_biodiv_gent_smaller %>% arrange(desc(Observations)) %>% filter(Observations < 30000)

#without date - score
tract_biodiv_gent_score <- full_join(tract_biodiv, gentrification_scores, by = c('census_block' = 'FIPS')) %>%
  filter(!is.na(Richness))

tract_biodiv_gent_score_smaller <- tract_biodiv_gent_score %>% filter(!is.na(Gentrification.Score))
gent_scores_biodiv <- tract_biodiv_gent_score_smaller 
#%>% arrange(desc(Observations)) %>% 
#  filter(Observations < 25000)

#with date
tract_biodiv_gent_date <- full_join(tract_biodiv_date, gentrification_tract_type, 
                                    by = c('census_block' = 'FIPS')) %>%
  filter(!is.na(typ_cat)) %>%
  filter(typ_cat != "[]")
gent_biodiv_date <- tract_biodiv_gent_date 
#%>% arrange(desc(Observations)) %>% 
 # filter(Observations < 30000)


# Define the categories of interest
categories_of_interest <- c("'EOG'", "'AdvG'", "'AdvG', 'BE'")

# Create a binary column based on the condition
tract_biodiv_gent$cat_binary <- ifelse(tract_biodiv_gent$typ_cat %in% 
                                         categories_of_interest, 'Gentrified', 'Not Gentrified')
tract_biodiv_gent_smaller$cat_binary <- ifelse(tract_biodiv_gent_smaller$typ_cat %in% 
                                                 categories_of_interest, 'Gentrified', 'Not Gentrified')
gent_biodiv_date$cat_binary <- ifelse(gent_biodiv_date$typ_cat %in% 
                                        categories_of_interest, 'Gentrified', 'Not Gentrified')


# PCoA Plots --------------------------------------------------------------
library(vegan) # Make sure the vegan package is installed and loaded
tract_biodiv_gent2 <- tract_biodiv_gent %>% select('census_block', 'cat_binary')
pcoa_tracts <- full_join(tract_biodiv_gent2, pivot_species_neighborhood_CA, by= 'census_block')

pcoa_tracts_gent <- pcoa_tracts %>% filter(cat_binary == 'Gentrified')
pcoa_tracts_notgent <- pcoa_tracts %>% filter(cat_binary == 'Not Gentrified')

abundance_data1 <- as.matrix(pcoa_tracts_gent[3:382])
beta_div1 <- vegdist(abundance_data1, method = "jaccard")
dim(beta_div1)
beta_div12 <- as.matrix(beta_div1)
beta_div12[is.na(beta_div12)] <- 0

pcoa1 <- cmdscale(beta_div12, eig=T, add=T)
x1 <- pcoa1$points[, 1]
y1 <- pcoa1$points[, 2]
pcoa_df1 <- data.frame(x = x1, y = y1)
pcoa_plot1 <- ggplot(pcoa_df1, aes(x = x1, y = y1)) +
  geom_point() +  # Add points
  labs(x = "PCoA 1", y = "PCoA 2") +  # Label axes
  ggtitle("PCoA Plot") +  # Add title
  theme_minimal()
pcoa_plot1

abundance_data2 <- as.matrix(pcoa_tracts_notgent[3:382])
beta_div2 <- vegdist(abundance_data2, method = "jaccard")
dim(beta_div2)
beta_div22 <- as.matrix(beta_div2)
beta_div22[is.na(beta_div22)] <- 0

pcoa2 <- cmdscale(beta_div22, eig=T, add=T)
x2 <- pcoa2$points[, 1]
y2 <- pcoa2$points[, 2]
pcoa_df2 <- data.frame(x = x2, y = y2)
pcoa_plot2 <- ggplot(pcoa_df2, aes(x = x2, y = y2)) +
  geom_point() +  # Add points
  labs(x = "PCoA 1", y = "PCoA 2") +  # Label axes
  ggtitle("PCoA Plot") +  # Add title
  theme_minimal()
pcoa_plot2

pcoa_df1$dataset <- "Gentrified"
pcoa_df2$dataset <- "Not Gentrified"
combined_df <- rbind(pcoa_df1, pcoa_df2)

pcoa_plot <- ggplot(combined_df, aes(x = x, y = y, color = dataset)) +
  geom_point() +  # Add points
  labs(x = "PCoA 1", y = "PCoA 2", color = "Dataset") +  # Label axes and legends
  ggtitle("PCoA - Gentrified vs. Non-Gentrified Beta Diversity") +  # Add title
  theme_minimal()  + stat_ellipse(geom="polygon", aes(fill = dataset), alpha = 0.2, show.legend = FALSE, level = 0.95)
pcoa_plot

#install.packages('ggforce')
library(ggforce)

cov_matrices <- lapply(split(combined_df, combined_df$dataset), function(df) cov(df[, c("x", "y")]))

# Function to plot confidence ellipsoids
plot_ellipsoids <- function(plot, cov_matrix, center, color) {
  e <- ellipse::ellipse(cov_matrix, centre = center, level = 0.95)
  plot + geom_path(data = e, aes(x = x, y = y), color = color)
}

# Add confidence ellipsoids to the plot
for (i in seq_along(cov_matrices)) {
  pcoa_plot <- plot_ellipsoids(pcoa_plot, cov_matrices[[i]], colMeans(combined_df[combined_df$dataset == levels(combined_df$dataset)[i], c("x", "y")]), unique(combined_df$dataset)[i])
}

pcoa_plot

permanova_result <- adonis(cbind(x, y) ~ dataset, data = combined_df, permutations = 999)
p_value <- permanova_result$aov.tab$`Pr(>F)`[1]
p_value # 0.003

# NDVI --------------------------------------------------------------------
library('readxl')
ndvi <- read_excel('/Users/dhruthi11/Desktop/Thesis/Health_NDVI_Parks_TableToExcel.xlsx') %>%
  select('census_block', 'MEAN')

ndvi_gent <- full_join(tract_biodiv_gent, ndvi, by = 'census_block') %>% 
  rename('NDVI' = 'MEAN')
ndvi_gent <- unique(ndvi_gent)


ndvi_obs <- ggplot(data = ndvi_gent, 
                        aes(x = NDVI, y = Observations, color = cat_binary)) +
  geom_smooth(method = 'lm')

ndvi_obs + labs(y = 'Number of Observations', x = 'NDVI', 
                     color = 'Gentrification Level', 
                     title = 'Observations vs NDVI',
                     tag = 'Fig. 6A') +
  theme_minimal() +
  theme(axis.title = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.title = element_blank(),
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("magenta4",
                                "palegreen4")) +
  stat_cor()

  
ndvi_richness <- ggplot(data = ndvi_gent, 
                          aes(x = NDVI, y = Richness, color = cat_binary)) +
  geom_smooth(method = 'lm')

ndvi_richness + labs(y = 'Species Richness', x = 'NDVI', 
                     color = 'Gentrification Level', 
                     title = 'Species Richness vs NDVI',
                     tag = 'Fig. 6B') +
  theme_minimal() +
  theme(axis.title = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.title = element_blank(),
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("magenta4",
                                "palegreen4")) +
  stat_cor()


ndvi_diversity <- ggplot(data = ndvi_gent, 
                        aes(x = NDVI, y = Diversity, color = cat_binary)) +
  geom_smooth(method = 'lm')

ndvi_diversity + labs(y = 'Shannon Diversity', x = 'NDVI', 
                      color = 'Gentrification Level', 
                      title = 'Shannon Diversity vs NDVI',
                      tag = 'Fig. 6C') +
  theme_minimal() +
  theme(axis.title = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.title = element_blank(),
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("magenta4",
                                "palegreen4")) +
  stat_cor()


# Wilcoxon Rank-Sum Test -----------------------------------------------------------------
tract_biodiv_gent_smaller_gent <- tract_biodiv_gent_smaller %>% filter(cat_binary == 'Gentrified')
tract_biodiv_gent_smaller_notgent <- tract_biodiv_gent_smaller %>% filter(cat_binary == 'Not Gentrified')

#not normal
shapiro.test(tract_biodiv_gent_smaller_gent$Observations)
shapiro.test(tract_biodiv_gent_smaller_notgent$Observations)

#not normal
shapiro.test(tract_biodiv_gent_smaller_gent$Richness)
shapiro.test(tract_biodiv_gent_smaller_notgent$Richness)

#not normal
shapiro.test(tract_biodiv_gent_smaller_gent$Diversity)
shapiro.test(tract_biodiv_gent_smaller_notgent$Diversity)

library(car)

#similar variance
leveneTest(Observations ~ cat_binary, data = tract_biodiv_gent_smaller)

#similar variance
leveneTest(Richness ~ cat_binary, data = tract_biodiv_gent_smaller)

#similar variance
leveneTest(Diversity ~ cat_binary, data = tract_biodiv_gent_smaller)

wilcox.test(tract_biodiv_gent_smaller_gent$Observations, tract_biodiv_gent_smaller_notgent$Observations)
mean(tract_biodiv_gent_smaller_gent$Observations)
sd(tract_biodiv_gent_smaller_gent$Observations)
mean(tract_biodiv_gent_smaller_notgent$Observations)
sd(tract_biodiv_gent_smaller_notgent$Observations)
wilcox.test(tract_biodiv_gent_smaller_gent$Richness, tract_biodiv_gent_smaller_notgent$Richness)
mean(tract_biodiv_gent_smaller_gent$Richness)
sd(tract_biodiv_gent_smaller_gent$Richness)
mean(tract_biodiv_gent_smaller_notgent$Richness)
sd(tract_biodiv_gent_smaller_notgent$Richness)
wilcox.test(tract_biodiv_gent_smaller_gent$Diversity, tract_biodiv_gent_smaller_notgent$Diversity)
mean(tract_biodiv_gent_smaller_gent$Diversity)
sd(tract_biodiv_gent_smaller_gent$Diversity)
mean(tract_biodiv_gent_smaller_notgent$Diversity)
sd(tract_biodiv_gent_smaller_notgent$Diversity)

# Plots -------------------------------------------------------------------

#2B and 3B and 4B
#boxplot(gent_biodiv$Observations ~ gent_biodiv$cat_binary)
#stripchart(tract_biodiv_gent_smaller$Observations ~ tract_biodiv_gent_smaller$cat_binary,              # Data
#           method = "jitter", # Random noise
#           pch = 19,          # Pch symbols
#           col = 4,           # Color of the symbol
#           vertical = TRUE,   # Vertical mode
#           add = TRUE)

#install.packages('prismatic')
library(prismatic)

#BOXPLOTS
gent_vs_observations <- ggplot(tract_biodiv_gent_smaller, aes(x = as.factor(cat_binary), y = Observations)) +
  geom_boxplot(color = c("darkslateblue",
                          "darkolivegreen4")) +
  geom_jitter(aes(color = as.factor(cat_binary)), width = 0.2, size = 0.7, alpha = 0.5)

gent_vs_richness <- ggplot(tract_biodiv_gent_smaller, aes(x = as.factor(cat_binary), y = Richness)) +
  geom_boxplot(color = c("darkslateblue",
                         "darkolivegreen4")) +
  geom_jitter(aes(color = as.factor(cat_binary)), width = 0.2, size = 0.7, alpha = 0.5)

gent_vs_diversity <- ggplot(tract_biodiv_gent_smaller, aes(x = as.factor(cat_binary), y = Diversity)) +
  geom_boxplot(color = c("darkslateblue",
                         "darkolivegreen4")) +
  geom_jitter(aes(color = as.factor(cat_binary)), width = 0.2, size = 0.7, alpha = 0.5)

#SCORE VERSUS ... (LINEPLOTS)
score_vs_observations <- ggplot(data = tract_biodiv_gent_score_smaller, 
                                aes(x = Gentrification.Score, y = Observations)) +
  geom_point(color = 'indianred') + 
  #geom_smooth(color = 'salmon4') + 
  geom_smooth(color = 'black', method='lm')

#score_vs_observations2 <- ggplot(data = gent_scores_biodiv, 
#                                aes(x = Gentrification.Score, y = Observations)) +
#  geom_point(color = 'indianred') + 
#  geom_smooth(color = 'salmon4', method='lm')

score_vs_richness <- ggplot(data = tract_biodiv_gent_score_smaller, 
                            aes(x = Gentrification.Score, y = Richness)) +
  geom_point(color = 'thistle3') + 
  geom_smooth(color = 'black')
  #geom_smooth(color = 'black', method='lm', linetype = 'dashed')

score_vs_diversity <- ggplot(data = tract_biodiv_gent_score_smaller, 
                             aes(x = Gentrification.Score, y = Diversity)) +
  geom_point(color = 'mediumpurple3') + 
  geom_smooth(color = 'black') 
  #geom_smooth(color = 'black', method='lm', linetype = 'dashed')

#TIME SERIES
gent_biodiv_date$date <- as.Date(gent_biodiv_date$date)
gent_biodiv_date$cat_binary <- factor(gent_biodiv_date$cat_binary)

time_vs_observations <- ggplot(data = gent_biodiv_date, 
                               aes(x = date, y = Observations, color = cat_binary)) +
  geom_smooth(method = 'lm')

time_vs_richness <- ggplot(data = gent_biodiv_date, 
                           aes(x = date, y = Richness, color = cat_binary)) +
  geom_smooth(method = 'lm')

time_vs_diversity <- ggplot(data = gent_biodiv_date, 
                            aes(x = date, y = Diversity, color = cat_binary)) +
  geom_smooth(method = 'lm')

# Clean Up Plots ----------------------------------------------------------

#Time Series
time_vs_observations + labs(y = 'Number of eBird Observations', x = 'Time', 
                            color = 'Gentrification Level', 
                            title = 'Change in Number of Observations Over Time',
                            tag = 'Fig. 1A') +
  theme_minimal() +
  theme(axis.title = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.title = element_blank(),
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("hotpink4",
                                "steelblue")) +
  stat_cor(label.y = c(575, 610)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

time_vs_richness + labs(y = 'Species Richness', x = 'Time', 
                        color = 'Gentrification Level', 
                        title = 'Change in Species Richness Over Time',
                        tag = 'Fig. 1B') +
  theme_minimal() +
  theme(axis.title = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.title = element_blank(),
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("hotpink4",
                                "steelblue")) +
  stat_cor(label.y = c(22, 23)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

time_vs_diversity + labs(y = 'Shannon Diversity', x = 'Time', 
                         color = 'Gentrification Level', 
                         title = 'Change in Shannon Diversity Over Time',
                         tag = 'Fig. 1C') +
  theme_minimal() +
  theme(axis.title = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.title = element_blank(),
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("hotpink4",
                                "steelblue")) +
  stat_cor(label.y = c(2.1, 2.16)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


#Line Plots
score_vs_observations + labs(y = 'Number of eBird Observations', x = 'Gentrification Score',
                             title = 'Number of Observations vs Gentrification Score', tag = 'Fig. 2A') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  #expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE, clip = "off") +
  stat_cor()

score_vs_richness + labs(y = 'Species Richness', x = 'Gentrification Score',
                         title = 'Species Richness vs Gentrification Score', tag = 'Fig. 2B') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  #expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE, clip = "off") +
  stat_cor()

score_vs_diversity + labs(y = 'Shannon Diversity', x = 'Gentrification Score',
                          title = 'Shannon Diversity vs Gentrification Score', tag = 'Fig. 2C') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  #expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE, clip = "off") +
  stat_cor(label.y = 4.5)


#Boxplots
gent_vs_observations + 
  labs(y = 'Number of eBird Observations', x = element_blank(), 
       color = 'Gentrification Level', title = 'Number of Observations vs Gentrification Level', 
       tag = 'Fig. 3A') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("darkslateblue",
                                "darkolivegreen4"))

gent_vs_richness + labs(y = 'Species Richness', x = element_blank(), 
                        color = 'Gentrification Level', title = 'Species Richness versus Gentrification Level',
                        tag = 'Fig. 3B') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("darkslateblue",
                                "darkolivegreen4"))



gent_vs_diversity + labs(y = 'Shannon Diversity', x = element_blank(), 
                         color = 'Gentrification Level', title = 'Shannon Diversity versus Gentrification Level',
                         tag = 'Fig. 3C') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("darkslateblue",
                                "darkolivegreen4"))


# Synanthropic/Generalist Birds ------------------------------------------------------

birds <- read.csv('/Users/dhruthi11/Downloads/ELEData/TraitData/AVONET1_BirdLife.csv')
birds$cat_binary <- ifelse(birds$Primary.Lifestyle == 'Generalist', 'Generalist', 'Not Generalist')
birds <- birds %>% select('Species1', 'cat_binary')

syn_birds_census <- full_join(sf_ebird_census2, birds, by = c('species' = 'Species1')) %>%
  filter(!is.na(genus))

generalists <- syn_birds_census %>% filter(cat_binary == 'Generalist')
not_generalists <- syn_birds_census %>% filter(cat_binary == 'Not Generalist')

#Generalists
generalists_grouped <- generalists %>%
  group_by(census_block, verbatimScientificName) %>%
  summarise(total_individuals = sum(individualCount))

generalists_pivot <- generalists_grouped %>%
  pivot_wider(
    id_cols = c("census_block"),
    names_from = "verbatimScientificName",
    values_from = "total_individuals"
  )

generalists_pivot[is.na(generalists_pivot)] <- 0


generalists_pivot$Richness <- apply(X = generalists_pivot[, c(2:53)], #check rows for columns
                                    MARGIN = 1,
                                    FUN = function(x) {
                                      sum(x > 0)
                                    })

generalists_pivot$Diversity <- apply(X = generalists_pivot[, c(2:53)],
                                     MARGIN = 1,
                                     FUN = function(x) {
                                       vegan::diversity(x = x, index = "shannon")
                                     })

#generalists_pivot$Beta <- apply(generalists_pivot[, c(2:53)],
#                                            MARGIN = 1,
#                                            FUN = function(x) {
#                                              jaccard_sim <- 1 - sum(x) / length(x)
#                                              return(jaccard_sim)
#                                            })

generalists_pivot$Observations <- apply(X = generalists_pivot[, c(2:53)],
                                        MARGIN = 1,
                                        FUN = function(x) {
                                          sum(x)
                                        })

generalists_pivot <- generalists_pivot %>% select('census_block', 
                                                  'Richness', 'Diversity', 
                                                  'Observations')

generalists_gent <- full_join(generalists_pivot, gentrification_tract_type, 
                              by = c('census_block' = 'FIPS')) %>%
  filter(!is.na(Richness))

generalists_gent <- generalists_gent %>% filter(!is.na(typ_cat)) %>%
  filter(typ_cat != "[]")
generalists_gent <- generalists_gent %>% arrange(desc(Observations))

# Create a binary column based on the condition
generalists_gent$cat_binary <- ifelse(generalists_gent$typ_cat %in% 
                                        categories_of_interest, 'Gentrified', 'Not Gentrified')

generalists_gent_score <- full_join(generalists_pivot, gentrification_scores, 
                                    by = c('census_block' = 'FIPS')) %>%
  filter(!is.na(Richness))

generalists_gent_score <- generalists_gent_score %>% filter(!is.na(Gentrification.Score))
generalists_gent_score <- generalists_gent_score %>% arrange(desc(Observations)) %>%
  select('census_block', 'Richness', 'Diversity', 'Observations', 'Gentrification.Score')


#Not Generalists
not_generalists_grouped <- not_generalists %>%
  group_by(census_block, verbatimScientificName) %>%
  summarise(total_individuals = sum(individualCount))

not_generalists_pivot <- not_generalists_grouped %>%
  pivot_wider(
    id_cols = c("census_block"),
    names_from = "verbatimScientificName",
    values_from = "total_individuals"
  )

not_generalists_pivot[is.na(not_generalists_pivot)] <- 0


not_generalists_pivot$Richness <- apply(X = not_generalists_pivot[, c(2:53)], #check rows for columns
                                        MARGIN = 1,
                                        FUN = function(x) {
                                          sum(x > 0)
                                        })

not_generalists_pivot$Diversity <- apply(X = not_generalists_pivot[, c(2:53)],
                                         MARGIN = 1,
                                         FUN = function(x) {
                                           vegan::diversity(x = x, index = "shannon")
                                         })

#not_generalists_pivot$Beta <- apply(not_generalists_pivot[, c(2:53)],
#                                MARGIN = 1,
#                                FUN = function(x) {
#                                  jaccard_sim <- 1 - sum(x) / length(x)
#                                  return(jaccard_sim)
#                                })

not_generalists_pivot$Observations <- apply(X = not_generalists_pivot[, c(2:53)],
                                            MARGIN = 1,
                                            FUN = function(x) {
                                              sum(x)
                                            })

not_generalists_pivot <- not_generalists_pivot %>% select('census_block', 
                                                          'Richness', 'Diversity', 
                                                          'Observations')

not_generalists_gent <- full_join(not_generalists_pivot, gentrification_tract_type, 
                                  by = c('census_block' = 'FIPS')) %>%
  filter(!is.na(Richness))

not_generalists_gent <- not_generalists_gent %>% filter(!is.na(typ_cat)) %>%
  filter(typ_cat != "[]")
not_generalists_gent <- not_generalists_gent %>% arrange(desc(Observations))

# Create a binary column based on the condition
not_generalists_gent$cat_binary <- ifelse(not_generalists_gent$typ_cat %in% 
                                            categories_of_interest, 'Gentrified', 'Not Gentrified')

not_generalists_gent_score <- full_join(not_generalists_pivot, gentrification_scores, 
                                        by = c('census_block' = 'FIPS')) %>%
  filter(!is.na(Richness))

not_generalists_gent_score <- not_generalists_gent_score %>% filter(!is.na(Gentrification.Score))
not_generalists_gent_score <- not_generalists_gent_score %>% arrange(desc(Observations)) %>%
  select('census_block', 'Richness', 'Diversity', 'Observations', 'Gentrification.Score')


# REDOING GENERALISTS -----------------------------------------------------

syn_birds_grouped <- syn_birds_census %>%
  group_by(census_block, cat_binary) %>%
  summarise(total_individuals = sum(individualCount)) %>%
  filter(!is.na(cat_binary))


syn_birds_pivot <- syn_birds_grouped %>%
  pivot_wider(
    id_cols = c("census_block"),
    names_from = "cat_binary",
    values_from = "total_individuals"
  )

syn_birds_pivot[is.na(syn_birds_pivot)] <- 0

generalists_gent2 <- generalists_gent %>% select(-'Observations', -'typ_cat', -'cat_binary') %>%
  rename('Generalist_Richness' = 'Richness', 'Generalist_Diversity' = 'Diversity')

not_generalists_gent2 <- not_generalists_gent %>% select(-'Observations', -'typ_cat') %>%
  rename('Not_Generalist_Richness' = 'Richness', 'Not_Generalist_Diversity' = 'Diversity')

syn_birds_level <- full_join(generalists_gent2, not_generalists_gent2, by = 'census_block') %>%
  filter(!is.na(cat_binary)) %>%
  filter(!is.na(Generalist_Diversity))

syn_birds_score <- full_join(generalists_gent_score, syn_birds_pivot, by = 'census_block') %>%
  rename('Generalist_Richness' = 'Richness', 'Generalist_Diversity' = 'Diversity') %>%
  select(-'Observations', -'Gentrification.Score')

syn_birds_score2 <- full_join(not_generalists_gent_score, syn_birds_score, by = 'census_block') %>%
  select(-'Observations') %>%
  rename('Not_Generalist_Richness' = 'Richness', 'Not_Generalist_Diversity' = 'Diversity')

#BOXPLOTS 

syn_birds_level_rich <- syn_birds_level %>% select('census_block', 'Generalist_Richness', 'Not_Generalist_Richness', 'cat_binary')
data_long_rich <- pivot_longer(syn_birds_level_rich, cols = ends_with("Richness"), names_to = "Generalist", values_to = "Richness")
data_long_rich$Generalist_bin <- ifelse(data_long_rich$Generalist == 'Generalist_Richness', 'Generalist', 'Not Generalist')
data_long_rich <- data_long_rich %>% select(-'Generalist') 
#%>% rename('Generalist_bin')

syn_birds_level_div <- syn_birds_level %>% select('census_block', 'Generalist_Diversity', 'Not_Generalist_Diversity', 'cat_binary')
data_long_div <- pivot_longer(syn_birds_level_div, cols = ends_with("Diversity"), names_to = "Generalist", values_to = "Diversity")
data_long_div$Generalist_bin <- ifelse(data_long_div$Generalist == 'Generalist_Diversity', 'Generalist', 'Not Generalist')
data_long_div <- data_long_div %>% select(-'Generalist', -'cat_binary')

syn_data_level_final <- full_join(data_long_rich, data_long_div, by = c('census_block', 'Generalist_bin'))

ggplot(syn_data_level_final, aes(x = cat_binary, y = Richness, fill = Generalist_bin)) +
  geom_boxplot(position = position_dodge(width = 0.8)) + 
  labs(y = 'Species Richness', x = element_blank(), fill = element_blank(), 
       color = 'Gentrification Level', title = 'Species Richness vs Gentrification Level', 
       tag = 'Fig. 5A') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        #legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_fill_manual(values = c("cornflowerblue",
                                "darkcyan"))

ggplot(syn_data_level_final, aes(x = cat_binary, y = Diversity, fill = Generalist_bin)) +
  geom_boxplot(position = position_dodge(width = 0.8)) + 
  labs(y = 'Shannon Diversity', x = element_blank(), fill = element_blank(), 
       color = 'Gentrification Level', title = 'Shannon Diversity vs Gentrification Level', 
       tag = 'Fig. 5B') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        #legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_fill_manual(values = c("cornflowerblue",
                               "darkcyan"))


#LINEPLOTS
syn_score_vs_richness <- ggplot(data = syn_birds_score2) +
  geom_point(aes(x = Gentrification.Score, y = Generalist_Richness, color = "Generalist"), alpha = 0.3) +
  #geom_smooth(aes(x = Gentrification.Score, y = Generalist_Richness, color = "Generalist")) +
  geom_smooth(aes(x = Gentrification.Score, y = Generalist_Richness, color = "Generalist")) +
  stat_cor(aes(x = Gentrification.Score, y = Generalist_Richness, color = "Generalist"), alpha = 1) +
  geom_point(aes(x = Gentrification.Score, y = Not_Generalist_Richness, color = "Not Generalist"), alpha = 0.3) +
  #geom_smooth(aes(x = Gentrification.Score, y = Not_Generalist_Richness, color = "Not Generalist")) +
  geom_smooth(aes(x = Gentrification.Score, y = Not_Generalist_Richness, color = "Not Generalist")) +
  stat_cor(aes(x = Gentrification.Score, y = Not_Generalist_Richness, color = "Not Generalist"), alpha = 1, label.y = 32.5) +
  scale_color_manual(values = c("Generalist" = "cornflowerblue", "Not Generalist" = "palevioletred"), name = NULL) +
  theme(legend.position = "right")  # Adjust legend position as needed

syn_score_vs_richness + labs(y = 'Species Richness', x = 'Gentrification Score',
                             title = 'Species Richness vs Gentrification Score', tag = 'Fig. 4A') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  coord_cartesian(expand = FALSE, clip = "off")

syn_score_vs_diversity <- ggplot(data = syn_birds_score2) +
  geom_point(aes(x = Gentrification.Score, y = Generalist_Diversity, color = "Generalist"), alpha = 0.3) +
  #geom_smooth(aes(x = Gentrification.Score, y = Generalist_Diversity, color = "Generalist")) +
  geom_smooth(aes(x = Gentrification.Score, y = Generalist_Diversity, color = "Generalist")) +
  stat_cor(aes(x = Gentrification.Score, y = Generalist_Diversity, color = "Generalist"), alpha = 1, label.y = 3.2) +
  geom_point(aes(x = Gentrification.Score, y = Not_Generalist_Diversity, color = "Not Generalist"), alpha = 0.3) +
  #geom_smooth(aes(x = Gentrification.Score, y = Not_Generalist_Diversity, color = "Not Generalist")) +
  geom_smooth(aes(x = Gentrification.Score, y = Not_Generalist_Diversity, color = "Not Generalist")) +
  stat_cor(aes(x = Gentrification.Score, y = Not_Generalist_Diversity, color = "Not Generalist"), alpha = 1, label.y = 3) +
  scale_color_manual(values = c("Generalist" = "cornflowerblue", "Not Generalist" = "palevioletred"), name = NULL) +
  theme(legend.position = "right")  # Adjust legend position as needed

syn_score_vs_diversity + labs(y = 'Shannon Diversity', x = 'Gentrification Score',
                              title = 'Shannon Diversity vs Gentrification Score', tag = 'Fig. 4B') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  coord_cartesian(expand = FALSE, clip = "off")


# T Test/Wilcoxons Generalists -----------------------------------------------------

syn_data_level_gen_gent <- syn_data_level_final %>% filter(cat_binary == 'Gentrified') %>% 
  filter(Generalist_bin == 'Generalist')
syn_data_level_gen_notgent <- syn_data_level_final %>% filter(cat_binary == 'Not Gentrified') %>% 
  filter(Generalist_bin == 'Generalist')
syn_data_level_notgen_gent <- syn_data_level_final %>% filter(cat_binary == 'Gentrified') %>% 
  filter(Generalist_bin == 'Not Generalist')
syn_data_level_notgen_notgent <- syn_data_level_final %>% filter(cat_binary == 'Not Gentrified') %>% 
  filter(Generalist_bin == 'Not Generalist')

shapiro.test(syn_data_level_gen_gent$Richness)
shapiro.test(syn_data_level_gen_notgent$Richness)
shapiro.test(syn_data_level_gen_gent$Diversity)
shapiro.test(syn_data_level_gen_notgent$Diversity)
shapiro.test(syn_data_level_notgen_gent$Richness)
shapiro.test(syn_data_level_notgen_notgent$Richness)
#normal - use t test
shapiro.test(syn_data_level_notgen_gent$Diversity)
#normal - use t test
shapiro.test(syn_data_level_notgen_notgent$Diversity)

syn_data_level_gent <- syn_data_level_final %>% filter(cat_binary == 'Gentrified')
syn_data_level_notgent <- syn_data_level_final %>% filter(cat_binary == 'Not Gentrified')
syn_data_level_gen <- syn_data_level_final %>% filter(Generalist_bin == 'Generalist')
syn_data_level_notgen <- syn_data_level_final %>% filter(Generalist_bin == 'Not Generalist')

leveneTest(Richness ~ Generalist_bin, data = syn_data_level_gent)
#not normal
leveneTest(Richness ~ Generalist_bin, data = syn_data_level_notgent)
leveneTest(Richness ~ cat_binary, data = syn_data_level_gen)
leveneTest(Richness ~ cat_binary, data = syn_data_level_notgen)

leveneTest(Diversity ~ Generalist_bin, data = syn_data_level_gent)
leveneTest(Diversity ~ Generalist_bin, data = syn_data_level_notgent)
leveneTest(Diversity ~ cat_binary, data = syn_data_level_gen)
leveneTest(Diversity ~ cat_binary, data = syn_data_level_notgen)

#generalists vs non generalists - gentrified
wilcox.test(syn_data_level_gen_gent$Richness, syn_data_level_notgen_gent$Richness)
mean(syn_data_level_gen_gent$Richness)
sd(syn_data_level_gen_gent$Richness)
mean(syn_data_level_notgen_gent$Richness)
sd(syn_data_level_notgen_gent$Richness)
wilcox.test(syn_data_level_gen_gent$Diversity, syn_data_level_notgen_gent$Diversity)
mean(syn_data_level_gen_gent$Diversity)
sd(syn_data_level_gen_gent$Diversity)
mean(syn_data_level_notgen_gent$Diversity)
sd(syn_data_level_notgen_gent$Diversity)

#generalists vs non generalists - not gentrified
wilcox.test(syn_data_level_gen_notgent$Richness, syn_data_level_notgen_notgent$Richness)
mean(syn_data_level_gen_notgent$Richness)
sd(syn_data_level_gen_notgent$Richness)
mean(syn_data_level_notgen_notgent$Richness)
sd(syn_data_level_notgen_notgent$Richness)
wilcox.test(syn_data_level_gen_notgent$Diversity, syn_data_level_notgen_notgent$Diversity)
mean(syn_data_level_gen_notgent$Diversity)
sd(syn_data_level_gen_notgent$Diversity)
mean(syn_data_level_notgen_notgent$Diversity)
sd(syn_data_level_notgen_notgent$Diversity)

#generalists - gentrified vs not gentrified
wilcox.test(syn_data_level_gen_gent$Richness, syn_data_level_gen_notgent$Richness)
wilcox.test(syn_data_level_gen_gent$Diversity, syn_data_level_gen_notgent$Diversity)

#non generalists - gentrified vs not gentrified (T TEST)
#not significant
t.test(syn_data_level_notgen_gent$Richness, syn_data_level_notgen_notgent$Richness)
t.test(syn_data_level_notgen_gent$Diversity, syn_data_level_notgen_notgent$Diversity)

# Generalist Plots - WRONG --------------------------------------------------------

#BOXPLOTS
generalists_gent_observations <- ggplot(generalists_gent, aes(x = as.factor(cat_binary), y = Observations)) +
  geom_boxplot(color = c("cornflowerblue",
                         "darkcyan")) +
  geom_jitter(aes(color = as.factor(cat_binary)), width = 0.2, size = 0.7, alpha = 0.5)

generalists_gent_richness <- ggplot(generalists_gent, aes(x = as.factor(cat_binary), y = Richness)) +
  geom_boxplot(color = c("cornflowerblue",
                         "darkcyan")) +
  geom_jitter(aes(color = as.factor(cat_binary)), width = 0.2, size = 0.7, alpha = 0.5)

generalists_gent_diversity <- ggplot(generalists_gent, aes(x = as.factor(cat_binary), y = Diversity)) +
  geom_boxplot(color = c("cornflowerblue",
                         "darkcyan")) +
  geom_jitter(aes(color = as.factor(cat_binary)), width = 0.2, size = 0.7, alpha = 0.5)

#LINEPLOTS
gen_score_vs_observations <- ggplot(data = generalists_gent_score, 
                                    aes(x = Gentrification.Score, y = Observations)) +
  geom_point(color = 'palevioletred') + 
  #geom_smooth(color = 'salmon4') + 
  geom_smooth(color = 'black', method='lm', linetype = 'dashed') +
  stat_cor()

gen_score_vs_richness <- ggplot(data = generalists_gent_score, 
                                aes(x = Gentrification.Score, y = Richness)) +
  geom_point(color = 'thistle') + 
  #geom_smooth(color = 'salmon4') + 
  geom_smooth(color = 'black', method='lm', linetype = 'dashed') +
  stat_cor()

gen_score_vs_diversity <- ggplot(data = generalists_gent_score, 
                                 aes(x = Gentrification.Score, y = Diversity)) +
  geom_point(color = 'rosybrown1') + 
  #geom_smooth(color = 'salmon4') + 
  geom_smooth(color = 'black', method='lm', linetype = 'dashed') +
  stat_cor()


#CLEAN UP - LINEPLOTS
gen_score_vs_observations + labs(y = 'Number of eBird Observations', x = 'Gentrification Score',
                                 title = 'Number of Observations vs Gentrification Score', tag = 'Fig. 4.1A') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  #expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE, clip = "off")

gen_score_vs_richness + labs(y = 'Species Richness', x = 'Gentrification Score',
                             title = 'Species Richness vs Gentrification Score', tag = 'Fig. 4.1B') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  #expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE, clip = "off")

gen_score_vs_diversity + labs(y = 'Shannon Diversity', x = 'Gentrification Score',
                              title = 'Shannon Diversity vs Gentrification Score', tag = 'Fig. 4.1C') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  #expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE, clip = "off")


#CLEAN UP - BOXPLOTS
generalists_gent_observations + 
  labs(y = 'Number of eBird Observations', x = element_blank(), 
       color = 'Gentrification Level', title = 'Number of Observations vs Gentrification Level', 
       tag = 'Fig. 5.1A') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("cornflowerblue",
                                "darkcyan"))

generalists_gent_richness + labs(y = 'Species Richness', x = element_blank(), 
                                 color = 'Gentrification Level', title = 'Species Richness versus Gentrification Level',
                                 tag = 'Fig. 5.1B') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("cornflowerblue",
                                "darkcyan"))



generalists_gent_diversity + labs(y = 'Shannon Diversity', x = element_blank(), 
                                  color = 'Gentrification Level', title = 'Shannon Diversity versus Gentrification Level',
                                  tag = 'Fig. 5.1C') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("cornflowerblue",
                                "darkcyan"))


# Not Generalist Plots - WRONG ----------------------------------------------------

#BOXPLOTS
not_generalists_gent_observations <- ggplot(not_generalists_gent, aes(x = as.factor(cat_binary), y = Observations)) +
  geom_boxplot(color = c("cornflowerblue",
                         "darkcyan")) +
  geom_jitter(aes(color = as.factor(cat_binary)), width = 0.2, size = 0.7, alpha = 0.5)

not_generalists_gent_richness <- ggplot(not_generalists_gent, aes(x = as.factor(cat_binary), y = Richness)) +
  geom_boxplot(color = c("cornflowerblue",
                         "darkcyan")) +
  geom_jitter(aes(color = as.factor(cat_binary)), width = 0.2, size = 0.7, alpha = 0.5)

not_generalists_gent_diversity <- ggplot(not_generalists_gent, aes(x = as.factor(cat_binary), y = Diversity)) +
  geom_boxplot(color = c("cornflowerblue",
                         "darkcyan")) +
  geom_jitter(aes(color = as.factor(cat_binary)), width = 0.2, size = 0.7, alpha = 0.5)


#LINEPLOTS
not_gen_score_vs_observations <- ggplot(data = not_generalists_gent_score, 
                                        aes(x = Gentrification.Score, y = Observations)) +
  geom_point(color = 'palevioletred') + 
  geom_smooth(color = 'salmon4') + 
  geom_smooth(color = 'black', method='lm', linetype = 'dashed')

not_gen_score_vs_richness <- ggplot(data = not_generalists_gent_score, 
                                    aes(x = Gentrification.Score, y = Richness)) +
  geom_point(color = 'thistle') + 
  geom_smooth(color = 'salmon4') + 
  geom_smooth(color = 'black', method='lm', linetype = 'dashed')

not_gen_score_vs_diversity <- ggplot(data = not_generalists_gent_score, 
                                     aes(x = Gentrification.Score, y = Diversity)) +
  geom_point(color = 'rosybrown1') + 
  geom_smooth(color = 'salmon4') + 
  geom_smooth(color = 'black', method='lm', linetype = 'dashed')


#CLEAN UP - LINEPLOTS
not_gen_score_vs_observations + labs(y = 'Number of eBird Observations', x = 'Gentrification Score',
                                     title = 'Number of Observations vs Gentrification Score', tag = 'Fig. 4.2A') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE, clip = "off")

not_gen_score_vs_richness + labs(y = 'Species Richness', x = 'Gentrification Score',
                                 title = 'Species Richness vs Gentrification Score', tag = 'Fig. 4.2B') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE, clip = "off")

not_gen_score_vs_diversity + labs(y = 'Shannon Diversity', x = 'Gentrification Score',
                                  title = 'Shannon Diversity vs Gentrification Score', tag = 'Fig. 4.2C') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13)) +
  expand_limits(x = 0, y = 0) +
  coord_cartesian(expand = FALSE, clip = "off")


#CLEAN UP - BOXPLOTS
not_generalists_gent_observations + 
  labs(y = 'Number of eBird Observations', x = element_blank(), 
       color = 'Gentrification Level', title = 'Number of Observations vs Gentrification Level', 
       tag = 'Fig. 5.2A') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("cornflowerblue",
                                "darkcyan"))

not_generalists_gent_richness + labs(y = 'Species Richness', x = element_blank(), 
                                     color = 'Gentrification Level', title = 'Species Richness versus Gentrification Level',
                                     tag = 'Fig. 5.2B') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("cornflowerblue",
                                "darkcyan"))



not_generalists_gent_diversity + labs(y = 'Shannon Diversity', x = element_blank(), 
                                      color = 'Gentrification Level', 
                                      title = 'Shannon Diversity versus Gentrification Level',
                                      tag = 'Fig. 5.2C') +
  theme_minimal() +
  theme(axis.title.y = element_text(vjust = 2, size = 13), 
        axis.text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "italic",
                                  margin = margin(10, 0, 10, 0),
                                  size = 13),
        legend.position = "none",
        panel.background = element_rect(fill = NA)) +
  scale_color_manual(values = c("cornflowerblue",
                                "darkcyan"))


# GLMM --------------------------------------------------------------------
#install.packages('glmmTMB')
library(glmmTMB)

#GET LAND AREA
with_aland <- sf_ebird_census2 %>%
  group_by(census_block, verbatimScientificName, ALAND.x) %>%
  summarise(total_individuals = sum(individualCount))

with_aland2 <- with_aland %>%
  pivot_wider(
    id_cols = c("census_block", 'ALAND.x'),
    names_from = "verbatimScientificName",
    values_from = "total_individuals"
  )

with_aland2[is.na(with_aland2)] <- 0

#Calculate richness for each row (total number of species with at least one species observed)
with_aland2$Richness <- apply(X = with_aland2[, c(3:379)], 
                                                #check rows for columns
                                                MARGIN = 1,
                                                FUN = function(x) {
                                                  sum(x > 0)
                                                })

with_aland2$Diversity <- apply(X = with_aland2[, c(3:379)],
                                                 MARGIN = 1,
                                                 FUN = function(x) {
                                                   vegan::diversity(x = x, index = "shannon")
                                                 })

#with_aland2$Beta <- apply(with_aland2[, c(3:379)],
#                                MARGIN = 1,
#                                FUN = function(x) {
#                                  jaccard_sim <- 1 - sum(x) / length(x)
#                                  return(jaccard_sim)
#                                })

with_aland2$Observations <- apply(X = with_aland2[, c(3:379)],
                                                    MARGIN = 1,
                                                    FUN = function(x) {
                                                      sum(x)
                                                    })

with_aland2 <- with_aland2 %>% select('census_block', 'ALAND.x',
                                                         'Richness', 'Diversity', 
                                                         'Observations')

with_aland_scores <- full_join(with_aland2, gentrification_scores, by = c('census_block' = 'FIPS')) %>%
  filter(!is.na(Richness))

with_aland_scores <- with_aland_scores %>% filter(!is.na(Gentrification.Score))
with_aland_scores <- with_aland_scores %>% arrange(desc(Observations)) %>% 
  select('census_block', 'ALAND.x', 'Richness', 'Diversity', 'Observations', 
         'Gentrification.Score', 'NAME')

ndvi_scores <- full_join(with_aland_scores, ndvi, by = 'census_block')  %>% 
  rename('NDVI' = 'MEAN') %>% select('census_block', 'ALAND.x', 'Richness', 
        'Diversity', 'Observations', 'Gentrification.Score', 'NDVI')
ndvi_scores <- unique(ndvi_scores) %>% filter(!is.na(Gentrification.Score))

ndvi_scores$census_block <- as.factor(ndvi_scores$census_block)

tract_biodiv_gent_smaller$census_block <- as.factor(tract_biodiv_gent_smaller$census_block)

ndvi_scores2 <- full_join(ndvi_scores, tract_biodiv_gent_smaller, by = c('census_block', 'Richness', 'Diversity', 'Observations')) %>% filter(!is.na(typ_cat)) %>%
  filter(typ_cat != "[]") %>%
  filter(!is.na(Richness)) %>%
  filter(!is.na(ALAND.x))

#Candidate Models


## Diversity
model_gent_div <- glmmTMB(Diversity ~ Gentrification.Score +
                        #(1|census_block) +
                        offset(log(ALAND.x)), data = ndvi_scores)

model_ndvi_div <- glmmTMB(Diversity ~ NDVI +
                        offset(log(ALAND.x)), data = ndvi_scores)

model_everything_div <- glmmTMB(Diversity ~ Gentrification.Score + NDVI +
                        offset(log(ALAND.x)), data = ndvi_scores)

model_nothing_div <- glmmTMB(Diversity ~ 1 +
                        offset(log(ALAND.x)), data = ndvi_scores)

summary(model_gent_div)
summary(model_ndvi_div)
summary(model_everything_div)
summary(model_nothing_div)

AIC(model_gent_div, model_ndvi_div, model_everything_div, model_nothing_div)

#install.packages("performance")
library(performance)

#plot(compare_performance(model_gent_div, model_ndvi_div, model_everything_div, model_nothing_div, rank = TRUE))

compare_performance(model_gent_div, model_ndvi_div, model_everything_div, model_nothing_div, rank = TRUE)


## Richness
model_gent_rich <- glmmTMB(Richness ~ Gentrification.Score +
                        #(1|census_block) +
                        offset(log(ALAND.x)), data = ndvi_scores)

model_ndvi_rich <- glmmTMB(Richness ~ NDVI +
                        offset(log(ALAND.x)), data = ndvi_scores)

model_everything_rich <- glmmTMB(Richness ~ Gentrification.Score + NDVI +
                        offset(log(ALAND.x)), data = ndvi_scores)

model_nothing_rich <- glmmTMB(Richness ~ 1 +
                        offset(log(ALAND.x)), data = ndvi_scores)

summary(model_gent_rich)
summary(model_ndvi_rich)
summary(model_everything_rich)
summary(model_nothing_rich)

AIC(model_gent_rich, model_ndvi_rich, model_everything_rich, model_nothing_rich)

compare_performance(model_gent_rich, model_ndvi_rich, model_everything_rich, model_nothing_rich, rank = TRUE)

#anova(model_gent_rich, model_ndvi_rich, model_everything_rich, model_nothing_rich, 
#      method = 'LRT')


## Observations
library(glmmTMB)
library(pscl)
#Zero-Inflated Poisson
model_gent_obs_zi <- zeroinfl(Observations ~ Gentrification.Score + offset(log(ALAND.x)), 
                              data = ndvi_scores, 
                              dist = "poisson")

model_ndvi_obs_zi <- zeroinfl(Observations ~ NDVI + offset(log(ALAND.x)), 
                              data = ndvi_scores, 
                              dist = "poisson")

model_everything_obs_zi <- zeroinfl(Observations ~ Gentrification.Score + NDVI + 
                                offset(log(ALAND.x)), 
                              data = ndvi_scores, 
                              dist = "poisson")

model_nothing_obs_zi <- zeroinfl(Observations ~ 1 + offset(log(ALAND.x)), 
                              data = ndvi_scores, 
                              dist = "poisson")

summary(model_gent_obs_zi)
summary(model_ndvi_obs_zi)
summary(model_everything_obs_zi)
summary(model_nothing_obs_zi)

AIC(model_gent_obs_zi, model_ndvi_obs_zi, model_everything_obs_zi, model_nothing_obs_zi)

compare_performance(model_gent_obs_zi, model_ndvi_obs_zi, model_everything_obs_zi, model_nothing_obs_zi, rank = TRUE)

#NEW OBS BOXPLOTS

ndvi_scores2 <- ndvi_scores2 %>% filter(Observations < 15000)
model_gentbi_obs_zi <- zeroinfl(Observations ~ cat_binary + offset(log(ALAND.x)), 
                              data = ndvi_scores2, 
                              dist = "poisson")

fitted_counts <- fitted(model_gentbi_obs_zi)

gent_vs_observations <- ggplot(ndvi_scores2, aes(x = as.factor(cat_binary), y = Observations)) +
  geom_boxplot(color = c("darkslateblue",
                         "darkolivegreen4"))+
  geom_point(aes(y = fitted_counts), colour = 'lightyellow3', position = position_jitter(width = 0.2), size = 0.7)

gent_vs_observations
#+
  #geom_jitter(aes(color = as.factor(cat_binary)), width = 0.2, size = 0.7, alpha = 0.5)

points(tract_biodiv_gent_smaller$cat_binary, fitted_counts, col = "lightyellow3")

# not poisson

model_gent_obs <- glmmTMB(Observations ~ Gentrification.Score +
                             #(1|census_block) +
                             offset(log(ALAND.x)), data = ndvi_scores)

model_ndvi_obs <- glmmTMB(Observations ~ NDVI +
                            offset(log(ALAND.x)), data = ndvi_scores)

model_everything_obs <- glmmTMB(Observations ~ Gentrification.Score + NDVI +
                                   offset(log(ALAND.x)), data = ndvi_scores)

model_nothing_obs <- glmmTMB(Observations ~ 1 +
                                offset(log(ALAND.x)), data = ndvi_scores)

summary(model_gent_obs)
summary(model_ndvi_obs)
summary(model_everything_obs)
summary(model_nothing_obs)

AIC(model_gent_obs, model_ndvi_obs, model_everything_obs, model_nothing_obs)

compare_performance(model_gent_obs, model_ndvi_obs, model_everything_obs, model_nothing_obs, rank = TRUE)

## Beta
#model_gent_beta <- glmmTMB(Beta ~ Gentrification.Score +
#                            #(1|census_block) +
#                            offset(log(ALAND.x)), data = ndvi_scores)

#model_ndvi_beta <- glmmTMB(Beta ~ NDVI +
#                            offset(log(ALAND.x)), data = ndvi_scores)

#model_everything_beta <- glmmTMB(Beta ~ Gentrification.Score + NDVI +
#                                  offset(log(ALAND.x)), data = ndvi_scores)

#model_nothing_beta <- glmmTMB(Beta ~ 1 +
#                               offset(log(ALAND.x)), data = ndvi_scores)

#summary(model_gent_beta)
#summary(model_ndvi_beta)
#summary(model_everything_beta)
#summary(model_nothing_beta)

#AIC(model_gent_beta, model_ndvi_beta, model_everything_beta, model_nothing_beta)

#compare_performance(model_gent_beta, model_ndvi_beta, model_everything_beta, model_nothing_beta, rank = TRUE)


# GLMM - generalists ---------------------------------------------

ndvi_scores_ndvi <- ndvi_scores %>% select('census_block', 'ALAND.x', 'NDVI')
syn_birds_score2$census_block <- as.factor(syn_birds_score2$census_block)

generalists_land <- full_join(ndvi_scores_ndvi, syn_birds_score2, by = 'census_block', multiple = 'any') %>% 
  distinct() %>% filter(!is.na(Generalist_Richness)) %>% filter(!is.na(Gentrification.Score))

## Generalist_Diversity
model_gent_gen_div <- glmmTMB(Generalist_Diversity ~ Gentrification.Score +
                            #(1|census_block) +
                            offset(log(ALAND.x)), data = generalists_land)

model_ndvi_gen_div <- glmmTMB(Generalist_Diversity ~ NDVI +
                            offset(log(ALAND.x)), data = generalists_land)

model_everything_gen_div <- glmmTMB(Generalist_Diversity ~ Gentrification.Score + NDVI +
                                  offset(log(ALAND.x)), data = generalists_land)

model_nothing_gen_div <- glmmTMB(Generalist_Diversity ~ 1 +
                               offset(log(ALAND.x)), data = generalists_land)

summary(model_gent_gen_div)
summary(model_ndvi_gen_div)
summary(model_everything_gen_div)
summary(model_nothing_gen_div)

AIC(model_gent_gen_div, model_ndvi_gen_div, model_everything_gen_div, model_nothing_gen_div)

compare_performance(model_gent_gen_div, model_ndvi_gen_div, model_everything_gen_div, model_nothing_gen_div, rank = TRUE)


## Richness
model_gent_gen_rich <- glmmTMB(Generalist_Richness ~ Gentrification.Score +
                             #(1|census_block) +
                             offset(log(ALAND.x)), data = generalists_land)

model_ndvi_gen_rich <- glmmTMB(Generalist_Richness ~ NDVI +
                             offset(log(ALAND.x)), data = generalists_land)

model_everything_gen_rich <- glmmTMB(Generalist_Richness ~ Gentrification.Score + NDVI +
                                   offset(log(ALAND.x)), data = generalists_land)

model_nothing_gen_rich <- glmmTMB(Generalist_Richness ~ 1 +
                                offset(log(ALAND.x)), data = generalists_land)

summary(model_gent_gen_rich)
summary(model_ndvi_gen_rich)
summary(model_everything_gen_rich)
summary(model_nothing_gen_rich)

AIC(model_gent_gen_rich, model_ndvi_gen_rich, model_everything_gen_rich, model_nothing_gen_rich)

compare_performance(model_gent_gen_rich, model_ndvi_gen_rich, model_everything_gen_rich, model_nothing_gen_rich, rank = TRUE)


# GLMM - non-generalists ---------------------------------------------

ndvi_scores_ndvi <- ndvi_scores %>% select('census_block', 'ALAND.x', 'NDVI')
syn_birds_score2$census_block <- as.factor(syn_birds_score2$census_block)

generalists_land <- full_join(ndvi_scores_ndvi, syn_birds_score2, by = 'census_block', multiple = 'any') %>% 
  distinct() %>% filter(!is.na(Generalist_Richness)) %>% filter(!is.na(Gentrification.Score))

## Diversity
model_gent_not_gen_div <- glmmTMB(Not_Generalist_Diversity ~ Gentrification.Score +
                                #(1|census_block) +
                                offset(log(ALAND.x)), data = generalists_land)

model_ndvi_not_gen_div <- glmmTMB(Not_Generalist_Diversity ~ NDVI +
                                offset(log(ALAND.x)), data = generalists_land)

model_everything_not_gen_div <- glmmTMB(Not_Generalist_Diversity ~ Gentrification.Score + NDVI +
                                      offset(log(ALAND.x)), data = generalists_land)

model_nothing_not_gen_div <- glmmTMB(Not_Generalist_Diversity ~ 1 +
                                   offset(log(ALAND.x)), data = generalists_land)

summary(model_gent_not_gen_div)
summary(model_ndvi_not_gen_div)
summary(model_everything_not_gen_div)
summary(model_nothing_not_gen_div)

AIC(model_gent_not_gen_div, model_ndvi_not_gen_div, model_everything_not_gen_div, model_nothing_not_gen_div)

compare_performance(model_gent_not_gen_div, model_ndvi_not_gen_div, model_everything_not_gen_div, model_nothing_not_gen_div, rank = TRUE)


## Richness
model_gent_not_gen_rich <- glmmTMB(Not_Generalist_Richness ~ Gentrification.Score +
                                 #(1|census_block) +
                                 offset(log(ALAND.x)), data = generalists_land)

model_ndvi_not_gen_rich <- glmmTMB(Not_Generalist_Richness ~ NDVI +
                                 offset(log(ALAND.x)), data = generalists_land)

model_everything_not_gen_rich <- glmmTMB(Not_Generalist_Richness ~ Gentrification.Score + NDVI +
                                       offset(log(ALAND.x)), data = generalists_land)

model_nothing_not_gen_rich <- glmmTMB(Not_Generalist_Richness ~ 1 +
                                    offset(log(ALAND.x)), data = generalists_land)

summary(model_gent_not_gen_rich)
summary(model_ndvi_not_gen_rich)
summary(model_everything_not_gen_rich)
summary(model_nothing_not_gen_rich)

AIC(model_gent_not_gen_rich, model_ndvi_not_gen_rich, model_everything_not_gen_rich, model_nothing_not_gen_rich)

compare_performance(model_gent_not_gen_rich, model_ndvi_not_gen_rich, model_everything_not_gen_rich, model_nothing_not_gen_rich, rank = TRUE)

# IGNORE (models) ---------------------------------------------------------

#model_obs <- glmmTMB(Observations ~ Gentrification.Score + 
#                   offset(log(ALAND.x)), data = with_aland_scores)
#summary(model_obs)

#model_rich <- glmmTMB(Richness ~ Gentrification.Score + 
#                       offset(log(ALAND.x)), data = with_aland_scores)
#summary(model_rich)

#model_div <- glmmTMB(Diversity ~ Gentrification.Score + 
#                       offset(log(ALAND.x)), data = with_aland_scores)
#summary(model_div)

#ndvi_scores$census_block_short <- ndvi_scores$census_block %% 6075000000
#variance_cb <- var(residuals(lmer(Diversity ~ (1|census_block), data = ndvi_scores)))

# Fit the model with starting values
#model_gent <- glmmTMB(Diversity ~ Gentrification.Score + (1|census_block) + offset(log(ALAND.x)), 
#                      data = ndvi_scores,
#                      start = list(theta = c(variance_cb)))

#nlevels(ndvi_scores$census_block)



# Pearson's Correlation -----------------------------------------------------------------

cor(ndvi_scores$NDVI, ndvi_scores$Gentrification.Score, method = 'pearson')

