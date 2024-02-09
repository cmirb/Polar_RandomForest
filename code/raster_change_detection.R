# @author: Charlotta Mirbach

#### Change Detection an Plotting in Polar Projection ####

### Description ###

# in this script, we will compare the current and future rasters and plot the change in the frequency of the different classes
# the data was preprocessed for my master thesis on Arctic Greening
# projections are based on CHELSA climate data and CMIP6 models
# projections are based on a random forest classification algorithm which was applied to caluclate
# how the distribution of salix lanata in the Russian Arctic will change under different climate scenarios

# occurrence data was based on:
# my own field recordings from 2021 (Siberia Expedition)
# GBIF and iNaturalist data
# Sentinel-2 satellite data 

# make maps of projected change

## please note that I have gotten better at loops in the meantime ##


### clean workspace ###
rm(list = ls(all.names = TRUE)) 
gc() 
setwd('D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/cropped/')

### load libraries ###

library(terra)
library(RColorBrewer)
library(rasterVis)
library(raster)
library(tidyterra)
library(ggplot2)
library(dplyr)
library(scales)


### load rasters ###
current <- rast('./current.tif')
plot(current)

ssp126_2041 <- rast('./ssp126_2041_crop.tif')
ssp126_2071 <- rast('./ssp126_2071_crop.tif')

ssp370_2041 <- rast('./ssp370_2041_crop.tif')
ssp370_2071 <- rast('./ssp370_2071_crop.tif')

ssp585_2041 <- rast('./ssp585_2041_crop.tif')
ssp585_2071 <- rast('./ssp585_2071_crop.tif')

### reclassify rasters ###

reclass_matrix <- matrix(c(0, 0.5, 0, 0.5, 1, 1), ncol=3, byrow=TRUE)

reclassified_raster <- classify(current, reclass_matrix, include.lowest = T, brackets = T)
plot(reclassified_raster)
freq(reclassified_raster)

ssp126_2041_re <- classify(ssp126_2041, reclass_matrix)
ssp126_2071_re <- classify(ssp126_2071, reclass_matrix)

ssp370_2041_re <- classify(ssp370_2041, reclass_matrix)
ssp370_2071_re <- classify(ssp370_2071, reclass_matrix)

ssp585_2041_re <- classify(ssp585_2041, reclass_matrix)
ssp585_2071_re <- classify(ssp585_2071, reclass_matrix)


### convert raster to data frame ###
current_re_freq <- as.data.frame(freq(reclassified_raster))

ssp126_2041_re_freq <- as.data.frame(freq(ssp126_2041_re))
ssp126_2071_re_freq <- as.data.frame(freq(ssp126_2071_re))

ssp370_2041_re_freq <- as.data.frame(freq(ssp370_2041_re))
ssp370_2071_re_freq <- as.data.frame(freq(ssp370_2071_re))

ssp585_2041_re_freq <- as.data.frame(freq(ssp585_2041_re))
ssp585_2071_re_freq <- as.data.frame(freq(ssp585_2071_re))


# rename the count column in each data frame to a unique name
current_re_df <- rename(current_re_freq, count_current = count)
ssp126_2041_df <- rename(ssp126_2041_re_freq, count_126_2041 = count)
ssp126_2071_df <- rename(ssp126_2071_re_freq, count_126_2071 = count)
ssp370_2041_df <- rename(ssp370_2041_re_freq, count_370_2041 = count)
ssp370_2071_df <- rename(ssp370_2071_re_freq, count_370_2071 = count)
ssp585_2041_df <- rename(ssp585_2041_re_freq, count_585_2041 = count)
ssp585_2071_df <- rename(ssp585_2071_re_freq, count_585_2071 = count)

# Replace numeric values in 'layer' column with the provided names from the CMIP6 models
replace_layer_names <- function(df) {
  df %>% mutate(layer = recode(layer,
                               "1" = "GFDL-ESM4",
                               "2" = "IPSL-CM6A-LR",
                               "3" = "MPI-ESM1-2-HR",
                               "4" = "MRI-ESM2-0",
                               "5" = "UKESM1-0-LL"))
}

# Apply the renaming function to each dataframe
ssp126_2041_df <- replace_layer_names(ssp126_2041_df)
ssp126_2071_df <- replace_layer_names(ssp126_2071_df)
ssp370_2041_df <- replace_layer_names(ssp370_2041_df)
ssp370_2071_df <- replace_layer_names(ssp370_2071_df)
ssp585_2041_df <- replace_layer_names(ssp585_2041_df)
ssp585_2071_df <- replace_layer_names(ssp585_2071_df)
current_re_df <- replace_layer_names(current_re_df)

# Combine the data frames based on the layer and value columns
all_data <- ssp126_2041_df %>%
  left_join(ssp370_2041_df[, c("layer", "value", "count_370_2041")], by = c("layer", "value")) %>%
  left_join(ssp585_2041_df[, c("layer", "value", "count_585_2041")], by = c("layer", "value")) %>%
  left_join(ssp126_2071_df[, c("layer", "value", "count_126_2071")], by = c("layer", "value")) %>%
  left_join(ssp370_2071_df[, c("layer", "value", "count_370_2071")], by = c("layer", "value")) %>%
  left_join(ssp585_2071_df[, c("layer", "value", "count_585_2071")], by = c("layer", "value"))

# This data frame contains the count of pixels for each layer and value combination
# Export the final combined data frame to a CSV
write.csv(all_data, "D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/binary/2024_frequencies_tr_0.5.csv", row.names = FALSE)


# Calculate the approximate area of a single pixel in square kilometers
pixel_area_km2 <- (0.008333333 * 111)^2 # the values are based on raster resolution and CRS

# Update all the count columns in the all_data dataframe
all_data <- all_data %>%
  mutate(across(starts_with("count"), ~ . * pixel_area_km2))

# Write the data frame to a file
write.csv(all_data, "D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/binary/2024_raster_frequencies_km2.csv", row.names = FALSE)

### Plot Raster with polar projection ###
# current
plot_current <- autoplot(current) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_minimal() +
  labs(
    fill = expression(paste("Current distribution of ", italic("Salix lanata"))),
    title = "CHELSA BIOCLIM+; 1981-2010"
  ) +
  theme(legend.position = 'bottom') +
  scale_fill_gradientn(colours = c('grey', 'forestgreen', 'darkgreen'),
                       breaks=c(0, 0.6, 0.9999), labels=format(c('0', '0.6','1')), na.value = 'transparent')

windows()
plot_current


plot_current <- autoplot(current) +
  coord_sf(crs = "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_minimal() +
  labs(
    fill = expression(paste("Current distribution of ", italic("Salix lanata"))),
    title = "CHELSA BIOCLIM+; 1981-2010"
  ) +
  theme(legend.position = 'bottom') +
  scale_fill_manual(values = c("grey", "forestgreen"), 
                    name = "Presence",
                    breaks = c(0, 1),
                    labels = c("Absence", "Presence"))

summary(reclassified_raster)
plot(reclassified_raster)


### Export rasters ###
# writeRaster(reclassified_raster, 'D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/binary/current.tif')
# writeRaster(ssp126_2041_re, 'D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/binary/ssp126_2041_re.tif')
# writeRaster(ssp126_2071_re, 'D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/binary/ssp126_2071_re.tif')
# writeRaster(ssp370_2041_re, 'D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/binary/ssp370_2041_re.tif')
# writeRaster(ssp370_2071_re, 'D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/binary/ssp370_2071_re.tif')
# writeRaster(ssp585_2041_re, 'D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/binary/ssp585_2041_re.tif')
# writeRaster(ssp585_2071_re, 'D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/binary/ssp585_2071_re.tif')
# 


### Reclassify the raster values based on the threshold ###
binary_raster <- current > 0.6

### reporject rasters if needed ###
crs_proj <- "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
proj_raster <- terra::project(binary_raster, crs_proj)

current <- terra::project(current, crs_proj)

ssp126_2041 <- terra::project(ssp126_2041, crs_proj)

ssp126_2071 <- terra::project(ssp126_2071, crs_proj)
ssp370_2041 <- terra::project(ssp370_2041, crs_proj)
ssp370_2071 <- terra::project(ssp370_2071, crs_proj)
ssp585_2041 <- terra::project(ssp585_2041, crs_proj)
ssp585_2071 <- terra::project(ssp585_2071, crs_proj)

writeRaster(ssp126_2041, './ssp126_2041_proj.tif')
writeRaster(ssp126_2071, './ssp126_2071_proj.tif')
writeRaster(ssp370_2041, './ssp370_2041_proj.tif')
writeRaster(ssp370_2071, './ssp370_2071_proj.tif')
writeRaster(ssp585_2041, './ssp585_2041_proj.tif')
writeRaster(ssp585_2071, './ssp585_2071_proj.tif')
writeRaster(current, 'D:/01-msc/01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/cropped/current_proj.tif')

ssp585_2041 <- rast('./ssp585_2041_proj.tif')
ssp585_2071 <- rast('./ssp585_2071_proj.tif')

plot(ssp585_2041)
plot(ssp585_2071)

### Plot the rasters ###
library(ggplot2)

plot_current <- ggplot() +
  geom_spatraster(data = current) +
 # facet_wrap(~lyr, ncol = 2) +
  scale_fill_whitebox_c(
    name = "Class. Prob.",
    palette = "muted",
    labels = scales::label_number(suffix = ""),
    #n.breaks = 2,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    fill = "",
    title = "CHELSA BIOCLIM+, 1981-2010, Predicted Distribution",
    subtitle = "Random Forest Prediction (Classification Probability)"
  ) +
  theme_minimal()

plot_126_2041 <- ggplot() +
  geom_spatraster(data = ssp126_2041) +
  facet_wrap(~lyr, ncol = 2) +
  theme_minimal() +
  scale_fill_whitebox_c(
    name = "Class. Prob.",
    palette = "muted",
    labels = scales::label_number(suffix = ""),
    n.breaks = 5,
    guide = guide_legend(reverse = TRUE),
    limits = c(0, 1) 
  ) +
  labs(
    fill = "",
    title = "CHELSA BIOCLIM+, 2041-2070, SSP 1.26, Predicted Distribution",
    subtitle = "Random Forest Prediction (Classification Probability)"
  ) +
  theme(
    plot.title = element_text(size = 26),        
    plot.subtitle = element_text(size = 22),     
    axis.title = element_text(size = 24),        
    axis.text = element_text(size = 20),         
    legend.text = element_text(size = 20),       
    legend.title = element_text(size = 18),
    strip.text.x = element_text(size = 22)
  )

plot_126_2071 <- ggplot() +
  geom_spatraster(data = ssp126_2071) +
  facet_wrap(~lyr, ncol = 2) +
  theme_minimal() +
  scale_fill_whitebox_c(
    name = "Class. Prob.",
    palette = "muted",
    labels = scales::label_number(suffix = ""),
    n.breaks = 5,
    guide = guide_legend(reverse = TRUE),
    limits = c(0, 1) 
  ) +
  labs(
    fill = "",
    title = "CHELSA BIOCLIM+, 2071-2100, SSP 1.26, Predicted Distribution",
    subtitle = "Random Forest Prediction (Classification Probability)"
  ) +
  theme(
    plot.title = element_text(size = 26),        
    plot.subtitle = element_text(size = 22),     
    axis.title = element_text(size = 24),        
    axis.text = element_text(size = 20),         
    legend.text = element_text(size = 20),       
    legend.title = element_text(size = 18),
    strip.text.x = element_text(size = 22)
  )

plot_370_2041 <- ggplot() +
  geom_spatraster(data = ssp370_2041) +
  facet_wrap(~lyr, ncol = 2) +
  theme_minimal() +
  scale_fill_whitebox_c(
    name = "Class. Prob.",
    palette = "muted",
    labels = scales::label_number(suffix = ""),
    n.breaks = 5,
    guide = guide_legend(reverse = TRUE),
    limits = c(0, 1) 
  ) +
  labs(
    fill = "",
    title = "CHELSA BIOCLIM+, 2041-2070, SSP 3.70, Predicted Distribution",
    subtitle = "Random Forest Prediction (Classification Probability)"
  ) +
  theme(
    plot.title = element_text(size = 26),        
    plot.subtitle = element_text(size = 22),     
    axis.title = element_text(size = 24),        
    axis.text = element_text(size = 20),         
    legend.text = element_text(size = 20),       
    legend.title = element_text(size = 18),
    strip.text.x = element_text(size = 22)
  )

plot_370_2071 <- ggplot() +
  geom_spatraster(data = ssp370_2071) +
  facet_wrap(~lyr, ncol = 2) +
  theme_minimal() +
  scale_fill_whitebox_c(
    name = "Class. Prob.",
    palette = "muted",
    labels = scales::label_number(suffix = ""),
    n.breaks = 5,
    guide = guide_legend(reverse = TRUE),
    limits = c(0, 1) 
  ) +
  labs(
    fill = "",
    title = "CHELSA BIOCLIM+, 2071-2100, SSP 3.70, Predicted Distribution",
    subtitle = "Random Forest Prediction (Classification Probability)"
  ) +
  theme(
    plot.title = element_text(size = 26),        
    plot.subtitle = element_text(size = 22),     
    axis.title = element_text(size = 24),        
    axis.text = element_text(size = 20),         
    legend.text = element_text(size = 20),       
    legend.title = element_text(size = 18),
    strip.text.x = element_text(size = 22)
  )

plot_585_2041 <- ggplot() +
  geom_spatraster(data = ssp585_2041) +
  facet_wrap(~lyr, ncol = 2) +
  theme_minimal() +
  scale_fill_whitebox_c(
    name = "Class. Prob.",
    palette = "muted",
    labels = scales::label_number(suffix = ""),
    n.breaks = 5,
    guide = guide_legend(reverse = TRUE),
    limits = c(0, 1) 
  ) +
  labs(
    fill = "",
    title = "CHELSA BIOCLIM+, 2041-2070, SSP 5.85, Predicted Distribution",
    subtitle = "Random Forest Prediction (Classification Probability)"
  ) +
  theme(
    plot.title = element_text(size = 26),        
    plot.subtitle = element_text(size = 22),     
    axis.title = element_text(size = 24),        
    axis.text = element_text(size = 20),         
    legend.text = element_text(size = 20),       
    legend.title = element_text(size = 18),
    strip.text.x = element_text(size = 22)
  )

plot_585_2071 <- ggplot() +
  geom_spatraster(data = ssp585_2071) +
  facet_wrap(~lyr, ncol = 2) +
  theme_minimal() +
  scale_fill_whitebox_c(
    name = "Class. Prob.",
    palette = "muted",
    labels = scales::label_number(suffix = ""),
    n.breaks = 5,
    guide = guide_legend(reverse = TRUE),
    limits = c(0, 1) 
  ) +
  labs(
    fill = "",
    title = "CHELSA BIOCLIM+, 2071-2100, SSP 5.85, Predicted Distribution",
    subtitle = "Random Forest Prediction (Classification Probability)"
  ) +
  theme(
    plot.title = element_text(size = 26),        
    plot.subtitle = element_text(size = 22),     
    axis.title = element_text(size = 24),        
    axis.text = element_text(size = 20),         
    legend.text = element_text(size = 20),       
    legend.title = element_text(size = 18),
    strip.text.x = element_text(size = 22)
  )

# print plots ###########
setwd('D:/01-msc/01-thesis/09-graphics/07_rf_projection_maps/')

png('ssp126_2041.png', width=1200, height=1200)
print(plot_126_2041)
dev.off()

png('ssp126_2071.png', width=1200, height=1200)
print(plot_126_2071)
dev.off()

png('ssp370_2041.png', width=1200, height=1200)
print(plot_370_2041)
dev.off()

png('ssp370_2071.png', width=1200, height=1200)
print(plot_370_2071)
dev.off()

png('ssp585_2041.png', width=1200, height=1200)
print(plot_585_2041)
dev.off()

png('ssp585_2071.png', width=1200, height=1200)
print(plot_585_2071)
dev.off()

png('current.png', width = 900, height = 600)
print(plot_current)
dev.off()
