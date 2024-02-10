#### Description ####
# @author: Charlotta Mirbach

# This script is used to visualize climate data for current and future climate
# Projections: Arctic Polar

# Data was downloaded from CHELSA BIOCLIM+ (Karger et al. 2017) and consists of 4 bioclimatic variables
# These 4 variables were used as predictors in a random forest model to project future species distributions


# load packages -----------------------------------------------------------



library(terra)
library(RColorBrewer)
library(rasterVis)
library(raster)
library(tidyterra)
library(ggplot2)
library(dplyr)
library(scales)
library(rtsVis)
 

# clear workspace ---------------------------------------------------------


rm(list = ls(all.names = TRUE))
gc()
setwd('D:/01-msc/01-thesis/01-data/02-output/02-climate/02_future/future_climate/')

# Function to load and subset rasters based on patterns
load_and_subset <- function(path, pattern = NULL, subset_pattern = NULL, is_current = FALSE) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  rasters <- rast(files)
  if (!is.null(subset_pattern)) {
    subsets <- lapply(subset_pattern, function(pat) terra::subset(rasters, grep(pat, names(rasters), value = TRUE)))
    names(subsets) <- subset_pattern
    return(subsets)
  } else if (is_current) {
    return(list(current = rasters))
  } else {
    return(list(full = rasters))
  }
}

# Load current climate data
current <- rast('D:/01-msc/01-thesis/01-data/02-output/02-climate/01_current/cur_biovars.tif')

# Define patterns for subsetting
biovars <- c("_bio02", "_bio05", "_bio12", "_bio19")
ssps <- c("_ssp126", "_ssp370", "_ssp585")

# Load and subset future climate data
climate_data <- list(
  "2041-2070" = load_and_subset('./2041-2070/', '*tif', biovars),
  "2071-2100" = load_and_subset('./2071-2100/', '*tif', biovars)
)

# Subset for SSPs
ssp_subsets <- function(data, ssps) {
  sapply(ssps, function(ssp) {
    lapply(data, function(biovar) terra::subset(biovar, grep(ssp, names(biovar), value = TRUE)))
  }, simplify = FALSE)
}

# Apply SSP subsetting
ssp_data <- lapply(climate_data, function(period) {
  lapply(period, ssp_subsets, ssps = ssps)
})

# Function to aggregate for lower resolution
aggregate_resolution <- function(data, fact = 10) {
  lapply(data, function(x) aggregate(x, fact = fact))
}

# Aggregate data for plotting
climate_data_agg <- lapply(climate_data, function(period) {
  lapply(period, aggregate_resolution)
})

# Plot current climate data for visualization
plot(current)

# Note: This code assumes the structure of the data and the desired operations are correctly interpreted.
# Adjustments may be needed based on the actual data and specific requirements.



#  plot current climate variables ---------------------------------------------------------
plot_bio02_1981 <- autoplot(bio02_1981) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_light() +
  labs(
    fill = 'Bio02, 1981-2010',
    title = "Mean Diurnal Temperature Range [°C], 1981-2010"
  ) +
  theme(legend.position = 'bottom')

plot_bio05_1981 <- autoplot(bio05_1981) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_light() +
  labs(
    fill = 'Bio05, 1981-2010',
    title = "Mean maximum temperature of warmest month [°C], 1981-2010"
  ) +
  theme(legend.position = 'bottom')+
  scale_fill_whitebox_c(
    palette = "deep", direction = -1,
    labels = scales::label_number(suffix = ""),  # Replace 'units' with the unit of your variable
    n.breaks = 5
  ) 

salix <- read.csv('D:/01-msc/01-thesis/01-data/02-output/01-occurences/salix_lanata.csv', sep=';')
salix_v <- vect(salix, geom=c("lon", "lat"), crs="+proj=longlat +datum=WGS84")
plot(salix_v)

occ_plot <- ggplot() +
  geom_spatraster(data = bio05_1981) +
  geom_spatvector(data = salix_v, aes(fill = source), color = NA)  +
  scale_fill_whitebox_c(
    palette = "deep", direction = -1,
    labels = scales::label_number(suffix = "º"),
    n.breaks = 5
  ) +
  theme_minimal() +
  coord_sf(crs = "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  labs(
    fill = 'Bio05, 1981-2010',
    title = "Occurrences of S. lanata" ,
    subtitle = "Mean maximum temperature of warmest month [°C], 1981-2010"
  ) +
  theme(legend.position = 'bottom')


# annual prec
plot_bio12_1981 <- autoplot(bio12_1981) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_light() +
  labs(
    fill = 'Bio12, 1981-2010',
    title = "Annual Precipitation Amount [kg m^-2], 1981-2010"
  ) +
  theme(legend.position = 'bottom') +
  scale_fill_whitebox_c(
    palette = "muted", direction = 1,
    labels = scales::label_number(suffix = ""),  # Replace 'units' with the unit of your variable
    n.breaks = 5
  ) 

plot_bio19_1981 <- autoplot(bio19_1981) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_light() +
  labs(
    fill = 'Bio12, 1981-2010',
    title = "Mean Monthly Precipitation of Coldest Quarter [kg m^-2], 1981-2010"
  ) +
  theme(legend.position = 'bottom') +
  scale_fill_whitebox_c(
    palette = "viridi", direction = -1,
    labels = scales::label_number(suffix = ""),  # Replace 'units' with the unit of your variable
    n.breaks = 5
  ) 



# plot future climate variables for 2041 - 2070 -------------------------------------------------------------

setwd('D:/01-msc/01-thesis/09-graphics/03_bioclim_maps/')

# mean diurnal range  ====

gc()
dev.off()
plot_bio02_2041 <- autoplot(bio02_2041, ncol =3) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  #geom_spatraster(data = bio02_2041) +
 # facet_wrap(~ lyr, ncol = 3) + 
  scale_fill_whitebox_c(
    palette = 'muted',
    labels = scales::label_number(suffix ='?'),
    limits = c(0,15)
  ) +
  labs(fill = 'Temperature [?C]',
       title = ' Mean Diurnal Temperature Range, 2041-2071') +
  theme_minimal()
png('bio02_2041.png', width=900, height=900)
print(plot_bio02_2041)
dev.off()

rm(plot_bio02_2041)

# summer temp ====

plot_bio05_2041 <- autoplot(bio05_2041, ncol = 3) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  # geom_spatraster(data = bio05_2041) +
  # facet_wrap(~ lyr, ncol = 3) + 
  scale_fill_whitebox_c(
    palette = 'muted',
    labels = scales::label_number(suffix ='?'),
    limits = c(0,40)
  ) +
  labs(fill = 'Temperature [?C]',
       title = ' Mean Daily Temperature of Warmest Month, 2041-2071') +
  theme_minimal()

png('bio05_2041.png', width=900, height=900)
print(plot_bio05_2041)
dev.off()

rm(plot_bio05_2041)


  
  # annual prec====

plot_bio12_2041 <- autoplot(bio12_2041, ncol = 3) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  # geom_spatraster(data = bio12_2041) +
  # facet_wrap(~ lyr, ncol = 3) + 
  scale_fill_whitebox_c(
    palette = 'deep',
    labels = scales::label_number(suffix =' '),
    limits = c(0,3000)
  ) +
  labs(fill = 'Precipitation [mm]',
       title = ' Annual Precipitation Amount, 2041-2071') +
  theme_minimal()
png('bio12_2041.png', width=900, height=900)
print(plot_bio12_2041)
dev.off()
rm(plot_bio12_2041)


# cold season prec ====
plot_bio19_2041 <- autoplot(bio19_2041, ncol = 3) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  # geom_spatraster(data = bio12_2041) +
  # facet_wrap(~ lyr, ncol = 3) + 
  scale_fill_whitebox_c(
    palette = 'deep',
    labels = scales::label_number(suffix =' '),
    limits = c(0,600)
  ) +
  labs(fill = 'Precipitation [mm]',
       title = 'Mean Monthly Precipitation Amount of the Coldest Quarter, 2041-2071') +
  theme_minimal()
png('bio19_2041.png', width=900, height=900)
print(plot_bio19_2041)
dev.off()

rm(plot_bio19_2041)

#  plot future climate variables for 2071 - 2100 --------------------------
# mean diurnal range ====

gc()

plot_bio02_2071 <- autoplot(bio02_2071, ncol =3) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  #geom_spatraster(data = bio02_2071) +
  # facet_wrap(~ lyr, ncol = 3) + 
  scale_fill_whitebox_c(
    palette = 'muted',
    labels = scales::label_number(suffix ='?'),
    limits = c(0,15)
  ) +
  labs(fill = 'Temperature [?C]',
       title = ' Mean Diurnal Temperature Range, 2071-2071') +
  theme_minimal()

png('bio02_2071.png', width=900, height=900)
print(plot_bio02_2071)
dev.off()

rm(plot_bio02_2071)

# summer temp ====

plot_bio05_2071 <- autoplot(bio05_2071, ncol = 3) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  # geom_spatraster(data = bio05_2071) +
  # facet_wrap(~ lyr, ncol = 3) + 
  scale_fill_whitebox_c(
    palette = 'muted',
    labels = scales::label_number(suffix ='?'),
    limits = c(0,40)
  ) +
  labs(fill = 'Temperature [?C]',
       title = ' Mean Daily Temperature of Warmest Month, 2071-2071') +
  theme_minimal()

png('bio05_2071.png', width=900, height=900)
print(plot_bio05_2071)
dev.off()

rm(plot_bio05_2071)

# annual prec ====

plot_bio12_2071 <- autoplot(bio12_2071, ncol = 3) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  # geom_spatraster(data = bio12_2071) +
  # facet_wrap(~ lyr, ncol = 3) + 
  scale_fill_whitebox_c(
    palette = 'deep',
    labels = scales::label_number(suffix =' '),
    limits = c(0,3000)
  ) +
  labs(fill = 'Precipitation [mm]',
       title = ' Annual Precipitation Amount, 2071-2071') +
  theme_minimal()


png('bio12_2071.png', width=900, height=900)
print(plot_bio12_2071)
dev.off()


rm(plot_bio12_2071)

# cold season prec ====
plot_bio19_2071 <- autoplot(bio19_2071, ncol = 3) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  # geom_spatraster(data = bio12_2071) +
  # facet_wrap(~ lyr, ncol = 3) + 
  scale_fill_whitebox_c(
    palette = 'deep',
    labels = scales::label_number(suffix =''),
    limits = c(0,600)
  ) +
  labs(fill = 'Precipitation [mm]',
       title = 'Mean Monthly Precipitation Amount of the Coldest Quarter, 2071-2071') +
  theme_minimal()

png('bio19_2071.png', width=900, height=900)
print(plot_bio19_2071)
dev.off()

rm(plot_bio19_2071)


############ plot current & obs #############
plot_bio02_1981
plot_bio05_1981
plot_bio12_1981
plot_bio19_1981
plot(salix_v)

########### occ plot again ##########
library(ggplot2)
library(sf)  # For handling spatial data

# crop salix v to e
# define spatial extent
e <- ext(25, 160,60,82)

# crop salix v to e
salix_v <- crop(salix_v, e)

# Convert SpatVector to an sf object for ggplot compatibility
salix_v_sf <- as_sf(salix_v)

# Plotting salix_v with the specified CRS
occ_plot_2 <- ggplot(data = salix_v_sf) +
  geom_sf() +
  theme_minimal() +
  coord_sf(crs = "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  labs(
    title = "Occurrences of S. lanata",
    subtitle = "Spatial distribution"
  )

occ_plot_3 <- ggplot(data = salix_v_sf) +
  geom_sf() +
  theme_light() +
  coord_sf(crs = "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  labs(
    title = "",
    subtitle = ""
  )

# Display the plot
occ_plot_2
plot_bio02_1981
plot_bio05_1981
plot_bio12_1981
plot_bio19_1981

setwd('D:/01-msc/01-thesis/09-graphics/09-data_input_plots/')
ggsave('occ_plot.png', occ_plot_2, width = 15, height = 10, units = "cm")
ggsave('occ_plot_3.png', occ_plot_3, width = 15, height = 10, units = "cm")
ggsave('plot_bio02_1981.png', plot_bio02_1981, width = 15, height = 10, units = "cm")
ggsave('plot_bio05_1981.png', plot_bio05_1981, width = 15, height = 10, units = "cm")
ggsave('plot_bio12_1981.png', plot_bio12_1981, width = 15, height = 10, units = "cm")
ggsave('plot_bio19_1981.png', plot_bio19_1981, width = 15, height = 10, units = "cm")


########## stack and tilt ###########
plot_bio02_1981 <- autoplot(bio02_1981) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_light() +
  labs(
    fill = 'Bio02, 1981-2010',
    title = ""
  ) +
  theme(legend.position = 'bottom')

plot_bio05_1981 <- autoplot(bio05_1981) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_light() +
  labs(
    fill = 'Bio05, 1981-2010',
    title = ""
  ) +
  theme(legend.position = 'bottom')+
  scale_fill_whitebox_c(
    palette = "deep", direction = -1,
    labels = scales::label_number(suffix = ""),  # Replace 'units' with the unit of your variable
    n.breaks = 5
  ) 


# annual prec
plot_bio12_1981 <- autoplot(bio12_1981) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_light() +
  labs(
    fill = 'Bio12, 1981-2010',
    title = ""
  ) +
  theme(legend.position = 'bottom') +
  scale_fill_whitebox_c(
    palette = "muted", direction = 1,
    labels = scales::label_number(suffix = ""),  # Replace 'units' with the unit of your variable
    n.breaks = 5
  ) 

plot_bio19_1981 <- autoplot(bio19_1981) +
  coord_sf(crs =
             "+proj=laea +lat_0=90 +lon_0=90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  theme_light() +
  labs(
    fill = 'Bio12, 1981-2010',
    title = ""
  ) +
  theme(legend.position = 'bottom') +
  scale_fill_whitebox_c(
    palette = "viridi", direction = -1,
    labels = scales::label_number(suffix = ""),  # Replace 'units' with the unit of your variable
    n.breaks = 5
  ) 

# Display the plot
occ_plot_2
plot_bio02_1981
plot_bio05_1981
plot_bio12_1981
plot_bio19_1981

setwd('D:/01-msc/01-thesis/09-graphics/09-data_input_plots/')
ggsave('occ_plot.png', occ_plot_2, width = 15, height = 10, units = "cm")
ggsave('occ_plot_4.png', occ_plot_3, width = 15, height = 10, units = "cm")
ggsave('plot_bio02_1981_1.png', plot_bio02_1981, width = 15, height = 10, units = "cm")
ggsave('plot_bio05_1981_1.png', plot_bio05_1981, width = 15, height = 10, units = "cm")
ggsave('plot_bio12_1981_1.png', plot_bio12_1981, width = 15, height = 10, units = "cm")
ggsave('plot_bio19_1981_1.png', plot_bio19_1981, width = 15, height = 10, units = "cm")

