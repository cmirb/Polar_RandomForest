#### Description ####
# @author: Charlotta Mirbach

# This script is used to project future climate data using a random forest model
# The random forest model was trained on current climate data and is used to project future climate data
# The goal is to use the RF model to predict future distributions of species (S. lanata)


# Preamble ---------------------------------------------------------------------
rm(list = ls(all.names = TRUE)) 
gc() 
setwd('E:/01-msc/')

library(terra)


# Load Data --------------------------------------------------------------------
v <- vect("D:/landmask/landmask_30s.shp")
chelsa <- list.files(('E:/output/1981-2010/chelsa_1981-2010.tif'), full.names = T)
chelsa <- rast('E:/output/1981-2010/chelsa_1981-2010.tif')
r <- rast(chelsa)
names(r)
rt <- subset(r, c(2,5,12,19))
r <- rt
names(rt)
v <- v[1,] # choose first layer of spat vector 

# Crop and mask raster --------------------------------------------------------
x <- crop(r, ext(v) + .01)
y <- mask(x, v)

# Export raster ----------------------------------------------------------------
terra::writeRaster(y, "D:/output/2041-2070/ssp585/ukesm/chelsa-2041-ssp585-ukesm-crop.tif", overwrite=T)
var_names <- c('CHELSA_bio01_1981-2010_V.2.1',
               'CHELSA_bio02_1981-2010_V.2.1',
               'CHELSA_bio03_1981-2010_V.2.1',
               'CHELSA_bio04_1981-2010_V.2.1',
               'CHELSA_bio05_1981-2010_V.2.1',
               'CHELSA_bio06_1981-2010_V.2.1',
               'CHELSA_bio07_1981-2010_V.2.1',
               'CHELSA_bio08_1981-2010_V.2.1',
               'CHELSA_bio09_1981-2010_V.2.1',
               'CHELSA_bio10_1981-2010_V.2.1',
               'CHELSA_bio11_1981-2010_V.2.1',
               'CHELSA_bio12_1981-2010_V.2.1',
               'CHELSA_bio13_1981-2010_V.2.1',
               'CHELSA_bio14_1981-2010_V.2.1',
               'CHELSA_bio15_1981-2010_V.2.1',
               'CHELSA_bio16_1981-2010_V.2.1',
               'CHELSA_bio17_1981-2010_V.2.1',
               'CHELSA_bio18_1981-2010_V.2.1',
               'CHELSA_bio19_1981-2010_V.2.1')

var_names <- c('CHELSA_bio02_1981-2010_V.2.1',
               'CHELSA_bio05_1981-2010_V.2.1',
               'CHELSA_bio12_1981-2010_V.2.1',
               'CHELSA_bio19_1981-2010_V.2.1')

# select variables 
# bio02	-0.45 mean diurnal range, used to calc bio03
# bio05	-0.53 temp of warmest month
# bio12	0.21 annual precipitation
# bio19	0.26 prec coldest quarter

bio <- subset(chelsa, c(2,5,12,19))
names(bio)

# Load future projections ------------------------------------------------------
# 2041-2070
# ssp126
path_future <- './01-thesis/01-data/02-output/06-predictions/04-cmip6-proj/'

# note: files are in multiple subdirectories
# directory names should be excluded from file lists
# but since R apparently doesnt have a native function for that
# new files will be written in the respective export folder
dat.files  <- list.files(path="./01-thesis/01-data/02-output/02-climate/02_future/2041-2070",
                         recursive=F,
                         pattern="*_ovr", # changed file extention due to interrupted loop, otherwise use *.tif
                         full.names=T,
                         include.dirs = F)
dat.files

# Call random forest model -----------------------------------------------------------
library(randomForest)
rff <- load('./01-thesis/01-data/02-output/07-models/01_rf_4vars/rf_tuned.RData')




# create raster bricks
# Project RF to different climate scenarios -------------------------------------
for (i in 1:length(dat.files)) {
  cat(i, '\t') # returns number of file it'S working on
  # start time
  cat(i, '\t') # returns number of file it'S working on
  s <- Sys.time()
  
  print(dat.files[i])
  temp <- rast(dat.files[i])
  
  names(temp) <- var_names
  pred <- predict(temp, rrf, na.rm=T)
  terra::writeRaster(pred, filename=paste0(dat.files[i], "_rf_proj.tif"), overwrite=TRUE)
  # writeRaster(pred,
  #             paste0('.',
  #                     dat.files[i],
  #                    '_proj_rf.tif'),
  #             overwrite = T)
  
  e <- Sys.time()
  # returns how long one loop took
  cat(e-s,'\n')
  
  gc()  
}

rrf

projections <- list.files(path = 'D:/output/2041-2070',
                          recursive=T,
                          pattern="*_rf_proj.tif",
                          full.names=T)

# Apply random model to future climate ------------------------------------------
# install.packages('randomForest')
library(randomForest)

# load random forest model 
rff <- load('D:/model/rf_4vars/rf_tuned.RData')
pfut <- predict(fut, rrf, na.rm=TRUE)
pcur <- predict(cur, rrf, na.rm=T)
windows()
plot(pfut, main = 'RF projection, SSP370, MRI-ESM, 2041-2070, salix lanata')
plot(pcur)
writeRaster(pfut, 'D:/projections/rf_4vars/ssp370-mri-esm-2041-70.tif')
writeRaster(pcur, 'D:/projections/rf_4vars/chelsa-1981-2010.tif')



ssp585_mpi_2041 <- rast('D:/projections/proj-rf-2041-ssp585-mpi.tif')
plot(ssp585_mpi_2041)
ssp370_mpi_2041 <- rast('D:/projections/proj-rf-2041-ssp370-mpi.tif')

cur_ssp370 <- ssp370_mpi_2041 - pcur
cur_ssp585 <- ssp585_mpi_2041 - pcur

changes <- list(cur_ssp370, cur_ssp585)
changes <- rast(changes)
names(changes) <- c('current_2041-70, SSP370', 'current_2041-70, SSP585')

par(mfrow=c(1,2))
plot(cur_ssp370, main= "change in p(occ) between 1981-2010 and 2041-2070 (SSP370, MPI-ESM)")
plot(cur_ssp585, main= "change in p(occ) between 1981-2010 and 2041-2070 (SSP585, MPI-ESM)")


# Set color palette
library(rasterVis)
library(RColorBrewer)
library(gridExtra)
myTheme=rasterTheme(region=brewer.pal('RdYlGn', n=11))

breakpoints <- c(-1, seq(-1, 1, 0))
colors <- c("red", RColorBrewer::brewer.pal(9, "Blues"))
plot(changes)
contour(changes, add=T)
levelplot(changes)

# Plot
par(mfrow=c(1,2))
p1 <- levelplot(cur_ssp370, par.settings=myTheme, margin=F)
p2 <- levelplot(cur_ssp585, par.settings=myTheme, margin=F)
grid.arrange(p1, p2, nrow=2)
