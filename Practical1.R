
library (raster)
library (rgdal)
library (ggplot2)
library (RStoolbox)
library (sp)
library (plyr)
library (dplyr) 


setwd('yourwd') 


#read data
Egypt_MODIS <- brick('Egypt_MODIS_Optical_Band_Subset')
Egypt_subset <- brick('ETM_egypt_subset.img')


#read meta data
meta_subset <- readLines('ETM_egypt_subset.hdr')
meta_MODIS <- readLines('Egypt_MODIS_Optical_Band_Subset.hdr')


##############################################################

#### A) EDA

##############################################################


### 1. initial raster plots

ggR(Egypt_subset, layer = 3, geom_raster = TRUE) #red layer
ggR(Egypt_subset, layer = 4, geom_raster = TRUE) #NIR


### 2. plot DN hists alongside images 
# **** NB xlab for Band 7 prints as Band 6 ****

vals_subset <- Egypt_subset[, ]
vals_subset <- data.frame(vals_subset)

for (i in 1:ncol(vals_subset)) {

print(ggR(Egypt_subset, layer = i, geom_raster = TRUE, stretch = 'lin'))
print(ggplot(vals_subset, aes(x = vals_subset[, i])) + geom_histogram(aes(x = vals_subset[, i]), binwidth = 2) + xlab(paste('Band', i)) + theme_classic())

}


### 3. extract value counts for all variables & plot scatter (equivalent to excel check of initial hist)

#val_counts <- apply(vals_subset, 2, table)
#val_counts <- lapply(val_counts, data.frame)

#for (i in 1:length(val_counts)) {
#  print(ggplot(val_counts[[i]]) + geom_point(aes(x = val_counts[[i]]$Var1, y = val_counts[[i]]$Freq)) + theme_classic())
#}

# or for direct checking in excel
# write.csv(vals_subset, 'vals.csv')


### 4. experiment with adjusting linear stretch

quants_low <- c(0, 0.02, 0.06, 0.1, 0.25) # create lower quantiles to stretch to
quants_high <- c(1, 0.98, 0.94, 0.9, 0.75) # upper quantiles

for (i in 1:length(quants_low)) {
  print(ggR(Egypt_subset, layer = 3, geom_raster = TRUE, stretch = 'lin', quantiles = c(quants_low[i], quants_high[i])))
}

# experiment with logarithmic stretch

for (i in 1:length(quants_low)) {
  print(ggR(Egypt_subset, layer = 3, geom_raster = TRUE, stretch = 'log', quantiles = c(quants_low[i], quants_high[i])))
}


#### 5. Multi-spectral display


ggRGB(Egypt_subset, r = 3, g = 2, b = 1, stretch = 'lin') # True colour RGB plot
ggRGB(Egypt_subset, r = 4, g = 3, b = 2, stretch = 'lin') # 4, 3, 2 plot (Vegetation red, grassland pinkish, urban areas bluish)



##############################################################

#### B) PG Extension section (Landsat section below under C))

##############################################################


### 1. crop to bottom right quarter

extent(Egypt_subset) #print to calc extent coords
bottom_right_corner <- c(324390, 346890, 3315360, 3331110)
Egypt_crop <- crop(Egypt_subset, bottom_right_corner)


### 2. plot NIR against Red

R_NIR <- data.frame(Egypt_crop[, ])[, 3:4]
ggplot(R_NIR) + geom_point(aes(x = R_NIR$band3..0.660000.Micrometers., y = R_NIR$band4..0.830000.Micrometers.)) +
xlab('Band 3') + ylab('Band 4') + theme_classic()

#the slightly peaked distribution at low band 3 values is indicative of some vegetation pixels being included.


### 3. Create boolean raster to show distribution of low Red, High NIR pixels

Red <- Egypt_crop[[3]]
NIR <- Egypt_crop[[4]]

Red_low <- calc(Red, function(x) {x < 75 }) # thresholds chosen from scatter plot
NIR_notlow <- calc(NIR, function(x) {x > 50}) 

Veg_rast <- Red_low  * NIR_notlow
plot(Veg_rast) #quick diagnostic plot shows veg in North West, surrounding river


### 4. Boolean raster showing only most dense veg (near peak of curve)

Red_dense <- calc(Red, function(x) {x < 75 }) # thresholds chosen from scatter plot
NIR_dense <- calc(NIR, function(x) {x > 75}) 

Dense_rast <- Red_dense * NIR_dense
plot(Dense_rast)




##############################################################

#### C) Analysis of Landsat Imagery

##############################################################


setwd('yourwd') 


### 1. read in landsat data (as a list first as some bands have different resolution)

Landfiles <- list.files(pattern = '.TIF')
LSat <- list()

for (i in 1:length(Landfiles)) {
  LSat[[i]] <- raster(Landfiles[i])
  
}


LSAT1 <- brick(LSat[-c(6,7,9)]) #keep only bands 1-5 & 7



### 2. Crop image to some arbitrary extent
new_extent <- c(350000, 400000, -1650000, -1600000)
LSAT1 <- crop(LSAT1, new_extent)



### 3. Mosaicking imagery - use raster::merge function

# S4 method for Raster,Raster
# merge(x, y, ..., tolerance=0.05, filename="", overlap=TRUE, ext=NULL)



##############################################################

#### D) Analysis of MODIS Imagery

##############################################################

ggRGB(Egypt_MODIS, r = 2, g = 1, b = 4) #4, 3, 2 false colour for MODIS
ggR(Egypt_MODIS) #greyscale for comparison

### histograms for false colour bands

MODIS_False_colour_vals <- Egypt_MODIS[,]
MODIS_False_colour_vals <- data.frame(MODIS_False_colour_vals[, c(1, 2, 4)])
names(MODIS_False_colour_vals) <- c('NIR', 'Red', 'Green')

for (i in 1:ncol(MODIS_False_colour_vals)) {
  
  print(ggplot(MODIS_False_colour_vals, aes(x = MODIS_False_colour_vals[, i])) + geom_histogram(aes(x = MODIS_False_colour_vals[, i]), binwidth = 0.01) + xlab(paste(names(MODIS_False_colour_vals[i]))) + theme_classic())
  
}


### load additional MODIS data




### Compare spatial resolutions of different MODIS products








