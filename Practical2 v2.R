
library (plyr)
library (dplyr) 
library (raster)
library (rgdal)
library (sp)
library (RStoolbox)
library (tidyr)
library (ggplot2)
library (dismo)

setwd('C:/Users/Oli/Documents/Modules/RS/Practicals/Practical 2/Practical 2') 


### Read in data


### CSV (A)
Chobe <- read.csv('Chobe_simple.csv') #copy and paste vals and column headers into a csv format


### AVIRIS (section B) & LANDSAT (B & C)
temp <- list.files(pattern = '.img')

for (i in 1:length(temp)) {
  assign(temp[i], brick(temp[i]))
}



##############################################################

#### A) Calculate NDVI for Chobe field measurements

##############################################################


Red_vals <- Chobe[Chobe$Wavelength..nm. == 650, ]
NIR_vals <- Chobe[Chobe$Wavelength..nm. == 750, ]
NDVI_vals <- rbind(Red_vals, NIR_vals)

NDVI_vals[3, ] <- apply(NDVI_vals, 2, function(x) {(x[2] - x[1]) / (x[2] + x[1])})
NDVI_vals[3, 1] <- NA
row.names(NDVI_vals) <- c('Red', 'NIR', 'NDVI')


##############################################################

#### B) AVIRIS IMAGE ANALYSIS

##############################################################

# divide the integer number by 500 (channels 1 to 160) and 1000 (channels 161 to 224)

  ### 1) explore NIR bands

ggR(AVIRISradiance_image.img, 53, stretch = 'lin') #band 53 in NIR; 856nm

### take samples at three contrasting locations

AVIRIS_53_1 <- AVIRISradiance_image.img[53][200]
AVIRIS_53_2 <- AVIRISradiance_image.img[53][100]
AVIRIS_53_3 <- AVIRISradiance_image.img[53][50]


### compare 53 to 60 

ggR(AVIRISradiance_image.img, 60, stretch = 'lin') #band 60 in NIR; 923nm

### compare to samples of 60

AVIRISradiance_image.img[60][200] - AVIRIS_53_1
AVIRISradiance_image.img[60][100] - AVIRIS_53_2
AVIRISradiance_image.img[60][50] - AVIRIS_53_3


  ### 2) explore bands in atmospheric absorption bands

ggR(AVIRISradiance_image.img, 110, stretch = 'lin') # highly distorted
ggR(AVIRISradiance_image.img, 115, stretch = 'lin') # very dark (low values)


  ### 3) Create spectral plots for different points


# select specific AVIRIS location with - AVIRISradiance_image.img[[i]][yval, xval]

### select points & extract values

points <- c(c(10, 320), c(200, 510), c(100, 180)) #arbitrary points - can fidle to see different points

pointvals <- raster::extract(AVIRISradiance_image.img, points)
pointvals <- data.frame(pointvals)
pointvals <- apply(pointvals, 2, function(x) {x / 10000}) #divide by max value to create reflectance %

### manipulate band names for x-axis

bands <- colnames(pointvals)
bands <- sapply(bands, substr, start = 2, stop = 8)
bands <- as.numeric(bands)

### manipulate data structure for plotting

row.names(pointvals) <- c('p1', 'p2', 'p3')
pointvals <- data.frame(t(pointvals))
points_tidy <- gather(pointvals)
points_tidy$bands <- rep(bands, times = 3)

### plot

ggplot(points_tidy, aes(x = bands, y = value)) + geom_line(aes(colour = key)) + theme_classic()


  ### 4) Compare radiance and reflectance data

# ggR(AVIRISreflectance_image.img, 50) ## plot band 50

### select points & extract values

pointvals2 <- raster::extract(AVIRISreflectance_image.img, points)
pointvals2 <- data.frame(pointvals2)
pointvals2 <- apply(pointvals2, 2, function(x) {x / 10000}) #divide by max value to create reflectance %

### manipulate band names for x-axis

bands2 <- colnames(pointvals2)
bands2 <- sapply(bands2, substr, start = 2, stop = 8)
bands2 <- as.numeric(bands2)

### manipulate data structure for plotting

row.names(pointvals2) <- c('p1', 'p2', 'p3')
pointvals2 <- data.frame(t(pointvals2))
points_tidy2 <- gather(pointvals2)
points_tidy2$bands <- rep(bands2, times = 3)

### plot

ggplot(points_tidy2, aes(x = bands, y = value)) + geom_line(aes(colour = key)) + theme_classic()


### reflectance is much more like a laboratory spectral pattern



  ### 5) Compare AVIRIS against LANDSAT ETM+

# ggR(simulated_tm_image.img, 4) ## simualated landsat visual

pointvals3 <- raster::extract(AVIRISreflectance_image.img, points)
pointvals3 <- data.frame(pointvals3)
pointvals3 <- apply(pointvals3, 2, function(x) {x / 10000}) #divide by max value to create reflectance %


### manipulate data structure for plotting


pointvals3 <- data.frame(t(pointvals3))
points_tidy3 <- gather(pointvals3)
points_tidy3$xval <- rep(1:224, times = 6)

### plot

ggplot(points_tidy3, aes(x = xval, y = value)) + geom_line(aes(colour = key)) + theme_classic()



##############################################################

#### C) CALIBRATION OF LANDSAT IMAGE AND BAND CALC

##############################################################

  ### 1) initial plots
ggRGB(Landsat_1987masked.img, r = 4, g = 3, b = 2, stretch = 'lin')
ggRGB(Landsat_2003masked.img, r = 4, g = 3, b = 2, stretch = 'lin')


  ### 2) Calibrate image data from DN to W/m2/spectral unit

LS_vec <- list(Landsat_1987masked.img, Landsat_2003masked.img)
  
for (i in 1:length(LS_vec)) {
  
  LS_vec[[i]][[1]] <- calc(LS_vec[[i]][[1]], function(x) {0.7787*x - 6.2})
  LS_vec[[i]][[2]] <- calc(LS_vec[[i]][[2]], function(x) {((300.9 --6.40) / (255-1))*(x-1) +- 6.40})
  LS_vec[[i]][[3]] <- calc(LS_vec[[i]][[3]], function(x) {((234.4 --5.0) / (255-1))*(x-1) +- 5.0})
  LS_vec[[i]][[4]] <- calc(LS_vec[[i]][[4]], function(x) {0.9455*x - 5.1})
  LS_vec[[i]][[5]] <- calc(LS_vec[[i]][[5]], function(x) {((47.57 --1) / (255-1))*(x-1) +- 1})
  LS_vec[[i]][[6]] <- calc(LS_vec[[i]][[6]], function(x) {((16.54 --0.35) / (255-1))*(x-1) +- 0.35})
  
  ## Lλ =  [ (LMAXλ - LMINλ)/(QCALMAX-QCALMIN) ]   *   (QCAL-QCALMIN) + LMINλ  
  
}

### *** NEEDS TO BE CALIBRATED FOR REFLECTANCE ***



  ### 3) Atmospheric Effect

min(Landsat_1987masked.img[1]) #low-moderate low value indicative of haze caused by Raleigh scattering
min(Landsat_2003masked.img[1]) # elevated minima

### extract min values for all channels
LS_bandmins <- list(1, 2)

for (i in 1:length(LS_vec)) {
  for (j in 1:nbands(LS_vec[[i]])) {
    LS_bandmins[[i]][j] <- quantile(LS_vec[[i]][j], 0.01)
  }
}

### plot as a function of band wavelength
bandmin_frame <- data.frame(LS_bandmins)
colnames(bandmin_frame) <- c(1987, 2003)
bandmin_frame <- gather(bandmin_frame)

LS_bandwiths <- c(0.48, 0.56, 0.66, 0.83, 1.65, 2.15)
bandmin_frame$xval <- LS_bandwiths

ggplot(bandmin_frame, aes(x = xval, y = value)) + geom_line(aes(colour = key)) + theme_classic()


### subtract bandmins 'Dark object subtract method'





