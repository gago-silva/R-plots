#Libraries
library(raster,sp,maptools,png,rgeos,maps,GISTools)

#Set your working directory
DIR <- "..."
setwd(DIR)

#Read Natural Earth 2.0 raster
natearth1 <- raster("NE2_LR_LC_SR.tif",band=1)
natearth2 <- raster("NE2_LR_LC_SR.tif",band=2)
natearth3 <- raster("NE2_LR_LC_SR.tif",band=3)
raster <- stack(natearth1,natearth2,natearth3)

#Read the spatial data
lakes <- readShapePoly("ne_10m_lakes",proj4string=CRS(proj4string(raster)))
urban_areas<-readShapePoly("ne_10m_urban_areas",proj4string=CRS(proj4string(raster)))
bathym_0<-readShapePoly("ne_10m_bathymetry_L_0",proj4string=CRS(proj4string(raster)))
bathym_200<-readShapePoly("ne_10m_bathymetry_K_200",proj4string=CRS(proj4string(raster)))
bathym_1000<-readShapePoly("ne_10m_bathymetry_J_1000",proj4string=CRS(proj4string(raster)))
bathym_2000<-readShapePoly("ne_10m_bathymetry_I_2000",proj4string=CRS(proj4string(raster)))
bathym_3000<-readShapePoly("ne_10m_bathymetry_H_3000",proj4string=CRS(proj4string(raster)))
bathym_4000<-readShapePoly("ne_10m_bathymetry_G_4000",proj4string=CRS(proj4string(raster)))
bathym_5000<-readShapePoly("ne_10m_bathymetry_F_5000",proj4string=CRS(proj4string(raster)))
bathym_6000<-readShapePoly("ne_10m_bathymetry_E_6000",proj4string=CRS(proj4string(raster)))
bathym_7000<-readShapePoly("ne_10m_bathymetry_D_7000",proj4string=CRS(proj4string(raster)))
bathym_8000<-readShapePoly("ne_10m_bathymetry_C_8000",proj4string=CRS(proj4string(raster)))
bathym_9000<-readShapePoly("ne_10m_bathymetry_B_9000",proj4string=CRS(proj4string(raster)))
bathym_10000<-readShapePoly("ne_10m_bathymetry_A_10000",proj4string=CRS(proj4string(raster)))

#Define Area of Interest (AOI)
e <- extent(90, 180, -60, 5) #Extent center: Australia
AOI <- as(e, "SpatialPolygons")
proj4string(AOI) <- CRS(proj4string(raster))

#Crop the raster 
rc <- crop(raster, e)

#Transform the raster into a spatial grid data frame
raster<-as(rc, 'SpatialGridDataFrame')

#Clip the Polygones and Polylines to the AOI
lakes <- gIntersection(lakes, AOI, byid=TRUE)
urban_areas <- gIntersection(urban_areas, AOI, byid=TRUE)
bathym_0 <- gIntersection(bathym_0, AOI, byid=TRUE)
bathym_200 <- gIntersection(bathym_200, AOI, byid=TRUE)
bathym_1000 <- gIntersection(bathym_1000, AOI, byid=TRUE)
bathym_2000 <- gIntersection(bathym_2000, AOI, byid=TRUE)
bathym_3000 <- gIntersection(bathym_3000, AOI, byid=TRUE)
bathym_4000 <- gIntersection(bathym_4000, AOI, byid=TRUE)
bathym_5000 <- gIntersection(bathym_5000, AOI, byid=TRUE)
bathym_6000 <- gIntersection(bathym_6000, AOI, byid=TRUE)
bathym_7000 <- gIntersection(bathym_7000, AOI, byid=TRUE)
bathym_8000 <- gIntersection(bathym_8000, AOI, byid=TRUE)
bathym_9000 <- gIntersection(bathym_9000, AOI, byid=TRUE)
bathym_10000 <- gIntersection(bathym_10000, AOI, byid=TRUE)

#Create and Export the map in png format
png("Australia.png")
par(mar = c(1,0,0,0))
#raster
image(raster,red="NE2_LR_LC_SR.1",green="NE2_LR_LC_SR.2",blue="NE2_LR_LC_SR.3")
#bathymetry polygons from 0 to below 10.000m
if (length(names(bathym_0))!=0){plot(bathym_0, col=rgb(73/255,198/255,255/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_200))!=0){plot(bathym_200, col=rgb(73/255,198/255,255/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_1000))!=0){plot(bathym_1000, col=rgb(73/255,176/255,255/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_2000))!=0){plot(bathym_2000, col=rgb(72/255,170/255,250/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_3000))!=0){plot(bathym_3000, col=rgb(71/255,166/255,244/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_4000))!=0){plot(bathym_4000, col=rgb(68/255,159/255,234/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_5000))!=0){plot(bathym_5000, col=rgb(65/255,150/255,224/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_6000))!=0){plot(bathym_6000, col=rgb(61/255,141/255,211/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_7000))!=0){plot(bathym_7000, col=rgb(56/255,129/255,193/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_8000))!=0){plot(bathym_8000, col=rgb(51/255,117/255,175/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_9000))!=0){plot(bathym_9000, col=rgb(47/255,105/255,159/255,255/255),border=FALSE, add=TRUE)}
if (length(names(bathym_10000))!=0){plot(bathym_10000, col=rgb(40/255,88/255,134/255,255/255),border=FALSE, add=TRUE)}
#Main lakes polygons
if (length(names(lakes))!=0){plot(lakes, border=FALSE, col=rgb(73/255,176/255,255/255,255/255),add=TRUE)}
#Urban areas polygons
if (length(names(urban_areas))!=0){plot(urban_areas, border=rgb(233/255,67/255,67/255), col=rgb(233/255,67/255,67/255),add=TRUE)}
#Main Title and sub-title
title("The Map Title goes here","Sub-title goes here",cex.main = 2,font.main= 4, col.main= "black", cex.sub = 0.75, font.sub = 1, col.sub = "darkgrey", line=-2)
#Scale
map.scale(xc=105, yc=-55, 10,"km",4)
#North
north.arrow(xb=95, yb=-55, len=1, lab="N")
#Legend for the urban areas
legend(112, -52, legend="Urban areas",fill=rgb(233/255,67/255,67/255), border=rgb(233/255,67/255,67/255),cex=1, bty="n")
dev.off()
