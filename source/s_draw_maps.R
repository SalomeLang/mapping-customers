# Copyright (c) 2015. All rights reserved. See the file LICENSE for license terms.
#########################################################################################
# File: s_draw_maps.r
# Proj: Mapping customers
# Desc: Draw the maps
# Auth: Salome Lang
# Date: 2015/11/28

# ==============================================================================
# Load the ZIP code areas with customer and poulation attributes
# ==============================================================================

# Use readOGR() to load spatial data and convert it to a spatial data frame
# 1st argument: directory of the shapefile
# 2nd argument: name of the shapefile without .shp extension
ch <- readOGR("output/shp_files", layer = "ch")

# Get some descriptives
# Get on overview of the shapefile with ogrInfo 
# This command even works before you load the shapefile.
ogrInfo("output/shp_files", "ch")
proj4string(ch) # nice, CRS is still WGS84 which is equivalent to EPSG:4326
# for explanation on CRS, see important 1 in s_merge_cust_pop_shape.r
names(ch@data) 
# Storing the data in a .prj file replaced all "." with "_" in the variable names

# Get the plain ZIP code map
pdf(file = "output/image_files/map_empty.pdf", width = 8.4, height = 4.7)
plot(ch)
dev.off()

# Get descriptives of variables of interest
str(ch@data)
summary(ch@data)

# ==============================================================================
# First, plot histograms of the variables of interest
# ==============================================================================

# Define colors
blue <- rgb(0, 0, 155, maxColorValue = 255)
green <- rgb(0, 176, 80, maxColorValue = 255)
yellow <- rgb(255, 192, 0, maxColorValue = 255)
red <- rgb(255, 0, 0, maxColorValue = 255)
lightblue <- rgb(0, 177, 220, maxColorValue = 255)
black <- rgb(0, 0, 0, maxColorValue = 255)

# Number of customers
pdf(file = "output/image_files/hist_cust.pdf", width = 8.4, height = 4.7)
hist(ch$cust,main = '', col = blue, xlab = "No. customers per ZIP code", 
     ylab = '',  xlim = c(0, 200), breaks = 200)
m.cu <- mean(ch$cust, na.rm=T) # 49.01
abline(v=m.cu, col=black, lwd=2)
mtext("Frequency", side=2, line=2.5)
dev.off()

# Purchase value
pdf(file = "output/image_files/hist_value.pdf", width = 8.4, height = 4.7)
hist(ch$purch/1000,main = '', col = red, xlab = "", 
     ylab = '', xlim = c(0, 3000), breaks=200)
m.pu <- mean(ch$purch, na.rm=T)/1000 # 443.8
abline(v=m.pu, col=black, lwd=2)
mtext("Purchase value per ZIP code", side=1, line=2.5)
mtext("(in CHF 1000)", side=1, line=3.5, cex=0.8)
mtext("Frequency", side=2, line=2.5)
dev.off()

# Population density
pdf(file = "output/image_files/hist_pop.dens.pdf", width = 8.4, height = 4.7)
hist(ch$pop,main = '', col = green, xlab = "", 
     ylab = '', xlim = c(0,20000), breaks=200)
m.pop <- mean(ch$pop, na.rm = T) # 2663
abline(v=m.pop, col=black, lwd=2)
mtext("No. inhabitants per ZIP code", side=1, line=2.5)
mtext("Frequency", side=2, line=2.5)
dev.off()

# Number of customers weighted by population density
pdf(file = "output/image_files/hist_rel.cust.pdf", width = 8.4, height = 4.7)
hist(ch$rel_cust,main = '', col = blue, xlab = "", 
     ylab = '', xlim = c(0, 0.1), breaks = 100)
m.rel <- mean(ch$rel_cust, na.rm = T) # 0.0176
abline(v=m.rel, col=black, lwd=2)
mtext("No.customers per ZIP code", side=1, line=2.5)
mtext("(weighted by population density)", side=1, line=3.5, cex=0.8)
mtext("Frequency", side=2, line=2.5)
dev.off()

# ==============================================================================
# Interactive Maps with {rleafmap}
# ==============================================================================

# See also http://www.francoiskeck.fr/rleafmap/

# Plot the customer dummy
# First as a common map
# Using the base plot function
pdf(file = "output/image_files/map_customers.pdf", width = 8.4, height = 4.7)
plot(ch, col=ch$d_cust, border="lightgray") # in black and white
dev.off()
plot(ch, col=rainbow(ch$d_cust)) # this one is for the nationalists (in red and white)

# Using choropleth from {GISTools} doesn't work for dummy variable
# auto.shading needs at least 3 levels, and otherwise forces them on the data

# Map 1: Absolute customer numbers
# Define color scheme 
gcol <- rev(heat.colors(5))
# Split the number of customers per ZIP code into 5 groups
summary(ch$cust) # I set the breaks according to the quantiles
gcut <- cut(ch$cust, breaks = c(0, 7, 18, 51, 100, 872))
# Assign color to each ZIP code according to group
region.col <- gcol[as.numeric(gcut)]
# Set up the popup
# My popup consists of text only
pop <- paste("Number of customers:", ch$cust, sep = "<br>")
# spLayer defines a new data layer from the spatial data
cs.lay <- spLayer(ch, fill.col = region.col,popup=pop)
# Define the underlying base map
mapquest.bm <- basemap("mapquest.map")      
# Define the user interface for the lower right corner of the map 
my.ui <- ui(attrib.text = "Customer Data: Proprietary Data of Swiss Retailer")
# Call to writeMap creates the interactive map in the Viewer-window
# width and height define the plot size
# setView defines the initial focus on the map (here set to Schweizer Mittelland)
# setZoom defines the initial zoom
writeMap(mapquest.bm, cs.lay, width = 840, height = 550, interface = my.ui, 
         setView = c(46.8, 8.2), setZoom = 8)

##### Didn't run ###############
# I think {rleafmap} doesn't allow for several layers of choropleth maps
# Adding 2nd data layer 
# I split the data in 5 groups as well and use "gcol" from above
# Split the purchase value per ZIP code into 5 groups
summary(ch$purch)
gcut.pu <- cut(ch$purch, breaks = c(0, 60000, 175000, 470000,1000000,8000000))
# Assign color to each ZIP code according to group
region.col.pu <- gcol[as.numeric(gcut.pu)]
# Add layer control to the user interface
my.ui.pu <- ui(attrib.text = "Customer Data: Proprietary Data of Swiss Retailer",
            layers= "topright")
# Name spLayers according to their content because they appea in the layer control
NumberofCustomers <- cs.lay
# Set up popup for purchase value-layer
pop.pu <- paste("Value of purchase:", ch$purch, sep = "<br>")
# Define spatial data layer for purchase value
PurchaseValue <- spLayer(ch, fill.col = region.col.pu,popup=pop.pu)
# Just add the new layer to the call to writeMap
# It really is as simple as that!!
writeMap(mapquest.bm, NumberofCustomers,PurchaseValue, width = 840, height = 550, 
         interface = my.ui.pu, setView = c(46.8, 8.2), setZoom = 8)
##### Didn't run ###############

# Map 2: Relative customer numbers
# Define color scheme 
gcol <- rev(heat.colors(5))
# Split the number of customers per ZIP code into 5 groups
summary(ch$rel_cust) # I set the breaks according to the quantiles
gcut <- cut(ch$rel_cust, breaks = c(0, 0.0088,0.0169, 0.0176, 0.0243, 0.3))
# Assign color to each ZIP code according to group
region.col <- gcol[as.numeric(gcut)]
# Set up the popup
# My popup consists of text only
pop <- paste("Relative number of customers:", round((ch$rel_cust*100),4), "%", sep = " ")
# spLayer defines a new data layer from the spatial data
cs.lay <- spLayer(ch, fill.col = region.col,popup=pop)
# Define the underlying base map
mapquest.bm <- basemap("mapquest.map")      
# Define the user interface for the lower right corner of the map 
my.ui <- ui(attrib.text = "Customer Data: Proprietary Data of Swiss Retailer")
# Call to writeMap creates the interactive map in the Viewer-window
# width and height define the plot size
# setView defines the initial focus on the map (here set to Schweizer Mittelland)
# setZoom defines the initial zoom
writeMap(mapquest.bm, cs.lay, width = 840, height = 550, interface = my.ui, 
         setView = c(46.8, 8.2), setZoom = 8)

# THE END