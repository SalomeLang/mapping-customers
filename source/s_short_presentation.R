# Copyright (c) 2015. All rights reserved. See the file LICENSE for license terms.
#########################################################################################
# File: s_short_presentation.r
# Proj: Mapping customers
# Desc: Provide illustrative maps for short presentation
# Auth: Salome Lang
# Date: 2015/12/03

# ==============================================================================
# Illustrations for short presentation, 12/03/2015
# ==============================================================================



# Add the city names from the customer data to ch shapefile ---------------

# Load ch shapefile
ch <- readOGR("output/shp_files", layer = "ch")

# Load customer data and transform PLZ to numeric
customers <- fread("input/customers_online.csv", encoding = "UTF-8")
customers <- customers[, 4:5, with=F]
customers[,PLZ.new:=as.integer(PLZ)]
customers[is.na(PLZ.new),] 
customers <- customers[!is.na(PLZ.new),]
customers$PLZ <-NULL 
setnames(customers, "PLZ.new", "PLZ")

# Keep first-mentioned city per ZIP code only
uni.customers <- customers[,.(City=City[1]),by=PLZ]

# Store row numbers to re-establish original sorting after merge
ch$idrow  <- 1:nrow(ch@data)
# Merge City names to the shapefile
ch@data <- merge(ch@data,uni.customers, by= "PLZ", all.x=T)
# Re-establish original sorting of the shapefile
ch@data <- ch@data[order(ch@data$idrow), ]
# Delete variable of row numbers
ch$idrow <- NULL

# Transform variable market potential to market potential in 1000 CHF 
# This is shorter when displayed in popup
ch$potential <- round(ch$potential/1000, 2)

# Save the new shapefile
writeOGR(ch, "output/shp_files","ch.new", driver = "ESRI Shapefile")

# Clear work space
rm(list=ls())


# Illustrative Map 1: Relative customer numbers ---------------------------

# Load new shapefile with City info
ch <- readOGR("output/shp_files", layer = "ch.new")

# Define color scheme 
gcol <- rev(heat.colors(5))
# Split the number of customers per ZIP code into 5 groups
summary(ch$rel_cust) # I set the breaks according to the quantiles
gcut <- cut(ch$rel_cust, breaks = c(0, 0.0088,0.0169, 0.0176, 0.0243, 0.3))
# Assign color to each ZIP code according to group
region.col <- gcol[as.numeric(gcut)]
# Set up the popup
# My popup consists of text only
pop <- paste(ch$PLZ,ch$City,"Relative no. customers:", round((ch$rel_cust*100),2), 
               "%",sep = " ")
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

# Plot map with width and height adjusted to full laptop screen size for presentation
writeMap(mapquest.bm, cs.lay, width = 1350, height = 630, interface = my.ui, 
         setView = c(46.8, 8.2), setZoom = 8)


# Map 2: Market Potential -------------------------------------------------

# Get summary statistics to define the gcuts
summary(ch$potential)
# Cut the values in 5 groups according to quantiles
gcut <- cut(ch$potential, breaks = c(0, 5200,10900, 25480, 27030, 360000))
region.col <- gcol[as.numeric(gcut)]
pop <- paste(ch$City,"Market Potential (in 1000 CHF):", round(ch$potential),"CHF",sep = " ")
cs.lay <- spLayer(ch, fill.col = region.col,popup=pop)
my.ui <- ui(attrib.text = "Customer Data: Proprietary Data of Swiss Retailer")
writeMap(mapquest.bm, cs.lay, width = 1350, height = 630, interface = my.ui, 
         setView = c(46.8, 8.2), setZoom = 8)


# END