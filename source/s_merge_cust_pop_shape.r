# Copyright (c) 2015. All rights reserved. See the file LICENSE for license terms.
#########################################################################################
# File: s_merge_cust_pop_shape.r
# Proj: Mapping customers
# Desc: Merge customer, population and shapefile data
# Auth: Salome Lang
# Date: 2015/11/16

# Input:
# - Input1: Customer data (provided by Chair for marketing and market research)
#   Description: Customer transaction data of Swiss retailer
# - Input2: Population size by ZIP code
#   Link to download:
#   www.bfs.admin.ch/bfs/portal/de/index/themen/01/02/blank/key/raeumliche_verteilung/agglomerationen.Document.154450.xls
#   Source: Bundesamt für Statistik BFS
#   Description: 
#   Ständige Wohnbevölkerung nach Postleitzahl, Staatsangehörigkeitskategorie, Geschlecht, Fünfjahresaltersklasse und Zivilstand
#   Filename: su-d-01.02.01.02.51
# - Input3: Shapefile (geodata) of ZIP code areas
#   Link to download:
#   http://www.cadastre.ch/internet/kataster/de/home/services/service/plz.html
#   Source: Swiss cadastral system
#   Download file "Shape LV03"

# ==============================================================================
# Load customer data, prepare variables of interest
# ==============================================================================

# Read in the data
# encoding = "UTF-8" for German Umlaute to be read in correctly
customers <- fread("input/customers_online.csv", encoding = "UTF-8")
summary(customers)

# Convert ZIP code to numeric variable
customers[,PLZ.new:=as.numeric(PLZ)]
# warning that NA's introduced
customers[is.na(PLZ.new),] 
# the introduced NA's are due to missing data
# there are 1518 observations (i.e. unique customers) with missing ZIP code and city

# Drop observations with missing ZIP code since these are only 1518 out of 156k observations
customers <- customers[!is.na(PLZ.new),]

# Keeping only the numeric ZIP code variable
customers$PLZ <-NULL 
setnames(customers, "PLZ.new", "PLZ")

# In the customer data each customer corresponds to one observation
# For the maps, I need one observation per ZIP code instead.
# Therefore, I aggregate the purchase data per ZIP code.

# And I add two variables of interest:
# cust.per.zip for number of customers per ZIP code
# purch.per.zip for the sum of the purchase values per ZIP code 
aggreg.cust <- customers[, .(cust.per.zip = uniqueN(CustID),
                             purch.per.zip = sum(AggPurchase)), by=PLZ]

# Add variable for average purchase value per customer
aggreg.cust[,av.purch:= round(purch.per.zip/cust.per.zip,2)]

# Checking for missing data
aggreg.cust[is.na(cust.per.zip)]
aggreg.cust[is.na(purch.per.zip)]
aggreg.cust[is.na(av.purch)]
# no missing data

# ==============================================================================
# Load population statistics on ZIP code level and merge to customer data
# ==============================================================================

# Load population statistics on zip code level
# The population statistics from December 2014 were the most recent ones available from BFS
pop.stat <- read.xlsx2("input/su-d-01.02.01.02.51.xls", sheetName = "2014", 
                       startRow=4, header =T)
# the last few rows are comments, so I drop them
pop.stat <- data.table(pop.stat[1:3190,])
setnames(pop.stat, "X.", "PLZ"); pop.stat
# Variables 7 to 25 correspond to age groups, e.g. X0.4 is age group of 0 to 4 years old
# Variables 26 to 30 correspond to marital status

# Convert number of people per zip code (Total) to numeric variable
str(pop.stat$Total) # Factor
pop.stat[,Total:=as.numeric(as.character(Total))] # no NA's introduced

# Prepare PLZ for merge, i.e. transform to numeric variable
unique(pop.stat$PLZ) # first observation "Schweiz" is the only non-numeric entry
# Drop it because we can aggregate the numbers differently
# But first check whether the population sum corresponds to the population value for "Schweiz"
sum(pop.stat$Total)/2 - pop.stat$Total[1] # is 0, i.e. population numbers correspond
# Drop observation "Schweiz"
pop.stat <- pop.stat[!Total==sum(Total)/2,]
# Convert ZIP code to numeric variable
pop.stat[,PLZ:=as.numeric(as.character(PLZ))] # no NA's introduced

# ==============================================================================
# Merge customer data to population statistics
# ==============================================================================

# Pre-merge descriptives
# Compare the number of ZIP codes
uniqueN(pop.stat$PLZ) # 3190 ZIP codes
uniqueN(aggreg.cust$PLZ) # 3372 ZIP codes 
uniqueN(aggreg.cust$PLZ)-uniqueN(pop.stat$PLZ) 
# Customer data covers at least 182 ZIP codes that are not found in the population statistics

# Compare inhabitants to customers
sum.cust <- sum(aggreg.cust$cust.per.zip) # 153'121 unique customers total
sum.pop <- sum(pop.stat$Total) # 8'237'666 registered inhabitants
sum.cust/sum.pop # our company serves less than 2% of the total population
# this is not a good point of departure for the maps

# Merge the customer data to the population statistics by ZIP code
cust.w.stat <- merge(aggreg.cust,pop.stat, by = "PLZ", all=TRUE, allow.cartesian=TRUE) 
uniqueN(cust.w.stat$PLZ) # 3538

# Check the ZIP codes in the customer data which don't match the ZIP codes in the population statistics 
cust.without.stat <- cust.w.stat[is.na(Total),]; cust.without.stat # 350 ZIP codes
cust.without.stat[, sum(cust.per.zip,na.rm = T)] # 650 customers total
# I don't know when the customer data was recorded.
# Therefore, some of the discrepancy may be due to ZIP codes that don't exist anymore.
# Also, I cannot rule out that some of the matched ZIP codes don't actually correspond 
# to the same geographical area.

# Check the ZIP codes without customers
stat.without.cust <- cust.w.stat[is.na(cust.per.zip),] # only 165 ZIP codes

# Keep only first 5 variables
# I would need more info about the customers to be able to exploit the statistics on
# gender, age, etc. covered in the population statistics
# I only keep the first three age groups for the later calculation of the market potential
names(cust.w.stat)
cust.stat <- cust.w.stat[,1:5,with=FALSE]

# Add a dummy variable for customers per zip code
# The condition to test is is.na() because cust.per.zip is NA where there are no customers
cust.stat[, cust.dummy:= ifelse(is.na(cust.per.zip),0,1)]
table(cust.stat$cust.dummy) # 165 ZIP codes without customers as stated above

# Add variable for relative customer density (weighted by population density)
cust.stat[, rel.cust:= round(cust.per.zip/Total,4)]
summary(cust.stat$rel.cust) # 513 NAs
summary(cust.stat$Total) # 348 NAs + 165 NAs from cust.per.zip corresponds to the 513 NAs 

# Add variable for market potential
# I define market potential as the average purchase value per customer times
# the population density of the ZIP code minus the purchase value realized in that ZIP code
av.purch.val <- mean(cust.stat$av.purch, na.rm = T)
cust.stat[, market.potential:= av.purch.val*Total-purch.per.zip]

# I don't kick out the cust.without.stat for now because I want to plot both
# the absolute and the relative number of customers in a ZIP code area later on

# ==============================================================================
# Load shapefile of ZIP code areas and merge to customer and population data
# ==============================================================================

# readOGR() loads shapefiles (.shp files) and converts them to spatial data frames
# 1st argument: directory of the shapefile
# 2nd argument: name of the shapefile without .shp extension
ch <- readOGR("input/PLZO_SHP_LV03", layer = "PLZO_PLZ")

# ==============================================================================
#!!!!!!!!!!! EXTREMELY IMPORTANT 1 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Different sources use different Coordinate Reference Systems (CRS)
# CRS indicate different standardizations of location descriptions.
# The CRS affects the way regional boundaries are drawn on a map.
# More importantly, if you want to combine different maps in R, you need to make
# sure to transform their CRS to one common CRS.
# Otherwise, R will only plot one map instead of combining several ones.
# See also document: OverviewCoordinateReferenceSystems.pdf
# ==============================================================================

# Check CRS of the Swiss ZIP code map
proj4string(ch) # this is not EPSG:4326
# Transform CRS to EPSG:4326
ch <- spTransform(ch, CRS("+init=epsg:4326"))
# I transform it to EPSG:4326 because this is the CRS that the package {rleafmap}
# uses to draw its basemaps. We will use {rleafmap} later on to plot interactive, 
# zoomable maps.

# ==============================================================================
#!!!!!!!!!!! EXTREMELY IMPORTANT 2 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# The SpatialPolygonsDataFrame loaded above has the same number of observations 
# in its data frame as it has polygons, verify:
nrow(ch@data) # 4167
length(ch@polygons) # 4167

# Now, we want to merge the data we have prepared above to the data frame of the
# SpatialPolygonsDataFrame. It is EXTREMELY IMPORTANT to specify in the merge 
# that all observations in the data frame of the SpatialPolygonsDataFrame
# are kept. Otherwise, the number of observations in your data frame will differ
# from the number of polygons, which will lead to all sorts of problems, e.g.
# - it is not possible to save your new SpatialPolygonsDataFrame
# - depending on the package, the polygons are not plotted at all, etc.,etc.
# ==============================================================================

# Store row numbers to re-establish original sorting after merge
ch$idrow  <- 1:nrow(ch@data)

# Merge customer and population data to SpatialPolygonsDataFrame
# Don't forget all.x=T!!!!!!
ch@data <- merge(ch@data, cust.stat, by= "PLZ", all.x=T)

# Re-establish original sorting of the shapefile
ch@data <- ch@data[order(ch@data$idrow), ]
# Delete variable of row numbers
ch$idrow <- NULL

# Getting some descriptives after merge to shapefile
summary(ch@data)
sum(is.na(ch@data$Total))
# There are only 20 ZIP codes found in the shapefile, 
# but not covered in the population statistics
# This means that most of the ZIP codes found in the customer data but missing in the
# population statistics, are missing in the shapefile as well.
# These ZIP codes must be wrong entries or old ZIP codes that don't exist anymore.

# Comment on PLZ with non-zero ZUSZIFF
# In the shapefile, some ZIP codes are registered several times with a non-zero ZUSZIFF,
# i.e. with an additional identifier. This doesn't matter for the maps, because
# merge automatically performs a 1-to-many merge of the cust.stat data to the shapefile.
# Therefore, one ZIP code from the cust.stat data may be plotted as three areas but all
# areas will have the same color.

# ==============================================================================
# Save the spatial data with attributes as SpatialPolygonsDataFrame
# ==============================================================================

# Instead of csv_files, my output for this project must be stored as shp_files
# in order not to lose the spatial information.
# I use writeOGR from the {rgdal} package to save the shapefile.
# In fact, the function saves 4 files to the stated directory:
# the shapefile (.shp) and its associated supporting files (.dbf,.shx and .prj)
# The function DOESN'T automatically replace files with the same name and there doesn't
# seem to be an option like "replace=T" or similar (delete files by hand to replace).
# writePolyShape from {maptools} is another function for storing spatial data, but it
# doesn't seem to store the CRS with the data (see important 1 above).

# Rename the variables because the .prj file 
# can only save variable names with up to 10 digits.
# Rename variables
colnames(ch@data)[7:13] <- c("cust", "purch", "av.purch", "pop", "d.cust", "rel.cust",
                             "potential")
# Check ogrDrivers to find out which drivers can be written (see column "write")
# with the writeOGR() command (define driver to store CRS, see important 1 above)
ogrDrivers()
writeOGR(ch, "output/shp_files","ch", driver = "ESRI Shapefile")

# Clear work space
rm(list=ls())

# THE END