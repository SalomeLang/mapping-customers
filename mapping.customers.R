# Copyright (c) 2015. All rights reserved. See the file LICENSE for license terms.
#########################################################################################
# File: SCRM.map.R
# Proj: Mapping customers
# Desc: Visualize data on maps
# Auth: Salome Lang
# Date: 2015/11/16

# Set up work space ######################################################################
# Clear work space
rm(list=ls())
# Set working directory
setwd("C:/Users/SalomeLang/Documents/MBA/SCRM.II=)/SCRM.group.project")

# Load packages
library(maptools) # needed??
library(xlsx) # to read .xlsx files
library(data.table) # for elegant data manipulation
library(rgdal) # to read/load and save shapefiles in R
library(GISTools)
library(ggplot2)
library(rleafmap) # for interactive maps

# Source relevant code ###################################################################

# 0 Illustration for Short Presentation
source("source/s_short_presentation.R")

# I Merge customer, population and shapefile data
source("source/s_merge_cust_pop_shape.r")
# the warning that NA's were introduced refers to the transformation of variable PLZ in the 
# customer data to numeric. The observations with PLZ=="NULL" become NA.

# II Draw the maps
source("source/s_draw_maps")

# THE END