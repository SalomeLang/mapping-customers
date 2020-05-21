# Copyright (c) 2015. All rights reserved. See the file LICENSE for license terms.
#########################################################################################
# File: s_mapping_customers_additional_task.r
# Proj: Mapping customers
# Desc: Measure for aggregated ZIP code areas, integrated in interactive tool 
# Auth: Salome Lang
# Date: 2016/01/14

# Swiss ZIP codes divide the country into 9 regions
# Source: https://de.wikipedia.org/wiki/Postleitzahl_%28Schweiz%29
# 1xxx Region westschweiz (Süd)
# 2xxx Region westschweiz (Nord)
# 3xxx Region Bern/Oberwallis
# 4xxx Region Basel
# 5xxx Region Aarau
# 6xxx Region Zentralschweiz, Tessin
# 7xxx Region Graubünden
# 8xxx Region Zürich, Thurgau
# 9xxx Region Ostschweiz

# Load shapefile
ch <- readOGR("output/shp_files", layer = "ch.new", encoding = "UTF-8")
summary(ch@data)

# Generate region variable in shapefile
ch$PLZ_region <- as.numeric(substr(ch$PLZ, 1,1))

# Generate data frame with region names
PLZ_region <- as.numeric(seq(1,9))
region_name <- rbind("Region Westschweiz (Süd)", "Region Westschweiz (Nord)",
                 "Region Bern/Oberwallis","Region Basel", "Region Aarau", 
                 "Region Zentralschweiz, Tessin", "Region Graubünden",
                 "Region Zürich, Thurgau", "Region Ostschweiz")
regions <- data.frame(PLZ_region,region_name)

# Generate relative customer density by region and merge to regions
rel_cust_data <- data.table(rel_cust=ch$rel_cust, PLZ_region=ch$PLZ_region)
rel_cust_data$rel_c_reg <- with(rel_cust_data$rel_cust)
rel_cust_data[,rel_c_reg := round(mean(rel_cust, na.rm = T),4), by=PLZ_region]

regions <- merge(regions, rel_c_reg)

# Merge region names to shapefile by variable PLZ_region and by keeping shapefile sorting
# For explanations and advices see s_merge_cust_pop_shape.new.r
ch$idrow  <- 1:nrow(ch@data)
ch@data <- merge(ch@data, regions, by= "PLZ_region", all.x=T)
ch@data <- ch@data[order(ch@data$idrow), ]
ch$idrow <- NULL
head(ch@data) # merge worked





