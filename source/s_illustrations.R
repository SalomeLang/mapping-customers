# Drawing maps with R


# Attributes in Spatial Data Frames ---------------------------------------
# p.66
data(newhaven)
ch <- readOGR("input/PLZO_SHP_LV03", layer = "PLZO_PLZ")

# Access the attributes associated with the polygons
data.frame(ch) # or
ch@data # same same
head(data.frame(ch))
colnames(data.frame(ch))
# Access particular variable directly
ch$PLZ # or
ch@data$PLZ # same same

attach(data.frame(ch))
PLZ # same as above
summary(ZUSZIFF)
hist(PLZ) # not informative, just to show what it does

# Mapping Polygons and Attributes -----------------------------------------
# p.68
choropleth(ch,ch$ZUSZIFF) # plots the ZIP codes with an additional digit in darker red 
# shading scheme is computed automatically from the variable that is mapped
# with function auto.shading 

# To add a legend, assign a shading scheme to the variable first
zus.shades <- auto.shading(ch$ZUSZIFF, n=7)
zus.shades
# zus.shades is a list with 2 elements: $breaks and $cols
locator() # click on a spot on the map plot where you would like to locate the legend
# Press esc to get coordinates
choropleth(ch,ch$ZUSZIFF, shading = zus.shades)
choro.legend(-29339.03,427436, zus.shades)
length(zus.shades$breaks) # length of breaks needs to be strictly larger than 2

# Therefore, use another variable


library(OpenStreetMap)

