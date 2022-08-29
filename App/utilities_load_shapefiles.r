# Load europe shape file
eu_shape <- st_read(dsn = "Data/shape_EU_simplified", 
    layer = "shape_EU_simplified")

# Load the lighter version of the ecoregions shapefile
shape_eco <- st_read(dsn = "Data/shape_eco_simplified", 
    layer = "shape_eco_simplified")


# ices_areas <- st_transform(shape_ices_areas, crs = 4326)

# Change one Ecoregion name (this comes handy when we filter the stock list table)
levels(shape_eco$Ecoregion)[match("Icelandic Waters",levels(shape_eco$Ecoregion))] <- "Iceland Sea"

# Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)
shape_eco$uid <- paste0("P", 1:17)

######## this is the library I used to reduce the shape file size for quicker app loading (we can test a keep = 0.01 and see how it looks)
# library(rmapshaper)
# # object.size(shape_eco)
# # df1 <- ms_simplify(shape_eco, keep = 0.05, keep_shapes = TRUE)
# # object.size(df1)
# # df2 <- ms_simplify(eu_shape, keep = 0.05, keep_shapes = TRUE)
# eu_shape <-df2
# shape_eco <- df1
# st_write(df1, "shape_eco_simplified.shp")
# st_write(df2, "shape_EU_simplified.shp")


# object.size(df2)
