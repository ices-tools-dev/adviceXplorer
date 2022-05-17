# Load europe shape file
eu_shape <- st_read(dsn = "Data/old_eu", 
    layer = "eu_shape")

# Load the lighter version of the ecoregions shapefile
shape_eco <- st_read(dsn = "Data/test_lowres", 
    layer = "ecoR_lowres")

# shape_ices_areas <- st_read(dsn = "Data/ICES_areas_low_res", 
#     layer = "ICES_areas_low_res")
# ices_areas <- st_transform(shape_ices_areas, crs = 4326)

# Change one Ecoregion name (this comes handy when we filter the stock list table)
levels(shape_eco$Ecoregion)[match("Icelandic Waters",levels(shape_eco$Ecoregion))] <- "Iceland Sea"

# Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)
shape_eco$uid <- paste0("P", 1:17)
# ices_areas$uid <- paste0("A", 1:66)


minZoom = 0
maxZoom = 13
resolutions <- 2*(2^(maxZoom:minZoom))
crs_laea <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:3035",
  proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0= -1.235660 +lat_0=60.346958",
  resolutions = resolutions)

# eu_shape <- st_set_precision(eu_shape, precision=10^2)
# st_write(eu_shape, "App/data/old_eu/eu_shape.shp")
# shape_eco <- st_set_precision(shape_eco, precision=10^2)

