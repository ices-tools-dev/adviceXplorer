# Download europe shape file
ecoregion = "Celtic Seas Ecoregion"
    eu <- area_definition(ecoregion)
    eu_shape <- eu$europe_shape

# Load the lighter version of the ecoregions shapefile
shape_eco <- st_read(dsn = "Data/test_lowres", 
    layer = "ecoR_lowres")

shape_ices_areas <- st_read(dsn = "Data/ICES_areas_low_res", 
    layer = "ICES_areas_low_res")
ices_areas <- st_transform(shape_ices_areas, crs = 4326)

# Change one Ecoregion name (this comes handy when we filter the stock list table)
levels(shape_eco$Ecoregion)[match("Icelandic Waters",levels(shape_eco$Ecoregion))] <- "Iceland Sea"

# Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)
shape_eco$uid <- paste0("P", 1:17)
ices_areas$uid <- paste0("A", 1:66)


minZoom = 0
maxZoom = 13
resolutions <- 2*(2^(maxZoom:minZoom))
crs_laea <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:3035",
  proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0= -1.235660 +lat_0=60.346958",
  resolutions = resolutions)


# # ######################### Map panel   
#     # Define the palette
#     bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
#     pal <- colorBin("YlOrRd", domain = shape_eco$Shape_Area, bins = bins)
    
#     # Define the interactive labels
#     labels <- sprintf(
#         "<strong>%s Ecoregion</strong><br/>%g Shape Area ",
#         shape_eco$Ecoregion, shape_eco$Shape_Area
#     ) %>% lapply(htmltools::HTML)
    
#     # output$map <- renderLeaflet({

#         leaflet(options = leafletOptions(crs = crs_laea, minZoom = minZoom, maxZoom = maxZoom)) %>% 
#             #addProviderTiles("Stamen.Toner") %>% 
#             addPolygons(data = ices_areas, 
#                 color = "#444444", 
#                 weight = 1,
#                 smoothFactor = 0.5,
#                 opacity = 0.7, 
#                 fillOpacity = 0.5,
#                 # fillColor = ~ pal(shape_eco$Shape_Area),
#                 # layerId = ~uid, # unique id for polygons
#                 highlightOptions = highlightOptions(
#                     color = "white", weight = 2,
#                     bringToFront = TRUE
#                 ),
#                 # label = labels,
#                 labelOptions = labelOptions(
#                     style = list("font-weight" = "normal", padding = "3px 8px"),
#                     textsize = "15px",
#                     direction = "auto"
#                 )) %>%
#             addPolygons(
#                 data = eu_shape, color = "black", weight = 1,
#                 smoothFactor = 0.5,
#                 opacity = 0.7, fillOpacity = 0.5,
#                 fillColor = "grey") %>%  
#              setView(lng = -1.235660 , lat = 60.346958, zoom = 1.2)  #%>% # centered around shetland coordinates
# #             #  fitBounds(-33.278368,35.995785,67.831473,90) 
# #     # })
# # shape <- read_sf(dsn = "D:/Profile/Documents/GitHub/online-advice/ICES_areas_", layer = "ICES_Areas_20160601_cut_dense_3857")
# # ICES_areas_lowres <- rmapshaper::ms_simplify(shape)
# # st_write(dsn = "D:/Profile/Documents/GitHub/online-advice/ICES_areas_low_res", layer = "ICES_areas_lowres.shp")   
# # st_write(ICES_areas_lowres, paste0(tempdir(), "/", "nc.shp"))

# leaflet()  %>% 
# addPolygons(data = shape_ices_areas)

# ices_areas <- st_transform(shape_ices_areas, crs = 4326)
