# Define the palette
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
bins2 <- c(2000, 10000, 100000, 300000, 500000, 700000, 1000000, 1500000, Inf)
pal_ecoR <- colorBin("YlOrRd", domain = shape_eco$Shape_Area, bins = bins)
# pal_ICES_areas <- colorBin("YlOrRd", domain = ices_areas$Area_km2, bins = bins2)

# Define the interactive labels
labels_ecoR <- sprintf(
    "<strong>%s Ecoregion</strong><br/>%g Shape Area ",
    shape_eco$Ecoregion, shape_eco$Shape_Area
) %>% lapply(htmltools::HTML)

# # Define interactive labels for iceas areas
# labels_ices_areas <- sprintf(
#     "<strong>%s ICES area</strong><br/>%g Shape Area ",
#     ices_areas$Area_Full, ices_areas$Area_km2
# ) %>% lapply(htmltools::HTML)

#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
## Map plot (this function has iceas ecor R and iceas areas as separate layers)
map_plot <- function(shape_eco, eu_shape, ices_areas, labels_ecoR, labels_ices_areas) {
    leaflet(options = leafletOptions(crs = crs_laea, minZoom = minZoom, maxZoom = maxZoom)) %>%
        # addProviderTiles("Stamen.Toner") %>%
        addPolygons(
            data = shape_eco,
            color = "#444444",
            weight = 1,
            smoothFactor = 0.5,
            opacity = 0.5,
            fillOpacity = 0.5,
            fillColor = ~ pal_ecoR(shape_eco$Shape_Area),
            group = "ICES Ecoregions",
            layerId = ~uid, # unique id for polygons
            highlightOptions = highlightOptions(
                color = "white", weight = 3,
                bringToFront = TRUE
            ),
            label = labels_ecoR,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
            )
        ) %>%
        addPolygons(
            data = eu_shape,
            color = "black",
            weight = 1,
            smoothFactor = 0.5,
            opacity = 0.7,
            fillOpacity = 0.5,
            fillColor = "grey",
            group = "Europe"
        ) %>%
        addPolygons(
            data = ices_areas,
            color = "black",
            weight = 1,
            smoothFactor = 0.5,
            opacity = 0.5,
            fillOpacity = 0.5,
            fillColor = ~ pal_ICES_areas(ices_areas$Area_km2),
            group = "ICES Areas",
            layerId = ~OBJECTID, # unique id for polygons
            highlightOptions = highlightOptions(
                color = "#ffffff", weight = 3,
                bringToFront = TRUE
            ),
            label = labels_ices_areas,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
            )
        ) %>%
        setView(lng = -1.235660, lat = 60.346958, zoom = 0.5) %>%
        addLayersControl(
            baseGroups = c("ICES Ecoregions", "ICES Areas"),
            # overlayGroups =c(),
            options = layersControlOptions(collapsed = FALSE)
        )
}
# map_plot(shape_eco, eu_shape, ices_areas, labels_ecoR, labels_ices_areas)

#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
## this second map function has only ecoregion, the map will be the updated using proxy,
## the colors are simpler also
map_plot_simple <- function(shape_eco, eu_shape){
    leaflet(options = leafletOptions(crs = crs_laea, minZoom = minZoom, maxZoom = maxZoom)) %>%
    addTiles() %>%
        addPolygons(
            data = shape_eco,
            fillColor = "white",
            fillOpacity = 0.5,
            color = "black",
            stroke = TRUE,
            weight = 1,
            group = "Eco_regions",
            layerId = ~Ecoregion, # unique id for polygons
            highlightOptions = highlightOptions(
                color = "black", weight = 3,
                bringToFront = TRUE
            ),
            label = ~Ecoregion,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
            )  %>% 
            addPolygons(
            data = shape_eco,
            fillColor = "red",
            fillOpacity = 0.5,
            color = "black",
            stroke = TRUE,
            weight = 1,
            layerId = ~OBJECTID,
            group = ~Ecoregion),
            highlightOptions = highlightOptions(
                color = "black", weight = 3,
                bringToFront = TRUE
            ),
            label = ~Ecoregion,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
            )  %>% 
            hideGroup(group = shape_eco$Ecoregion)

        ) %>%
        addPolygons(
            data = eu_shape,
            color = "black",
            weight = 1,
            fillOpacity = 0.5,
            fillColor = "grey",
            group = "Europe"
        ) %>%
        setView(lng = -1.235660, lat = 60.346958, zoom = 0.5)
}



#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
map_ecoregion <- function(shape_eco, eu_shape) {
    leaflet(options = leafletOptions(crs = crs_laea, minZoom = minZoom, maxZoom = maxZoom)) %>%
                # addTiles() %>%
                addPolygons(
                    data = eu_shape,
                    color = "black",
                    weight = 1,
                    fillOpacity = 0.4,
                    fillColor = "#fddfc2", #"#E8EAEA"
                    group = "Europe"
                ) %>%
                addPolygons(
                    data = shape_eco,
                    fillColor = "#71B5BC",
                    fillOpacity = 0.08,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~Ecoregion,
                    group = "Eco_regions",
                    label = ~Ecoregion
                ) %>%
                addPolygons(
                    data = shape_eco,
                    fillColor = "#F15D2A",
                    fillOpacity = 0.7,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Ecoregion
                ) %>%
                setView(lng = -1.235660, lat = 60.346958, zoom = 0.5) %>%
                hideGroup(group = shape_eco$Ecoregion)
} 

#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
map_ices_areas <- function(ices_areas, eu_shape) {
    leaflet(options = leafletOptions(crs = crs_laea, minZoom = minZoom, maxZoom = maxZoom)) %>%
                addPolygons(
                    data = eu_shape,
                    color = "black",
                    weight = 1,
                    fillOpacity = 0.2,
                    fillColor = "#fddfc2",
                    group = "Europe"
                ) %>%
                addPolygons(
                    data = ices_areas,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~Area_Full,
                    group = "ices_areas",
                    label = ~Area_Full
                ) %>%
                addPolygons(
                    data = ices_areas,
                    fillColor = "#F15D2A",
                    fillOpacity = 0.7,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Area_Full
                ) %>%
                setView(lng = -1.235660, lat = 60.346958, zoom = 0.5) %>%
                hideGroup(group = ices_areas$Area_Full)
}
