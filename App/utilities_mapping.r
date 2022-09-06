#' Function to plot the intercative map of ecoregions.
#'
#' @param shape_eco (ecoregions' shapefile)
#' @param eu_shape (europe's shapefile)
#'
#' @return leaflet object
#'
#' @note
#' 
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' map_ecoregion(shape_eco, eu_shape)
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
#' 
map_ecoregion <- function(shape_eco, eu_shape) {
    minZoom <- 0
    maxZoom <- 13
    resolutions <- 2 * (2^(maxZoom:minZoom))
    crs_laea <- leafletCRS(
        crsClass = "L.Proj.CRS", code = "EPSG:3035",
        proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0= -1.235660 +lat_0=60.346958",
        resolutions = resolutions
    )



    leaflet(options = leafletOptions(crs = crs_laea, minZoom = minZoom, maxZoom = maxZoom)) %>%
        # addTiles() %>%
        addPolygons(
            # data = st_set_precision(eu_shape, precision=10^2),
            data = eu_shape,
            color = "black",
            weight = 1,
            fillOpacity = 0.4,
            fillColor = "#fddfc2", # "#E8EAEA"
            group = "Europe"
        ) %>%
        addPolygons(
            # data = st_set_precision(shape_eco, precision=10^2),
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
            # data = st_set_precision(shape_eco, precision=10^2),
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

