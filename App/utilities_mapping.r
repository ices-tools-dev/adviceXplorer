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


#' Crop larger map to smaller map
#'
#' Using an optional buffer, reduce a map to the size of the bounding box of a smaller map
#' 
#' @param large_map sf object
#' @param small_map sf object
#' @param buffer the size of the buffer to be used (m)
#' @param ... 
#'
#' @return
crop_large_map_to_small <- function(large_map, small_map, buffer=0, ...){
  
  buffered_bbox <- small_map %>% sf::st_union() %>%
    sf::st_buffer(dist = buffer) %>%
    sf::st_bbox() %>% 
    sf::st_as_sfc()
  
  cropped_map <- large_map %>% sf::st_intersection(buffered_bbox)
}


#' Generates a map containing ices areas together with relevant European landmasses
#' 
#' (land masses are cropped and overseas territories do not appear)
#'
#' @param buffer_size The size of the buffer used around ices areas (m)
#' @param resolution_reduction the reduction in resolution expressed as a ratio
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
create_eu_map <- function(buffer_size = 1300000, resolution_reduction = 0.15, path){
  
  land_masses <- sf::st_read("../../GIS/ne_50m_land/ne_50m_land.shp") 
  ices_regions <-  sf::st_read(dsn = "App/Data/ICES_areas_low_res/ICES_areas_low_res.shp")
  land_crs <- sf::st_crs(land_masses)
  land_masses <- sf::st_transform(land_masses, sf::st_crs(ices_regions))
  ices_land_areas <- crop_large_map_to_small(large_map = land_masses, small_map = ices_regions, buffer = buffer_size) %>% 
    rmapshaper::ms_simplify(keep = resolution_reduction, keep_shapes= TRUE) %>% 
    sf::st_transform(land_crs) %>% 
    sf::st_combine() %>% 
    sf::st_segmentize(dfMaxLength = 1000)

  sf::st_write(ices_land_areas, dsn = path, delete_dsn = TRUE)
}
# create_eu_map(path = "./App/Data/shape_EU_simplified/shape_EU_simplified2.shp")
# temp_map <- sf::st_read(dsn = "./App/Data/shape_EU_simplified/shape_EU_simplified2.shp")
# map_ecoregion(shape_eco, eu_shape = temp_map)


#' Server side functionality for the landing page map panel
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
map_panel_server <- function(input, output, session) {
  
  # Render Map 1
  output$map1 <- renderLeaflet({
    map_ecoregion(shape_eco, eu_shape)
  }) # END RENDER LEAFLET map1
  
  proxy_1 <- leafletProxy("map1")
  
  # create empty vector to hold all click ids
  selected_1 <- reactiveValues(groups = vector())
  
  # find index
  observeEvent(input$map1_shape_click, {
    
    ## calculate index of ecoregion selected in shape_eco
    idx_1 <- match(input$map1_shape_click$id, shape_eco$Ecoregion)
    
    if (input$map1_shape_click$group == "Eco_regions") {
      selected_1$groups <- c(selected_1$groups, input$map1_shape_click$id)
      
      proxy_1 %>%
        showGroup(group = input$map1_shape_click$id) 
      
      
      ## this js code allows for the stock slection tab to be enabled once one coregion is clicked
      runjs("$(tab).removeClass('disabled');")#%>%
      
      
      # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
    } else {
      selected_1$groups <- setdiff(selected_1$groups, input$map1_shape_click$group)
      proxy_1 %>%
        hideGroup(group = input$map1_shape_click$group) #%>%
      
      
    }
    updateSelectizeInput(session,
                         inputId = "selected_locations",
                         label = "ICES Ecoregions",
                         choices = shape_eco$Ecoregion,
                         selected = selected_1$groups
    )
    
  })
  
  observeEvent(input$selected_locations,
               {
                 removed_via_selectInput <- setdiff(selected_1$groups, input$selected_locations)
                 added_via_selectInput <- setdiff(input$selected_locations, selected_1$groups)
                 
                 if (length(removed_via_selectInput) > 0) {
                   selected_1$groups <- input$selected_locations
                   
                   proxy_1 %>% hideGroup(group = removed_via_selectInput)
                 }
                 
                 if (length(added_via_selectInput) > 0) {
                   selected_1$groups <- input$selected_locations
                   
                   proxy_1 %>% showGroup(group = added_via_selectInput)
                   
                   ## this js code allows for the stock slection tab to be enabled once one coregion is clicked
                   runjs("$(tab).removeClass('disabled');")
                 }
               },
               ignoreNULL = FALSE
  )
}
