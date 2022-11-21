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
