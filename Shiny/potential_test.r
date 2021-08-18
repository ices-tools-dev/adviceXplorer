library(shiny)
library(leaflet)
library(sf)
library(dplyr)

#load shapefile
nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  st_transform(4326)

shinyApp(
  ui = fluidPage(
    
    "Update selectize input by clicking on the map",
    
    leafletOutput("map"),
    "I would like the selectize input to update to show all the locations selected,",
    "but also when items are removed here, they are removed on the map too, so linked to the map.",
    selectizeInput(inputId = "selected_locations",
                   label = "selected",
                   choices = nc$NAME,
                   selected = NULL,
                   multiple = TRUE)
  ),
  
  server <- function(input, output, session){
    
    #create empty vector to hold all click ids
    selected_ids <- reactiveValues(ids = vector())
    
    #initial map output
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = nc,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~NAME,
                    group = "regions",
                    label = ~NAME) %>%
        addPolygons(data = nc,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~CNTY_ID,
                    group = ~NAME) %>%
        hideGroup(group = nc$NAME) # nc$CNTY_ID
    }) #END RENDER LEAFLET
    
    #define leaflet proxy for second regional level map
    proxy <- leafletProxy("map")
    
    #create empty vector to hold all click ids
    selected <- reactiveValues(groups = vector())
    
    observeEvent(input$map_shape_click, {
      if(input$map_shape_click$group == "regions"){
        selected$groups <- c(selected$groups, input$map_shape_click$id)
        proxy %>% showGroup(group = input$map_shape_click$id)
      } else {
        selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        proxy %>% hideGroup(group = input$map_shape_click$group)
      }
      updateSelectizeInput(session,
                           inputId = "selected_locations",
                           label = "",
                           choices = nc$NAME,
                           selected = selected$groups)
    })
    
    observeEvent(input$selected_locations, {
      removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
      added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
      
      if(length(removed_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% hideGroup(group = removed_via_selectInput)
      }
      
      if(length(added_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% showGroup(group = added_via_selectInput)
      }
    }, ignoreNULL = FALSE)
    
  })



##########################################################################################################################


library(spData)
library(leaflet)
library(sf)

# loading shapes of countries from the package spData

data(world)
world <- st_read(system.file("shapes/world.gpkg", package="spData"))

# creating a sf objet with oceanian countries boundaries

oceania <- world[world$continent=="Oceania",]

#loading points events from the quakes dataset

data(quakes)

#Creating a leaflet object with points and polygons

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng=quakes$long,
                   lat=quakes$lat,
                   col="blue",
                   radius=3,
                   stroke=FALSE,
                   fillOpacity = 0.7,
                   #options = markerOptions(minZoom=15, maxZoom=20), # Oldcode
                   group = "Quake Points") %>%                       # Newcode
  addPolygons(data= oceania,
              col="red") %>%                        
  groupOptions("Quake Points", zoomLevels = 15:20)                   # Newcode



  ############################################################################################################


library(shiny)
library(leaflet)
library(maps)

ui <- shinyUI(fluidPage(
    fluidRow(
        tags$style(type = "text/css", "#livemap {height: calc(100vh - 80px) !important;}"),
        leafletOutput("livemap")
    )
))


server <- shinyServer(function(input, output){
    output$livemap <- renderLeaflet({
        mapStates <- map("state", fill = TRUE, plot = FALSE)

        ## chuck on a zoom
        mapStates$zoom <- sample(5:8, size = length(mapStates$name), replace = T)

        leaflet(mapStates) %>%
            addTiles() %>%
            addPolygons(color = "#444444",
                                    weight = 1,
                                    layer = ~mapStates$name,   ## LayerID defined
                                    smoothFactor = 0.5,
                                    opacity = 1.0,
                                    fillOpacity = 0.5,
                                    fillColor = terrain.colors(50, alpha = 1),
                                    highlightOptions = highlightOptions(color = "black", weight = 2, 
                                                                                                            bringToFront = TRUE))
    })

    observe({
        click <- input$livemap_shape_click
        if(is.null(click))
            return()

        ## use the click to access the zoom and set the view according to these
        ## the click$id is now returned with the 'name' of the state
        ## because we specified it in the LayerId argument
        idx <- which(mapStates$name == click$id)
        z <- mapStates$zoom[[idx]]

        leafletProxy("livemap") %>% 
            setView(lng = click$lng, lat = click$lat, zoom = z)
    })
})

shinyApp(ui, server)


###################################################################################################

library(shiny)
library(leaflet)

 ui <- shinyUI(fluidPage(
    fluidRow(
        tags$style(type = "text/css", "#livemap {height: calc(100vh - 80px) !important;}"),
        leafletOutput("livemap")
    )
))


library(shiny)
  library(leaflet)
  library(maps)

  shinyServer(function(input, output){
       output$livemap <- renderLeaflet({
            mapStates <- map("state", fill = TRUE, plot = FALSE)
            mapStates$zoom <- c(7.3, 7.1, 7.5, 6.2, 7.2, 9.2, 4.0, 7.0,
                                7.3, 6.5, 7.0, 7.4, 7.5, 7.5, 7.8, 7.4,
                                7.1, 8.3, 8.6, 8.6, 8.6, 7.0, 7.0, 6.7,
                                7.3, 7.2, 7.0, 7.5, 6.6, 7.8, 8.0, 7.0,
                                7.2, 7.2, 7.2, 7.2, 7.6, 7.6, 7.6, 7.4,
                                7.6, 7.6, 7.2, 7.6, 9.4, 7.8, 7.4, 7.6,
                                6.2, 7.0, 8.0, 7.6, 7.6, 7.6, 7.3, 7.3,
                                7.3, 7.3, 7.3, 7.6, 7.2, 7.2)
           leaflet(mapStates) %>%
                addTiles() %>%
                addPolygons(color = "#444444",
                            weight = 1,
                            layer = ~mapStates$names,
                            smoothFactor = 0.5,
                            opacity = 1.0,
                            fillOpacity = 0.5,
                            fillColor = terrain.colors(50, alpha = 1),
                            highlightOptions = highlightOptions(color = "black",
                                                                weight = 2,
                                                                bringToFront = TRUE))
       })
       # Observe click on shapes (i.e., states)
       observe({
            click <- input$livemap_shape_click
            if(is.null(click))
                 return()
            idx <- which(mapStates$names == click$id)
            # Get zoom level for the state
            z <- mapStates$zoom[[idx]]
            # Get state name to render new map
            idx <- mapStates$names[[idx]]
            mapInd <- map("county", idx, fill = TRUE, plot = FALSE)

            leafletProxy("livemap") %>%
                 clearShapes() %>%
                 addPolygons(data = mapInd,
                             color = "#444444",
                             weight = 1,
                             smoothFactor = 0.5,
                             opacity = 1.0,
                             fillOpacity = 0.5,
                             fillColor = terrain.colors(10, alpha = 1)) %>%
                 setView(lng = ((mapInd$range[[1]] + mapInd$range[[2]])/2),
                         lat = ((mapInd$range[[3]] + mapInd$range[[4]])/2),
                         zoom = z)
       })
       # Observe click outside of shapes (i.e., reset the map to the "USA" original)
       observe({
            click <- input$livemap_click
            if(is.null(click))
                 return()
            leafletProxy("livemap") %>%
                 clearShapes() %>%
                 addPolygons(data = mapStates,
                             color = "#444444",
                             weight = 1,
                             layer = ~mapStates$names,
                             smoothFactor = 0.5,
                             opacity = 1.0,
                             fillOpacity = 0.5,
                             fillColor = terrain.colors(50, alpha = 1),
                             highlightOptions = highlightOptions(color = "black",
                                                                 weight = 2,
                                                                 bringToFront = TRUE)) %>%
                 setView(lng = ((mapStates$range[[1]] + mapStates$range[[2]])/2),
                         lat = ((mapStates$range[[3]] + mapStates$range[[4]])/2),
                         zoom = 4)
       })

  })
shinyApp(ui, server)



###########################################################################################

# https://stackoverflow.com/questions/41104576/changing-styles-when-selecting-and-deselecting-multiple-polygons-with-leaflet-sh
library(raster)
library(shiny)
library(leaflet)
library(sf)
library(dplyr)

#load shapefile
rwa_raw <- getData("GADM", country = "RWA", level = 1)
rwa <- st_as_sf(rwa_raw)

shinyApp(
  ui = fluidPage(

    "Update selectize input by clicking on the map",

    leafletOutput("map"),
    "I would like the selectize input to update to show all the locations clicked,",
    "but also when items are removed here, they are removed on the map too, so linked to the map.",
    selectizeInput(inputId = "clicked_locations",
                   label = "Clicked",
                   choices = rwa$NAME_1,
                   selected = NULL,
                   multiple = TRUE)
  ),

  server <- function(input, output, session){

    #create empty vector to hold all click ids
    clicked_ids <- reactiveValues(ids = vector())

    #initial map output
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = rwa,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~NAME_1,
                    group = "regions",
                    label = ~NAME_1)
    }) #END RENDER LEAFLET

    observeEvent(input$map_shape_click, {

      #create object for clicked polygon
      click <- input$map_shape_click

      #define leaflet proxy for second regional level map
      proxy <- leafletProxy("map")

      #append all click ids in empty vector
      clicked_ids$ids <- c(clicked_ids$ids, click$id) # name when clicked, id when unclicked
        
      #shapefile with all clicked polygons - original shapefile subsetted by all admin names from the click list
      clicked_polys <- rwa %>%
        filter(NAME_1 %in% clicked_ids$ids)
    
      #if the current click ID [from GID_1] exists in the clicked polygon (if it has been clicked twice)
      if(click$id %in% clicked_polys$GID_1){

        #define vector that subsets NAME that matches GID_1 click ID - needs to be different to above
        name_match <- clicked_polys$NAME_1[clicked_polys$GID_1 == click$id]
print(name_match)
        print("//////////")
        #remove the current click$id AND its name match from the clicked_polys shapefile
        clicked_ids$ids <- clicked_ids$ids[!clicked_ids$ids %in% click$id]
        clicked_ids$ids <- clicked_ids$ids[!clicked_ids$ids %in% name_match]

        # just to see
        print(clicked_ids$ids)

        # update
        updateSelectizeInput(session,
                             inputId = "clicked_locations",
                             label = "",
                             choices = rwa$NAME_1,
                             selected = clicked_ids$ids)

        #remove that highlighted polygon from the map
        proxy %>% removeShape(layerId = click$id)

      } else {

        #map highlighted polygons
        proxy %>% addPolygons(data = clicked_polys,
                              fillColor = "red",
                              fillOpacity = 0.5,
                              weight = 1,
                              color = "black",
                              stroke = TRUE,
                              layerId = clicked_polys$GID_1)

        # just to see
        print(clicked_ids$ids)

        # update
        updateSelectizeInput(session,
                          inputId = "clicked_locations",
                          label = "",
                          choices = rwa$NAME_1,
                          selected = clicked_ids$ids)

      } #END CONDITIONAL
    }) #END OBSERVE EVENT
  }) #END SHINYAPP


#####################################################################################

library(shiny)
library(leaflet)
library(sf)
library(dplyr)



#load shapefile
nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  st_transform(4326)
nc <- shape_eco
shinyApp(
  ui = fluidPage(
    
    "Update selectize input by clicking on the map",
    
    leafletOutput("map"),
    "I would like the selectize input to update to show all the locations selected,",
    "but also when items are removed here, they are removed on the map too, so linked to the map.",
    selectizeInput(inputId = "selected_locations",
                   label = "selected",
                   choices = shape_eco$Ecoregion,
                   selected = NULL,
                   multiple = TRUE)
  ),
  
  server <- function(input, output, session){
    
    #create empty vector to hold all click ids
    selected_ids <- reactiveValues(ids = vector())
    
    #initial map output
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = nc,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~Ecoregion,
                    group = "Eco_regions",
                    label = ~Ecoregion) %>%
        addPolygons(data = nc,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Ecoregion) %>%
        hideGroup(group = nc$Ecoregion) # nc$CNTY_ID
    }) #END RENDER LEAFLET
    
    #define leaflet proxy for second regional level map
    proxy <- leafletProxy("map")
    
    #create empty vector to hold all click ids
    selected <- reactiveValues(groups = vector())
    
    observeEvent(input$map_shape_click, {
      if(input$map_shape_click$group == "Eco_regions"){
        selected$groups <- c(selected$groups, input$map_shape_click$id)
        proxy %>% showGroup(group = input$map_shape_click$id)
      } else {
        selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        proxy %>% hideGroup(group = input$map_shape_click$group)
      }
      updateSelectizeInput(session,
                           inputId = "selected_locations",
                           label = "",
                           choices = shape_eco$Ecoregion,
                           selected = selected$groups)
    })
    
    observeEvent(input$selected_locations, {
      removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
      added_via_selectInput <- setdiff(input$selected_locations, selected$groups)
      
      if(length(removed_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% hideGroup(group = removed_via_selectInput)
      }
      
      if(length(added_via_selectInput) > 0){
        selected$groups <- input$selected_locations
        proxy %>% showGroup(group = added_via_selectInput)
      }
    }, ignoreNULL = FALSE)
    
  })


######################################################################################
## test per zoom

nc <- shape_eco
# calculate the centroid of each polygon

sf_cent <- st_coordinates(st_centroid(shape_eco))
sf_cent_map_X <- mean(sf_cent[, 1])
sf_cent_map_Y <- mean(sf_cent[, 2])
sf_cent_map <- c(sf_cent_map_X, sf_cent_map_Y)
### test[2,1] [row,column X]
### test[2,2] [row,column Y]

shinyApp(
    ui = fluidPage(
        "Update selectize input by clicking on the map",

        leafletOutput("map"),
        "I would like the selectize input to update to show all the locations selected,",
        "but also when items are removed here, they are removed on the map too, so linked to the map.",
        selectizeInput(
            inputId = "selected_locations",
            label = "ICES Ecoregions",
            choices = shape_eco$Ecoregion,
            selected = NULL,
            multiple = TRUE
        ),
        selectizeInput(
            inputId = "selected_areas",
            label = "ICES Areas",
            choices = shape_eco$Ecoregion,
            selected = NULL,
            multiple = TRUE
        )
    ),

    server <- function(input, output, session) {

        # create empty vector to hold all click ids
        selected_ids <- reactiveValues(ids = vector())

        # initial map output
        output$map <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addPolygons(
                    data = nc,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~Ecoregion,
                    group = "Eco_regions",
                    label = ~Ecoregion
                ) %>%
                addPolygons(
                    data = nc,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Ecoregion
                ) %>%
                hideGroup(group = nc$Ecoregion) # nc$CNTY_ID
        }) # END RENDER LEAFLET

        # define leaflet proxy for second regional level map
        proxy <- leafletProxy("map")

        # create empty vector to hold all click ids
        selected <- reactiveValues(groups = vector())

        # find index


        observeEvent(input$map_shape_click, {
            ## calculate index of ecoregion selected in shape_eco
            idx <- match(input$map_shape_click$id, shape_eco$Ecoregion)

            if (input$map_shape_click$group == "Eco_regions") {
                selected$groups <- c(selected$groups, input$map_shape_click$id)
                proxy %>%
                    showGroup(group = input$map_shape_click$id) %>%
                    setView(
                        lng = sf_cent[idx, 1],
                        lat = sf_cent[idx, 2],
                        zoom = 3
                    )

                # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
            } else {
                selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
                proxy %>% hideGroup(group = input$map_shape_click$group)  %>% 
                setView(
                        lng = sf_cent_map[1],
                        lat = sf_cent_map[2],
                        zoom = 1
                    )
            }
            updateSelectizeInput(session,
                inputId = "selected_locations",
                label = "ICES Ecoregions",
                choices = shape_eco$Ecoregion,
                selected = selected$groups
            )
        })

        observeEvent(input$selected_locations,
            {
                removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
                added_via_selectInput <- setdiff(input$selected_locations, selected$groups)

                if (length(removed_via_selectInput) > 0) {
                    selected$groups <- input$selected_locations
                    proxy %>% hideGroup(group = removed_via_selectInput)
                }

                if (length(added_via_selectInput) > 0) {
                    selected$groups <- input$selected_locations
                    proxy %>% showGroup(group = added_via_selectInput)
                }
            },
            ignoreNULL = FALSE
        )
    }
)

############################################################################################
######### Test for plotting ices areas and zooms{still not working}

## test per zoom

nc <- shape_eco
# calculate the centroid of each polygon

sf_cent <- st_coordinates(st_centroid(shape_eco))
sf_cent_map_X <- mean(sf_cent[, 1])
sf_cent_map_Y <- mean(sf_cent[, 2])
sf_cent_map <- c(sf_cent_map_X, sf_cent_map_Y)
### test[2,1] [row,column X]
### test[2,2] [row,column Y]

shinyApp(
    ui = fluidPage(
        "Update selectize input by clicking on the map",

        leafletOutput("map"),
        "I would like the selectize input to update to show all the locations selected,",
        "but also when items are removed here, they are removed on the map too, so linked to the map.",
        selectizeInput(
            inputId = "selected_locations",
            label = "ICES Ecoregions",
            choices = shape_eco$Ecoregion,
            selected = NULL,
            multiple = TRUE
        ),
        selectizeInput(
            inputId = "selected_areas",
            label = "ICES Areas",
            choices = ices_areas$Area_Full,
            selected = NULL,
            multiple = TRUE
        )
    ),

    server <- function(input, output, session) {

        # create empty vector to hold all click ids
        selected_ids <- reactiveValues(ids = vector())

        # initial map output
        output$map <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addPolygons(
                    data = nc,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~Ecoregion,
                    group = "Eco_regions",
                    label = ~Ecoregion
                ) %>%
                addPolygons(
                    data = nc,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Ecoregion
                ) %>%
                addPolygons(
                    data = ices_areas,
                    fillColor = "trasparent",
                    # fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~Area_Full,
                    group = "ICES_Areas",
                    label = ~Area_Full
                )  %>% 
                addPolygons(
                    data = ices_areas,
                    fillColor = "#ffee00",
                    fillOpacity = 0.1,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Area_Full
                )  %>% 
                hideGroup(group = c(nc$Ecoregion, ices_areas$Area_full)) # nc$CNTY_ID
        }) # END RENDER LEAFLET

        # define leaflet proxy for second regional level map
        proxy <- leafletProxy("map")

        # create empty vector to hold all click ids
        selected <- reactiveValues(groups = vector())
        print(selected)
        # find index


        observeEvent(input$map_shape_click, {
          #print(input$map_shape_click)
            ## calculate index of ecoregion selected in shape_eco
            idx <- match(input$map_shape_click$id, shape_eco$Ecoregion)

            if (input$map_shape_click$group == "Eco_regions") {
                selected$groups <- c(selected$groups, input$map_shape_click$id)
                proxy %>%
                    showGroup(group = input$map_shape_click$id) %>%
                    # setView(
                    #     lng = sf_cent[idx, 1],
                    #     lat = sf_cent[idx, 2],
                    #     zoom = 3
                    # )  %>% 
                    addPolygons(
                    data = ices_areas,
                    # fillColor = "white",
                    # fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~Area_Full,
                    group = "ICES_Areas",
                    label = ~Area_Full
                    )
                print(selected$groups)
                # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
            } else {
                selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
                print(selected$groups)
                proxy %>% hideGroup(group = input$map_shape_click$group)  #%>% 
                # setView(
                #         lng = sf_cent_map[1],
                #         lat = sf_cent_map[2],
                #         zoom = 1
                #     )
            }
            updateSelectizeInput(session,
                inputId = "selected_locations",
                label = "ICES Ecoregions",
                choices = shape_eco$Ecoregion,
                selected = selected$groups
            )
        })

        observeEvent(input$selected_locations,
            {
                removed_via_selectInput <- setdiff(selected$groups, input$selected_locations)
                added_via_selectInput <- setdiff(input$selected_locations, selected$groups)

                if (length(removed_via_selectInput) > 0) {
                    selected$groups <- input$selected_locations
                    proxy %>% hideGroup(group = removed_via_selectInput)
                }

                if (length(added_via_selectInput) > 0) {
                    selected$groups <- input$selected_locations
                    proxy %>% showGroup(group = added_via_selectInput)
                }
            },
            ignoreNULL = FALSE
        )

        ####### the same but for Areas
        observeEvent(input$selected_areas,
            {
                removed_via_selectInput <- setdiff(selected$groups, input$selected_areas)
                added_via_selectInput <- setdiff(input$selected_areas, selected$groups)

                if (length(removed_via_selectInput) > 0) {
                    selected$groups <- input$selected_areas
                    proxy %>% hideGroup(group = removed_via_selectInput)
                }

                if (length(added_via_selectInput) > 0) {
                    selected$groups <- input$selected_areas
                    proxy %>% showGroup(group = added_via_selectInput)
                }
            },
            ignoreNULL = FALSE
        )
        # ### for ices areas NOT WORKING
        # observeEvent(input$map_shape_click, {
        #   print(input$map_shape_click)
        #     ## calculate index of ecoregion selected in shape_eco
        #     idx <- match(input$map_shape_click$id, ices_areas$Area_Full)

        #     if (input$map_shape_click$group == "ICES_Areas") {
        #         selected$groups <- c(selected$groups, input$map_shape_click$id)
        #         proxy %>%
        #             showGroup(group = input$map_shape_click$id) %>%
        #             # setView(
        #             #     lng = sf_cent[idx, 1],
        #             #     lat = sf_cent[idx, 2],
        #             #     zoom = 3
        #             # )  %>% 
        #             addPolygons(
        #             data = ices_areas,
        #             # fillColor = "white",
        #             # fillOpacity = 0.5,
        #             color = "black",
        #             stroke = TRUE,
        #             weight = 1,
        #             layerId = ~Area_Full,
        #             group = "ICES_Areas",
        #             label = ~Area_Full
        #             )

        #         # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
        #     } else {
        #         selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        #         proxy %>% hideGroup(group = input$map_shape_click$group)  %>% 
        #         setView(
        #                 lng = sf_cent_map[1],
        #                 lat = sf_cent_map[2],
        #                 zoom = 1
        #             )
        #     }
        #     updateSelectizeInput(session,
        #         inputId = "selected_areas",
        #         label = "ICES Areas",
        #         choices = ices_areas$Area_Full,
        #         selected = selected$groups
        #     )
        # })

    }
)


###################################################################################### WORKING
## test for having 2 tabs for two maps (ecoregions and areas)

nc <- shape_eco
# calculate the centroid of each polygon

sf_cent <- st_coordinates(st_centroid(shape_eco))
sf_cent_map_X <- mean(sf_cent[, 1])
sf_cent_map_Y <- mean(sf_cent[, 2])
sf_cent_map <- c(sf_cent_map_X, sf_cent_map_Y)
### test[2,1] [row,column X]
### test[2,2] [row,column Y]

shinyApp(
    ui = fluidPage(
        "Update selectize input by clicking on the map",
         
        tabsetPanel(
          tabPanel("ICES Ecoregions", leafletOutput("map1")),
          tabPanel("ICES Areas", leafletOutput("map2"))
        ),
        selectizeInput(
            inputId = "selected_locations",
            label = "ICES Ecoregions",
            choices = shape_eco$Ecoregion,
            selected = NULL,
            multiple = TRUE,
            #placeholder = 'select Ecoregion(s)'
        ),
        selectizeInput(
            inputId = "selected_areas",
            label = "ICES Areas",
            choices = ices_areas$Area_Full,
            selected = NULL,
            multiple = TRUE,
            #placeholder = 'select ICES Area(s)'
        )
        
    ),

    server <- function(input, output, session) {

        # create empty vector to hold all click ids
        selected_ids <- reactiveValues(ids = vector())

        # map output Ecoregion
        output$map1 <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addPolygons(
                    data = nc,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~Ecoregion,
                    group = "Eco_regions",
                    label = ~Ecoregion
                ) %>%
                addPolygons(
                    data = nc,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Ecoregion
                ) %>%
                hideGroup(group = nc$Ecoregion) # nc$CNTY_ID
        }) # END RENDER LEAFLET

        # map output Areas
        output$map2 <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
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
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Area_Full
                ) %>%
                hideGroup(group = ices_areas$Area_Full) # nc$CNTY_ID
        }) # END RENDER LEAFLET

        ############################## Interactive section Ecoregions ######################
        # define leaflet proxy for Ecoregion map
        proxy_1 <- leafletProxy("map1")

        # create empty vector to hold all click ids
        selected_1 <- reactiveValues(groups = vector())
        
        # find index


        observeEvent(input$map1_shape_click, {
            ## calculate index of ecoregion selected in shape_eco
            idx_1 <- match(input$map1_shape_click$id, shape_eco$Ecoregion)
            #print(idx_1)
            if (input$map1_shape_click$group == "Eco_regions") {
                selected_1$groups <- c(selected_1$groups, input$map1_shape_click$id)
                proxy_1 %>%
                    showGroup(group = input$map1_shape_click$id) %>%
                    setView(
                        lng = sf_cent[idx_1, 1],
                        lat = sf_cent[idx_1, 2],
                        zoom = 3
                    )

                # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
            } else {
                selected_1$groups <- setdiff(selected_1$groups, input$map1_shape_click$group)
                proxy_1 %>% hideGroup(group = input$map1_shape_click$group)  %>% 
                setView(
                        lng = sf_cent_map[1],
                        lat = sf_cent_map[2],
                        zoom = 1
                    )
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
                }
            },
            ignoreNULL = FALSE

        )


        ############################## Interactive section Areas ######################
        # define leaflet proxy for Ecoregion map
        proxy_2 <- leafletProxy("map2")

        # create empty vector to hold all click ids
        selected_2 <- reactiveValues(groups = vector())
        
        # find index


        observeEvent(input$map2_shape_click, {
            ## calculate index of ecoregion selected in shape_eco
            idx_2 <- match(input$map2_shape_click$id, ices_areas$Area_Full)
            #print(idx_2)
            if (input$map2_shape_click$group == "ices_areas") {
                selected_2$groups <- c(selected_2$groups, input$map2_shape_click$id)
                proxy_2 %>%
                    showGroup(group = input$map2_shape_click$id) #%>%
                    # setView(
                    #     lng = sf_cent[idx_1, 1],
                    #     lat = sf_cent[idx_1, 2],
                    #     zoom = 3
                    # )

                # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
            } else {
                selected_2$groups <- setdiff(selected_2$groups, input$map2_shape_click$group)
                proxy_2 %>% hideGroup(group = input$map2_shape_click$group)  %>% 
                setView(
                        lng = sf_cent_map[1],
                        lat = sf_cent_map[2],
                        zoom = 1
                    )
            }
            updateSelectizeInput(session,
                inputId = "selected_areas",
                label = "ICES Areas",
                choices = ices_areas$Area_Full,
                selected = selected_2$groups
            )
        })
        
        observeEvent(input$selected_areas,
            {
                removed_via_selectInput <- setdiff(selected_2$groups, input$selected_areas)
                added_via_selectInput <- setdiff(input$selected_areas, selected_2$groups)

                if (length(removed_via_selectInput) > 0) {
                    selected_2$groups <- input$selected_areas
                    proxy_2 %>% hideGroup(group = removed_via_selectInput)
                }

                if (length(added_via_selectInput) > 0) {
                    selected_2$groups <- input$selected_areas
                    proxy_2 %>% showGroup(group = added_via_selectInput)
                }
            },
            ignoreNULL = FALSE
        )
    }
)



####################################### test filtering with only the sid table
if (interactive()) {

  library(shiny)
  library(shinyWidgets)

  #data("mpg", package = "ggplot2")

  ui <- fluidPage(
    fluidRow(
      column(
        width = 10, offset = 1,
        tags$h3("Filter data with selectize group"),
        panel(
          selectizeGroupUI(
            id = "my-filters",
            params = list(
              EcoRegion = list(inputId = "EcoRegion", title = "EcoRegion:"),
              StockKeyLabel = list(inputId = "StockKeyLabel", title = "StockKeyLabel:"),
              SpeciesCommonName = list(inputId = "SpeciesCommonName", title = "SpeciesCommonName:"),
              #DataCategory = list(inputId = "DataCategory", title = "DataCategory:"),
              ICES_area = list(inputId = "ICES_area", title = "ICES_area")
            )
          ), status = "primary"
        ),
        DT::dataTableOutput(outputId = "table")
      )
    )
  )

  server <- function(input, output, session) {
    res_mod <- callModule(
      module = selectizeGroupServer,
      id = "my-filters",
      data = stock_list_all,
      vars = c("EcoRegion", "StockKeyLabel", "SpeciesCommonName", "ICES_area")
    )
    output$table <- DT::renderDataTable(res_mod())
  }

  shinyApp(ui, server)

}



################################################################################
## test for combining slecetize input and filtering of the table SID

nc <- shape_eco
# calculate the centroid of each polygon

sf_cent <- st_coordinates(st_centroid(shape_eco))
sf_cent_map_X <- mean(sf_cent[, 1])
sf_cent_map_Y <- mean(sf_cent[, 2])
sf_cent_map <- c(sf_cent_map_X, sf_cent_map_Y)
### test[2,1] [row,column X]
### test[2,2] [row,column Y]

shinyApp(
    ui = fluidPage(
        "Update selectize input by clicking on the map",
         fluidRow(
        tabsetPanel(
          tabPanel("ICES Ecoregions", leafletOutput("map1")),
          tabPanel("ICES Areas", leafletOutput("map2"))
        ),
        selectizeInput(
            inputId = "selected_locations",
            label = "ICES Ecoregions",
            choices = shape_eco$Ecoregion,
            selected = NULL,
            multiple = TRUE,
            #placeholder = 'select Ecoregion(s)'
        ),
        selectizeInput(
            inputId = "selected_areas",
            label = "ICES Areas",
            choices = ices_areas$Area_Full,
            selected = NULL,
            multiple = TRUE,
            #placeholder = 'select ICES Area(s)'
        ),
        DT::dataTableOutput(outputId = "table")
         )
      
    
         ),

    server <- function(input, output, session) {

        # create empty vector to hold all click ids
        selected_ids <- reactiveValues(ids = vector())

        # map output Ecoregion
        output$map1 <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addPolygons(
                    data = nc,
                    fillColor = "white",
                    fillOpacity = 0.5,
                    color = "black",
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~Ecoregion,
                    group = "Eco_regions",
                    label = ~Ecoregion
                ) %>%
                addPolygons(
                    data = nc,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Ecoregion
                ) %>%
                hideGroup(group = nc$Ecoregion) # nc$CNTY_ID
        }) # END RENDER LEAFLET

        # map output Areas
        output$map2 <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
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
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Area_Full
                ) %>%
                hideGroup(group = ices_areas$Area_Full) # nc$CNTY_ID
        }) # END RENDER LEAFLET

        ############################## Interactive section Ecoregions ######################
        # define leaflet proxy for Ecoregion map
        proxy_1 <- leafletProxy("map1")

        # create empty vector to hold all click ids
        selected_1 <- reactiveValues(groups = vector())
        
        # find index


        observeEvent(input$map1_shape_click, {
            ## calculate index of ecoregion selected in shape_eco
            idx_1 <- match(input$map1_shape_click$id, shape_eco$Ecoregion)
            #print(idx_1)
            if (input$map1_shape_click$group == "Eco_regions") {
                selected_1$groups <- c(selected_1$groups, input$map1_shape_click$id)
                print(selected_1$groups)
                proxy_1 %>%
                    showGroup(group = input$map1_shape_click$id) %>%
                    setView(
                        lng = sf_cent[idx_1, 1],
                        lat = sf_cent[idx_1, 2],
                        zoom = 3
                    )

                # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
            } else {
                selected_1$groups <- setdiff(selected_1$groups, input$map1_shape_click$group)
                proxy_1 %>% hideGroup(group = input$map1_shape_click$group)  %>% 
                setView(
                        lng = sf_cent_map[1],
                        lat = sf_cent_map[2],
                        zoom = 1
                    )
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
                    print(selected_1$groups)
                    proxy_1 %>% hideGroup(group = removed_via_selectInput)
                }

                if (length(added_via_selectInput) > 0) {
                    selected_1$groups <- input$selected_locations
                    print(selected_1$groups)
                    proxy_1 %>% showGroup(group = added_via_selectInput)
                }
            },
            ignoreNULL = FALSE

        )


        ############################## Interactive section Areas ######################
        # define leaflet proxy for Ecoregion map
        proxy_2 <- leafletProxy("map2")

        # create empty vector to hold all click ids
        selected_2 <- reactiveValues(groups = vector())
        
        # find index


        observeEvent(input$map2_shape_click, {
            ## calculate index of ecoregion selected in shape_eco
            idx_2 <- match(input$map2_shape_click$id, ices_areas$Area_Full)
            #print(idx_2)
            if (input$map2_shape_click$group == "ices_areas") {
                selected_2$groups <- c(selected_2$groups, input$map2_shape_click$id)
                proxy_2 %>%
                    showGroup(group = input$map2_shape_click$id) #%>%
                    # setView(
                    #     lng = sf_cent[idx_1, 1],
                    #     lat = sf_cent[idx_1, 2],
                    #     zoom = 3
                    # )

                # print(match(input$map_shape_click$id, shape_eco$Ecoregion))
            } else {
                selected_2$groups <- setdiff(selected_2$groups, input$map2_shape_click$group)
                proxy_2 %>% hideGroup(group = input$map2_shape_click$group)  %>% 
                setView(
                        lng = sf_cent_map[1],
                        lat = sf_cent_map[2],
                        zoom = 1
                    )
            }
            updateSelectizeInput(session,
                inputId = "selected_areas",
                label = "ICES Areas",
                choices = ices_areas$Area_Full,
                selected = selected_2$groups
            )
        })
        
        observeEvent(input$selected_areas,
            {
                removed_via_selectInput <- setdiff(selected_2$groups, input$selected_areas)
                added_via_selectInput <- setdiff(input$selected_areas, selected_2$groups)

                if (length(removed_via_selectInput) > 0) {
                    selected_2$groups <- input$selected_areas
                    proxy_2 %>% hideGroup(group = removed_via_selectInput)
                }

                if (length(added_via_selectInput) > 0) {
                    selected_2$groups <- input$selected_areas
                    proxy_2 %>% showGroup(group = added_via_selectInput)
                }
            },
            ignoreNULL = FALSE
        )
        
        ######## this part in not working, or running, the selected_1$groups has the string of char I want to use to filter the table
        # output$table <- renderTable(
        #   #print(req(selected_1$groups))
        # #   # Uncomment the two lines with comments if you want to make it mandatory to chose a continent to show the table

        # #   # req(input$RegionSelect)
        # #   #req(selected_1$groups)
            stock_list_all %>%
        # #     # filter(Region %in% input$RegionSelect) %>%
        #      #filter(EcoRegion %in% selected_1$groups)
          
        # )
    }
)
