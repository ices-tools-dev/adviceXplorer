############# adding more fields
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
         
        tabsetPanel(
          tabPanel("ICES Ecoregions", leafletOutput("map1")),
          tabPanel("ICES Areas", leafletOutput("map2"))
        ),
        panel(
        selectizeInput(
            inputId = "selected_locations",
            label = "ICES Ecoregions",
            choices = shape_eco$Ecoregion,
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = 'Select Ecoregion(s)')
        ),
        selectizeInput(
            inputId = "selected_areas",
            label = "ICES Areas",
            choices = ices_areas$Area_Full,
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = 'Select ICES Area(s)')
        ),
        
          selectizeGroupUI(
            id = "my-filters",
            params = list(
            #   EcoRegion = list(inputId = "EcoRegion", title = "EcoRegion:"),
              StockKeyLabel = list(inputId = "StockKeyLabel", title = "StockKeyLabel:"),
              SpeciesCommonName = list(inputId = "SpeciesCommonName", title = "SpeciesCommonName:"),
              DataCategory = list(inputId = "DataCategory", title = "DataCategory:"),
              ICES_area = list(inputId = "ICES_area", title = "ICES_area")
            )
          ), status = "primary"
        ),
        DTOutput("tbl_summary")
        ),

    server <- function(input, output, session) {

        # create empty vector to hold all click ids
        selected_ids <- reactiveValues(ids = vector())

        # map output Ecoregion
        output$map1 <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addPolygons(
                    data = shape_eco,
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
                    data = shape_eco,
                    fillColor = "red",
                    fillOpacity = 0.5,
                    weight = 1,
                    color = "black",
                    stroke = TRUE,
                    layerId = ~OBJECTID,
                    group = ~Ecoregion
                ) %>%
                hideGroup(group = shape_eco$Ecoregion) # nc$CNTY_ID
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


        
        stock_list_long <- separate_ecoregions(stock_list_all)

        eco_filter <- reactive({
          req(input$selected_locations)
          print(input$selected_locations)

          temp_df <- data.frame()
          for (i in 1:length(input$selected_locations)) {
              temp_1 <- stock_list_long %>% filter(str_detect(EcoRegion, input$selected_locations[i]))
              temp_df <- rbind(temp_df, temp_1)
          }
          stock_list_long <- temp_df

        #   stock_list_long %>%
          
        #     filter(str_detect(stock_list_long$EcoRegion, input$selected_locations))
                })
        
        # res_mod <- reactive({
        res_mod <- callModule(
          module = selectizeGroupServer,
          id = "my-filters",
          # data = separate_ecoregions(stock_list_all, selected_1$groups),
          data = eco_filter,
          vars = c("EcoRegion", "StockKeyLabel", "SpeciesCommonName", "DataCategory", "ICES_area")
        )



        # eco_filter <- reactive({
        #   stock_list_long <- separate_ecoregions(stock_list_all)
        #   req(input$selected_locations)
        #   stock_list_long %>%
        #     filter(str_detect(stock_list_long$EcoRegion, input$selected_locations))
        # })
        # res_mod <- reactive({})
        # observe({
        #   res_mod <<- callModule(
        #     module = selectizeGroupServer,
        #     id = "my-filters",
        #     data = eco_filter(),
        #     vars = c("EcoRegion", "StockKeyLabel", "SpeciesCommonName", "DataCategory", "ICES_area")
        #   )
        # })




        ######## this part in not working, or running, the selected_1$groups has the string of char I want to use to filter the table
        output$tbl_summary <- DT::renderDT(res_mod()
          # print(req(selected_1$groups))
          # req(selected_1$groups, selected_2$groups)
          # stock_list_all_filter <- stock_list_all %>% filter(str_detect(EcoRegion, selected_1$groups) & str_detect(ICES_area, selected_2$groups))
          # res_mod <- callModule(
          # module = selectizeGroupServer,
          # id = "my-filters",
          # data = stock_list_all_filter,
          # vars = c("StockKeyLabel", "SpeciesCommonName", "DataCategory"))
          # res_mod()
        
        
          
        )
    }
)




# stock_list_long <- separate_ecoregions(stock_list_all)
# loc <- c("Bay of Biscay and the Iberian Coast", "Celtic")
# temp <- data.frame()
# for(i in 1:length(loc)) {
#     temp_1 <- stock_list_long %>% filter(str_detect(EcoRegion, loc[i]))
#     temp <- rbind(temp,temp_1)
# }

# tibble(stock_list_long %>% filter(str_detect(EcoRegion, loc)))
# df_new <- mydf_long %>% filter(str_detect(EcoRegion, array))
# # df_new <- mydf_long[mydf_long$EcoRegion %in% array,]
# tibble(df_new)
