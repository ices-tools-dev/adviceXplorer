# Define server logic
server <- function(input, output, session) {

    cat(getwd())

    ######################### Map panel   
    # Define the palette
    # bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
    # pal <- colorBin("YlOrRd", domain = shape_eco$Shape_Area, bins = bins)
    sf_cent <- st_coordinates(st_centroid(shape_eco))
    sf_cent_map_X <- mean(sf_cent[, 1])
    sf_cent_map_Y <- mean(sf_cent[, 2])
    sf_cent_map <- c(sf_cent_map_X, sf_cent_map_Y)
    # Define the interactive labels
    labels <- sprintf(
        "<strong>%s Ecoregion</strong><br/>%g Shape Area ",
        shape_eco$Ecoregion, shape_eco$Shape_Area
    ) %>% lapply(htmltools::HTML)
    
    # output$map <- renderLeaflet({
    #     # map_ecoregion(shape_eco, eu_shape)

    #     # leaflet(options = leafletOptions(crs = crs_laea, minZoom = minZoom, maxZoom = maxZoom)) %>% 
    #     #     #addProviderTiles("Stamen.Toner") %>% 
    #     #     addPolygons(data = shape_eco, 
    #     #         color = "#444444", 
    #     #         weight = 1,
    #     #         smoothFactor = 0.5,
    #     #         opacity = 0.7, 
    #     #         fillOpacity = 0.5,
    #     #         fillColor = ~ pal(shape_eco$Shape_Area),
    #     #         layerId = ~uid, # unique id for polygons
    #     #         highlightOptions = highlightOptions(
    #     #             color = "white", weight = 2,
    #     #             bringToFront = TRUE
    #     #         ),
    #     #         label = labels,
    #     #         labelOptions = labelOptions(
    #     #             style = list("font-weight" = "normal", padding = "3px 8px"),
    #     #             textsize = "15px",
    #     #             direction = "auto"
    #     #         )) %>%
    #     #     addPolygons(
    #     #         data = eu_shape, color = "black", weight = 1,
    #     #         smoothFactor = 0.5,
    #     #         opacity = 0.7, fillOpacity = 0.5,
    #     #         fillColor = "grey") %>%  
    #     #         setView(lng = -1.235660, lat = 60.346958, zoom = 0.5)
    #         #  setView(lng = 25.783660, lat = 71.170953, zoom = 3.2) # nordKap coordinates
    # })

    output$map1 <- renderLeaflet({
        map_ecoregion(shape_eco, eu_shape)
            # leaflet(options = leafletOptions(crs = crs_laea, minZoom = minZoom, maxZoom = maxZoom)) %>%
            #     # addTiles() %>%
            #     addPolygons(
            #         data = shape_eco,
            #         fillColor = "white",
            #         fillOpacity = 0.5,
            #         color = "black",
            #         stroke = TRUE,
            #         weight = 1,
            #         layerId = ~Ecoregion,
            #         group = "Eco_regions",
            #         label = ~Ecoregion
            #     ) %>%
            #     addPolygons(
            #         data = shape_eco,
            #         fillColor = "red",
            #         fillOpacity = 0.5,
            #         weight = 1,
            #         color = "black",
            #         stroke = TRUE,
            #         layerId = ~OBJECTID,
            #         group = ~Ecoregion
            #     ) %>%
            #     hideGroup(group = shape_eco$Ecoregion) # nc$CNTY_ID
        }) # END RENDER LEAFLET

        # map output Areas
        output$map2 <- renderLeaflet({
            map_ices_areas(ices_areas, eu_shape)
            # leaflet(options = leafletOptions(crs = crs_laea, minZoom = minZoom, maxZoom = maxZoom)) %>%
            #     # addTiles() %>%
            #     addPolygons(
            #         data = ices_areas,
            #         fillColor = "white",
            #         fillOpacity = 0.5,
            #         color = "black",
            #         stroke = TRUE,
            #         weight = 1,
            #         layerId = ~Area_Full,
            #         group = "ices_areas",
            #         label = ~Area_Full
            #     ) %>%
            #     addPolygons(
            #         data = ices_areas,
            #         fillColor = "red",
            #         fillOpacity = 0.5,
            #         weight = 1,
            #         color = "black",
            #         stroke = TRUE,
            #         layerId = ~OBJECTID,
            #         group = ~Area_Full
            #     ) %>%
            #     hideGroup(group = ices_areas$Area_Full) # nc$CNTY_ID
        }) # END RENDER LEAFLET
    ###############################################################END of MAPS

    ################################################################# new interactive filtering first part

    ############################## Interactive section Ecoregions ######################
        # define leaflet proxy for Ecoregion map
        proxy_1 <- leafletProxy("map1")

        # create empty vector to hold all click ids
        selected_1 <- reactiveValues(groups = vector())
        
        # find index

observe({
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
    ########################################################### end reactive part
    
    ########################################################### tranform the sid dataframe
    stock_list_long <- separate_ecoregions(stock_list_all)
    # print(tibble(stock_list_long))

    ###########################################################  function to use the input from the maps and the sid filtering

    eco_filter <- reactive({
          req(input$selected_locations)
          print(input$selected_locations)

          temp_df <- data.frame()
          for (i in 1:length(input$selected_locations)) {
              temp_1 <- stock_list_long %>% filter(str_detect(EcoRegion, input$selected_locations[i]))
              temp_df <- rbind(temp_df, temp_1)
          }
          print(tibble(temp_df))
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
            vars = c(
                "StockDatabaseID", "StockKey", "StockKeyLabel", "SpeciesScientificName", "SpeciesCommonName",
                "ExpertGroup", "AdviceDraftingGroup", "DataCategory", "YearOfLastAssessment", "AssessmentFrequency",
                "YearOfNextAssessment", "AdviceReleaseDate", "AdviceCategory", "AdviceType", "TrophicGuild",
                "FisheriesGuild", "SizeGuild", "Published"
            ) # , "ICES_area")
        )



    output$tbl <- DT::renderDT(res_mod(),
        extensions = "Buttons",
        selection = "single",
        caption = "Select the row for the fish stock of interest and then click on the 'Stock development over time' panel",
        options = list(
            dom = "Bfrtip",
            pageLength = 300,
            buttons = c("csv"),
            columnDefs = list(
                list(
                    targets = 5,
                    render = JS(
                        "function(data, type, row, meta) {",
                        "return type === 'display' && data.length > 15 ?",
                        "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                        "}"
                    )
                )
            )
        )
    )
    
   



    # output$headline <- renderPrint({
    # h3(paste0("You clicked value ", input$tbl_cell_clicked$value," and ", input$tbl_cell_clicked$row)) #$value
    # # print(input$tbl_cell_clicked$value)
    # })
    





#     ############################################################this part is the old filtering method 30082021
#     # click on polygon
    #  observeEvent(input$tbl_cell_clicked$value, { 
         
        #  observe({
        
#         event <- input$map1_shape_click
#         print(input$map1_shape_click)
#         # message <- paste("Ecoregion name is:", shape_eco$Ecoregion[shape_eco$uid == event$id])
        
#         # output$Ecoregion <- renderText(message)

#         key_subset <- as.character(shape_eco$Ecoregion[shape_eco$uid == event$id])
#         print(key_subset)
        
#         #stock_list_all <- read.csv("./Shiny/FilteredStocklist_all.csv")
#         stock_list_all <- jsonlite::fromJSON(
#             URLencode(
#                 "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021&$select=StockKey, StockKeyLabel, EcoRegion, SpeciesScientificName,  SpeciesCommonName, DataCategory, ExpertGroup"
#             )
#         )$value
#         #### I'm adding this next line just to check what happens if I subset for only cat1 stocks
#         stock_list_all <- stock_list_all  %>% filter(DataCategory == "1")
        
#         ### if no polygon is clicked, just show all stocks in the table
#         if (identical(key_subset, character(0))) {
#             output$tbl <- renderDT(stock_list_all, 
#             extensions = 'Buttons', 
#             options = list(dom = 'Bfrtip', pageLength = 300,#lengthChange = TRUE,
#             buttons = c('csv')
#             )                         
#             )
#         } else {
#             # Subset based on polygon click
#             subset <- stock_list_all %>% filter(str_detect(EcoRegion, key_subset))

#             # reactive data frame which creates the number of actionButtons needed
#             df <- reactiveVal(
#                 tibble(
#                     subset,
#                     Actions = shinyInput(
#                         FUN = actionButton,
#                         n = nrow(subset),
#                         id = "button_",
#                         label = "Show Advice",
#                         onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
#                     )
#                 )
#             )
            
#             output$tbl <- DT::renderDT({

#                 df()
#                 #extensions = 'Buttons', 
#                 #options = list(dom = 'Bfrtip', pageLength = 300,buttons = c('csv')
#             },
#             # Don't escape any HTML in the table (i.e. the actionButton)
#             escape = FALSE,
            
#             # turn off row selection otherwise you'll also select that row when you
#             # click on the actionButton 
#             selection = 'none',

#             # add buttons to download csv
#             extensions = 'Buttons', 
#             options = list(dom = 'Bfrtip', pageLength = 300,buttons = c('csv'))
#             )
#         }
    
#             # When a button is clicked, employee is set to the employee name
#             #  associated with the clicked row
    advice_action <- eventReactive(input$tbl_rows_selected, {
#     # take the value of input$select_button, e.g. "button_1"
#     # get the button number (1) and assign to selectedRow
#     selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
#     # get the value of the "Name" column in the data.frame for that row
    
#     stock_name <- as.character(df()[selectedRow, "StockKeyLabel"])
    filtered_row <- res_mod()[input$tbl_rows_selected,]
    print(filtered_row$StockKeyLabel)
    stock_name <- filtered_row$StockKeyLabel    
    
    #   # Dowload the data        
    # data_sag <- access_sag_data(stock_name, 2020)
    data_sag <- access_sag_data(stock_name, 2020)

    
    catches <- data_sag %>% select(Year, catches, landings, discards)#,#,
    R <- data_sag %>% select(Year, low_recruitment, recruitment, high_recruitment) #%>% na.omit()
    f <- data_sag %>% select(Year, low_F, F, high_F, FLim, Fpa, FMSY)
    SSB <- data_sag %>% select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger) 
    list_df <- quality_assessment_data(stock_name)
    #the bit below could be potentially be replaced by the sag status? summary table option?
    SAG_summary <- data_sag %>% select(Year, 
                    recruitment, high_recruitment, low_recruitment, 
                    SSB, high_SSB, low_SSB,
                    catches, landings,
                    F, high_F, low_F)
    
#     big_data <- list_df[[1]]
# big_data_last_year <- list_df[[2]]

    list(catches = catches, R = R, f = f, SSB = SSB, big_data = list_df[[1]], big_data_last_year = list_df[[2]], SAG_summary = SAG_summary)
        
    })
    

    ######################### Advice panel

    output$all_plots <- renderPlotly({
        data_list = advice_action()

        rv <- reactiveValues(
             catches_df = data_list$catches,
             r_df = data_list$R,
             f_df = data_list$f,
             SSB_df = data_list$SSB
        )
        figure_1_plots(rv$catches_df,rv$r_df,rv$f_df,rv$SSB_df,rv$catches_df$Year, rv$catches_df$catches ,rv$catches_df$landings, rv$catches_df$discards,
        rv$r_df$recruitment,rv$r_df$low_recruitment,rv$r_df$high_recruitment,rv$f_df$low_F, rv$f_df$F, rv$f_df$high_F, rv$f_df$FLim, rv$f_df$Fpa, rv$f_df$FMSY,
        rv$SSB_df$low_SSB, rv$SSB_df$SSB, rv$SSB_df$high_SSB, rv$SSB_df$Blim, rv$SSB_df$Bpa, rv$SSB_df$MSYBtrigger)


    })

    # #### Plot 1 Landings and discards
    # output$catches <- renderPlotly({

    #     data_list = advice_action()
        
    #      rv <- reactiveValues(
    #          catches_df = data_list$catches
    #      )

    #     figure_1_catches(rv$catches_df, rv$catches_df$Year, rv$catches_df$catches ,rv$catches_df$landings, rv$catches_df$discards)
    # })
    # #### Plot 2 Recruitment
    # output$R <- renderPlotly({

    #     data_list = advice_action()
         
    #      rv <- reactiveValues(
    #          r_df = data_list$R
    #      )

    #     figure_2_recruitment(rv$r_df, rv$r_df$Year, rv$r_df$recruitment,rv$r_df$low_recruitment,rv$r_df$high_recruitment)
    # })
    # #### Plot 3 fish mortality 
    # output$f <- renderPlotly({
    #     data_list = advice_action()
         
    #      rv <- reactiveValues(
    #          f_df = data_list$f
    #      )

    #     #### third plot
    #     figure_3_fish_mortality(rv$f_df, rv$f_df$Year, rv$f_df$low_F, rv$f_df$F, rv$f_df$high_F, rv$f_df$FLim, rv$f_df$Fpa, rv$f_df$FMSY)
    # })
    # #### Plot 4 SSB
    # output$SSB <- renderPlotly({

    #     data_list = advice_action()
         
    #      rv <- reactiveValues(
    #          SSB_df = data_list$SSB
    #      )

    #     ### forth plot
    #     figure_4_SSB(rv$SSB_df, rv$SSB_df$Year, rv$SSB_df$low_SSB, rv$SSB_df$SSB, rv$SSB_df$high_SSB, rv$SSB_df$Blim, rv$SSB_df$Bpa, rv$SSB_df$MSYBtrigger)
    # })
    #### Plot 5 quality of assessment
    output$Q_Ass <- renderPlotly({

        data_list = advice_action()
         
         rv <- reactiveValues(
             Q_Ass_df1 = data_list$big_data,
             Q_Ass_df2 = data_list$big_data_last_year
         )

        ### forth plot
        quality_assessment_plots(rv$Q_Ass_df1, rv$Q_Ass_df2)
        # figure_4_SSB(rv$SSB_df, rv$SSB_df$Year, rv$SSB_df$low_SSB, rv$SSB_df$SSB, rv$SSB_df$high_SSB, rv$SSB_df$Blim, rv$SSB_df$Bpa, rv$SSB_df$MSYBtrigger)
    })
    
    # output$tbl_summary <- DT::renderDT({
    #     data_list = advice_action()
    #     rv <- reactiveValues(
    #          data_SAG = data_list$SAG_summary
    #      )
    #      rv$data_SAG
    #      },
    #      extensions = 'Buttons', 
    #         options = list(dom = 'Bfrtip', pageLength = 10,#lengthChange = TRUE,
    #         buttons = c('csv')
    #         )

    # )
})
output$In_Construction <- renderText({ "In Construction" })
}