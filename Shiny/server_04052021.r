# Define server logic
server <- function(input, output) {

    ######################### Map panel   
    # Define the palette
    bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
    pal <- colorBin("YlOrRd", domain = shape_eco$Shape_Area, bins = bins)
    
    # Define the interactive labels
    labels <- sprintf(
        "<strong>%s Ecoregion</strong><br/>%g Shape Area ",
        shape_eco$Ecoregion, shape_eco$Shape_Area
    ) %>% lapply(htmltools::HTML)
    
    output$map <- renderLeaflet({

        leaflet() %>% 
            #addProviderTiles("Stamen.Toner") %>% 
            addPolygons(data = shape_eco, 
                color = "#444444", 
                weight = 1,
                smoothFactor = 0.5,
                opacity = 0.7, 
                fillOpacity = 0.5,
                fillColor = ~ pal(shape_eco$Shape_Area),
                layerId = ~uid, # unique id for polygons
                highlightOptions = highlightOptions(
                    color = "white", weight = 2,
                    bringToFront = TRUE
                ),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                )) %>%
            addPolygons(
                data = eu_shape, color = "black", weight = 1,
                smoothFactor = 0.5,
                opacity = 0.7, fillOpacity = 0.5,
                fillColor = "grey") %>%  
             setView(lng = 25.783660, lat = 71.170953, zoom = 3) # nordKap coordinates
    })
    
    # click on polygon
    observe({ 
        
        event <- input$map_shape_click
        
        # message <- paste("Ecoregion name is:", shape_eco$Ecoregion[shape_eco$uid == event$id])
        
        # output$Ecoregion <- renderText(message)

        key_subset <- as.character(shape_eco$Ecoregion[shape_eco$uid == event$id])
        print(key_subset)
        
        #stock_list_all <- read.csv("./Shiny/FilteredStocklist_all.csv")
        stock_list_all <- jsonlite::fromJSON(
            URLencode(
                "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021&$select=StockKey, StockKeyLabel, EcoRegion, SpeciesScientificName,  SpeciesCommonName, ExpertGroup"
            )
        )$value

        if (identical(key_subset, character(0))) {
            output$tbl <- renderDT(stock_list_all, extensions = 'Buttons', 
            options = list(dom = 'Bfrtip', pageLength = 300,#lengthChange = TRUE,
            buttons = c('csv')
            )                         
            )
        } else {
            # Subset based on polygon click
            subset <- stock_list_all %>% filter(str_detect(EcoRegion, key_subset))

            # reactive data frame which creates the number of actionButtons needed
            df <- reactiveVal(
                tibble(
                    subset,
                    Actions = shinyInput(
                        FUN = actionButton,
                        n = nrow(subset),
                        id = "button_",
                        label = "Advice",
                        onclick = 'Shiny.setInputValue(\"select_button\", this.id, {priority: \"event\"})'
                    )
                )
            )
            
            output$tbl <- DT::renderDT({

                df()
                #extensions = 'Buttons', 
                #options = list(dom = 'Bfrtip', pageLength = 300,buttons = c('csv')
            },
            # Don't escape any HTML in the table (i.e. the actionButton)
            escape = FALSE,
            
            # turn off row selection otherwise you'll also select that row when you
            # click on the actionButton 
            selection = 'none'
            )
        }
    
            # When a button is clicked, employee is set to the employee name
            #  associated with the clicked row
    advice_action <- eventReactive(input$select_button, {
    # take the value of input$select_button, e.g. "button_1"
    # get the button number (1) and assign to selectedRow
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    
    # get the value of the "Name" column in the data.frame for that row
    
    stock_name <- as.character(df()[selectedRow, "StockKeyLabel"])
    
    #   # Dowload the data        
    data_sag <- access_sag_data(stock_name, 2020)

    
    catches <- data_sag %>% select(Year, catches, landings, discards)#,#,
    R <- data_sag %>% select(Year, low_recruitment, recruitment, high_recruitment) #%>% na.omit()
    f <- data_sag %>% select(Year, low_F, F, high_F, FLim, Fpa, FMSY)
    SSB <- data_sag %>% select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger) 
    
    list(catches = catches, R = R, f = f, SSB = SSB)
        
    })
    

    ######################### Advice panel

    #### Plot 1 Landings and discards
    output$catches <- renderPlotly({

        data_list = advice_action()
        
         rv <- reactiveValues(
             catches_df = data_list$catches
         )

        figure_1_catches(rv$catches_df, rv$catches_df$Year, rv$catches_df$catches ,rv$catches_df$landings, rv$catches_df$discards)
    })
    #### Plot 2 Recruitment
    output$R <- renderPlotly({

        data_list = advice_action()
         
         rv <- reactiveValues(
             r_df = data_list$R
         )

        figure_2_recruitment(rv$r_df, rv$r_df$Year, rv$r_df$recruitment,rv$r_df$low_recruitment,rv$r_df$high_recruitment)
    })
    #### Plot 3 fish mortality 
    output$f <- renderPlotly({
        data_list = advice_action()
         
         rv <- reactiveValues(
             f_df = data_list$f
         )

        #### third plot
        figure_3_fish_mortality(rv$f_df, rv$f_df$Year, rv$f_df$low_F, rv$f_df$F, rv$f_df$high_F, rv$f_df$FLim, rv$f_df$Fpa, rv$f_df$FMSY)
    })
    #### Plot 4 SSB
    output$SSB <- renderPlotly({

        data_list = advice_action()
         
         rv <- reactiveValues(
             SSB_df = data_list$SSB
         )

        ### forth plot
        figure_4_SSB(rv$SSB_df, rv$SSB_df$Year, rv$SSB_df$low_SSB, rv$SSB_df$SSB, rv$SSB_df$high_SSB, rv$SSB_df$Blim, rv$SSB_df$Bpa, rv$SSB_df$MSYBtrigger)
    })
    

})

}