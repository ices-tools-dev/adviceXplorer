######################Toy example with Ecoregions

library(htmlwidgets)
library(dplyr)
library(ggplot2)
library(dygraphs)
library(htmltools)
library(widgetframe)
library(icesSAG)
library(plotly)

library(shiny)
library(shinythemes)
library(glue)


library(sf)
library(leaflet)
library(fisheryO)
library(DT)
library(tidyverse)

source("Shiny/utilities_shiny_formatting.r")
source("Shiny/utilities_plotting.r")
source("Shiny/utilities_sag_data.r")

ecoregion = "Celtic Seas Ecoregion"
    eu <- area_definition(ecoregion)
    eu_shape <- eu$europe_shape


shape_eco <- st_read(dsn = "Shiny/test_lowres", 
    layer = "ecoR_lowres")

levels(shape_eco$Ecoregion)[match("Icelandic Waters",levels(shape_eco$Ecoregion))] <- "Iceland Sea"



shape_eco$uid <- paste0("P", 1:17)


#' Programmatically create a Shiny input
#' 
#' @param FUN function to create the input
#' @param n number of inputs to be created
#' @param id ID prefix for each input
shinyInput <- function(FUN, n, id, ...) {

  # for each of n, create a new input using the FUN function and convert
  # to a character
  vapply(seq_len(n), function(i){
    as.character(FUN(paste0(id, i), ...))
  }, character(1))
  
}


#side_width <- 5

# Define UI 
#ui <- fluidPage(
    
    # Application title
    #titlePanel("Ecoregion Selection"),
ui <-
    navbarPage(
        # tab title
        windowTitle = "TAF Advice Tool",

        # navbar title
        title =
            shiny::div(img(
                src = "Shiny/www/ICES_logo_orange.PNG",
                style = "margin-top: -14px; padding-right:10px;padding-bottom:10px",
                height = 60
            )),
        # tabsetPanel(
        tabPanel(
            "Map",
            sidebarLayout(
                # Top panel with widgets sold
                # wellPanel(
                #     textOutput("Ecoregion")
                # ),

                # the map itself
                sidebarPanel(
                    div(
                        class = "outer",
                        tags$style(type = "text/css", ".outer {position: fixed; top: 61px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                        # width = side_width,
                        leafletOutput("map", width = "35%", height = "100%")
                    )
                ),
                mainPanel(
                    width = 8,
                    # div(class="outer",
                    # tags$style(type = "text/css", ".outer {position: fixed; top: 61px; left: 500px; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                    DTOutput("tbl")
                )
            )
            # )
        ),
        tabPanel(
            "Advice",
            sidebarLayout(
                sidebarPanel = allocations_infopanel,
                mainPanel = allocations_plotspanel
            )
            # includeMarkdown("Instructions.Rmd")
        ),
        # extra tags, css etc
        tags$style(type = "text/css", "li {font-size: 17px;}"),
        tags$style(type = "text/css", "p {font-size: 18px;}"),
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        tags$head(tags$style(HTML("#go{background-color:#dd4814}"))),
        theme = shinytheme("united"),
        position = "fixed-top",

        tags$script(HTML("var header = $('.navbar > .container-fluid');
    header.append('<div style=\"float:right\"><a href=\"https://github.com/ices-taf/2020_bss.27.4bc7ad-h_catchAllocationTool\"><img src=\"GitHub-Mark-32px.png\" alt=\"alt\" style=\"margin-top: -14px; padding-right:5px;padding-top:25px;\"></a></div>');
    console.log(header)"))
    )

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


    # # click on a marker
    # observe({ 
        
    #     event <- input$map_marker_click
        
    #     message <- paste("widgets sold in", points$name[points$uid == event$id],":", points$widgets[points$uid == event$id]) 
        
    #     output$widgets <- renderText(message)
        
        
     #})
}
shinyApp(ui = ui, server = server)
