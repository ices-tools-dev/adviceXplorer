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

ecoregion = "Celtic Seas Ecoregion"
    eu <- area_definition(ecoregion)
    eu_shape <- eu$europe_shape


shape_eco <- st_read(dsn = "Shiny/test_lowres", 
    layer = "ecoR_lowres")

levels(shape_eco$Ecoregion)[match("Icelandic Waters",levels(shape_eco$Ecoregion))] <- "Iceland Sea"



shape_eco$uid <- paste0("P", 1:17)


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
        #subset <- dplyr::filter(stock_list_all, grepl(key_subset, EcoRegion))
        
        
        #subset <- stock_list_all %>% filter(str_detect(EcoRegion, key_subset))
        #output$tbl <- renderDT(subset, options = list(lengthChange = FALSE))
        if (identical(key_subset, character(0))) {
            output$tbl <- renderDT(stock_list_all, extensions = 'Buttons', 
            options = list(dom = 'Bfrtip', pageLength = 300,#lengthChange = TRUE,
            buttons = c('csv')
            )                         
            )
        } else {
            subset <- stock_list_all %>% filter(str_detect(EcoRegion, key_subset))
            output$tbl <- renderDT(subset, extensions = 'Buttons', 
            options = list(dom = 'Bfrtip', pageLength = 300,#lengthChange = TRUE,
            buttons = c('csv')
            )
            )
        }

    })
    

    ######################### Advice panel
    SAGsummary <- getSAG("cod.27.22-24", 2019,
    data = "summary", combine = TRUE, purpose = "Advice"
    )
    SAGrefpts <- getSAG("cod.27.22-24", 2019,
    data = "refpts", combine = TRUE, purpose = "Advice"
    )

    data_sag <- cbind(SAGsummary, SAGrefpts)
    data_sag <- subset(data_sag, select = -fishstock)


    rv <- reactiveValues(
        catches = data_sag %>% select(Year, catches, landings, discards) %>% na.omit(),
        R = data_sag %>% select(Year, low_recruitment, recruitment, high_recruitment)
            %>% na.omit(),
        f = data_sag %>% select(Year, low_F, F, high_F, FLim, Fpa, FMSY) %>% na.omit(),
        SSB = data_sag %>% select(Year, low_SSB, SSB, high_SSB, Blim, Bpa, MSYBtrigger) %>% na.omit()
    )
    #### Plot 1 Landings and discards
    output$catches <- renderPlotly({
        figure_1_catches(rv$catches, rv$catches$Year, rv$catches$landings, rv$catches$discards)
    })
    #### Plot 2 Recruitment
    output$R <- renderPlotly({
        figure_2_recruitment(rv$R, rv$R$Year, rv$R$recruitment,rv$R$low_recruitment,rv$R$high_recruitment)
    })
    #### Plot 3 fish mortality 
    output$f <- renderPlotly({
        #### third plot
        figure_3_fish_mortality(rv$f, rv$f$Year, rv$f$low_F, rv$f$F, rv$f$high_F, rv$f$FLim, rv$f$Fpa, rv$f$FMSY)
    })
    #### Plot 4 SSB
    output$SSB <- renderPlotly({
        ### forth plot
        figure_4_SSB(rv$SSB, SSB$Year, SSB$low_SSB, SSB$SSB, SSB$high_SSB, SSB$Blim, SSB$Bpa, SSB$MSYBtrigger)
    })
    
    # # click on a marker
    # observe({ 
        
    #     event <- input$map_marker_click
        
    #     message <- paste("widgets sold in", points$name[points$uid == event$id],":", points$widgets[points$uid == event$id]) 
        
    #     output$widgets <- renderText(message)
        
        
     #})
}
shinyApp(ui = ui, server = server)