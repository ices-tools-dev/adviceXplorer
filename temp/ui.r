
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

# sources
#source("../Shiny/utilities_load_shapefiles.r")

# Download europe shape file
ecoregion = "Celtic Seas Ecoregion"
    eu <- area_definition(ecoregion)
    eu_shape <- eu$europe_shape

# Load the lighter version of the ecoregions shapefile
shape_eco <- st_read(dsn = "test_lowres", 
    layer = "ecoR_lowres")
# Change one Ecoregion name (this comes handy when we filter the stock list table)
levels(shape_eco$Ecoregion)[match("Icelandic Waters",levels(shape_eco$Ecoregion))] <- "Iceland Sea"

# Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)
shape_eco$uid <- paste0("P", 1:17)


source("../Shiny/utilities_shiny_formatting.r")
source("../Shiny/utilities_plotting.r")
source("../Shiny/utilities_sag_data.r")
source("../Shiny/utilities_shiny_Input.r")



    navbarPage(
        # tab title
        windowTitle = "TAF Advice Tool",

        # navbar title
        title =
            shiny::div(img(
                src = "ICES_logo_orange.PNG",
                style = "margin-top: -14px; padding-right:10px;padding-bottom:10px",
                height = 60
            )),
        # tabsetPanel(
        tabPanel(
            "Stock Selection",
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
            "Stock development over time",
            sidebarLayout(
                sidebarPanel = allocations_infopanel,
                mainPanel = allocations_plotspanel
            )
            # includeMarkdown("Instructions.Rmd")
        ),
        # extra tags, css etc
        tags$style(type = "text/css", "li {font-size: 20px;}"),
        tags$style(type = "text/css", "p {font-size: 21px;}"),
        tags$style(type = "text/css", "body {padding-top: 70px;}"),
        tags$head(tags$style(HTML("#go{background-color:#dd4814}"))),
        theme = shinytheme("united"),
        position = "fixed-top",

        tags$script(HTML("var header = $('.navbar > .container-fluid');
    header.append('<div style=\"float:right\"><a href=\"https://https://github.com/ices-tools-dev/online-advice\"><img src=\"GitHub-Mark-32px.png\" alt=\"alt\" style=\"margin-top: -14px; padding-right:5px;padding-top:25px;\"></a></div>');
    console.log(header)"))
    )