############# Libraries ############
library(data.table)
library(dplyr)
library(dygraphs)
library(DT)
library(fisheryO)
library(htmltools)
library(htmlwidgets)
library(ggplot2)
library(ggradar)
library(ggtext)
library(glue)
library(gsubfn)
library(icesFO)
library(icesSAG)
library(icesTAF)
library(icesVocab)
library(leaflet)
library(plotly)
library(reshape2)
library(rintrojs)
library(RColorBrewer)
library(RCurl)
library(rvest)
library(scales)
library(sf)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(tidyverse)
library(tm)
library(widgetframe)
library(icesASD)





########## Load utilities ############
source("utilities_help.r")
source("utilities_SID_data.r")
source("utilities_load_shapefiles.r")
source("utilities_plotting.r")
source("utilities_mapping.r")
source("utilities_sag_data.r")
source("utilities_catch_scenarios.r")
source("utilities_shiny_formatting.r")
source("utilities_calendar.r")
source("utilities_resources.r")


title_html <- tags$a(
    href = "https://ices-taf.shinyapps.io/online-single-stock-advice/",
        tags$img(
            src = "https://www.ices.dk/SiteCollectionImages/ICES%20logos/NEGATIVE%20ICES-logo.png",
            style = "margin-top: -10px; padding-right:10px;padding-bottom:10px",
            height = "50px"
        )
)
tagList(
    useShinyjs(),
    introjsUI(),
    tags$script(src = "https://kit.fontawesome.com/ac71e9cf8e.js"),
    tags$head(includeHTML(("google-analytics.html"))), 
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$script(                                                                        #####we can modify this to have the tabs inactive until a stock is chosen
    '
    var tab = $(\'a[data-value="Stock Selection"]\').parent().addClass("disabled");
    $(function(){
      $(tab.parent()).on("click", "li.disabled", function(e) {
        e.preventDefault();
        return false;
      });
    });
    '
  ),
    



navbarPage(
    
    position = "fixed-top",
    collapsible = TRUE,
    # tab title
    windowTitle = "Online Advice",
    id = "tabset",
    fluid = TRUE,
    # navbar title
    title = title_html,
    tabPanel(
        "Stock selection",
        style = "max-height: 100vh; overflow-y: auto; overflow-x: auto; !important;", 
        sidebarLayout(
            sidebarPanel = stock_selection_left_side(),
            mainPanel = stock_selection_right_side()
            
        )
    ),

########################################## New version of SAG plots ############################
    tabPanel(
            "Development over time",
            style = " max-height: 100vh; overflow-y: auto; overflow-x: auto; !important;", 
            splitLayout(
                cellWidths = c("40%", "60%"),
                header_left_panel_stock_info("stock_infos1"),
                header_right_panel_headline("Advice_Headline1")
            ),
            
            # sidebarLayout(
            #     sidebarPanel = SAG_plots_left_panel(),
            #     mainPanel = SAG_plots_right_panel()

            # )
            fluidRow(
                  
                column(6,withSpinner(plotlyOutput("plot1", height = "100%", width = "100%"))),   
                column(6,withSpinner(plotlyOutput("plot2", height = "100%", width = "100%")))
            )
            ,
            fluidRow(
                column(6,withSpinner(plotlyOutput("plot3", height = "100%", width = "100%"))),
                column(6,withSpinner(plotlyOutput("plot4", height = "100%", width = "100%")))
                # )
                # )
                )
         
        ),
    tabPanel(
            "Quality of assessment",
            style = " max-height: 100vh; overflow-y: auto; overflow-x: auto; !important;", 
            splitLayout(
                cellWidths = c("40%", "60%"),
                header_left_panel_stock_info("stock_infos2"),
                header_right_panel_headline("Advice_Headline2")
            ),
            fluidRow(
                  
                column(4,withSpinner(plotlyOutput("plot5", height = "100%", width = "100%"))),   
                column(4,withSpinner(plotlyOutput("plot6", height = "100%", width = "100%"))),
                column(4,withSpinner(plotlyOutput("plot7", height = "100%", width = "100%")))
            )
            # quality_of_assessment()
        ),

######################################################################################################

    tabPanel(
        "Catch scenarios",
        style = " max-height: 100vh; overflow-y: auto; overflow-x: auto; !important;", 
        splitLayout(
            cellWidths = c("40%", "60%"),
            header_left_panel_stock_info("stock_infos3"),
            header_right_panel_headline("Advice_Headline3")
        ),
        sidebarLayout(
            sidebarPanel = catch_scenarios_left_panel(),
            mainPanel = catch_scenarios_right_panel()
        )
        
    ),
    navbarMenu(
            "Resources",
            tabPanel(
                "Contact & feedback",
                htmlOutput("contact_feedback")
            ),
            tabPanel(
                "Data sources",
                htmlOutput("data_sources")
            ),
            tabPanel(
                "Data disclaimer & policy",
                htmlOutput("data_disclaimer_policy")
            ),
            tabPanel(
                "Citation",
                htmlOutput("citation")
            )
        )
)   
)




