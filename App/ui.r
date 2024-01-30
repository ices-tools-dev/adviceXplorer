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
library(zip)
library(datamods)




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
    href = "https://ices-taf.shinyapps.io/advicexplorer/",
        tags$img(
            src = "https://www.ices.dk/SiteCollectionImages/ICES%20logos/NEGATIVE%20ICES-logo.png",
            style = "margin-top: -10px; padding-right:10px;padding-bottom:10px",
            height = "50px"
        )
)

options(spinner.type = 5, 
        spinner.color = "#f15d22",
        spinner.size = 0.7)

        
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
    
    position = "static-top",
    collapsible = TRUE,
    # tab title
    windowTitle = "adviceXplorer",
    id = "tabset",
    fluid = TRUE,
    # navbar title
    title = title_html,
    tabPanel(
        "Stock selection",
        style = "max-height: 90vh; overflow-y: auto; overflow-x: hidden; !important;", 
        sidebarLayout(
            sidebarPanel = stock_selection_left_side(),
            mainPanel = stock_selection_right_side()
            
        )
    ),

########################################## New version of SAG plots ############################
    tabPanel(
            "Development over time",
            style = "max-height: 90vh; overflow-y: auto; overflow-x: hidden; !important;", 
            splitLayout(
                cellWidths = c("40%", "60%"),
                header_left_panel_stock_info("stock_infos1"),
                header_right_panel_headline("Advice_Headline1")
            ),
            sidebarPanel(
             width = 12,
            SAG_plots_1_2_fluid(),
            br(),
            SAG_plots_3_4_fluid()
            )
            ),

    tabPanel(
            "Quality of assessment",
            style = "overflow-y: auto; overflow-x: hidden;", 
            splitLayout(
                cellWidths = c("40%", "60%"),
                header_left_panel_stock_info("stock_infos2"),
                header_right_panel_headline("Advice_Headline2")
            ),            
            quality_of_assessment_fluid()
        ),

######################################################################################################

    tabPanel(
        "Catch scenarios",
        style = " max-height: 90vh; overflow-y: auto; overflow-x: hidden; !important;", 
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




