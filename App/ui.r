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





########## Load utilities ############
source("utilities_help.r")
source("utilities_SID_data.r")
source("utilities_load_shapefiles.r")
source("utilities_plotting.r")
source("utilities_mapping.r")
source("utilities_sag_data.r")
source("utilities_shiny_Input.r")
source("utilities_catch_scenarios.r")
source("utilities_shiny_formatting.r")
source("utilities_calendar.r")
source("utilities_resources.r")




title_html <- tags$a(
    href = "https://ices-taf.shinyapps.io/online-single-stock-advice/",
    target = "_blank",
        tags$img(
            src = "https://www.ices.dk/SiteCollectionImages/ICES%20logos/NEGATIVE%20ICES-logo.png",
            style = "margin-top: -10px; padding-right:10px;padding-bottom:10px",
            height = "50px"
        )
)
tagList(
    useShinyjs(),
    introjsUI(),    
    tags$head(tags$script(type="text/javascript", src = "code.js")),



navbarPage(
    
    # tab title
    windowTitle = "Online Advice",
    id = "tabset",
    fluid = TRUE,
    # navbar title
    title = title_html,
    tabPanel(
        "Stock Selection",
        sidebarLayout(
            sidebarPanel = stock_selection_left_side(),
            mainPanel = stock_selection_right_side()
            
        )
    ),

########################################## New version of SAG plots ############################
    navbarMenu(
            "Stock assessment trends",
            tabPanel(
                "Development over time",
                splitLayout(
                    cellWidths = c("40%", "60%"),
                    sag_plots_stock_info_left_panel(),
                    sag_plots_stock_info_center_panel()
                ),
                
                sidebarLayout(
                    sidebarPanel = SAG_plots_left_panel(),
                    mainPanel = SAG_plots_right_panel()
                )
             
            ),
            tabPanel(
                "Quality of assessment",
                splitLayout(
                    cellWidths = c("40%", "60%"),
                    qualAssess_plots_stock_info_left_panel(),
                    qualAssess_plots_stock_info_center_panel()
                ),
                quality_of_assessment()
            )
        ),

######################################################################################################

    tabPanel(
        "Catch Scenarios",
        splitLayout(
            cellWidths = c("40%", "60%"),
            catch_scenario_stock_info_left_panel(),
            catch_scenario_stock_info_center_panel()
        ),
        sidebarLayout(
            sidebarPanel = catch_scenarios_left_panel(),
            mainPanel = catch_scenarios_right_panel()
        )
        
    ),
    navbarMenu(
            "Resources",
            # style = "max-height: 90vh; overflow-y: auto;",
            tabPanel(
                "Contact & Feedback",
                htmlOutput("contact_feedback")
            ),
            tabPanel(
                "Data sources",
                htmlOutput("data_sources")
            ),
            tabPanel(
                "Data Disclaimer & Policy",
                htmlOutput("data_disclaimer_policy")
            ),
            tabPanel(
                "Citation",
                htmlOutput("citation")
            )
        ),
    
    ###### extra tags, css, JS etc
    
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
    
    theme = shinytheme("cerulean"), 
    position = "fixed-top",
)   
)


