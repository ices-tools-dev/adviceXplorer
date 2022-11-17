############# Libraries ############
library(glue)
library(sf)
library(leaflet)
library(fisheryO)
library(DT)
library(tidyverse)
library(icesVocab)
library(tm)
library(shinyWidgets)
library(shinyjs)
library(reshape2)
library(icesFO)
library(icesTAF)
library(rvest)
library(gsubfn)
library(stringr)
library(htmlwidgets)
library(dplyr)
library(ggplot2)
library(dygraphs)
library(htmltools)
library(widgetframe)
library(icesSAG)
library(plotly)
library(shinythemes)
library(shinyalert)
library(data.table)
library(RColorBrewer)
library(shinycssloaders)
library(tidyr)
library(rintrojs)
library(scales)
library(ggradar)
library(shinyBS)
library(ggtext)
library(RCurl)




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
            # sidebarPanel = maps_panels(),
            # mainPanel = selectize_panel()
            # sidebarPanel = 
            sidebarPanel = stock_selection_left_side(),
            mainPanel = stock_selection_right_side()
            
        )
    ),
    
    # tabPanel(
    #     "Stock Selection", style = "max-height: 90vh; overflow-y: auto; margin: auto;",
    #     tipify(
    #         actionButton(inputId = "help_tab2", label = NULL, style = "position: sticky; top: 0%; right:15%; width: 40px; height: 40px; background: url('info.png');  background-size: cover; background-position: center; border: 1px solid transparent;"),
    #         title = "Click here for help", placement = "bottom", trigger = "hover"),
    #     DTOutput("tbl")
    # ),

########################################## New version of SAG plots ############################
    navbarMenu(
            "Stock assessment trends",
            tabPanel(
                "Development over time",
                splitLayout(
            # style = "border: 1px solid silver; height: 15vh; overflow-y: auto;",
                    cellWidths = c("40%", "60%"),
                    sag_plots_stock_info_left_panel(),
                    sag_plots_stock_info_center_panel()
                    # sag_plots_stock_info_right_panel()
                ),
                # tipify(
                # actionButton(inputId = "help_tab3", label = NULL, style = "top: 1%; left:7%; width: 40px; height: 40px; background: url('info.png');  background-size: cover; background-position: center; border: 1px solid transparent;"), 
                # title = "Click here fof help", placement = "right", trigger = "hover"),
                
                # tipify(
                # myDownloadButton("download_SAG_Data"),
                # title = "Download the plot data", placement = "right", trigger = "hover"),


                # withSpinner(htmlOutput("stock_infos", height = "10%", width = "100%")),
                
                sidebarLayout(
                    sidebarPanel = SAG_plots_left_panel(),
                    mainPanel = SAG_plots_righ_panel()
            )
             
            ),
            tabPanel(
                "Quality of assessment",
                splitLayout(
            # style = "border: 1px solid silver; height: 15vh; overflow-y: auto;",
                    cellWidths = c("40%", "60%"),
                    qualAssess_plots_stock_info_left_panel(),
                    qualAssess_plots_stock_info_center_panel()
                    # qualAssess_plots_stock_info_right_panel()
                ),
                # tipify(
                # actionButton(inputId = "help_tab4", label = NULL, style = "width: 40px; height: 40px; background: url('info.png');  background-size: cover; background-position: center; border: 1px solid transparent;"),
                # title = "Click here fof help", placement = "right", trigger = "hover"),

                # tipify(
                # myDownloadButton("download_SAG_Quality_Data"),
                # title = "Download the plot data", placement = "right", trigger = "hover"),

                # withSpinner(htmlOutput("stock_infos2", height = "10%", width = "100%")),
                quality_of_assessment()
            )
        ),

######################################################################################################

    tabPanel(
        "Catch Scenarios",
        splitLayout(
            # style = "border: 1px solid silver; height: 15vh; overflow-y: auto;",
            cellWidths = c("40%", "60%"),
            catch_scenario_stock_info_left_panel(),
            catch_scenario_stock_info_center_panel()
            # catch_scenario_stock_info_right_panel()
        ),
        
        # withSpinner(htmlOutput("Advice_Summary", height = "10%", width = "100%")),

        sidebarLayout(
            sidebarPanel = catch_scenarios_left_panel(),
            mainPanel = catch_scenarios_right_panel()
        )
        
    ),
    tabPanel(
        "Resources", style = "max-height: 90vh; overflow-y: auto;",
        htmlOutput("citation")
        
    ),
    # extra tags, css etc
    
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$script(
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
    
    theme = shinytheme("cerulean"),  ##### need to work on this, the orange is part of the css theme united, check bslib in forked repo
    position = "fixed-top",
)   
)


