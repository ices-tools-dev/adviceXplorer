# Libraries

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
library(icesVocab)
library(tm)
library(shinyWidgets)
library(shinyjs)
library(reshape2)
library(scales)
library(ggradar)
library(icesFO)
library(icesTAF)

# required if using most recent version of sf
sf::sf_use_s2(FALSE)

################################
# sources
source("Shiny/utilities_load_shapefiles.r")
source("Shiny/utilities_shiny_formatting.r")
source("Shiny/utilities_plotting.r")
source("Shiny/utilities_mapping.r")
source("Shiny/utilities_sag_data.r")
source("Shiny/utilities_shiny_Input.r")
source("Shiny/utilities_SID_data.r")
source("Shiny/utilities_catch_scenarios.r")

## If this code is run for the first time and the SAG data in not present on the local machine
## the following line will download the last 5 years of SAG data (summary and ref points).
## This process will take several minutes but, once the data is in the local folder, 
## the app will run much faster. 
if (!file.exists("SAG_ 2021/SAG_summary.csv")) {
    source("Shiny/update_SAG_data.r")
}





# ui and server
# source("Shiny/ui_05052021.r")
# source("Shiny/server_18052021.r")

### run app
# shinyApp(server = server, ui = ui)


### runApp function (Colin way of running the app which shows the png images in folder www)
runApp("temp")
