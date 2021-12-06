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

## run the file below if it is the first time you use the app, 
## it will take several minutes to dowload all the DAG data for the past
## 5 years. However, the app will run faster afterwards as a result.
# source("Shiny/update_SAG_data.r")



# ui and server
# source("Shiny/ui_05052021.r")
# source("Shiny/server_18052021.r")

### run app
# shinyApp(server = server, ui = ui)


### runApp function (Colin way of running the app which shows the png images in folder www)
runApp("temp")
