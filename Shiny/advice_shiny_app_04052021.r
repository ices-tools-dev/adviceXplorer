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

################################
# set working directory
setwd("D:/Profile/Documents/GitHub/online-advice")

# sources
source("Shiny/utilities_load_shapefiles.r")
source("Shiny/utilities_shiny_formatting.r")
source("Shiny/utilities_plotting.r")
source("Shiny/utilities_sag_data.r")
source("Shiny/utilities_shiny_Input.r")
source("Shiny/utilities_SID_data.r")

# ui and server
source("Shiny/ui_05052021.r")
source("Shiny/server_18052021.r")

# source("Shiny/temp/ui.r")
# source("Shiny/temp/sever.r")



### run app
# shinyApp(server = server, ui = ui)

### run app (Colin version)
runApp("temp")
