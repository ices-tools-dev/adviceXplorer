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



################################
# set working directory
setwd("D:/Profile/Documents/GitHub/online-advice")

# sources
source("Shiny/utilities_plotting.r")
source("Shiny/ui_08042021.r")
source("Shiny/server_26042021.r")



### run app
shinyApp(server = server, ui = ui)
