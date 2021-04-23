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


# sources
source("D:/Profile/Documents/GitHub/VISA_tool/R/explorationVisa/Shiny/utilities_plotting.r")
source("D:/Profile/Documents/GitHub/VISA_tool/R/explorationVisa/Shiny/ui_08042021.r")
source("D:/Profile/Documents/GitHub/VISA_tool/R/explorationVisa/Shiny/server_08042021.r")



### run app
shinyApp(server = server, ui = ui)
