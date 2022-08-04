
library(icesTAF)
library(icesSAG)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(icesFO)
library(stringr)
library(purrr)
library(htmltools)

setwd("App")

source("utilities_sag_data.r")
source("utilities_SID_data.r")
source("update_SAG_data.r")
source("update_SID_data.r")

year <- 2019

update_SAG(year)
update_SID(year)

stock <- "wit.27.3a47d"
df <- access_sag_data_local(stock, year)

# get sag settings
key <- df %>% head(1) %>% pull(AssessmentKey)

browseURL(paste0("https://standardgraphs.ices.dk/ViewCharts.aspx?key=", key))

options(icesSAG.use_token = TRUE)
sagSettings <- icesSAG::getSAGSettingsForAStock(key)
sagSettings

source("utilities_plotting.r")

ICES_plot_2(df, sagSettings)
ICES_plot_2(df)


# test in app

library(shiny)
runApp(".")
