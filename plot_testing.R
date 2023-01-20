
library(icesTAF)
library(icesSAG)
library(data.table)
library(dplyr)
library(tidyr)
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

year <- 2021

update_SAG(year)
update_SID(year)

stock <- c("her.27.28", "cap.27.1-2", "ghl.27.1-2", "cod.27.2.coastS", "reg.27.561214")[5]
access_sag_data_local <- function(stock_code, year) {
#   
    # Dowload the data
    df_summary <- fread(sprintf("App/Data/SAG_%s/SAG_summary.csv", year)) ####there is a space after SAG_ fix this below
    SAGsummary <- df_summary %>% filter(fishstock == stock_code)

    df_refpts <- fread(sprintf("App/Data/SAG_%s/SAG_refpts.csv", year)) ####there is a space after SAG_ fix this below
    SAGrefpts <- df_refpts %>% filter(StockKeyLabel == stock_code)

    data_sag <- merge(SAGsummary, SAGrefpts)
    data_sag <- data_sag %>% select(-fishstock) %>% filter(StockPublishNote == "Stock published")
    
    return(data_sag)
}
df <- access_sag_data_local(stock, year)
df$recruitment
shadeYears <- c(2018,2019)



recruitment_shaded <- df %>% filter(Year %in% shadeYears)
# view sag page
key <- df %>% head(1) %>% pull(AssessmentKey)
browseURL(paste0("https://standardgraphs.ices.dk/ViewCharts.aspx?key=", key))

source("utilities_plotting.r")

#ICES_plot_1(df)
#ICES_plot_2(df)
#ICES_plot_3(df)
ICES_plot_4(df)

# test in app


library(shiny)
runApp("App")


getSAGSettings <- function(assessmentkey) {
    sagSettings <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/StockSettings?assessmentKey=%s", assessmentkey)
        )
    )
}
assessmentkey <- 14095
settings <- getSAGSettings(assessmentkey)
df<- settings[!(settings$settingValue == ""), ]
