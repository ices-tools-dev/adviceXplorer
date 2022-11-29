#### This code will update both SAG and SID data at regular time intervals and re-deploy the app at the end
#### Libraries

library(tidyverse)
library(reshape2)
library(icesTAF)
library(dplyr)
library(icesSAG)
library(icesFO)
library(data.table)
library(rsconnect)

setwd("./App")

source("utilities_SID_data.r")
source("update_SAG_data.r")
source("update_SID_data.r")




## Ideally, this function would run every hour on the server to update sag
## it will take several minuts to run it locally for all the years, for now I will leave this
# function commented out, so it does not waste time when teh app is run locally
years <- c(2022, 2021, 2020, 2019, 2018, 2017)
for (year in years) {
    update_SAG(year)
    update_SID(year)
}

source("deploy.r")

