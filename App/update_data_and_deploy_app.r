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
UpdateDataApp(mode = "AllYears")


# source("deploy.r")

