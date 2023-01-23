
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
assessmentkey <- 13003
settings <- getSAGSettings(assessmentkey)
df<- settings[!(settings$settingValue == ""), ]




catch_scenario_list <- get_Advice_View_info("hom.27.2a4a5b6a7a-ce-k8", 2019)
table <- get_catch_scenario_table(catch_scenario_list)
table_stand <- standardize_catch_scenario_table(table)

stock_name <- "cod.27.47d20"
year <- 2022
catches_data<- access_sag_data_local("hom.27.2a4a5b6a7a-ce-k8", 2019)
catches_data <- catches_data %>%
    filter(Purpose == "Advice") %>%
    select(Year, catches, landings, discards) #%>% na.omit()

# Function to check if a column is made up of all NA values
    is_na_column <- function(dataframe, col_name) {
        return(all(is.na(dataframe[, ..col_name])))
    }

    if (is_na_column(catches_data,"catches")){
      catches_data$catches <- rowSums(catches_data[,c("landings", "discards")], na.rm=TRUE)
      catches_data <- catches_data %>% select(-c("landings", "discards"))
    }



  catches_data <- catches_data %>% add_column(cat = "Historical Catches")
  catch_scenario_table <- table_stand$table %>% select(Year, TotCatch, cat)


  catch_scenario_list_previous_year <- get_Advice_View_info(stock_name, year - 1)

    catches_data <- catches_data %>% mutate(catches = ifelse(Year == year,  as.numeric(catch_scenario_list_previous_year$adviceValue), catches)) %>% na.omit()
#   catches_data <- catches_data %>% mutate(catches = c(catches[-n()], as.numeric(catch_scenario_list_previous_year$adviceValue))) 

  catches_data_year_before <- catch_scenario_table
  catches_data_year_before$Year <- catch_scenario_table$Year - 1 ## assessmnet year

  catches_data_year_before$TotCatch <- tail(catches_data$catches, 1)

  catches_data <- setNames(catches_data, names(catch_scenario_table))
  final_df <- rbind(catches_data, catches_data_year_before, catch_scenario_table)

  final_df <- na.omit(final_df)
wrangle_catches_with_scenarios(access_sag_data_local(query$stockkeylabel, query$year), catch_scenario_table()$table, query$stockkeylabel, query$year)


  catch_scenario_list <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s&year=%s", "tur.27.4", 2018)
    )
  )


######################################################################################

get_Advice_View_info <- function(stock_name, year) {
  catch_scenario_list <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s&year=%s", stock_name, year)
    )
  )
  
  if (!is_empty(catch_scenario_list)){
  catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE, adviceStatus == "Advice")
  } else {
     catch_scenario_list <- list()
  }
  
  return(catch_scenario_list)
}

get_catch_scenario_table <- function(catch_scenario_list) {
  catch_scenario_table <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getCatchScenariosTable/%s", catch_scenario_list$adviceKey)
    )
  )

  if (length(catch_scenario_table) != 0) {
  catch_scenario_table <- catch_scenario_table %>%
    pivot_wider(
      names_from = c(aK_ID, aK_Label, yearLabel, unit, stockDataType),
      names_glue = "{aK_Label} ({yearLabel}) _{stockDataType}_",
      values_from = value
    ) %>%
    select(-assessmentKey, -adviceKey, -cS_Basis, -aR_ID)


  catch_scenario_table <- catch_scenario_table %>% add_column(Year = catch_scenario_list$assessmentYear + 1, .before = "cS_Label")
  } else {
    catch_scenario_table <- character(0) 
  }

  return(catch_scenario_table)
}
standardize_catch_scenario_table <- function(tmp) {
  if (!is_empty(tmp)) {
  
  tmp_unified <- data.frame()

  # Year
  pattern <- c("Year")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp[, c(subset)]

  # cS_Label"
  pattern <- c("cS_Label")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])

  # cS_Purpose"
  pattern <- c("cS_Purpose")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])

  # Ftotal"
  pattern <- c("_FTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # Fwanted"
  pattern <- c("_Fwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F_wanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # Total catch"
  pattern <- c("_CatchTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TotCatch = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # % TAC change"
  pattern <- c("_TACchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TACchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # % Advice change"
  pattern <- c("_Advchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(ADVICEchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # SSB"
  pattern <- c("_StockSize_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSB = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }
  
  # % SSB change "
  pattern <- c("_StockSizechange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSBchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # Save the names to re-use them later when diplaying the table
  colnames(tmp_unified) <- sub(" _.*_", "", colnames(tmp_unified))
  col_names_for_display <- colnames(tmp_unified)
  
  # rename columns to standard names
  colnames(tmp_unified) <- c("Year", "cat", "cS_Purpose", "F", "F_wanted", "TotCatch", "TAC change", "ADVICE change", "SSB", "SSB change")

  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "BasisAdvice", "Basis Of Advice")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "OtherScenarios", "Other Scenarios")
  }
  else {
    tmp_unified <- character(0)
    col_names_for_display <- character(0)
  }
  
  return(list(table = tmp_unified, cols = col_names_for_display))
}
catch_scenario_list <- get_Advice_View_info("cod.27.5a", 2022)
table <- get_catch_scenario_table(catch_scenario_list)
table_stand <- standardize_catch_scenario_table(table)

is_na_column <- function(list) {
        return(all(is.na(list)))
    }
if (is_na_column(data.frame(table_stand$table$F))){
  print("yes")
}

data.frame(table_stand$table)
Basis <- table_stand$table$cS_Purpose == "Basis Of Advice"

data.frame(table_stand$table[c(Basis),])


tmp <- data.frame(table_stand$table)
    
    labels <- sprintf(
            "Catch Scenario: %s", tmp$cat
        ) %>% lapply(htmltools::HTML)

    
    Basis <- tmp[tmp$cS_Purpose == "Basis Of Advice",]

# Function to check if a column is made up of all NA values
    is_na_column <- function(dataframe, col_name) {
        return(all(is.na(dataframe[, col_name])))
    }
if (is_na_column(tmp, "F")){
  print("yes")
}
names(tmp)
