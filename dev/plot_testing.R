
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
assessmentkey <- 17630
settings <- getSAGSettings(assessmentkey)
df<- settings[!(settings$settingValue == ""), ]




catch_scenario_list <- get_Advice_View_info("hom.27.2a4a5b6a7a-ce-k8", 2019)
table <- get_catch_scenario_table(catch_scenario_list)
table_stand <- standardize_catch_scenario_table(table)

stock_name <- "ane.27.9a"
year <- 2022
catches_data<- access_sag_data_local("nep.fu.17", 2022)
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

  # Total catch"
  pattern <- c("_CatchTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TotCatch = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

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

  # HR
  pattern <- c("_HR_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(HR = NA)
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
  # dead discards"
  pattern <- c("_CatchUnwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(CatchUnwanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # surviving discards"
  pattern <- c( "surviving")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(CatchUnwantedSurviving = NA)
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
  colnames(tmp_unified) <- c("Year", "cat", "cS_Purpose", "TotCatch", "F", "F_wanted", "HR", "SSB","CatchUnwanted","CatchUnwantedSurviving", "TAC change", "ADVICE change", "SSB change")

  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "BasisAdvice", "Basis Of Advice")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "OtherScenarios", "Other Scenarios")
  }
  else {
    tmp_unified <- character(0)
    col_names_for_display <- character(0)
  }
  
  return(list(table = tmp_unified, cols = col_names_for_display))
}








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

catch_scenario_list_1 <- get_Advice_View_info("nep.fu.17", 2022)
catch_scenario_list_2 <- get_Advice_View_info("cod.27.7e-k", 2022-1)
table_1 <- get_catch_scenario_table(catch_scenario_list_1)
table_2 <- get_catch_scenario_table(catch_scenario_list_2)
table_stand_1 <- standardize_catch_scenario_table(table_1)
table_stand_2 <- standardize_catch_scenario_table(table_2)





scale_catch_scenarios_for_radialPlot <- function(old_catch_scen_table, new_catch_scen_table){
  if (!is_empty(new_catch_scen_table)) {
  Basis <- old_catch_scen_table[old_catch_scen_table$cS_Purpose == "Basis Of Advice",]
  catch_scen_table_perc <- new_catch_scen_table[, c("Year", "cat", "cS_Purpose")]
  
  catch_scen_table_perc$F <- (new_catch_scen_table$F - Basis$F) / Basis$F *100
  catch_scen_table_perc$F_wanted <- (new_catch_scen_table$F_wanted - Basis$F_wanted) / Basis$F_wanted *100
  catch_scen_table_perc$HR <- (new_catch_scen_table$HR - Basis$HR) / Basis$HR *100
  catch_scen_table_perc$TotCatch <- (new_catch_scen_table$TotCatch - Basis$TotCatch) / Basis$TotCatch *100
  catch_scen_table_perc$`TAC change` <- new_catch_scen_table$`TAC change`
  catch_scen_table_perc$`ADVICE change` <- new_catch_scen_table$`ADVICE change`
  catch_scen_table_perc$SSB <- (new_catch_scen_table$SSB - Basis$SSB) / Basis$SSB *100
  catch_scen_table_perc$`SSB change` <- new_catch_scen_table$`SSB change`
  } else {
    catch_scen_table_perc <- character(0)
  }
  
  return(catch_scen_table_perc)
}


old_catch_scen_table <- table_stand_2$table
new_catch_scen_table <- table_stand_1$table
scale_catch_scenarios_for_radialPlot(table_stand_2$table, table_stand_1$table)

# calculate_perc_change <- function(new_catch_scen_table_column, Basis, catch_scen_table_perc_column){
#   catch_scen_table_perc$F <- (new_catch_scen_table$F - Basis$F[1]) / Basis$F[1] *100
#   ifelse(new_catch_scen_table$F == Basis$F[1] & Basis$F[1] == 0, 1, (new_catch_scen_table$F - Basis$F[1]) / Basis$F[1])
# }



# calculate_perc_change <- function(new_catch_scen_table, Basis, catch_scen_table_perc) {
  
#   for (m in 4:ncol(new_catch_scen_table)) {  
#   for (i in seq_len(nrow(new_catch_scen_table))) {
#     if (Basis[1,m] == 0) {
#       catch_scen_table_perc[i,m] <- 1 * 100
#       if (new_catch_scen_table[i,m] == Basis[1,m]) {
#         catch_scen_table_perc[i,m] <- 0
#       }
#     } else {
#       catch_scen_table_perc[i,m] <- 100 * ((new_catch_scen_table[i,m] - Basis[1,m]) / Basis[1,m])
#     }
#   }
# }
# }
catch_scenario_list_1 <- get_advice_view_info("nep.fu.17", 2022)
catch_scenario_list_2 <- get_Advice_View_info("her.27.irls", 2022-1)
table_1 <- get_catch_scenario_table(catch_scenario_list_1$adviceKey, 2022)
table_2 <- get_catch_scenario_table(catch_scenario_list_2)
table_stand_1 <- standardize_catch_scenario_table(table_1)
table_stand_2 <- standardize_catch_scenario_table(table_2)

changes_columns <- table_stand_1$table %>% select("cat","TAC change","ADVICE change", "SSB change")


keep.cols <- c("Year", "cat" , "cS_Purpose","F", "F_wanted","HR","TotCatch","SSB")
df_old <- table_stand_2$table %>% select(keep.cols)
df_new <- table_stand_1$table %>% select(keep.cols)

df_old <- df_old[,colSums(is.na(df_old))<nrow(df_old)]
df_new <- df_new[,colSums(is.na(df_new))<nrow(df_new)]
Basis <- df_old[df_old$cS_Purpose == "Basis Of Advice",]
df_new <- df_new %>% na.omit()
catch_scen_table_perc <- df_new[, c("Year", "cat", "cS_Purpose")]

which(is.na(df_new), arr.ind=TRUE)

df_new <- df_new[c(-9,-10),]
catch_scen_table_perc <- catch_scen_table_perc[c(-9,-10),]
catch_scen_table_perc <- calculate_perc_change(df_new, Basis, catch_scen_table_perc)
catch_scen_table_perc <- catch_scen_table_perc %>% left_join(., changes_columns, by = c("cat"))

calculate_perc_change <- function(df_new, Basis, catch_scen_table_perc) {

  for (m in 4:ncol(df_new)) {
    for (i in seq_len(nrow(df_new))) {
      if (Basis[1, m] == 0) {
        catch_scen_table_perc[i, m] <- 1 * 100
        if (df_new[i, m] == Basis[1, m]) {
          catch_scen_table_perc[i, m] <- 0
        }
      } else {
        catch_scen_table_perc[i, m] <- 100 * ((df_new[i, m] - Basis[1, m]) / Basis[1, m])
      }
    }
  }
  return(catch_scen_table_perc)
}
catch_scen_table_perc <- catch_scen_table_perc %>% left_join(., changes_columns, by = c("cat"))

a <- 0
b<- 0
# 100*((b-a)/a)

if (a == 0) {
  c <- 1 * 100
  if (b == a) {
    c <- 0
  }
} else {
  c <- 100 * ((b - a) / a)
}
print(c)


scale_catch_scenarios_for_radialPlot <- function(old_catch_scen_table, new_catch_scen_table){
  if (!is_empty(new_catch_scen_table) & !is_empty(old_catch_scen_table)) {



  Basis <- old_catch_scen_table[old_catch_scen_table$cS_Purpose == "Basis Of Advice",]
  catch_scen_table_perc <- new_catch_scen_table[, c("Year", "cat", "cS_Purpose", "F")]
  
  catch_scen_table_perc$F <- calculate_perc_change(new_catch_scen_table$F, Basis$F, catch_scen_table_perc$F)
  catch_scen_table_perc$F <- (new_catch_scen_table$F - Basis$F) / Basis$F *100
  catch_scen_table_perc$F_wanted <- (new_catch_scen_table$F_wanted - Basis$F_wanted) / Basis$F_wanted *100
  catch_scen_table_perc$HR <- (new_catch_scen_table$HR - Basis$HR) / Basis$HR *100
  catch_scen_table_perc$TotCatch <- (new_catch_scen_table$TotCatch - Basis$TotCatch) / Basis$TotCatch *100
  catch_scen_table_perc$`TAC change` <- new_catch_scen_table$`TAC change`
  catch_scen_table_perc$`ADVICE change` <- new_catch_scen_table$`ADVICE change`
  catch_scen_table_perc$SSB <- (new_catch_scen_table$SSB - Basis$SSB) / Basis$SSB *100
  catch_scen_table_perc$`SSB change` <- new_catch_scen_table$`SSB change`
  } else {
    catch_scen_table_perc <- character(0)
  }
  
  return(catch_scen_table_perc)
}



# Use which() to find the row indices where second_column is not NA
not_na_indices <- which(!is.na(catches_data$SSB))

# Use the result to index the first_column vector and find the minimum value
min_value <- min(catches_data$Year[not_na_indices])



########################################################################5
library(icesASD)
library(httr)
library(jsonlite)

year <- 2019
stock <- "aru.27.5a14"
test <- get_advice_view_info(stock, year)

if (is_empty(test)) {
  test <- get_advice_view_info(stock, year - 1)
  if (!is_empty(test)) {
    test <- test %>% filter(year + 1 == format(as.POSIXct(adviceApplicableUntil), format = "%Y"))
  } else {
    test <- list()
  }
} else {
  if (nrow(test > 1)) {
    test <- test %>% filter(year + 1 == format(as.POSIXct(adviceApplicableUntil), format = "%Y"))
  } else {
    test <- test
  }
}
test

if (nrow(test) > 1) {
  test <- test %>% filter(year + 1 == format(as.POSIXct(adviceApplicableUntil), format = "%Y"))
} else if (is_empty(test)) {
  test <- get_advice_view_info("bli.27.5b67", year - 1)
  test <- test %>% filter(year + 1 == format(as.POSIXct(adviceApplicableUntil), format = "%Y"))
}


get_additional_landing_data <- function(assessmentKey) {
  out <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/SummaryTable?assessmentKey=%s", assessmentKey)
        )
    )  
  df <- data.frame(Year = out$lines$year, ibc = out$lines$ibc, unallocated_Removals = out$lines$unallocated_Removals)
  return(df)
}

out <- jsonlite::fromJSON(
            URLencode(
                sprintf("https://sag.ices.dk/SAG_API/api/S
    ummaryTable?assessmentKey=%s", 17689)))






api <- function(stock_name = NULL, year = NULL, adviceKey = NULL, api = c("record", "table", "notes")) {
  
  record_api_url <- "https://sg.ices.dk/adviceview/API/getAdviceViewRecord"
  table_api_url <- "https://sg.ices.dk/adviceview/API/getCatchScenariosTable"
  notes_api_url <- "https://sg.ices.dk/adviceview/API/getCatchScenariosNotes"


  api <- match.arg(api)
  if (api == "record") {
    api_url <- get(paste0(api, "_api_url"))
    url <- paste0(api_url, "?", "stockcode=", stock_name, "&year=", year)
    url <- parse_url(url)
    url <- build_url(url)
    url
  } else {
    api_url <- get(paste0(api, "_api_url"))
    url <- paste0(api_url, "/", adviceKey)
    url <- parse_url(url)
    url <- build_url(url)
    url
  }
}
get_advice_view_info <- function(stock_name, year) {
 
  catch_scenario_list <-
        read_json(
            api(stock_name = stock_name, year = year, api = "record"),
            simplifyVector = TRUE
        )
  
  if (!is_empty(catch_scenario_list)){
  catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE, adviceStatus == "Advice")
  } else {
     catch_scenario_list <- list()
  }
  
  return(catch_scenario_list)
}


dates <- as.POSIXct(test$adviceApplicableFrom)
format(as.POSIXct(test$adviceApplicableFrom), format="%Y")
test$adviceApplicableFrom


shorten_labels <- function(catch_scenarios_array) {
        
        for (i in 1:length(catch_scenarios_array)) {
            if (nchar(catch_scenarios_array[i]) > 20) {
                catch_scenarios_array[i] <- paste0(substr(catch_scenarios_array[i], 1, 20), "...")
            } else {
                catch_scenarios_array[i] <- catch_scenarios_array[i]
            }
        }
        return(catch_scenarios_array)
    }
shorten_labels(table_1$cS_Label)

library(icesASD)
library(icesASD)
library(httr)
library(jsonlite)

year <- 2022
stock <- "hom.27.2a4a5b6a7a-ce-k8"
test <- icesASD::get_advice_view_info(stock,year)
notes <- icesASD::get_catch_scenario_notes(test$adviceKey)



SAGrefpts <- getSAG("cod.27.47d20", 2022,
        data = "refpts", combine = TRUE, purpose = "Advice"
    )
SAGsummary <- getSAG("cod.27.47d20", 2022,
        data = "summary", combine = TRUE, purpose = "Advice"
    )



xx<- icesASD::get_catch_scenario_table(2968, 2022)
test <- standardize_catch_scenario_table(tmp)
 tmp <- xx
standardize_catch_scenario_table <- function(tmp) {
  if (!is_empty(tmp)) {
  
  tmp_unified <- data.frame()

  # Year
  pattern <- c("Year")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <-  data.frame(tmp[subset])

  # cS_Label"
  pattern <- c("cS_Label")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[subset]) ####this works!

  # cS_Purpose"
  pattern <- c("cS_Purpose")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[subset])

  # Total catch"
  pattern <- c("_CatchTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TotCatch = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # Ftotal"
  pattern <- c("_FTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # Fwanted"
  pattern <- c("_Fwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F_wanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # HR
  pattern <- c("_HR_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(HR = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # SSB"
  pattern <- c("_StockSize_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSB = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # dead discards"
  pattern <- c("_CatchUnwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(CatchUnwanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # surviving discards"
  pattern <- c( "surviving")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(CatchUnwantedSurviving = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # % TAC change"
  pattern <- c("_TACchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TACchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # % Advice change"
  pattern <- c("_Advchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(ADVICEchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # % SSB change "
  pattern <- c("_StockSizechange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSBchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset])
  }

  # Save the names to re-use them later when diplaying the table
  colnames(tmp_unified) <- sub(" _.*_", "", colnames(tmp_unified))
  col_names_for_display <- colnames(tmp_unified)
  
  # rename columns to standard names
  # colnames(tmp_unified) <- c("Year", "cat", "cS_Purpose", "TotCatch", "F", "F_wanted", "HR", "SSB", "TAC change", "ADVICE change", "SSB change")
  colnames(tmp_unified) <- c("Year", "cat", "cS_Purpose", "TotCatch", "F", "F_wanted", "HR", "SSB","CatchUnwanted","CatchUnwantedSurviving", "TAC change", "ADVICE change", "SSB change")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "BasisAdvice", "Basis Of Advice")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "OtherScenarios", "Other Scenarios")
  }
  else {
    tmp_unified <- character(0)
    col_names_for_display <- character(0)
  }
  
  return(list(table = tmp_unified, cols = col_names_for_display))
}
