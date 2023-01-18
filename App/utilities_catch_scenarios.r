#' Returns a df with the metadata for the advice view record for a specific stock and year. It uses the advice view web-service.
#'
#' @param stock_name
#' @param year
#' 
#' @return metadata for the advice view record
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' get_Advice_View_info("bll.27.3a47de", 2021)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
#' 
get_Advice_View_info <- function(stock_name, year) {
  catch_scenario_list <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s&year=%s", stock_name, year)
    )
  )
  # #########################
  # if (length(catch_scenario_list) == 0) {
  #   catch_scenario_list <- jsonlite::fromJSON(
  #     URLencode(
  #       sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s&year=%s", stock_name, year - 1)
  #     )
  #   )
  # }
  # ######################

  catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE, adviceStatus == "Advice")
  return(catch_scenario_list)
}

#' Returns an HTML string containing the summary of the advice view record to be displayed on top of the Advice page
#'
#' @param catch_scenario_list
#' @param StockDescription
#'
#' @return HTML string
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' StockDescription(df, StockDescription)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
#' 
get_Advice_View_Summary <- function(catch_scenario_list, StockDescription) {
  advice_requester <- catch_scenario_list$adviceRequester
  advice_requester <- gsub("~", ", ", advice_requester)

  catch_scenario_advice_sentence <- HTML(paste0(
    "<font size=", 3, ">", "Stock description: ", "<b>", StockDescription, "</b><br/>",
    "<font size=", 3, ">", "Stock code: ", "<b>", catch_scenario_list$stockCode, "</b><br/>",
    "<font size=", 3, ">", "Assessment year: ", "<b>", catch_scenario_list$assessmentYear, "</b><br/>"
  ))

  return(catch_scenario_advice_sentence)
} 

#' Returns an HTML string containing the headline advice and a link to the advice sheet
#'
#' @param catch_scenario_list

#'
#' @return HTML string with link
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' get_Advice_View_Headline(df)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
get_Advice_View_Headline <- function(catch_scenario_list, session) {
  catch_scenario_advice_sentence <- HTML(
    paste0(
      "<span class='hovertext' data-hover='Click here to access the pdf version of the Advice'>",
      "<a href='", get_advice_doi(catch_scenario_list$assessmentKey), "' target='_blank'>",
      "<b><i><font size=4> Headline advice </font></b></i><i class='fa-solid fa-up-right-from-square'></i></a></span>",
      "<br/>",
      "<font size=3>", catch_scenario_list$adviceSentence, "</font>"
    )
  )
  return(catch_scenario_advice_sentence)
}

#' Returns an HTML string containing some basic info on the selected stock and year
#'
#' @param CommonName
#' @param stockcode
#' @param assessmentYear
#' @param description
#' 
#'
#' @return HTML string
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' get_Stock_info(stockcode, StockDescription, assessmentYear)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
get_Stock_info <- function(CommonName, stockcode,  assessmentYear, description) { #StockDescription,
  stock_info_sentence <- HTML(paste0("<b><i><font size=", 4, ">", "Stock information:","</font></b></i><br/>",
                                              "<font size=", 3, ">","Common name: ", "<b>", CommonName,"</b><br/>",
                                              "<font size=", 3, ">","Stock code: ", "<b>", stockcode,"</b><br/>",
                                              "<font size=", 3, ">","Assessment year: ", "<b>", assessmentYear,"</b><br/>"),
                                              "<font size=", 3, ">","Location: ", "<b>", parse_location_from_stock_description(description),"</b>")
return(stock_info_sentence)
}

#' Returns the catch scenario table for the selected stock and year using the advice view web service.
#'
#' @param catch_scenario_list

#' @return df
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' get_catch_scenario_table(catch_scenario_list)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
#' 
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

#' Returns an HTML string containing the catch scenario table's footnotes.
#'
#' @param catch_scenario_list

#' @return string
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' get_catch_scenario_table(catch_scenario_list)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
#' 
get_catch_scenario_notes <- function(catch_scenario_list) {
 
  catch_scenario_table_notes <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getCatchScenariosNotes/%s", catch_scenario_list$adviceKey) # )
    )
  )

  if (length(catch_scenario_table_notes) != 0) {
  catch_scenario_table_notes <- catch_scenario_table_notes %>% select(-catchOptionsTableKey, -adviceKey)

  string_notes <- HTML(
    paste0("<ul>",paste0("<li><font size=2>",catch_scenario_table_notes$symbol, " "), paste0(catch_scenario_table_notes$notes, "</font></li>"), "</ul>"))
  } else {
    string_notes <- character(0)
  }
  
  return(string_notes)
}




#' Returns a version of the catch scenario table with standardised columns and columns headings
#'
#' @param tmp (catch scenario table)
#'
#' @return df
#'
#' @note
#' this function is usally run sequantially with get_catch_scenario_table()
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' standardize_catch_scenario_table(df)
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
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
  colnames(tmp_unified) <- c("Year", "cat", "cS_Purpose", "F", "TotCatch", "TAC change", "ADVICE change", "SSB", "SSB change")

  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "BasisAdvice", "Basis Of Advice")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "OtherScenarios", "Other Scenarios")
  }
  else {
    tmp_unified <- character(0)
    col_names_for_display <- character(0)
  }
  
  return(list(table = tmp_unified, cols = col_names_for_display))
}


#' Returns a df that merges historical catches, previous year advice and current year catch scenarios
#'
#' @param catches_data (from SAG)
#' @param catch_scenario_table (from advice view)
#' @param stock_name
#' @param year
#'
#' @return df
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
wrangle_catches_with_scenarios <- function(catches_data, catch_scenario_table, stock_name, year) {
  
  catches_data <- catches_data %>%
    filter(Purpose == "Advice") %>%
    select(Year, catches)

  catches_data <- catches_data %>% add_column(cat = "Historical Catches")
  catch_scenario_table <- catch_scenario_table %>% select(Year, TotCatch, cat)


  catch_scenario_list_previous_year <- get_Advice_View_info(stock_name, year - 1)


  catches_data <- catches_data %>% mutate(catches = c(catches[-n()], as.numeric(catch_scenario_list_previous_year$adviceValue))) #### this will be substituted by advice value from advice list of previous year

  catches_data_year_before <- catch_scenario_table
  catches_data_year_before$Year <- catch_scenario_table$Year - 1 ## assessmnet year

  catches_data_year_before$TotCatch <- tail(catches_data$catches, 1)

  catches_data <- setNames(catches_data, names(catch_scenario_table))
  final_df <- rbind(catches_data, catches_data_year_before, catch_scenario_table)

  final_df <- na.omit(final_df)

  return(final_df)
}

#' Returns a catch scenario plot with values in %. The values of the current catch scenario table are adjusted based on the previous year basis of advice.
#'
#' @param old_catch_scen_table (from advice view)
#' @param new_catch_scen_table (from advice view)

#'
#' @return df
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' 
#' }
#'
#' @references
#'
#' 
#'
#' @export
scale_catch_scenarios_for_radialPlot <- function(old_catch_scen_table, new_catch_scen_table){
  if (!is_empty(new_catch_scen_table)) {
  Basis <- old_catch_scen_table[old_catch_scen_table$cS_Purpose == "Basis Of Advice",]
  catch_scen_table_perc <- new_catch_scen_table[, c("Year", "cat", "cS_Purpose")]
  
  catch_scen_table_perc$F <- (new_catch_scen_table$F - Basis$F) / Basis$F *100
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


