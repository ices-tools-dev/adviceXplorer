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
get_Advice_View_Headline <- function(catch_scenario_list, replaced_advice_doi) {
  catch_scenario_advice_sentence <- HTML(
    paste0(
      "<span class='hovertext' data-hover='Click here to access the pdf version of the Advice'>",
      "<a href='", get_advice_doi(catch_scenario_list$assessmentKey), "' target='_blank'>",
      "<b><i><font size=4> Headline advice </font></b></i><i class='fa-solid fa-up-right-from-square'></i></a></span>",
      "<br/>",
      "<font size=3>", catch_scenario_list$adviceSentence, "</font>",
      if (!is_empty(replaced_advice_doi)) {
        paste0(
          "<br/>",
          "<span class='hovertext' data-hover='Click here for the replaced Advice'>",
          "<a href='", replaced_advice_doi, "' target='_blank'>",
          "<font size=3> Replaced advice </font><i class='fa-solid fa-up-right-from-square'></i></a></span>"
        )
      },
      paste0(
        "<br/>",
        "<span class='hovertext' data-hover='Click here dowload all the plots' data'>",
        downloadLink("download_SAG_Data", HTML("<font size= 3>Download assessment data </font><i class='fa-solid fa-cloud-arrow-down'></i></span>"))
      )
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

#' Returns an HTML string containing the catch scenario table's footnotes.
#'
#' @param ASDadviceKey

#' @return string
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' get_catch_scenario_table(ASDadviceKey)
#' }
#'
#' @references
#' https://sg.ices.dk/adviceview/AdviceList
#' 
#'
#' @export
#' 
format_catch_scenario_notes <- function(ASDadviceKey) {

  catch_scenario_table_notes <- icesASD::getCatchScenariosNotes(ASDadviceKey)

  if (length(catch_scenario_table_notes) != 0) {
    catch_scenario_table_notes <- catch_scenario_table_notes %>% select(-adviceKey)
    string_notes <- HTML(
      paste0("<ul>", paste0("<li><font size=2>", catch_scenario_table_notes$symbol, " "), paste0(catch_scenario_table_notes$notes, "</font></li>"), "</ul>")
    )
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
  tmp_unified <-  data.frame(tmp[subset][1])

  # cS_Label"
  pattern <- c("cS_Label")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[subset][1]) ####this works!

  # cS_Purpose"
  pattern <- c("cS_Purpose")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])

  # Total catch"
  pattern <- c("_CatchTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TotCatch = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # Ftotal"
  pattern <- c("_FTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # Fwanted"
  pattern <- c("_Fwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(F_wanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # HR
  pattern <- c("_HR_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(HR = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # SSB"
  pattern <- c("_StockSize_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSB = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # dead discards"
  pattern <- c("_CatchUnwanted_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(CatchUnwanted = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # surviving discards"
  pattern <- c( "surviving")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(CatchUnwantedSurviving = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # % TAC change"
  pattern <- c("_TACchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(TACchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # % Advice change"
  pattern <- c("_Advchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(ADVICEchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
  }

  # % SSB change "
  pattern <- c("_StockSizechange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
    tmp_unified <- tmp_unified %>% add_column(SSBchange = NA)
  } else {
    tmp_unified <- tmp_unified %>% add_column(tmp[subset][1])
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
wrangle_catches_with_scenarios <- function(catches_data, catch_scenario_table, catch_scenario_list_previous_year, stock_name, year, additional_LandingData) {
  
  catches_data <- catches_data %>%
    filter(Purpose == "Advice") %>%
    select(Year, catches, landings, discards) %>% 
    left_join(y = additional_LandingData, by = "Year")


  #  Function to check if a column is made up of all NA values
    is_na_column <- function(dataframe, col_name) {
        return(all(is.na(dataframe[, ..col_name])))
    }

    if (is_na_column(catches_data,"catches")){
      catches_data$catches <- rowSums(catches_data[,c("landings", "discards","ibc","unallocated_Removals")], na.rm=TRUE)
      catches_data <- catches_data %>% select(c("Year", "catches"))
    } else{
      catches_data <- catches_data %>% select(c("Year", "catches"))
    }

  catches_data <- catches_data %>% add_column(cat = "Historical Catches")
  catch_scenario_table <- catch_scenario_table %>% select(Year, TotCatch, cat)


  # catch_scenario_list_previous_year <- get_Advice_View_info(stock_name, year - 1)

  catches_data <- catches_data %>% mutate(catches = ifelse(Year == year,  as.numeric(catch_scenario_list_previous_year$adviceValue), catches)) %>% na.omit()
  
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
scale_catch_scenarios_for_radialPlot <- function(old_catch_scen_table, new_catch_scen_table) {
  if (!is_empty(new_catch_scen_table) | !is_empty(new_catch_scen_table)) {
    changes_columns <- new_catch_scen_table %>% select("cat", "TAC change", "ADVICE change", "SSB change")

    keep.cols <- c("Year", "cat", "cS_Purpose", "F", "F_wanted", "HR")
    df_old <- old_catch_scen_table %>% 
      select(all_of(keep.cols)) %>% 
      drop_cols_with_all_nas()
    
    df_new <- new_catch_scen_table %>% 
      select(all_of(keep.cols)) %>% 
      drop_cols_with_all_nas() %>% 
      na.omit()

    Basis <- df_old[df_old$cS_Purpose == "Basis Of Advice", ]
    catch_scen_table_perc <- df_new[, c("Year", "cat", "cS_Purpose")]
    catch_scen_table_perc <- calculate_perc_change(df_new, Basis, catch_scen_table_perc)
    catch_scen_table_perc <- catch_scen_table_perc %>% left_join(., changes_columns, by = c("cat")) #%>% relocate("SSB change", .after = SSB)
    
  } else {
    catch_scen_table_perc <- character(0)
  }
  
  return(catch_scen_table_perc)
}


#' Drop columns from a data frame if they only contain NA values
#'
#' @param df a data frame
#'
#' @return a data frame where no columns contain only NA
#'
#' @examples
#' df <- data.frame(a = c(NA, NA, 1), b = rep(NA,3))
#' drop_all_na_cols(df)
#' 
drop_cols_with_all_nas <- function(df){
  df[,colSums(is.na(df)) < nrow(df)]
}

#' Returns a catch scenario plot with values in %. 
#' The values of the current catch scenario table are adjusted based on the previous year basis of advice.
#' If the previous year value == 0, the % of change is going to be 100%.
#' If the current year and previous year value are equal, then the percantage of change is 0.
#'
#' @param new_catch_scen_table (from advice view)
#' @param Basis
#' @param catch_scen_table_perc 

#'
#' @return catch_scen_table_perc (df)
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
  names(catch_scen_table_perc) <- names(df_new)
  return(catch_scen_table_perc)
}