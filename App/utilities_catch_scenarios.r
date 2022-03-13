#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
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
# Catch scenarios table
########################################################### tranform the sid dataframe
get_Advice_View_info <- function(stock_name) {
  catch_scenario_list <- jsonlite::fromJSON(
    URLencode(
      # "https://sg.ices.dk/adviceview/API/getAdviceViewRecord?year=2020"
      sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s", stock_name)
    )
  )

  catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE)
  catch_scenario_advice_sentence <- catch_scenario_list$adviceSentence
  catch_scenario_advice_link <- catch_scenario_list$adviceLink
  catch_scenario_list <- subset(catch_scenario_list, select = -c(adviceSentence, adviceLink, linkToAdviceView, mpwebLink))

  # reshape table from horizontal to vertical
  
  x <- colnames(catch_scenario_list[, -1])
  t <- melt(catch_scenario_list, measure.vars = x, variable.name = "advice View", value.name = "Values", na.rm = TRUE)

  table_vert_adviceView <- subset(t, select = -c(adviceKey))
  return(table_vert_adviceView)
}

#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
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
get_Advice_View_sentence <- function(stock_name) {
  catch_scenario_list <- jsonlite::fromJSON(
    URLencode(
      # "https://sg.ices.dk/adviceview/API/getAdviceViewRecord?year=2020"
      sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s", stock_name)
    )
  )

catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE)
catch_scenario_advice_sentence <- catch_scenario_list$adviceSentence
return(catch_scenario_advice_sentence)
}

#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
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
get_catch_scenario_table <- function(stock_name) {
  catch_scenario_list <- jsonlite::fromJSON(
    URLencode(
      # "https://sg.ices.dk/adviceview/API/getAdviceViewRecord?year=2020"
      sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s", stock_name)
    )
  )

  catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE)

  catch_scenario_table <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getCatchScenariosTable/%s", catch_scenario_list$adviceKey) # )
    )
  )
  catch_scenario_table <- catch_scenario_table %>%
    pivot_wider(
      names_from = c(aK_ID, aK_Label, yearLabel, unit, stockDataType),
      names_glue = "{aK_Label} ({yearLabel})",
      values_from = value
    ) %>%
    select(-adviceKey, -cS_Basis, -aR_ID) #%>%
    # by(
    #   .$cS_Purpose,
    #   function(x) {
    #     select(x, -cS_Purpose)
    #   }
    # ) %>%
    # unclass()
  return(catch_scenario_table)
}


#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
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
standardize_catch_scenario_table <- function(tmp) {
  tmp$Year <- 2021
  ###################################### code tests to try to accept as many catch scen tables headings

  tmp_unified <- data.frame()
  # Year
  pattern <- c("Year")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  tmp_unified <- tmp[, c(subset)]

  # tmp_unified <-unlist(tmp[,c(subset)],use.names = FALSE)
  
  # cS_Label"
  pattern <- c("cS_Label")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$cat <- tmp[,c(subset)]
  tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  
  # cS_Purpose"
  pattern <- c("cS_Purpose")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$cat <- tmp[,c(subset)]
  tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  
  # Ftotal"
  pattern <- c("Ftotal", "F_total", "F total", "Total F", "F age")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$F <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(F = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }
  
  # Total catch"
  pattern <- c("Total catch")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$TotCatch <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(TotCatch = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }
  

  # % TAC change"
  pattern <- c("% TAC ", "TAC", "TAC ", "% TAC")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$TACchange <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(TACchange = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }

  # % Advice change"
  pattern <- c("% Advice change", "Advice change", "% advice change")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$ADVICEchange <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(ADVICEchange = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }
  
  # SSB"
  pattern <- c("SSB (2021)")
  subset <- which(names(tmp) == pattern)
  if (length(subset) == 0) {
    pattern <- c("SSB (2020)")
    subset <- which(names(tmp) == pattern)
  }

  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(SSB = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }
  
  # % SSB change "
  pattern <- c("% SSB change", "SSB change")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(SSBchange = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)])
  }
  
# rename columns to standard names
  colnames(tmp_unified) <- c("Year", "cat","cS_Purpose", "F", "TotCatch", "TAC change", "ADVICE change", "SSB", "SSB change")

#   tmp_unified <- tmp_unified %>% do(bind_rows(., data.frame(Year = 2022, cat = "ref", F = 0, TotCatch = 0, TACchange = 0, ADVICEchange = 0, SSBchange = 0, SSB = 0)))
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "BasisAdvice", "Basis Of Advice")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "OtherScenarios", "Other Scenarios")

  return(tmp_unified)
  # tmp3 <- tmp2 %>% relocate("SSB", .before = "SSBchange")
}


#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
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
wrangle_catches_with_scenarios <- function(catches_data, catch_scenario_table) {
    catches_data <- catches_data %>% select(Year, catches)
    catches_data <- catches_data %>% add_column(cat = "Historical Catches")
    catch_scenario_table <- catch_scenario_table %>% select(Year, TotCatch, cat)

    catches_data_year_before <- catch_scenario_table
    catches_data_year_before$Year <- 2020
    catches_data_year_before$TotCatch <- catches_data$catches[catches_data$Year == 2020]

    # print(catches_data)
    # print(catch_scenario_table)
    # print(catches_data_year_before)

    catches_data <- setNames(catches_data, names(catch_scenario_table))
    final_df <- rbind(catches_data, catches_data_year_before, catch_scenario_table)
    return(final_df)
}


#' Returns ....
#'
#' Downloads ...
#'
#' @param stock_name
#'
#' @return 
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
make_app_citation <- function() {
  string_citation <- HTML(
    paste0(
      br(),
      "<b>", "<font size=", 5, ">", "Data Policy", "</font>", "</b>", "<br/>",
      "<font size=", 3, ">",
      # img(src = "by.png", height = "100px"),
      "<img src= 'by.png'", " height= '100px'/>","<br/>",
      br(),
      "Under the revised ", "<a href='","https://www.ices.dk/data/guidelines-and-policy/Pages/ICES-data-policy.aspx", "' target='_blank'>", "ICES Data Policy","</a>",
      " all public data are under the Creative Commons licence ",
      "<a href='","https://creativecommons.org/licenses/by/4.0/", "' target='_blank'>", "(CC BY 4.0).","</a>","<br/>",
      br(),
      "The ICES Online Advice Shiny app diplays data collected from the following sources:
  <ul><li>", a("GIS", href = "https://gis.ices.dk/sf/index.html"), "</li>",
      "<li>", a("SID", href = "https://www.ices.dk/data/assessment-tools/Pages/stock-information-database.aspx"), "</li>",
      "<li>", a("SAG", href = "https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx"), "</li>",
      "<li>", "Advice View", "</li></ul>", "</font>",
      br(),
      br(),
      "<b>", "<font size=", 5, ">", "Citation", "</font>", "</b>", "<br/>",
      "<font size=", 3, ">",
      "Please refer to ", "<a href='","https://www.ices.dk/data/guidelines-and-policy/Pages/ICES-data-policy.aspx", "' target='_blank'>", "ICES Data Policy","</a>", 
      " for full conditions and guidance on citation.<br/>
      When publishing results from the app the minimum citation should include: <br/>
      <br/>
      International Council for the Exploration of the Sea (ICES). (", Sys.Date(), "). ICES Single-Stock Online Advice.
      https://ices-taf.shinyapps.io/online-single-stock-advice/?assessmentkey=", getQueryString(), "</font><br/>",
      br(),
      "<b>", "<font size=", 5, ">", "Contact & Feedback", "</font>", "</b>", "<br/>",
      "<font size=", 3, ">",
      "You can contact us via ", "<a href = ", "'mailto: luca.lamoni@ices.dk'", ">email</a>", "<br/>",
      "You can submit an issue to our GitHub ", "<a href='","https://github.com/ices-tools-dev/online-advice/issues", "' target='_blank'>", "repository.","</a>",
      "</font>"

    )
  )

  return(string_citation)
}
