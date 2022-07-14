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
get_Advice_View_info <- function(stock_name, year) {
  
  catch_scenario_list <- jsonlite::fromJSON(
    URLencode(
      # "https://sg.ices.dk/adviceview/API/getAdviceViewRecord?year=2020"
      # sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s", stock_name)
      # sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?assessmentkey=%s", assessmentkey)
      sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s&year=%s", stock_name, year)
    )
  )

  catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE, adviceStatus == "Advice")
  # catch_scenario_advice_sentence <- catch_scenario_list$adviceSentence
  # catch_scenario_advice_link <- catch_scenario_list$adviceLink
  # catch_scenario_list <- subset(catch_scenario_list, select = -c(adviceSentence, adviceLink, linkToAdviceView, mpwebLink))

  # reshape table from horizontal to vertical
  
  # x <- colnames(catch_scenario_list[, -1])
  # t <- reshape2::melt(catch_scenario_list, measure.vars = x, variable.name = "advice_View", value.name = "Values", na.rm = TRUE)

  # table_vert_adviceView <- subset(t, select = -c(adviceKey))
  # print(catch_scenario_list)
  return(catch_scenario_list)
}
stock_name <- "cod.27.47d20"
year <- 2021
catch_scenario_list <- get_Advice_View_info(stock_name, year)
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
get_Advice_View_Summary <- function(catch_scenario_list, StockDescription) {
  # catch_scenario_list <- jsonlite::fromJSON(
  #   URLencode(
  #     # "https://sg.ices.dk/adviceview/API/getAdviceViewRecord?year=2020"
  #     sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s&year=%s", stock_name, year)
  #   )
  # )
  # catch_scenario_list <- get_Advice_View_info(stock_name, year)
# stockCode <- df[df$`advice View` == "stockCode",]$Values
# advice_requester <- df[df$`advice View` == "adviceRequester",]$Values
advice_requester <- catch_scenario_list$adviceRequester
advice_requester <- gsub("~", ", ", advice_requester)
# assessmentYear <- df[df$`advice View` == "assessmentYear",]$Values
# adviceSentence <- df[df$`advice View` == "adviceSentence",]$Values
# catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE)
# catch_scenario_advice_sentence <- df$adviceSentence
# advice_requester <- catch_scenario_list$adviceRequester

# advice_requester <- gsub("~", ", ", df$adviceRequester)
# HTML(paste0("<b>","<font size=", 5, ">", "Headline advice:","</font>","</b>", br(),"<font size=", 3, ">", advice_view_sentence(),"</font>"))

catch_scenario_advice_sentence <- HTML(paste0("<font size=", 3, ">","Stock description: ", "<b>", StockDescription,"</b><br/>",
                                              "<font size=", 3, ">","Stock code: ", "<b>", catch_scenario_list$stockCode,"</b><br/>",                                              
                                              "<font size=", 3, ">","Advice requester: ", "<b>", advice_requester,"</b><br/>",
                                              "<font size=", 3, ">","Assessment year: ", "<b>", catch_scenario_list$assessmentYear,"</b><br/><br/>",
                                              # "<b><i>","<font size=", 4, ">", "Headline advice:","</font>","</b></i><br/>",
                                              # "<font size=", 3, ">",catch_scenario_list$adviceSentence,"</font>"
                                              actionButton(inputId = "preview", label = NULL, style = "top: 1%; left:7%; width: 50px; height: 50px; background: url('calendar.png');  background-size: cover; background-position: center;")
                                              ))
# catch_scenario_advice_sentence <- paste0("Stock code: ", "<b>", stock_name,"</b><br/><br/>", catch_scenario_advice_sentence)
return(catch_scenario_advice_sentence)
}

get_Advice_View_Headline <- function(catch_scenario_list) {

catch_scenario_advice_sentence <- HTML(paste0("<b><i><font size=", 4, ">", "Headline advice:","</font></b></i><br/>",
                                              "<font size=", 3, ">",catch_scenario_list$adviceSentence,"</font>"))
# catch_scenario_advice_sentence <- paste0("Stock code: ", "<b>", stock_name,"</b><br/><br/>", catch_scenario_advice_sentence)
return(catch_scenario_advice_sentence)
}



# tezst <- get_Advice_View_sentence(stock_name, year)
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
get_catch_scenario_table <- function(catch_scenario_list) {
  # catch_scenario_list <- jsonlite::fromJSON(
  #   URLencode(
  #     # "https://sg.ices.dk/adviceview/API/getAdviceViewRecord?year=2020"
  #     # sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s", stock_name)
  #     sprintf("https://sg.ices.dk/adviceview/API/getAdviceViewRecord?stockcode=%s&year=%s", stock_name, year)
  #   )
  # )
  # catch_scenario_list <- get_Advice_View_info(stock_name, year)

  # catch_scenario_list <- catch_scenario_list %>% filter(adviceViewPublished == TRUE)

  catch_scenario_table <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getCatchScenariosTable/%s", catch_scenario_list$adviceKey) # )
    )
  )
  catch_scenario_table <- catch_scenario_table %>%
    pivot_wider(
      names_from = c(aK_ID, aK_Label, yearLabel, unit, stockDataType),
      names_glue = "{aK_Label} ({yearLabel}) _{stockDataType}_",
      values_from = value
    ) %>%
    select(-assessmentKey,-adviceKey, -cS_Basis, -aR_ID) #%>%
    # by(
    #   .$cS_Purpose,
    #   function(x) {
    #     select(x, -cS_Purpose)
    #   }
    # ) %>%
    # unclass()
 
  catch_scenario_table <- catch_scenario_table %>% add_column(Year = catch_scenario_list$assessmentYear + 1, .before = "cS_Label")
  # print(catch_scenario_table)
  # print("1-------------------")
  return(catch_scenario_table)
}
catch_scenario_table <- get_catch_scenario_table(catch_scenario_list)

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
get_catch_scenario_notes <- function(catch_scenario_list) {
 
   catch_scenario_table_notes <- jsonlite::fromJSON(
    URLencode(
      sprintf("https://sg.ices.dk/adviceview/API/getCatchScenariosNotes/%s", catch_scenario_list$adviceKey) # )
    )
  )
  catch_scenario_table_notes <- catch_scenario_table_notes %>% select(-catchOptionsTableKey, -adviceKey)

  string_notes <- HTML(
    paste0("<ul>",paste0("<li><font size=2>",catch_scenario_table_notes$symbol, " "), paste0(catch_scenario_table_notes$notes, "</font></li>"), "</ul>"))
  return(string_notes)
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
  
  # tmp$Year <- 2020 #assesment year + 1
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
  tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  
  # cS_Purpose"
  pattern <- c("cS_Purpose")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$cat <- tmp[,c(subset)]
  tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  
  # Ftotal"
  # pattern <- c("Ftotal", "F_total", "F total", "Total F", "F age", "F")
  pattern <- c("_FTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$F <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(F = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }
  
  # Total catch"
  # pattern <- c("Total catch", "Catch")
  pattern <- c("_CatchTotal_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$TotCatch <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(TotCatch = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }
  
  
  # % TAC change"
  # pattern <- c("% TAC ", "TAC", "TAC ", "% TAC")
  pattern <- c("_TACchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$TACchange <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(TACchange = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }

  # % Advice change"
  # pattern <- c("% Advice change", "Advice change", "% advice change")
  pattern <- c("_Advchange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$ADVICEchange <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(ADVICEchange = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }
  # SSB"
  # pattern <- c("% Advice change", "Advice change", "% advice change")
  pattern <- c("_StockSize_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  # tmp_unified$ADVICEchange <- tmp[,c(subset)]
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(SSB = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }
  # # SSB"
  # pattern <- c(paste0("SSB (", tmp$Year[1]+1, ")"))
  # subset <- which(names(tmp) == pattern)
  # if (length(subset) == 0) {
  #   pattern <- c(paste0("SSB (", tmp$Year[1], ")"))
  #   subset <- which(names(tmp) == pattern)
  # }

  # if (!any(subset)) {
  #     tmp_unified <- tmp_unified %>% add_column(SSB = NA)
  # } else {
  #     tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  # }
  
  # % SSB change "
  # pattern <- c("% SSB change", "SSB change")
  pattern <- c("_StockSizechange_")
  subset <- grepl(paste(pattern, collapse = "|"), names(tmp))
  if (!any(subset)) {
      tmp_unified <- tmp_unified %>% add_column(SSBchange = NA)
  } else {
      tmp_unified <- tmp_unified %>% add_column(tmp[, c(subset)][1])
  }
  
  # print(data.frame(names(tmp_unified)))
  # fwrite(data.frame(names(tmp_unified)), "Data/catch_scen_col_names.csv")
  colnames(tmp_unified) <- sub(" _.*_", "", colnames(tmp_unified))
  # sub(" _.*_", "", a)############################################################ need to run this before saving the names
  fwrite(as.list(names(tmp_unified)), file = "Data/catch_scen_col_names.txt")
# rename columns to standard names
  colnames(tmp_unified) <- c("Year", "cat","cS_Purpose", "F", "TotCatch", "TAC change", "ADVICE change", "SSB", "SSB change")

#   tmp_unified <- tmp_unified %>% do(bind_rows(., data.frame(Year = 2022, cat = "ref", F = 0, TotCatch = 0, TACchange = 0, ADVICEchange = 0, SSBchange = 0, SSB = 0)))
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "BasisAdvice", "Basis Of Advice")
  tmp_unified$cS_Purpose <- str_replace_all(tmp_unified$cS_Purpose, "OtherScenarios", "Other Scenarios")
  # print(tmp_unified)
  return(tmp_unified)
  # tmp3 <- tmp2 %>% relocate("SSB", .before = "SSBchange")
}

# install.packages("gt")
# library(gt)
# tmp_unified %>% gt() %>% tab_header(
#     title = "S&P 500") %>% tab_footnote(
  
#   footnote = paste0(catch_scenario_table_notes$symbol, catch_scenario_table_notes$notes),
#   locations = NULL,
#   placement =  "right"
# )
# catch_scenario_table_st <- standardize_catch_scenario_table(catch_scenario_table)
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
wrangle_catches_with_scenarios <- function(catches_data, catch_scenario_table, stock_name, year) {
  
    catches_data <- catches_data %>% filter(Purpose == "Advice") %>% select(Year, catches)
    catches_data <- catches_data %>% add_column(cat = "Historical Catches")
    catch_scenario_table <- catch_scenario_table %>% select(Year, TotCatch, cat)


    catch_scenario_list_previous_year <- get_Advice_View_info(stock_name, year-1)
    # print(catch_scenario_list_previous_year$adviceValue)

    catches_data <- catches_data %>% mutate(catches = c(catches[-n()], as.numeric(catch_scenario_list_previous_year$adviceValue))) #### this will be substituted by advice value from advice list of previous year

    catches_data_year_before <- catch_scenario_table
    catches_data_year_before$Year <- catch_scenario_table$Year - 1 ## assessmnet year
    # catches_data_year_before$TotCatch <- catches_data$catches[catches_data$Year == 2018]
    catches_data_year_before$TotCatch <- tail(catches_data$catches,1)

    catches_data <- setNames(catches_data, names(catch_scenario_table))
    final_df <- rbind(catches_data, catches_data_year_before, catch_scenario_table)
    # final_df <- rbind(catches_data,  catch_scenario_table)
    final_df <- na.omit(final_df)
    
    return(final_df)
}

