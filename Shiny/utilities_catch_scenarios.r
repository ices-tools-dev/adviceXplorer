# Catch scenarios table
########################################################### tranform the sid dataframe


# stock_name <- "cod.27.47d20"

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
# tibble(catch_scenario_table)