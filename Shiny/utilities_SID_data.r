#### load the SID list of stocks, filter for cat 1 and then run the function to get ices_areas for each stock # nolint
stock_list_all <- jsonlite::fromJSON(
            URLencode(
                # "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2020&$select=StockKey, StockKeyLabel, EcoRegion, SpeciesScientificName,  SpeciesCommonName, DataCategory"
                "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021&$select=StockDatabaseID, StockKey, StockKeyLabel, SpeciesScientificName,  SpeciesCommonName, EcoRegion, ExpertGroup, AdviceDraftingGroup, DataCategory, YearOfLastAssessment, AssessmentFrequency, YearOfNextAssessment, AdviceReleaseDate, AdviceCategory, AdviceType, TrophicGuild, FisheriesGuild, SizeGuild, Published"
            )
        )$value
        #### I'm adding this next line just to check what happens if I subset for only cat1 stocks
        stock_list_all <- stock_list_all  %>% filter(DataCategory == "1")

        ###loop through the stock labels and find the corresponding iceas areas, add these to a column
        for (i in 1:dim(stock_list_all)[1]) {
          # tryCatch({ # I inserted this to avoid the loop from crashing, Colin is working on the bug icesVocab::getCodeDetail
          # #print(i)
          stock_list_all$ICES_area[i] <- str_flatten(getStockAreas(stock_list_all$StockKeyLabel[i]), ", ")
        # }, error=function(e){})
        }


### this function separate rows with multiple ecoregions per row to 1 ecoregion per row + filter for the selection of ecoregions
separate_ecoregions <- function(stock_list_all) {
  mydf <- stock_list_all
  s <- strsplit(mydf$EcoRegion, split = ", ")
  # a <- strsplit(mydf$ICES_area, split = ", ")
  mydf_long <- data.frame(
    StockDatabaseID = rep(mydf$StockDatabaseID, sapply(s, length)),
    StockKey = rep(mydf$StockKey, sapply(s, length)),
    StockKeyLabel = rep(mydf$StockKeyLabel, sapply(s, length)),
    EcoRegion = unlist(s),
    ICES_area = rep(mydf$ICES_area, sapply(s, length)),
    SpeciesScientificName = rep(mydf$SpeciesScientificName, sapply(s, length)),
    SpeciesCommonName = rep(mydf$SpeciesCommonName, sapply(s, length)),
    ExpertGroup = rep(mydf$ExpertGroup, sapply(s, length)),
    AdviceDraftingGroup = rep(mydf$AdviceDraftingGroup, sapply(s, length)),
    DataCategory = rep(mydf$DataCategory, sapply(s, length)),
    YearOfLastAssessment = rep(mydf$YearOfLastAssessment, sapply(s, length)),
    AssessmentFrequency = rep(mydf$AssessmentFrequency, sapply(s, length)),
    YearOfNextAssessment = rep(mydf$YearOfNextAssessment, sapply(s, length)),
    AdviceReleaseDate = rep(mydf$AdviceReleaseDate, sapply(s, length)),
    AdviceCategory = rep(mydf$AdviceCategory, sapply(s, length)),
    AdviceType = rep(mydf$AdviceType, sapply(s, length)),
    TrophicGuild = rep(mydf$TrophicGuild, sapply(s, length)),
    FisheriesGuild = rep(mydf$FisheriesGuild, sapply(s, length)),
    SizeGuild = rep(mydf$SizeGuild, sapply(s, length)),
    Published = rep(mydf$Published, sapply(s, length))
  )
  # req(EcoRegion_filter)
  # mydf_long <- mydf_long %>% filter(str_detect(EcoRegion, EcoRegion_filter))
  return(mydf_long)
}

stock_list_long <- separate_ecoregions(stock_list_all)


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
