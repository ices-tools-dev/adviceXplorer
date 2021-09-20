#### load the SID list of stocks, filter for cat 1 and then run the function to get ices_areas for each stock # nolint
stock_list_all <- jsonlite::fromJSON(
            URLencode(
                "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2020&$select=StockKey, StockKeyLabel, EcoRegion, SpeciesScientificName,  SpeciesCommonName, DataCategory"
                # "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021&$select=StockKey, StockKeyLabel, SpeciesScientificName,  SpeciesCommonName, EcoRegion, ExpertGroup, AdviceDraftingGroup, DataCategory, YearOfLastAssessment, AssessmentFrequency, YearOfNextAssessment, AdviceReleaseDate, AdviceCategory, AdviceType, TrophicGuild, FisheriesGuild, SizeGuild, Published"
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
    StockKeyLabel = rep(mydf$StockKeyLabel, sapply(s, length)),
    EcoRegion = unlist(s),
    ICES_area = rep(mydf$ICES_area, sapply(s, length)),
    SpeciesScientificName = rep(mydf$SpeciesScientificName, sapply(s, length)),
    SpeciesCommonName = rep(mydf$SpeciesCommonName, sapply(s, length)),
    DataCategory = rep(mydf$DataCategory, sapply(s, length))    
  )
  # req(EcoRegion_filter)
  # mydf_long <- mydf_long %>% filter(str_detect(EcoRegion, EcoRegion_filter))
  return(mydf_long)
}