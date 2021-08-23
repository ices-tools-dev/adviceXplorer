#### load the SID list of stocks, filter for cat 1 and then run the function to get ices_areas for each stock # nolint
stock_list_all <- jsonlite::fromJSON(
            URLencode(
                "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021&$select=StockKey, StockKeyLabel, EcoRegion, SpeciesScientificName,  SpeciesCommonName, DataCategory"
            )
        )$value
        #### I'm adding this next line just to check what happens if I subset for only cat1 stocks
        stock_list_all <- stock_list_all  %>% filter(DataCategory == "1")

        ###loop through the stock labels and find the corresponding iceas areas, add these to a column
        for (i in 1:dim(stock_list_all)[1]) {
            stock_list_all$ICES_area[i] <- str_flatten(getStockAreas(stock_list_all$StockKeyLabel[i]), ", ")
        }