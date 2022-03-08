#### Create an array of years fron 2017 to 2021
Year <- c(2017, 2018, 2019, 2020, 2021)
Years <- data.frame(Year)


# this list is temporary, it is just to limit the searches to the stocks already in advice_view
advice_view_stocks <- c(
  "cod.27.5a",
  "cod.21.1",
  "cod.27.47d20",
  "had.27.7a",
  "had.27.6b",
  "had.27.7b-k",
  "had.27.46a20",
  "had.27.1-2",
  "her.27.irls",
  "her.27.20-24",
  "her.27.nirs",
  "her.27.3a47d",
  "nop.27.3a4",
  "ple.27.420",
  "ple.27.7d",
  "ple.27.7a",
  "pok.27.3a46",
  "pok.27.1-2",
  "san.sa.1r",
  "san.sa.2r",
  "san.sa.3r",
  "san.sa.4",
  "sol.27.4",
  "spr.27.3a4",
  "tur.27.4",
  "whg.27.47d",
  "wit.27.3a47d"
)


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
#### create a function to get SID

download_SID <- function(Year) {
  stock_list_all <- jsonlite::fromJSON(
    URLencode(
      sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockDatabaseID, StockKey, StockKeyLabel, SpeciesScientificName,  SpeciesCommonName, EcoRegion, ExpertGroup, AdviceDraftingGroup, DataCategory, YearOfLastAssessment, AssessmentFrequency, YearOfNextAssessment, AdviceReleaseDate, AdviceCategory, AdviceType, TrophicGuild, FisheriesGuild, SizeGuild, Published", Year)
      # sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s", Year)
      # "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2020&$select=StockKey, StockKeyLabel, EcoRegion, SpeciesScientificName,  SpeciesCommonName, DataCategory"
      # "http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq 2021&$select=StockDatabaseID, StockKey, StockKeyLabel, SpeciesScientificName,  SpeciesCommonName, EcoRegion, ExpertGroup, AdviceDraftingGroup, DataCategory, YearOfLastAssessment, AssessmentFrequency, YearOfNextAssessment, AdviceReleaseDate, AdviceCategory, AdviceType, TrophicGuild, FisheriesGuild, SizeGuild, Published"
    )
  )$value
  #### I'm adding this next line just to check what happens if I subset for only cat1 stocks
  stock_list_all <- stock_list_all %>% filter(DataCategory == "1")

  stock_list_all <- stock_list_all %>% filter(StockKeyLabel %in% advice_view_stocks)
  
  
  
  # ### loop through the stock labels and find the corresponding iceas areas, add these to a column
  # for (i in 1:dim(stock_list_all)[1]) {
  #   tryCatch(
  #     { # I inserted this to avoid the loop from crashing, Colin is working on the bug icesVocab::getCodeDetail
  #       # print(i)
  #       stock_list_all$ICES_area[i] <- str_flatten(getStockAreas(stock_list_all$StockKeyLabel[i]), ", ")
  #     },
  #     error = function(e) {}
  #   )
  # }
  return(stock_list_all)
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
### this function separate rows with multiple ecoregions per row to 1 ecoregion per row + filter for the selection of ecoregions
separate_ecoregions <- function(stock_list_all) {
  mydf <- stock_list_all
  s <- strsplit(mydf$EcoRegion, split = ", ")
  # a <- strsplit(mydf$ICES_area, split = ", ")
  mydf_long <- data.frame(
    # StockDatabaseID = rep(mydf$StockDatabaseID, sapply(s, length)),
    # StockKey = rep(mydf$StockKey, sapply(s, length)),
    StockKeyLabel = rep(mydf$StockKeyLabel, sapply(s, length)),
    EcoRegion = unlist(s),
    # ICES_area = rep(mydf$ICES_area, sapply(s, length)),
    # SpeciesScientificName = rep(mydf$SpeciesScientificName, sapply(s, length)),
    SpeciesCommonName = rep(mydf$SpeciesCommonName, sapply(s, length)),
    ExpertGroup = rep(mydf$ExpertGroup, sapply(s, length)),
    # AdviceDraftingGroup = rep(mydf$AdviceDraftingGroup, sapply(s, length)),
    DataCategory = rep(mydf$DataCategory, sapply(s, length)),
    YearOfLastAssessment = rep(mydf$YearOfLastAssessment, sapply(s, length)),
    # AssessmentFrequency = rep(mydf$AssessmentFrequency, sapply(s, length)),
    # YearOfNextAssessment = rep(mydf$YearOfNextAssessment, sapply(s, length)),
    # AdviceReleaseDate = rep(mydf$AdviceReleaseDate, sapply(s, length)),
    AdviceCategory = rep(mydf$AdviceCategory, sapply(s, length)),
    # AdviceType = rep(mydf$AdviceType, sapply(s, length)),
    # TrophicGuild = rep(mydf$TrophicGuild, sapply(s, length)),
    # FisheriesGuild = rep(mydf$FisheriesGuild, sapply(s, length)),
    # SizeGuild = rep(mydf$SizeGuild, sapply(s, length)),
    Published = rep(mydf$Published, sapply(s, length))
  )
  # req(EcoRegion_filter)
  # mydf_long <- mydf_long %>% filter(str_detect(EcoRegion, EcoRegion_filter))
  return(mydf_long)
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
createLink_advice_pdf <- function(StockKeyLabel, AssessmentYear) {
  paste0("<a href='","https://www.ices.dk/sites/pub/Publication%20Reports/Advice/",AssessmentYear,"/", AssessmentYear,"/", StockKeyLabel,".pdf","' target='_blank'>", StockKeyLabel,"</a>")
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
createLink_expert_group <- function(ExpertGroup) {
  # paste0("<a href='","https://www.ices.dk/sites/pub/Publication%20Reports/Advice/",AssessmentYear,"/", AssessmentYear,"/", StockKeyLabel,".pdf","'>", StockKeyLabel,"</a>")
  paste0("<a href='","https://www.ices.dk/community/groups/Pages/", ExpertGroup, ".aspx", "' target='_blank'>", ExpertGroup,"</a>")
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
match_stockcode_to_illustration <- function(StockKeyLabel, df) {
  
  df_temp <- data.frame(matrix(NA, nrow = dim(df)[1], ncol = 1))
  colnames(df_temp) <- "Ill_file"
  
  for (i in 1:dim(df)[1]) {
    temp <- list.files("www", pattern = substr(df$StockKeyLabel[i], 1, 3))
    if (identical(temp, character(0))) {
      temp <- "fish.png"
    }
    # a$stock[i] <- df$StockKeyLabel[i]
    df_temp$Ill_file[i] <- temp
    # print(df$StockKeyLabel[i])
  }
  return(df_temp$Ill_file)
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
sid_table_links <- function(df){
  
  df$icon <- paste0('<img src=', "'", match_stockcode_to_illustration(df$StockKeyLabel, df), "'", ' height=40>') 
  # reference fish icon place holder <a href="https://www.flaticon.com/free-icons/fish" title="fish icons">Fish icons created by vectorsmarket15 - Flaticon</a>
  df$advice_url <- createLink_advice_pdf(df$StockKeyLabel, df$YearOfLastAssessment)
  df$group_url <- createLink_expert_group(df$ExpertGroup)
  return(df)
}
