#### Create an array of years fron 2017 to 2022
Year <- c(2018, 2019, 2020, 2021, 2022)
Years <- data.frame(Year)


# this list is temporary, it is just to limit the searches to the stocks already in advice_view
advice_view_stocks <- c(
  "bll.27.3a47de",
  # "cod.27.5a",
  # "cod.21.1",
  "cod.27.47d20",
  "dab.27.3a4",
  "gug.27.3a47d",
  "fle.27.3a4",
  # "had.27.7a",
  # "had.27.6b",
  # "had.27.7b-k",
  "had.27.46a20",
  # "had.27.1-2",
  # "her.27.irls",
  # "her.27.20-24",
  # "her.27.nirs",
  # "her.27.3a47d",
  "lem.27.3a47d",
  "mur.27.3a47d",
  "nop.27.3a4",
  "ple.27.420",
  "ple.27.7d",
  # "ple.27.7a",
  "pok.27.3a46",
  # "pok.27.1-2",
  "pol.27.3a4",
  # "san.sa.1r",
  # "san.sa.2r",
  # "san.sa.3r",
  # "san.sa.4",  
  "sol.27.4",
  "sol.27.7d",
  # "spr.27.3a4",
  "tur.27.3a",
  "tur.27.4",
  "whg.27.3a",
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

download_SID <- function(Year) {
  stock_list_all <- jsonlite::fromJSON(
    URLencode(
      sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockDatabaseID, StockKey, StockKeyLabel, SpeciesScientificName,  SpeciesCommonName, EcoRegion, ExpertGroup, AdviceDraftingGroup, DataCategory, YearOfLastAssessment, AssessmentFrequency, YearOfNextAssessment, AdviceReleaseDate, AdviceCategory, AdviceType, TrophicGuild, FisheriesGuild, SizeGuild, Published, AssessmentKey", Year)
    )
  )$value
  stock_list_all <- stock_list_all %>% filter(StockKeyLabel %in% advice_view_stocks)
  return(stock_list_all)
}

#' Arrange ecoregions onto one row each, then filter
#' this function separate rows with multiple ecoregions per row to 1 ecoregion per row + filter for the selection of ecoregions
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
### 
separate_ecoregions <- function(stock_list_all) {
  mydf <- stock_list_all
  s <- strsplit(mydf$EcoRegion, split = ", ")
  mydf_long <- data.frame(
    StockKeyLabel = rep(mydf$StockKeyLabel, sapply(s, length)),
    EcoRegion = unlist(s),
    SpeciesCommonName = rep(mydf$SpeciesCommonName, sapply(s, length)),
    ExpertGroup = rep(mydf$ExpertGroup, sapply(s, length)),
    DataCategory = rep(mydf$DataCategory, sapply(s, length)),
    YearOfLastAssessment = rep(mydf$YearOfLastAssessment, sapply(s, length)),
    AdviceCategory = rep(mydf$AdviceCategory, sapply(s, length)),
    AssessmentKey = rep(mydf$AssessmentKey, sapply(s, length))
  )
  
  return(mydf_long)
}


#
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
    
    df_temp$Ill_file[i] <- temp
    
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
createLink_SAG_db <- function(assessmentKey) {
  paste0("<a href='","https://standardgraphs.ices.dk/ViewCharts.aspx?key=", assessmentKey,"' target='_blank'>",
  "<img src= 'database.png'", " height= '30px'/>", "</a>")
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
createLink_visa_tool <- function(assessmentKey) {
  paste0("<a href='","https://gis.ices.dk/sf/index.html?widget=visa&assessmentKey=", assessmentKey,"' target='_blank'>",
  "<img src= 'map.png'", " height= '30px'/>", "</a>")
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
  df <- createLink_advice_pdf(df)
  df$group_url <- createLink_expert_group(df$ExpertGroup)
  df$SAG_url <- createLink_SAG_db(df$AssessmentKey)
  df$visa_url <- createLink_visa_tool(df$AssessmentKey)
  
  return(df)
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

createLink_advice_pdf <- function(df) {
  
  
  advice_DOI_data <- fread("Data/DOI/single_stock_2017_15.8.2022.csv", 
                            header = TRUE, 
                            col.names = c("Stock_code", "Year", "Publication_date", "old_pdf_link", "doi"))

  FO_DOI_data <- fread("Data/DOI/overviews_2017_4.5.2022.csv", 
                            header = TRUE, 
                            col.names = c("Year", "Publication_date", "Advice_product", "Ecoregion", "Title", "old_pdf_link", "doi"))
  
  for (i in 1:dim(df)[1]) {
    list_doi <- advice_DOI_data %>% filter(Year == df$YearOfLastAssessment[i]) %>% 
                                    filter(Stock_code == df$StockKeyLabel[i])
    list_doi <- list_doi$doi


    FO_doi <- FO_DOI_data %>%
      filter(Advice_product == "Fisheries Overviews") %>%
      filter(Year == df$YearOfLastAssessment[i]) %>%
      filter(str_detect(df$EcoRegion[i], Ecoregion))
    FO_doi <- FO_doi$doi


    if (identical(list_doi, character(0))) {
      list_doi <- "not_available"
    } else if (identical(FO_doi, character(0))) {
      FO_doi <- "not_available"
    }

    list_doi <- strsplit(list_doi, "\\s+")
    FO_doi <- strsplit(FO_doi, "\\s+")


    if (length(list_doi) > 1) {
      list_doi <- list_doi[[length(list_doi)]]
    } else if (length(FO_doi) > 1) {
      FO_doi <- FO_doi[[length(FO_doi)]]
    } else {
      list_doi <- list_doi
      FO_doi <- FO_doi
    }
    list_doi <- paste0("<a href='", list_doi, "' target='_blank'>", "<img src= 'pdf-file.png'", " height= '30px'/>", "</a>")
    FO_doi <- paste0("<a href='", FO_doi, "' target='_blank'>", "<img src= 'seafood.png'", " height= '30px'/>", "</a>")

    df$doi[i] <- list_doi
    df$FO_doi[i] <- FO_doi
  }

  return(df)
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

callback1 <- function(df) {
  value_rdbtn_to_preSelect <- paste0("rdbtn_", readr::parse_number(df$Select[1]))
  stringjs <- paste0("$('input[name=rdbtn]').on('click', function(){ var value = $('input[name=rdbtn]:checked').val(); Shiny.setInputValue('rdbtn', value); }); var btn = document.querySelectorAll('[value=", value_rdbtn_to_preSelect, "]')[0].click(); btn.checked=true;")
  return(stringjs)
}