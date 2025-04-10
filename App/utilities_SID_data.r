#### Create an array of years fron 2017 to 2022
# Year <- c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
Year <- seq(from = as.numeric(format(Sys.Date(), "%Y")), to = 2018, by = -1)
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

advice_view_stocks_for_lunch <- c(
"ane.27.8",
# "ane.27.9a",
"ank.27.78abd",
"ank.27.8c9a",
"aru.27.123a4",
"bli.27.nea",
"bll.27.22-32",
"bll.27.3a47de",
"bsf.27.nea",
"bsk.27.nea",
"bwp.27.2729-32",
"cap.27.1-2",
"cod.21.1",
"cod.21.1a-e",
"cod.2127.1f14",
"cod.27.1-2",
# "cod.27.2.coastS",
"cod.27.21",
"cod.27.22-24",
"cod.27.24-32",
"cod.27.47d20",
"cod.27.5a",
# "cod.27.6a",
"cod.27.6b",
# "cod.27.7a",
"cod.27.7e-k",
"dab.27.22-32",
# "dgs.27.nea",
"gfb.27.nea",
# "ghl.27.1-2",
"gug.27.3a47d",
"gug.27.nea",
"gur.27.3-8",
"had.27.1-2",
"had.27.46a20",
# "had.27.5a",
"had.27.5b",
"had.27.7b-k",
"her.27.1-24a514a",
"her.27.20-24",
"her.27.28",
# "her.27.3031",
"her.27.3a47d",
# "her.27.5a",
# "her.27.6aN",
# "her.27.6aS7bc",
"her.27.irls",
# "hke.27.3a468abd",
"hke.27.8c9a",
"hom.27.2a4a5b6a7a-ce-k8",
"hom.27.3a4bc7d",
"hom.27.9a",
"lem.27.3a47d",
# "lex.27.5a6a",
"lin.27.1-2",
"mac.27.nea",
"meg.27.8c9a",
"mon.27.8c9a",
"nep.fu.10",
"nep.fu.3-4",
"nep.fu.32",
"nep.fu.33",
"nep.fu.34",
"nep.fu.5",
"nep.fu.6",
"nep.fu.7",
"nep.fu.8",
"nep.fu.9",
"nep.fu.11",
"nep.fu.12",
"nep.fu.13",
"nep.fu.14",
"nep.fu.15",
"nep.fu.16",
"nep.fu.17",
"nep.fu.19",
"nep.fu.2021",
"nep.fu.22",
"pil.27.7",
# "ple.27.5a",
# "ple.27.7a",
"ple.27.7bc",
"ple.27.7d",
"ple.27.89a",
"pok.27.1-2",
"pok.27.3a46",
"pol.27.89a",
# "por.27.nea",
"raj.27.1012",
# "reb.27.1-2",
"reb.27.14b",
"reg.27.1-2",
"reg.27.561214",
"rjb.27.89a",
"rjc.28.6",
"rjm.27.8",
"san.sa.1r",
"san.sa.2r",
"san.sa.3",
"sbr.27.10",
"sol.27.20-24",
"sol.27.4",
"sol.27.7e",
"sol.27.8ab",
"sol.27.8c9a",
"spr.27.3a4",
"spr.27.22-32",
"spr.27.7de",
"syt.27.67",
# "thr.27.nea",
"tsu.27.nea",
"tur.27.3a",
"tur.27.4",
"usk.27.6b",
"whb.27.1-91214",
"whg.27.3a",
"whg.27.47d",
"whg.27.7b-ce-k",
"wit.27.3a47d"
)

adviceXplorer_stocks_to_exclude_for_lunch <- c("cod.27.1-2", 
                                                "ghl.27.1-2", 
                                                "had.27.1-2", 
                                                "reb.27.1-2",  
                                                "cap.27.1-2",
                                                "nep.fu.10",
                                                "pra.27.1-2"
)

#' Downloads the list of stocks from SID for a particular year using a web service
#'
#' @param Year
#'
#' @return stock_list_all (the list of stocks)
#'
#' @note
#' In the webservice string we can already subset the SID table for the columns we are interested in
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' df <- download_SID(2021)
#' }
#'
#' @references
#'https://sid.ices.dk/Default.aspx


download_SID <- function(Year) {
  stock_list_all <- jsonlite::fromJSON(
    URLencode(
      sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel, StockKeyDescription, SpeciesCommonName, EcoRegion, DataCategory, YearOfLastAssessment, AssessmentFrequency, YearOfNextAssessment, AssessmentKey", Year)
    )
  )$value
  # stock_list_all <- stock_list_all %>% filter(!StockKeyLabel %in% adviceXplorer_stocks_to_exclude_for_lunch)
  return(stock_list_all)
}

#' This function unlists the Ecoregion column cells so that each row of the resulting df will correspond to one ecoregion only
#'
#' @param stock_list_all (list of stocks returned by download_SID(Year))
#'
#' @return mydf_long (a df in whitch each row corresponds to one ecoregion only)
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' separate_ecoregions(df)
#' }
#'
#' @references
#' 
separate_ecoregions <- function(stock_list_all) {
  stock_list_all %>%
    mutate(EcoRegion = as.character(EcoRegion)) %>%
    separate_rows(EcoRegion, sep = ", ")
}

#' Get location description of stock codes from full SID description
#'
#' The full description of a stock includes name and Latin name, plus Area and Description 
#' codes that are not understandable to all. 
#' This function extracts the location in english names from the description
#' 
#' @param stock_description 
#'
#' @return stock_location
#' @export
#'
#' @examples 
#' stock_description_string <- "Brill (Scophthalmus rhombus) in Subarea 4 and divisions 3.a and 7.d-e (North Sea, Skagerrak and Kattegat, English Channel)"
#' parse_location_from_stock_description(stock_description_string
#' 
parse_location_from_stock_description <- function(stock_description) {
  
  location_with_codes <- stringr::str_extract(pattern = "(?<=in ).*", stock_description)
  stock_location <- stringr::str_extract_all(string = location_with_codes, pattern = "(?<=\\().+?(?=\\))") %>% 
    purrr::map_chr(.f = ~ {stringr::str_c(.x, collapse = ", ")})
}

# #' Returns an HTML string to provide the hyperlink to the expert group page withing the list of stocks table
# #'
# #' @param ExpertGroup
# #'
# #' @return HTML string
# #'
# #' @note
# #' Can add some helpful information here
# #'
# #' @seealso
# #'
# #' @examples
# #' \dontrun{
# #' 
# #' }
# #'
# #' @references

# createLink_expert_group <- function(ExpertGroup) {
  
#   paste0("<a href='","https://www.ices.dk/community/groups/Pages/", ExpertGroup, ".aspx", "' target='_blank'>", ExpertGroup,"</a>")
# }

#' Returns a df column with an added column which includes the matching name of the fish drawing files 
#'
#' @param StockKeyLabel (stock code)
#' @param df (stock list df)
#'
#' @return df_temp$Ill_file
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' match_stockcode_to_illustration("wit.27.3a47d", df)
#' }
#'
#' @references
#'

match_stockcode_to_illustration <- function(StockKeyLabel, df) {
  sapply(StockKeyLabel, function(key) {
    temp <- list.files("www", pattern = substr(key, 1, 3))
    if (length(temp) == 0) "fish.png" else temp[1]
  })
}
#' Returns the HTML string to create the hyperlink to the SAG database
#'
#' @param assessmentKey
#'
#' @return string
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' createLink_SAG_db(15609)
#' }
#'
# createLink_SAG_db <- function(assessmentKey) {
#   SAG_string <- paste0("<a href='","https://standardgraphs.ices.dk/ViewCharts.aspx?key=", assessmentKey,"' target='_blank'>",
#   "<img src= 'database.png'", " height= '30px'/>", "</a>")
#   return(SAG_string)
# }


#' Returns a javascript string that allows to pre-select the the first radio-button of the filtered stock list table.
#'
#' @param df
#'
#' @return stringjs
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' callback1(df)
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

#' Returns a javascript string that allows to select a stock via the radio-button of the filtered stock list table.
#'
#' @param df
#'
#' @return stringjs
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' callback1(df)
#' }
#'
#' @references
#'
#' 
#'
#' @export
#'
callback <- c(
  "$('input[name=rdbtn]').on('click', function(){",
  "  var value = $('input[name=rdbtn]:checked').val();",
  "  Shiny.setInputValue('rdbtn', value);",
  "});"
)

#' Returns a the url of the pdf of the advice in the new library
#'
#' @param assessmentKey
#'
#' @return stringjs
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' }
#'
#' @references
#'
#' 
#'
#' @export
#'
get_advice_doi <- function(assessmentKey) {
  url <- URLencode(
    paste0("https://sag.ices.dk/SAG_API/api/AdviceLink/", assessmentKey)
  )
  doi <- getURL(url, followlocation = TRUE)
  return(doi)
}

