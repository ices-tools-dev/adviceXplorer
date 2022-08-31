options(icesSAG.use_token = FALSE)


#' Downloads SAG data using icesSAG library and web services
#'
#' @param stock_name
#' @param year
#'
#' @return an aggregated dataframe which includes SAGsummary and SAG reference points
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#'access_sag_data("wit.27.3a47d", 2019)
#' }
#'
#' @references
#'
#'
#'
#' @export
#'
access_sag_data <- function(stock_code, year) {

    # Dowload the data
    SAGsummary <- getSAG(stock_code, year,
        data = "summary", combine = TRUE, purpose = "Advice"
    )
    SAGrefpts <- getSAG(stock_code, year,
        data = "refpts", combine = TRUE, purpose = "Advice"
    )

    data_sag <- cbind(SAGsummary, SAGrefpts)
    data_sag <- subset(data_sag, select = -fishstock)
    data_sag <- filter(data_sag, StockPublishNote == "Stock published")
    
}

#' Reads SAG data that is stored locally
#'
#' @param stock_name
#' @param year
#'
#' @return an aggregated dataframe which includes SAG summary and SAG reference points
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#'access_sag_data_local("wit.27.3a47d", 2019)
#' }
#'
#' @references
#'
#'
#'
#' @export
#'
access_sag_data_local <- function(stock_code, year) {
  browser()
    # Dowload the data
    df_summary <- fread(sprintf("Data/SAG_%s/SAG_summary.csv", year)) ####there is a space after SAG_ fix this below
    SAGsummary <- df_summary %>% filter(fishstock == stock_code)

    df_refpts <- fread(sprintf("Data/SAG_%s/SAG_refpts.csv", year)) ####there is a space after SAG_ fix this below
    SAGrefpts <- df_refpts %>% filter(StockKeyLabel == stock_code)

    data_sag <- merge(SAGsummary, SAGrefpts)
    data_sag <- data_sag %>% select(-fishstock) %>% filter(StockPublishNote == "Stock published")
  
    return(data_sag)
}



#' Reads SAG data stored locally for multiple years prior to the year provided (ex year = 2019, years of data returned = c(2017,2018,2019))
#'
#' @param stock_name
#' @param year
#' 
#' @return an aggregated dataframe of SAG data from different years
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso access_sag_data_local()
#'
#' @examples
#' \dontrun{
#'quality_assessment_data_local("wit.27.3a47d", 2021)
#' }
#'
#' @references
#'
#'
#'
#' @export
#'
# function to dowload the quality assessemnt data
quality_assessment_data_local <- function(stock_code, year){

years <- c(2022, 2021, 2020, 2019, 2018)
years <- years[years <= year]
datalist = list()

for (year in years) {
    
    data_temp <- try(access_sag_data_local(stock_code, year)) 

    if (isTRUE(class(data_temp) == "try-error")) {
        next
    } else {
      
        data_temp <- filter(data_temp, between(Year, 2005, 2022))
        data_temp <- data_temp %>% select(Year,
                                            recruitment, RecruitmentAge,
                                            SSB, Bpa, Blim, MSYBtrigger, stockSizeDescription, stockSizeUnits,
                                            F, FLim, Fpa, FMSY, Fage, fishingPressureDescription,
                                            AssessmentYear, StockPublishNote,Purpose, SAGStamp)

        data_temp$RecruitmentAge <- as.character(data_temp$RecruitmentAge)
        data_temp$stockSizeDescription <- as.character(data_temp$stockSizeDescription)
        data_temp$ stockSizeUnits <- as.character(data_temp$ stockSizeUnits)
        data_temp$Fage <- as.character(data_temp$Fage)
        data_temp$fishingPressureDescription <- as.character(data_temp$fishingPressureDescription)

        datalist[[year]] <- data_temp
    }
}

#print(tibble(datalist))
### bind data in unique df
big_data <- dplyr::bind_rows(datalist)  ####################probem is with this function

# find last asseement year
# last_year <- tail(big_data$AssessmentYear, n=1)

# subset last year
# big_data_last_year <- big_data  %>% filter(AssessmentYear == last_year)

# take out non published data from before 2021 in big data
big_data <- filter(big_data, StockPublishNote == "Stock published")
big_data <- filter(big_data, Purpose == "Advice")
# put together the published data from before 2021 with the unpublished from 2021
# big_data <- rbind(big_data, big_data_last_year)
big_data <- big_data  %>% distinct()

#make assessmentYear as factor
big_data$AssessmentYear <- as.factor(big_data$AssessmentYear)
# big_data_last_year$AssessmentYear <- as.factor(big_data_last_year$AssessmentYear)

# df_list <- list(big_data, big_data_last_year)
# print(big_data)
return(big_data)
}





#' Function for getting ices_areas for each stock
#'
#' @param stock_name
#'
#' @return the list of ICES areas for a particular stock
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
getStockAreas <- function(stockCode) {
  details <- getCodeDetail(code = stockCode, code_type = "ICES_StockCode")
  areas <- details$children$codes[details$children$code_types$Key == "ICES_Area", ]
  areas$Key
}


getSAGSettings <- function(assessmentkey) {
    sagSettings <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/StockSettings?assessmentKey=%s", assessmentkey)
        )
    )
}

getSAGSummary <- function(assessmentkey) {
    sagSummary <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/SummaryTable?assessmentKey=%s", assessmentkey)
            # sprintf("https://sag.ices.dk/SAG_API/api/StockSettings?assessmentKey=%s", assessmentkey)
        )
    )    
}


# sagSummary <- flatten(sagSummary)

#     sagSummary$assessmentKey <- rep(sagSummary$assessmentKey, length(sagSummary$lines$year))
#     sagSummary$stockPublishNote <- rep(sagSummary$stockPublishNote, length(sagSummary$lines$year))
#     sagSummary$purpose <- rep(sagSummary$purpose, length(sagSummary$lines$year))
#     sagSummary$fAge <- rep(sagSummary$fAge, length(sagSummary$lines$year))
#     sagSummary$fishStock <- rep(sagSummary$fishStock, length(sagSummary$lines$year))
#     sagSummary$recruitmentAge <- rep(sagSummary$recruitmentAge, length(sagSummary$lines$year))
#     sagSummary$assessmentYear <- rep(sagSummary$assessmentYear, length(sagSummary$lines$year))
#     sagSummary$units <- rep(sagSummary$units, length(sagSummary$lines$year))
#     sagSummary$stockSizeDescription <- rep(sagSummary$stockSizeDescription, length(sagSummary$lines$year))
#     sagSummary$stockSizeUnits <- rep(sagSummary$stockSizeUnits, length(sagSummary$lines$year))
#     sagSummary$fishingPressureDescription <- rep(sagSummary$fishingPressureDescription, length(sagSummary$lines$year))
#     sagSummary$fishingPressureUnits <- rep(sagSummary$fishingPressureUnits, length(sagSummary$lines$year))

#     sagSummaryf <- do.call(cbind.data.frame, sagSummary)
# SAGsummary <- getSAG("had.27.46a20", 2021,
#         data = "summary", combine = TRUE, purpose = "Advice"
#     )
# sagSummary$lines$ibc

# library(tidyverse)
# flatten(sagSummary)
# length(sagSummary$lines$year)
# rep(sagSummary$assessmentKey,50)

# sagSummary %>% 
# mutate(Value = map(Value, as.data.frame),
#          Value = map(Value, rownames_to_column, 'a'),
#          Value = map(Value, ~gather(., assessmentKey, lines, -a))) %>% 
#   unnest(Value) %>% 
#   complete(Step, a, b)