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
# stock_code <- "alf.27.nea"
# year <- 2024
# test <- access_sag_data_local(stock_code, year)
access_sag_data_local <- function(stock_code, year) {
  # out1 <-
  #   lapply(
  #     year,
  #     function(i) {
  #       fread(sprintf("Data/SAG_%s/SAG_summary.csv", i))
  #     }
  #   )
  # SAGsummary <- do.call(rbind, out1)

  # out2 <-
  #   lapply(
  #     year,
  #     function(j) {
  #       fread(sprintf("Data/SAG_%s/SAG_refpts.csv", j))
  #     }
  #   )
  # SAGrefpts <- do.call(rbind, out2)

  # data_sag <- merge(SAGsummary, SAGrefpts) %>% filter(FishStock == stock_code)
  out3 <-
    lapply(
      year,
      function(i) {
        fread(sprintf("Data/SAG_%s/SAG.csv", i))
      }
    )
  SAGsummary <- do.call(rbind, out3)

  data_sag <- SAGsummary %>%
  filter(StockKeyLabel == stock_code) %>% 
    # select(-FishStock) %>%
    # filter(StockPublishNote == "Stock published") %>%
    mutate(across(everything(), ~ if (class(.) == "integer64") as.integer(.) else .))

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

quality_assessment_data_local <- function(stock_code, year, assessmentComponent) {
  years <- c(2024, 2023, 2022, 2021, 2020)
  years <- years[years <= year]
  datalist <- list()

  data_temp <- try(access_sag_data_local(stock_code, years))
  if (isTRUE(class(data_temp) == "try-error")) {
    next
  } else {
    data_temp <- filter(data_temp, between(Year, 2005, 2024))
    
    data_temp <- data_temp %>% select(
      Year,
      Recruitment, RecruitmentAge,
      StockSize, Bpa, Blim, MSYBtrigger, StockSizeDescription, StockSizeUnits,
      FishingPressure, Flim, Fpa, FMSY, FAge, FishingPressureDescription,
      AssessmentYear, Purpose, SAGStamp, AssessmentComponent
    )
    data_temp$AssessmentComponent[data_temp$AssessmentComponent == "" | is.na(data_temp$AssessmentComponent) | data_temp$AssessmentComponent == 0] <- "N.A." # this probably needs to go when they update ASD from "N.A." to NA
    data_temp$RecruitmentAge <- as.character(data_temp$RecruitmentAge)
    data_temp$StockSizeDescription <- as.character(data_temp$StockSizeDescription)
    data_temp$StockSizeUnits <- as.character(data_temp$StockSizeUnits)
    data_temp$FAge <- as.character(data_temp$FAge)
    data_temp$FishingPressureDescription <- as.character(data_temp$FishingPressureDescription)
  }
  # take out non published data from before 2021 in big data
  SAG_data <- filter(data_temp, Purpose == "Advice" & AssessmentComponent == assessmentComponent) %>% distinct()
  # make assessmentYear as factor
  SAG_data$AssessmentYear <- as.factor(SAG_data$AssessmentYear)

  return(SAG_data)
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

#' Downloads the SAG settings for each SAG plot using the SAG web-service.
#'
#' @param assessmentkey
#'
#' @return df of SAG settings
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
getSAGSettings <- function(assessmentkey) {
    sagSettings <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/StockSettings?assessmentKey=%s", assessmentkey)
        )
    )
}

#' Downloads the new version of the SAG summary. (output needs to be formatted)
#'
#' @param assessmentkey
#'
#' @return df of SAG summary
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
getSAGSummary <- function(assessmentkey, combine = TRUE) {
    out <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/SummaryTable?assessmentKey=%s", assessmentkey)
            # sprintf("https://sag.ices.dk/SAG_API/api/StockSettings?assessmentKey=%s", assessmentkey)
        )
    ) 

# drop any null entries (happens when not published stocks creep in)
  out <- out[!sapply(out, is.null)]
    
  # combine tables
  if (length(out) > 1 && combine) {
    # form new column names for combined data frame
    outNames <- unique(unlist(lapply(out, names)))

    # rbind, adding in missing columns as characters
    out1 <-
      do.call(rbind,
        lapply(unname(out), function(x) {
          # are any columns missing?
          missing.cols <- !outNames %in% names(x)
          if (any(missing.cols)) {
            # add on missing columns as characters
            x[outNames[missing.cols]] <- NA
          }
          # reorder columns
          x[outNames]
        }))
        
        # take out rows where all columns are NA
        df2 <- subset(out1,!is.na(out1[,1]))

        # identify the columns that are not "lines" (ie. where values are stored)
        columns.to.add <- grep('lines', names(out), invert = TRUE, value = TRUE)

        # turn list to df and replicate values for the size of df2
        fixed_df <- bind_rows(out[columns.to.add]) %>% slice(rep(1:n(), each = nrow(df2)))

        # combine the 2 df to obtain final SAG data
        final_df <- cbind(fixed_df, df2)

        # renumber the rows
        rownames(final_df) = NULL
    # finally resimplify
    
  } else if (length(out) == 1) {
    final_df <- out[[1]]
  }
return(final_df)
}

#' Creates a download button to be displayed when SAG data can be downloaded.
#'
#' @param outputId
#'
#' @return HTML tag
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
myDownloadButton <- function(outputId){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, NULL, style = "width: 35px; height: 35px; background: url('downloading.png');  background-size: cover; background-position: center; border: 1px solid transparent;")
}



#' Returns a data.frame with ibc and unallocated removals (to be integrated with icesSAG)
#'
#' @param assessmentKey
#'
#' @return data.frame
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
get_additional_landing_data <- function(assessmentKey) {
  out <- jsonlite::fromJSON(
        URLencode(
            sprintf("https://sag.ices.dk/SAG_API/api/SummaryTable?assessmentKey=%s", assessmentKey)            
        )
    )  
  df <- data.frame(Year = out$Lines$Year, ibc = out$Lines$IBC, unallocated_Removals = out$Lines$Unallocated_Removals)
  return(df)
}

#' Returns a link to the replaced advice, if present
#'
#' @param year
#' @param stock_code
#'
#' @return char array
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
get_link_replaced_advice <- function(StockKeyLabel,year) {
  link <- access_sag_data_local(StockKeyLabel, year) %>% filter(Purpose == "Replaced")
  link <- link$LinkToAdvice[1]
  return(link)
}


