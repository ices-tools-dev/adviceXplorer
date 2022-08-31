#' Returns data from the ICES Stock Assessment database.
#'
#' Downloads summary and reference points from the ICES Stock Assessment database for a specific year
#' and places the two csv files in a year-specific folder called "SAG_xxxx" (for example: "SAG_2021")
#'
#' @param year the year required#'
#'
#' @return 2 csv files
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' years <- c(2021, 2020, 2019, 2018, 2017)
#' for (year in years) {
#'    update_SAG(year)
#' }
#' }
#'
#' @references
#'
#' The ICES stock assessment graphs Database web sevices: \url{http://standardgraphs.ices.dk/stockList.aspx}
#' ICES Transparent Assessment Framework: \url{https://taf.ices.dk}
#'
#' @export
#'
options(icesSAG.use_token = FALSE)
update_SAG <- function(year){
    mkdir(paste0("Data/SAG_", year))

    # lookup for assessment key for summary
    sag <-
      getListStocks(year = year) %>%
      select(AssessmentKey, StockKeyLabel, AssessmentYear, Purpose, StockDescription, ModifiedDate, SAGStamp, LinkToAdvice) %>%
      rename(fishstock = StockKeyLabel)

    summary <- load_sag_summary(year) %>%
      left_join(sag, by = c("fishstock", "AssessmentYear", "Purpose"))

    write.taf(summary, file = "SAG_summary.csv", dir = paste0("Data/SAG_", year), quote = TRUE)

    refpts <- load_sag_refpts(year)
    write.taf(refpts, file = "SAG_refpts.csv", dir = paste0("Data/SAG_", year))
}


# internal version of icesFO functions, to run on one year only.
load_sag_summary <- function(year) {
  out <- icesSAG::getSAG(
    stock = NULL, year, data = "summary",
    purpose = "Advice", combine = TRUE
  )
  sid <- load_sid(year)
  sid <- dplyr::filter(sid, !is.na(YearOfLastAssessment))
  sid <- dplyr::select(
    sid, StockKeyLabel, YearOfLastAssessment,
    PreviousStockKeyLabel
  )
  colnames(sid) <- c("fishstock", "AssessmentYear", "PreviousStockKeyLabel")
  old <- dplyr::filter(sid, AssessmentYear < 2017)
  out1 <- merge(out, sid,
    by = c("fishstock", "AssessmentYear"),
    all = FALSE
  )
  out2 <- merge(out, old,
    by.x = c("fishstock", "AssessmentYear"),
    by.y = c("PreviousStockKeyLabel", "AssessmentYear"),
    all = FALSE
  )
  out2$fishstock <- out2$fishstock.y
  out2 <- subset(out2, select = -fishstock.y)
  out <- merge(out1, out2, all = TRUE)
  out <- subset(out, select = -PreviousStockKeyLabel)
  unique(out)
}


load_sag_refpts <- function(year) {
  out <- icesSAG::getSAG(
    stock = NULL, year, purpose = "Advice",
    data = "refpts", combine = TRUE
  )
  sid <- load_sid(year)
  sid <- dplyr::filter(sid, !is.na(YearOfLastAssessment))
  sid <- dplyr::select(
    sid, StockKeyLabel, YearOfLastAssessment,
    PreviousStockKeyLabel
  )
  colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "PreviousStockKeyLabel")
  old <- dplyr::filter(sid, AssessmentYear < 2017)
  out1 <- merge(out, sid,
    by = c("StockKeyLabel", "AssessmentYear"),
    all = FALSE
  )
  out2 <- merge(out, old,
    by.x = c("StockKeyLabel", "AssessmentYear"),
    by.y = c("PreviousStockKeyLabel", "AssessmentYear"),
    all = FALSE
  )
  out2$StockKeyLabel <- out2$StockKeyLabel.y
  out2 <- subset(out2, select = -StockKeyLabel.y)
  out <- merge(out1, out2, all = TRUE)
  out <- subset(out, select = -PreviousStockKeyLabel)
  unique(out)
}
