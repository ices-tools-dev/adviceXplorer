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
make_app_citation <- function() {
  string_citation <- HTML(
    paste0(
      
      "<b>", "<font size=", 5, ">", "Contact & Feedback", "</font>", "</b>", "<br/>",
      "<font size=", 3, ">",
      "You can contact us via ", "<a href = ", "'mailto: luca.lamoni@ices.dk'", ">email</a>", "<br/>",
      "You can submit an issue to our GitHub ", "<a href='","https://github.com/ices-tools-dev/online-advice/issues", "' target='_blank'>", "repository.","</a>",
      "</font>",  "<br/>",
      br(),

      "<b>", "<font size=", 5, ">", "Data sources", "</font>", "</b>", "<br/>",
      "The ICES Online Advice Shiny app diplays data collected from the following sources:
      <ul><li>", a("GIS", href = "https://gis.ices.dk/sf/index.html"), "</li>",
      "<li>", a("SID", href = "https://www.ices.dk/data/assessment-tools/Pages/stock-information-database.aspx"), "</li>",
      "<li>", a("SAG", href = "https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx"), "</li>",
      "<li>",  a("Advice View", href = "https://sg.ices.dk/adviceview/Account/Login?ReturnUrl=%2Fadviceview%2Fmanage"), "</li></ul>", "</font>",
      "<br/>",

      "<b>", "<font size=", 5, ">", "Data Disclaimer", "</font>", "</b>", "<br/>",
      "<font size=", 3, ">", "The ICES Online Advice is ", "<b>", "in development",  "</b>", 
      ". Please be aware that some of the figures, graphs and links displayed here may not be correct. Please refer to the pdf version for the official advice.", "<br/>",
      "The general ICES Data Disclaimer can be found ","<a href='","https://www.ices.dk/Pages/Disclaimer.aspx", "' target='_blank'>", "here","</a>", "</font>", "<br/>",
      "<br/>",

      "<b>", "<font size=", 5, ">", "Citation", "</font>", "</b>", "<br/>",
      "<font size=", 3, ">",
      "Please refer to ", "<a href='","https://www.ices.dk/data/guidelines-and-policy/Pages/ICES-data-policy.aspx", "' target='_blank'>", "ICES Data Policy","</a>", 
      " for full conditions and guidance on citation.<br/>
      When publishing results from the app the minimum citation should include: <br/>
      <br/>

      <i><b>ICES Single-Stock Online Advice (https://ices-taf.shinyapps.io/online-single-stock-advice/?assessmentkey=", getQueryString(),"), ", Sys.Date(),". ICES, Copenhagen.</b></i></font><br/><br/>",
      # "International Council for the Exploration of the Sea (ICES). (", Sys.Date(), "). ICES Single-Stock Online Advice.
      # https://ices-taf.shinyapps.io/online-single-stock-advice/?assessmentkey=", getQueryString(), "</font><br/>",
      # br(),
      
      "<b>", "<font size=", 5, ">", "Data Policy", "</font>", "</b>", "<br/>",
      "<font size=", 3, ">",
      
      "Under the revised ", "<a href='","https://www.ices.dk/data/guidelines-and-policy/Pages/ICES-data-policy.aspx", "' target='_blank'>", "ICES Data Policy","</a>",
      " all public data are under the Creative Commons licence ",
      "<a href='","https://creativecommons.org/licenses/by/4.0/", "' target='_blank'>", "(CC BY 4.0).</a><br/><br/>",
      # img(src = "by.png", height = "100px"),
      "<img src= 'by.png'", " height= '100px'/>"
      
      
     

    )
  )

  return(string_citation)
}