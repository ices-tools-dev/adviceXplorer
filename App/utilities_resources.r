#'This function create an HTML output for the Resources tab of the app. It includes the Contact & Feedback (microsoft form),
#' Data Sources, Data disclaimer, Citation, Data policy and Resources
#' 
#' @param null
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
#' make_app_citation()
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
      "You can submit an issue to our GitHub ", "<a href='","https://github.com/ices-tools-dev/online-advice/issues", "' target='_blank'>", "repository.","</a><br/>",
      "Please give us your feedback: ","<br/><br/><p align='center'><iframe width='640px' height='480px' src='https://forms.office.com/Pages/ResponsePage.aspx?id=ziCy4DVXaESR3wXK5f8f3DhC6UPgEXpGqPSwaKKiWWRUNDBLOTMwQTdRVTNZQlFET041N0FYWkw3SC4u&embed=true' frameborder='0' marginwidth='0' marginheight='0' style='border: none; max-width:100%; max-height:100vh' allowfullscreen webkitallowfullscreen mozallowfullscreen msallowfullscreen> </iframe>",
      "</font>",  "<br/>",
      br(),

      "<p align='left'><b>", "<font size=", 5, ">", "Data sources", "</font>", "</b>", "<br/>",
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
      "<img src= 'by.png'", " height= '100px'/><br/><br/>",

      "<b>", "<font size=", 5, ">", "Resources used in this site", "</font></b><br/>",
      "<font size=", 2, ">",
      "Fish illustrations: Food and Agriculture Organization of the United Nations, Original Scientific Illustrations Archive. Reproduced with permission, <br/>",
      "<a href='https://www.flaticon.com/free-icons/fish' title='fish icons'>Fish icons created by vectorsmarket15 - Flaticon,</a><br/>",
      "<a href='https://www.flaticon.com/free-icons/pdf' title='pdf icons'>Pdf icons created by Creative Stall Premium - Flaticon,</a><br/>",
      "<a href='https://www.flaticon.com/free-icons/database' title='database icons'>Database icons created by srip - Flaticon,</a><br/>",
      "<a href='https://www.flaticon.com/free-icons/seafood' title='seafood icons'>Seafood icons created by iconixar - Flaticon,</a><br/>",
      "<a href='https://www.flaticon.com/free-icons/map' title='map icons'>Map icons created by Freepik - Flaticon,</a><br/>",
      "<a href='https://www.flaticon.com/free-icons/info' title='info icons'>Info icons created by Stockio - Flaticon,</a><br/>",
      "<a href='https://www.flaticon.com/free-icons/calendar' title='calendar icons'>Calendar icons created by Freepik - Flaticon,</a><br/>",
      "<a href='https://www.flaticon.com/free-icons/link' title='link icons'>Link icons created by Uniconlabs - Flaticon,</a><br/>",
      "<a href='https://www.flaticon.com/free-icons/download' title='download icons'>Download icons created by kosonicon - Flaticon</a>",
      "</font>"
      
      
     

    )
  )

  return(string_citation)
}

#' Creates a date that should represent the last date the app was updated. (in development)
#' 
#' @param null
#'
#' @return HTML string
#'
#' @note
#' Can add some helpful information here
#'
#' @seealso
#'
#' @examples
#' \dontrun{
#' make_app_update_date()
#' }
#'
#' @references
#'
#' 
#'
#' @export
make_app_update_date <- function() {
  string_update <- HTML(
    paste0("<font size= 2><i>App last update: ", Sys.Date(),"</i></font>")
  )

}