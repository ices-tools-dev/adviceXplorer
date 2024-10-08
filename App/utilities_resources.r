#'This function create an HTML output for the Resources tab of the app. It includes the Contact & Feedback (microsoft form)
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
#' make_contact_and_feedback()
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
make_contact_and_feedback <- function() {
  string_citation <- HTML(
    paste0(
      
      "<b>", "<font size=5>", "Contact & Feedback", "</font>", "</b>", "<br/>",
      "<font size=3>",
      "You can contact us via ", "<a href = ", "'mailto: luca.lamoni@ices.dk'", ">email</a>", "<br/>",
      "You can submit an issue to our GitHub ", "<a href='","https://github.com/ices-tools-dev/online-advice/issues", "' target='_blank'>", "repository.","</a><br/>",
      "Please give us your feedback: ","<br/><br/><p align='center'><iframe width='820px' height='580px' src='https://forms.office.com/Pages/ResponsePage.aspx?id=ziCy4DVXaESR3wXK5f8f3DhC6UPgEXpGqPSwaKKiWWRUNDBLOTMwQTdRVTNZQlFET041N0FYWkw3SC4u&embed=true' frameborder='0' marginwidth='0' marginheight='0' style='border: none; max-width:100%; max-height:100vh' allowfullscreen webkitallowfullscreen mozallowfullscreen msallowfullscreen> </iframe>",
      "</font>",  "<br/>"
      )
  )

  return(string_citation)
}


#'This function create an HTML output for the Resources tab of the app. It includes the Data Sources
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
#' make_data_sources()
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
make_data_sources <- function() {
  string_citation <- HTML(
    paste0(
      
      "<p align='left'><b><font size= 5>", "Data sources", "</font>", "</b>", "<br/>",
      "The ICES adviceXplorer diplays data collected from the following sources:",
      "<ul><li><a href='","https://gis.ices.dk/sf/index.html", "' target='_blank'>", "ICES Spatial Facility","</a></li>",
      "<li><a href='","https://www.ices.dk/data/assessment-tools/Pages/stock-information-database.aspx", "' target='_blank'>", "Stock Information Database (SID)","</a></li>",
      "<li><a href='","https://www.ices.dk/data/assessment-tools/Pages/stock-assessment-graphs.aspx", "' target='_blank'>", "Stock Asssessment Graphs (SAG)","</a></li>",
      "<li><a href='","http://asd.ices.dk/AdviceList", "' target='_blank'>", "Advice and Scenarios Database (ASD)","</a></li></ul></font><br/>",
      "<font size= 3>The fish illustrations were courteously provided by Food and Agriculture Organization of the United Nations (FAO), Original Scientific Illustrations Archive. Reproduced with permission</font>"

    )
  )

  return(string_citation)
}

#'This function create an HTML output for the Resources tab of the app. It includes the Data disclaimer and Data policy
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
#' make_data_disclaimer_and_policy()
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
make_data_disclaimer_and_policy <- function() {
  string_citation <- HTML(
    paste0(
      
      "<b>", "<font size=", 5, ">", "Data Disclaimer", "</font>", "</b>", "<br/>",
      "<font size=", 3, ">", "Please be aware that some of the figures, graphs and tables displayed here may not be entirely correct. Please refer to the pdf version for the official advice.", "<br/>",
      "The general ICES Data Disclaimer can be found ","<a href='","https://www.ices.dk/Pages/Disclaimer.aspx", "' target='_blank'>", "here","</a>", "</font>", "<br/>","<br/>",

      "<b>", "<font size=", 5, ">", "Data Policy", "</font>", "</b>", "<br/>",
      "<font size=", 3, ">","Under the revised ", "<a href='","https://www.ices.dk/data/guidelines-and-policy/Pages/ICES-data-policy.aspx", "' target='_blank'>", "ICES Data Policy","</a>",
      " all public data are under the Creative Commons licence ",
      "<a href='","https://creativecommons.org/licenses/by/4.0/", "' target='_blank'>", "(CC BY 4.0).</a><br/><br/>",
      "<img src= 'by.png'", " height= '100px'/><br/><br/>"
    )
  )

  return(string_citation)
}

#'This function create an HTML output for the Resources tab of the app. It includes the Citation
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
#' make_citation()
#' }
#'
#' @references
#'
#' 
#'
#' @export
#' 
make_citation <- function() {
  string_citation <- HTML(
    paste0(

      "<b>", "<font size=", 5, ">", "Citation", "</font>", "</b>", "<br/>",
      "<font size=", 3, ">",
      "Please refer to ", "<a href='","https://www.ices.dk/data/guidelines-and-policy/Pages/ICES-data-policy.aspx", "' target='_blank'>", "ICES Data Policy","</a>", 
      " for full conditions and guidance on citation.<br/>
      When publishing results from the app the minimum citation should include: <br/>
      <br/>
      
      <i><b>ICES adviceXplorer. ", Sys.Date(),". ICES, Copenhagen, Denmark.",
      "<a href='https://ices-taf.shinyapps.io/advicexplorer/?assessmentkey=", getQueryString()$assessmentkey, "&assessmentcomponent=", getQueryString()$assessmentcomponent, 
      "'target='_blank'> https://ices-taf.shinyapps.io/advicexplorer/?assessmentkey=",  getQueryString()$assessmentkey, "&assessmentcomponent=", getQueryString()$assessmentcomponent, "</a>","</b></i></font><br/><br/>"
 
    )
  )

  return(string_citation)
}



