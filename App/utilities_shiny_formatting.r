#' Creates the UI element for the left side of the stock selection tab, which includes
#' the ecoregion map and the additional filterinr panel
#'
#' @return UI element
#'
#' @note
#' 
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
stock_selection_left_side <- function() {
  sidebarPanel(
    width = 5,
    tabPanel(
      "ICES Ecoregions",
      tags$style(type = "text/css", "#map1 {height: calc(62vh - 220px) !important;} overflow-y: auto;"),
      withSpinner(leafletOutput("map1", height = "100%", width = "100%"))
    ),
    HTML("</br>"),
    panel(
      selectizeInput(
        inputId = "selected_locations",
        label = "ICES Ecoregions",
        choices = sort(shape_eco$Ecoregion),
        selected = "Greater North Sea",
        multiple = FALSE,
        width = "100%",
        options = list(
          placeholder = "Select Ecoregion(s)"
        )
      ),
      selectizeInput(
        inputId = "selected_years",
        label = "Assessment Year",
        choices = Years$Year,
        selected = 2022,
        multiple = FALSE,
        width = "100%",
        options = list(
          placeholder = "Select assessment year"
        )
      ),
      selectizeGroupUI(
        id = "my-filters",
        params = list(
          StockKeyLabel = list(inputId = "StockKeyLabel", title = "Stock code:"),
          SpeciesCommonName = list(inputId = "SpeciesCommonName", title = "Common name:")
        ),
        inline = FALSE
      ),
      heading = "Additional data filtering",
      status = "primary"
    ),
    htmlOutput("app_last_update")
  )
}

#' Creates the UI element for the right side of the stock selection tab, which includes
#' the stock list table
#'
#' @return UI element
#'
#' @note
#' 
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
stock_selection_right_side <- function(){
  mainPanel(
    width = 7, style = "max-height: 90vh; overflow-y: auto; ",#margin: auto;
    withSpinner(DTOutput("tbl"))
  )
}


################################## SAG plots tab

#' Creates the UI element of the left panel of the "Stock dev over time" tab,
#' which includes the landings and F plots
#'
#' @return UI element
#'
#' @note
#' 
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
SAG_plots_left_panel <- function(){
  sidebarPanel(
    width = 6, style = "height: 65vh; overflow-y: auto;",
    panel(
      title = "Catches",
        withSpinner(plotlyOutput("plot1", height = "100%", width = "100%"))
    ),
    panel(
      title = "F",
        withSpinner(plotlyOutput("plot3", height = "100%", width = "100%"))
    )
  )
}


#' Creates the UI element of the right panel of the "Stock dev over time" tab,
#' which includes the recruitment and SSB plots.
#'
#' @return UI element
#'
#' @note
#' 
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
SAG_plots_right_panel <- function(){
  sidebarPanel(
    width = 6, style = "height: 65vh; overflow-y: auto;",
    panel(
      title = "Recruitment",
      fillPage(
        tags$style(type = "text/css", "#plot2  overflow-y: auto; !important;}"), #{height: calc(5vh - 10px); width:calc(100vw - 10px)
        withSpinner(plotlyOutput("plot2", height = "100%", width = "100%"))
      )
    ),
    panel(
      title = "SSB",
      fillPage(
        tags$style(type = "text/css", "#plot4  overflow-y: auto; !important;}"), # {height:calc(50vh - 10px); width: calc(100vw - 10px)
        withSpinner(plotlyOutput("plot4", height = "100%", width = "100%"))
      )
    )
  )
}
##############################################Quality of assessment tab

#' Creates the UI element of the quality of assessment plots
#'
#' @return UI element
#'
#' @note
#' 
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
quality_of_assessment <- function(){
  splitLayout(
    style = "border: 1px solid silver; height: 80vh; overflow-y: auto;",  
    cellWidths = c("33%", "33%", "33%"),
    cellArgs = list(style = "padding: 6px"),
    panel(
      title = "5",
      fillPage(
        tags$style(type = "text/css", "#plot5  overflow-y: auto; !important;}"), # {height: calc(5vh - 10px); width:calc(100vw - 10px)
        withSpinner(plotlyOutput("plot5", height = "100%", width = "100%"))
      )
    ),
    panel(
      title = "6",
      fillPage(
        tags$style(type = "text/css", "#plot6  overflow-y: auto; !important;}"), # {height: calc(5vh - 10px); width:calc(100vw - 10px)
        withSpinner(plotlyOutput("plot6", height = "100%", width = "100%"))
      )
    ),
    panel(
      title = "7",
      fillPage(
        tags$style(type = "text/css", "#plot7  overflow-y: auto; !important;}"), # {height: calc(5vh - 10px); width:calc(100vw - 10px)
        withSpinner(plotlyOutput("plot7", height = "100%", width = "100%"))
      )
    )
  )
}
####################################### Advice tab

#' Creates the UI element of left panel of the Advice tab, which includes 
#' the F/SSB/Catches plot, the historical catches plot, the radial plot
#' and the lollipop plot.
#' 
#' @return UI element
#'
#' @note
#' 
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
catch_scenarios_left_panel <- function() {
  sidebarPanel(
    width = 6, style = "height: 65vh; overflow-y: auto;",
    panel(
      title = "Catch_scenario_F_SSB",
      fillPage(
        tags$style(type = "text/css", "#catch_scenario_plot_3  overflow-y: auto; !important;}"), # {height:calc(50vh - 10px); width: calc(100vw - 10px)
        withSpinner(plotlyOutput("catch_scenario_plot_3", height = "30%", width = "100%"))
      )
    ),
    panel(
      tabsetPanel(
        tabPanel(
          "Catch time series",
          uiOutput("catch_scenarios"),
          withSpinner(plotlyOutput("TAC_timeline", height = "100%", width = "100%"))
        ),
        tabPanel(
          "% of change: radial plot",
          uiOutput("catch_scenarios_radial"),
          withSpinner(plotlyOutput("Radial_plot", height = "100%", width = "100%"))
        ),
        tabPanel(
          "% of change: lollipop plot",
          uiOutput("catch_indicators_lollipop"),
          withSpinner(plotlyOutput("Lollipop_plot", height = "100%", width = "100%"))
        )
      )
    )
  )
}


#' Creates the UI element of right panel of the Advice tab, which includes 
#' the headline of advice, catch scenario table and its footnotes.
#' 
#' @return UI element
#'
#' @note
#' 
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
catch_scenarios_right_panel <- function(){
  sidebarPanel(
    width = 6, style = "height: 65vh; overflow-y: auto;",
    
    panel(
      fillPage(
        tags$style(type = "text/css", "#table overflow-y: auto; !important;"), #{height: calc(80vh - 10px); calc(100vw - 10px)}
        withSpinner(DTOutput("table", height = "90%", width = "100%")),
        htmlOutput("footnotes", height = "90%", width = "100%")
      )
    )
  )
}


#' Creates the UI element of left panel of the header, providing
#' the stock info.
#' 
#' @return UI element
#'
#' @note
#' 
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
header_left_panel_stock_info <- function(id) {
  wellPanel(
    style = "height: fit-content; overflow-y: auto; white-space: normal;",
    panel(
      title = "Stock Info",
      withSpinner(htmlOutput(id, height = "100%", width = "100%"))
    )
  )
}



#' Creates the UI element right panel of the header, showing 
#' the headline of the advice.
#' 
#' @return UI element
#'
#' @note
#' 
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
header_right_panel_headline <- function(id) {
  wellPanel(
    style = "height: fit-content; overflow-y: auto; white-space: normal;",
    panel(
      withSpinner(htmlOutput(id, height = "100%", width = "100%"))
    )
  )
}




