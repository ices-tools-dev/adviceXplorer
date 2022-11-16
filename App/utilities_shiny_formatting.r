#' Creates the UI element of the ecoregion map
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
maps_panels <- function(){
    sidebarPanel(
      width = 8,
      tabPanel(
        "ICES Ecoregions",
        fillPage(
          tags$style(type = "text/css", "#map1 {height: calc(100vh - 140px) !important;}"), #
          withSpinner(
            leafletOutput("map1", height = "100%", width = "100%")          
          )        
        )
      )
  )
}


#' Creates the UI element of data filtering panel
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
selectize_panel <- function(){
  mainPanel(
    width = 4, style = "max-height: 90vh; overflow-y: auto;",
    tipify(
      actionButton(inputId = "help_tab1", label = NULL, style = "position: absolute; top: 1%; right:4%; width: 30px; height: 30px; background: url('info.png');  background-size: cover; background-position: center; border: 1px solid transparent;"),
      title = "Click here for help", placement = "left", trigger = "hover"),
    panel(
      selectizeInput(
        inputId = "selected_locations",
        label = "ICES Ecoregions",
        choices = sort(shape_eco$Ecoregion),
        selected = "Greater North Sea",
        multiple = TRUE,
        width = "100%",
        options = list(
          placeholder = "Select Ecoregion(s)"
        )
      ),
      #######
      selectizeInput(
        inputId = "selected_years",
        label = "Year",
        choices = Years$Year,
        selected = 2021,
        multiple = FALSE,
        width = "100%",
        options = list(
          placeholder = "Select ICES Area(s)"
        )
      ),
      #######
      selectizeGroupUI(
        id = "my-filters",
        params = list(
          StockKeyLabel = list(inputId = "StockKeyLabel", title = "Stock code:"),
          SpeciesCommonName = list(inputId = "SpeciesCommonName", title = "Common name:"),
          ExpertGroup = list(inputId = "ExpertGroup", title = "ExpertGroup:"),
          DataCategory = list(inputId = "DataCategory", title = "Data category:"),
          YearOfLastAssessment = list(inputId = "YearOfLastAssessment", title = "Year of last assessment:"),
          AdviceCategory = list(inputId = "AdviceCategory", title = "Advice category:")
        ),
        inline = FALSE
      ),
      heading = "Data filtering",
      status = "primary"
    ),
    htmlOutput("app_last_update")
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
    width = 6, style = "height: 80vh; overflow-y: auto;",
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
SAG_plots_righ_panel <- function(){
  sidebarPanel(
    width = 6, style = "height: 80vh; overflow-y: auto;",
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
    width = 6, style = "height: 70vh; overflow-y: auto;",
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
          "Historical catches",
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
    width = 6, style = "height: 70vh; overflow-y: auto;",
    # panel(
    #   withSpinner(htmlOutput("Advice_Headline", height = "10%", width = "100%"))
    
    # ),
    
    panel(
      title = "Catch scenario table",
      fillPage(
        tags$style(type = "text/css", "#table overflow-y: auto; !important;"), #{height: calc(80vh - 10px); calc(100vw - 10px)}
        withSpinner(DTOutput("table", height = "90%", width = "100%")),
        htmlOutput("footnotes", height = "90%", width = "100%")
      )
    )
  )
}


##### sag plots
sag_plots_stock_info_left_panel <- function() {
  wellPanel(
    style = "height: 25vh; overflow-y: auto; white-space: normal;",
    panel(
      title = "Stock Info",
      withSpinner(htmlOutput("stock_infos1", height = "100%", width = "100%"))
      )
  )
}

sag_plots_stock_info_center_panel <- function() {
  wellPanel(
    style = "height: 25vh; overflow-y: auto; white-space: normal;",
    panel(
      title = "Headline",
      withSpinner(htmlOutput("Advice_Headline1", height = "100%", width = "100%"))
      )
  )
}

sag_plots_stock_info_right_panel <- function() {
  wellPanel(
    style = "height: 25vh; overflow-y: auto; white-space: normal;",
    panel(
      title = "Links",
      HTML(
        paste0("<b><i><font size=4>Links:</font></b></i><br/>")
      ),
      tipify(
        actionButton(inputId = "help_tab3", label = NULL, style = "top: 1%; left:7%; width: 35px; height: 35px; background: url('info.png');  background-size: cover; background-position: center; border: 1px solid transparent;"),
        title = "Click here fof help", placement = "right", trigger = "hover"
      ),
      tipify(
        actionButton(inputId = "library_advice_link1", label = NULL, hover = T, style = "top: 1%; left:7%; width: 35px; height: 35px; background: url('pdf.png'); background-size: cover; background-position: center; border: 1px solid transparent; padding: 10px"),
        title = "Link to ICES library", placement = "right", trigger = "hover"
      ),
      tipify(
        myDownloadButton("download_SAG_Data"),
        title = "Download the plot data", placement = "right", trigger = "hover"
      )
    )
  )
}
#### quality of assessment
qualAssess_plots_stock_info_left_panel <- function() {
  wellPanel(
    style = "height: 25vh; overflow-y: auto; white-space: normal;",
    panel(
      title = "Stock Info",
      withSpinner(htmlOutput("stock_infos2", height = "100%", width = "100%"))
      )
  )
}

qualAssess_plots_stock_info_center_panel <- function() {
  wellPanel(
    style = "height: 25vh; overflow-y: auto; white-space: normal;",
    panel(
      title = "Headline",
      withSpinner(htmlOutput("Advice_Headline2", height = "100%", width = "100%"))
      )
  )
}

qualAssess_plots_stock_info_right_panel <- function() {
  wellPanel(
    style = "height: 25vh; overflow-y: auto; white-space: normal;",
    panel(
      title = "Links",
      HTML(
        paste0("<b><i><font size=4>Links:</font></b></i><br/>")
      ),
      tipify(
        actionButton(inputId = "help_tab4", label = NULL, style = "width: 35px; height: 35px; background: url('info.png');  background-size: cover; background-position: center; border: 1px solid transparent;"),
        title = "Click here fof help", placement = "right", trigger = "hover"
      ),
      tipify(
        actionButton(inputId = "library_advice_link2", label = NULL, hover = T, style = "top: 1%; left:7%; width: 35px; height: 35px; background: url('pdf.png'); background-size: cover; background-position: center; border: 1px solid transparent; padding: 10px"),
        title = "Link to ICES library", placement = "right", trigger = "hover"
      ),
      tipify(
        myDownloadButton("download_SAG_Quality_Data"),
        title = "Download the plot data", placement = "right", trigger = "hover"
      )
    )
  )
}




#### catch scenarios

catch_scenario_stock_info_left_panel <- function() {
  wellPanel(
    style = "height: 25vh; overflow-y: auto; white-space: normal;",
    panel(
      title = "Stock Info",
      withSpinner(htmlOutput("stock_infos3", height = "100%", width = "100%"))
      )
  )
}

catch_scenario_stock_info_center_panel <- function() {
  wellPanel(
    style = "height: 25vh; overflow-y: auto; white-space: normal;",
    panel(
      title = "Headline",
      withSpinner(htmlOutput("Advice_Headline3", height = "100%", width = "100%"))
      )
  )
}

catch_scenario_stock_info_right_panel <- function() {
  wellPanel(
    style = "height: 25vh; overflow-y: auto; white-space: normal;",
    panel(
      title = "Links",
      HTML(
        paste0("<b><i><font size=4>Links:</font></b></i><br/>")
      ),
      tipify(
        actionButton(inputId = "help_tab5", label = NULL, hover = T, style = "top: 1%; left:7%; width: 35px; height: 35px; background: url('info.png');  background-size: cover; background-position: center; border: 1px solid transparent; padding: 10px;"),
        title = "Click here fof help", placement = "right", trigger = "hover"
      ),
      tipify(
        actionButton(inputId = "library_advice_link3", label = NULL, hover = T, style = "top: 1%; left:7%; width: 35px; height: 35px; background: url('pdf.png'); background-size: cover; background-position: center; border: 1px solid transparent; padding: 10px"),
        title = "Link to ICES library", placement = "right", trigger = "hover"
      ),
      tipify(
        actionButton(inputId = "advice_view_link", label = NULL, hover = T, style = "top: 1%; left:7%; width: 35px; height: 35px; background: url('link.png'); background-size: cover; background-position: center; border: 1px solid transparent; padding: 10px"),
        title = "Link for the full advice view record", placement = "right", trigger = "hover"
      )
    )
  )
}
