side_width <- 4

allocations_infopanel <-
  sidebarPanel(
    width = 8,
    panel(
      title = "plots",
      fillPage(
        tags$style(type = "text/css", "#all_plots {height: calc(99vh - 200px) !important;}"),
        plotlyOutput("all_plots", height = "100%", width = "100%")
      ),
      h5(helpText("Stock Development over time"))
    )
  )


allocations_plotspanel <- sidebarPanel(
                            width = 4,
                            panel(title = "Quality of Assessment",
                                  fillPage(tags$style(type = "text/css", "#Q_Ass {height: calc(99vh - 200px) !important;}"),
                                           plotlyOutput("Q_Ass", height = "100%", width = "100%")
                                           ),
                                  h5(helpText("Quality of Assessment"))
                                  )
)


maps_panels <-
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

selectize_panel <-
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

################################## SAG plots tab
SAG_plots_left_panel <- sidebarPanel(
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

SAG_plots_righ_panel <- sidebarPanel(
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

##############################################Quality of assessment tab
quality_of_assessment <- splitLayout(
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

####################################### Advice tab
catch_scenarios_left_panel <- sidebarPanel(
  width = 6, style = "height: 80vh; overflow-y: auto;",
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
  

catch_scenarios_right_panel <- sidebarPanel(
  width = 6, style = "height: 80vh; overflow-y: auto;",
  panel(
    withSpinner(htmlOutput("Advice_Headline", height = "10%", width = "100%"))
  
  ),
  
  panel(
    title = "Catch scenario table",
    fillPage(
      tags$style(type = "text/css", "#table overflow-y: auto; !important;"), #{height: calc(80vh - 10px); calc(100vw - 10px)}
      withSpinner(DTOutput("table", height = "90%", width = "100%")),
      htmlOutput("footnotes", height = "90%", width = "100%")
    )
  )
)

