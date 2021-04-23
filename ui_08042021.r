


side_width <- 5

# allocations input panel
allocations_inputpanel <-
  sidebarPanel(
    width = side_width
  )

# results side panel
allocations_resultspanel <-
  mainPanel(
    width = 12 - side_width,
    tabsetPanel(
        tabPanel(
            title = "Catches",
            plotlyOutput("catches"),
            h5(helpText("Figure 1: Catches"))
            # actionButton("r_catches", "Get Stock Data")
        ),
        tabPanel(
            title = "Recruitment",
            plotlyOutput("R"),
            h5(helpText("Figure 2: Stock recruitment"))
            # actionButton("r_recr", "Get Stock Data")
        ),
        tabPanel(
            title = "Fi",
            plotlyOutput("f"),
            h5(helpText("Figure 3: Fish mortality"))
            # actionButton("r_f", "Get Stock Data")
        ),
        tabPanel(
            title = "SSB",
            plotlyOutput("SSB"),
            h5(helpText("Figure 4: SSB"))
            # actionButton("r_SSB", "Get Stock Data")
        )
    )
  )






# user interface
ui <-
  navbarPage(
    # tab title
    windowTitle = "TAF Advice Tool",

    # navbar title
    title =
      shiny::div(img(src = 'ICES_logo_orange.PNG',
              style = "margin-top: -14px; padding-right:10px;padding-bottom:10px",
              height = 60)),

  tabPanel(
      "Ecoregions map access",
      div(class="outer",
      tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
      leafletOutput("mymap", width = "42%", height = "100%")
      #includeMarkdown("Introduction.Rmd")
    )),
    
  tabPanel(
    "Direct stock access",
    #includeMarkdown("Instructions.Rmd")
  ),

  tabPanel(
    "Advice",
    sidebarLayout(
      sidebarPanel = allocations_inputpanel,
      mainPanel = allocations_resultspanel
    )#,
    #allocations_resultspanel_wide
  ),

  tabPanel(
    "Instructions",
    #includeMarkdown("UsefulLinks.Rmd")
  ),

  # extra tags, css etc
  tags$style(type = "text/css", "li {font-size: 17px;}"),
  tags$style(type = "text/css", "p {font-size: 18px;}"),
  tags$style(type = "text/css", "body {padding-top: 70px;}"),
  tags$head(tags$style(HTML('#go{background-color:#dd4814}'))),
  theme = shinytheme("united"),
  position = "fixed-top",

  tags$script(HTML("var header = $('.navbar > .container-fluid');
    header.append('<div style=\"float:right\"><a href=\"https://github.com/ices-taf/2020_bss.27.4bc7ad-h_catchAllocationTool\"><img src=\"GitHub-Mark-32px.png\" alt=\"alt\" style=\"margin-top: -14px; padding-right:5px;padding-top:25px;\"></a></div>');
    console.log(header)"))
)
