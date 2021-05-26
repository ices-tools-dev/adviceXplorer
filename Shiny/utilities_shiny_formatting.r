side_width <- 4

# allocations advice information panel
allocations_infopanel <-
  sidebarPanel(
    width = side_width
  )

# advice plot side panel
allocations_plotspanel <-
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
            title = "Fishing Pressure",
            plotlyOutput("f"),
            h5(helpText("Figure 3: Fish mortality"))
            # actionButton("r_f", "Get Stock Data")
        ),
        tabPanel(
            title = "SSB",
            plotlyOutput("SSB"),
            h5(helpText("Figure 4: SSB"))
            # actionButton("r_SSB", "Get Stock Data")
        ),
        tabPanel(
            title = "Quality of Assessment",
            plotlyOutput("Q_Ass"),
            h5(helpText("Figure 4: Quality of Assessment"))
            # actionButton("r_SSB", "Get Stock Data")
        )
    ),
    DTOutput("tbl_summary")
    )
