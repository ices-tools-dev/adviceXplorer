
navbarPage(
    # tab title
    windowTitle = "TAF Advice Tool",
    id = "tabset",
    fluid = TRUE,
    # navbar title
    title =
        shiny::div(img(
            src = "ICES_logo.PNG",
            style = "margin-top: -14px; padding-right:10px;padding-bottom:10px",
            height = 60
        )),

    useShinyjs(),
    tags$head(tags$style(HTML
                         ("#table tr:hover {
	                          background-color: rgba(240, 136, 33, 0.4) !important;
                            }"))),
    #tabsetPanel(#id = "tabset",
    tabPanel(
        "Data Filtering",
        sidebarLayout(
            sidebarPanel = maps_panels,
            mainPanel = selectize_panel
        )
        # )
    ),
    
    tabPanel(
        "Stock Selection", style = "max-height: 90vh; overflow-y: auto;",
        DTOutput("tbl")#,
                # useShinyjs(),
                # inlineCSS(list("table1" = "font-size: 15px"))
    ),
    tabPanel(
        "Stock development over time",
        sidebarLayout(
            sidebarPanel = allocations_infopanel,
            mainPanel = allocations_plotspanel
        )
        # includeMarkdown("Instructions.Rmd")
    ),
    
    tabPanel(
        "Catch Options & Advice",
        sidebarLayout(
            sidebarPanel(
                width = 3, style = "max-height: 90vh; overflow-y: auto;",#style = "overflow-y:scroll; max-height: 600px; position:relative;",
                DTOutput("Advice_View")#,
                # useShinyjs(),
                # inlineCSS(list("table2" = "font-size: 10px"))
            ),
            # browser(),
            # mainPanel(# this is not running 
            #     width = 9,
            #         htmlOutput("Advice_Sentence"),
            #         div(
            #         class = "outer",
            #         tags$style(type = "text/css", ".outer {position: relative; top: 61px; left: 0; right: 0; bottom: 61px; overflow: hidden; padding: 50}"),
            #         plotlyOutput("catch_scenario_plot_1", width = "100%", height = "100%")),
            #         # plotlyOutput("catch_scenario_plot_2"),
            #         div(
            #         class = "outer",
            #         tags$style(type = "text/css", ".outer {position: relative; top: 61px; left: 0; right: 0; bottom: 61px; overflow: hidden; padding: 50}"),
            #         plotlyOutput("catch_scenario_plot_2",width = "100%", height = "100%")),
            #         # DTOutput("catch_scenario_table")
            #         div(
            #         class = "outer",
            #         tags$style(type = "text/css", ".outer {position: relative; top: 61px; left: 0; right: 0; bottom: 61px; overflow: hidden; padding: 50}"),
            #         DTOutput("catch_scenario_table",width = "100%", height = "100%"))
            #         )
            mainPanel(
                width = 9, style = "max-height: 90vh; overflow-y: auto;",
                htmlOutput("Advice_Sentence"),
                tabsetPanel(
                    tabPanel(
                        "option_plot1",
                        plotlyOutput("catch_scenario_plot_1")
                    ),
                    tabPanel(
                        "option_plot2",
                        plotlyOutput("catch_scenario_plot_2")
                    )
                ),
                DTOutput("catch_scenario_table")
            )
        )
    ),
    
    
    tabPanel(
        "Catch Option & Advice 2",
        sidebarLayout(
            sidebarPanel = catch_scenarios_left_panel,
            mainPanel = catch_scenarios_right_panel
            # sidebarPanel(
            #         width = 6, style = "max-height: 90vh; overflow-y: auto;",
            #         htmlOutput("Advice_Sentence2"),
            #         plotlyOutput("catch_scenario_plot_3"),
            #         plotlyOutput("TAC_timeline")
            # ),
            # mainPanel(
            #     width = 6, style = "max-height: 90vh; overflow-y: auto;",
            #     timevisOutput("advice_timeline"),
            #     DTOutput("table")

            # )
        )
        
        # verbatimTextOutput("headline")
    ),
    #),# close tabsetpanel
    
    # extra tags, css etc
    tags$style(type = "text/css", "li {font-size: 20px;}"),
    tags$style(type = "text/css", "p {font-size: 21px;}"),
    tags$style(type = "text/css", "body {padding-top: 70px;}"),
    tags$head(tags$style(HTML("#go{background-color:#14c6dd}"))), ##dd4814 0range
    theme = shinytheme("united"),  ##### need to work on this, the orange is part of the css theme united, check bslib in forked repo
    position = "fixed-top",
    tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://github.com/ices-tools-dev/online-advice\"><img src=\"GitHub-Mark-32px.png\" alt=\"alt\" style=\"margin-top: -14px; padding-right:5px;padding-top:25px;\"></a></div>');
console.log(header)"))
)


