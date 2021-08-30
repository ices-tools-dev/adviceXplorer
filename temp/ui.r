

navbarPage(
    # tab title
    windowTitle = "TAF Advice Tool",

    # navbar title
    title =
        shiny::div(img(
            src = "ICES_logo_orange.PNG",
            style = "margin-top: -14px; padding-right:10px;padding-bottom:10px",
            height = 60
        )),
    # tabsetPanel(
    tabPanel(
        "Data Filtering",
        sidebarLayout(
            # Top panel with widgets sold
            # wellPanel(
            #     textOutput("Ecoregion")
            # ),

            # the map itself
            sidebarPanel(
                div(
                    class = "outer",
                    tags$style(type = "text/css", ".outer {position: fixed; top: 61px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                    # width = side_width,
                    leafletOutput("map", width = "35%", height = "100%")
                )
            ),
            mainPanel(
                # width = 8,
                # div(class="outer",
                # tags$style(type = "text/css", ".outer {position: fixed; top: 61px; left: 500px; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                # DTOutput("tbl")
            )
        )
        # )
    ),
    tabPanel(
        "Stock Selection",
        DTOutput("tbl")
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
        "Catch Options/Advice"
    ),
    tabPanel(
        "Resources"
    ),
    # extra tags, css etc
    tags$style(type = "text/css", "li {font-size: 20px;}"),
    tags$style(type = "text/css", "p {font-size: 21px;}"),
    tags$style(type = "text/css", "body {padding-top: 70px;}"),
    tags$head(tags$style(HTML("#go{background-color:#dd4814}"))),
    theme = shinytheme("united"),
    position = "fixed-top",

    tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><a href=\"https://https://github.com/ices-tools-dev/online-advice\"><img src=\"GitHub-Mark-32px.png\" alt=\"alt\" style=\"margin-top: -14px; padding-right:5px;padding-top:25px;\"></a></div>');
console.log(header)"))
)