library(shiny)

ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:13px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:monospace;
            }"))),
    titlePanel("My Shiny App"),
    sidebarLayout(
        sidebarPanel(),
        mainPanel(
            p("p creates a paragraph of text."),
            p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
            strong("strong() makes bold text."),
            em("em() creates italicized (i.e, emphasized) text."),
            br(),
            code("code displays your text similar to computer code"),
            div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
            br(),
            p("span does the same thing as div, but it works with",
              span("groups of words", style = "color:blue"),
              "that appear inside a paragraph.")
        )
    )
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)