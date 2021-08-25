library(shiny)
library(ambient)

ui <- function(request) {
    fluidPage(
    sidebarLayout(
        sidebarPanel(
            sliderInput("frequency", "Frequency", min = 0, max = 1, value = 0.01),
            selectInput("fractal", "Fractal", choices = c("none", "fbm", "billow", "rigid-multi")),
            sliderInput("gain", "Gain", min = 0, max = 1, value = 0.5),
        ),
        mainPanel(
            plotOutput("result")
        )
    )
  )
}


server <- function(input, output, session) {

    grid <- long_grid(seq(1, 10, length.out = 1000), seq(1, 10, length.out = 1000))

    noise <- reactive({
        ambient::gen_simplex(
            x = grid$x,
            y = grid$y,
            seed = 42,
            frequency = input$frequency,
            fractal = input$fractal,
            gain = input$gain
        )
    })

    output$result <- renderPlot({
        plot(grid, noise())
    })

    observe({
        reactiveValuesToList(input)
        session$doBookmark()
    })
    onBookmarked(updateQueryString)
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")