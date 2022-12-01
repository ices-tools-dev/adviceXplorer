#####  potential solution for guiding the user from stock selection to Advice and Scenarios

library(shiny)
library(DT)

server <- function(input, output) {
    output$iris_type <- DT::renderDataTable({
        datatable(data.frame(Species=paste0("<a href='#filtered_data'",
                                            "alt='",unique(iris$Species),"'",                                                 
                                            "onclick=\"",
                                            "tabs = $('.tabbable .nav.nav-tabs li');",
                                            "tabs.each(function() {",
                                            "$(this).removeClass('active')",
                                            "});",
                                            "$(tabs[1]).addClass('active');",
                                            "tabsContents = $('.tabbable .tab-content .tab-pane');",
                                            "tabsContents.each(function() {",
                                            "$(this).removeClass('active')",
                                            "});",
                                            "$(tabsContents[1]).addClass('active');",
                                            "$('#filtered_data').trigger('change').trigger('shown');",
                                            "Shiny.onInputChange('species', getAttribute('alt'));",
                                            "\">",
                                            unique(iris$Species),
                                            "</a>")),
                  escape = FALSE)
    })

    output$filtered_data <- DT::renderDataTable({
        if(is.null(input$species)){
            datatable(iris)
        }else{
            datatable(iris[iris$Species %in% input$species, ])
        }
    })
    }

ui <- shinyUI(fluidPage(
    mainPanel(
        tabsetPanel(
            tabPanel("Iris Type", DT::dataTableOutput("iris_type")),
            tabPanel("Filtered Data", DT::dataTableOutput("filtered_data"))
        )
    )
))

shinyApp(ui = ui, server = server)