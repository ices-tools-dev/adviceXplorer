library(shiny)
library(DT)

n <- 6
dat <- data.frame(
  Select = sprintf(
    '<input type="radio" name="rdbtn" value="%s"/>', 1:n
  ),
  YN = rep(FALSE, n),
  ID = 1:n,
  stringsAsFactors = FALSE
)

callback <- c(
  "$('input[name=rdbtn]').on('click', function(){",
  "  var value = $('input[name=rdbtn]:checked').val();",
  "  Shiny.setInputValue('rdbtn', value);",
  "});"
)

shinyApp(
  ui = fluidPage(
    title = "Radio buttons in a table",
    DTOutput("foo"),
    h3("Selected row:"),
    verbatimTextOutput("sel")
  ),
  server = function(input, output, session) {
    output[["foo"]] <- renderDT(
      dat, escape = FALSE, selection = 'none', server = FALSE,
      options = list(dom = 't', paging = FALSE, ordering = FALSE),
      callback = JS(callback)
    )
    output[["sel"]] <- renderPrint({
      input[["rdbtn"]]
    })
  }
)