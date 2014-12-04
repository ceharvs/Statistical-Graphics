shinyServer(function(input, output) {
  function(input, output) {
    output$text1 <- renderText({
      paste("You have selected", input$var)
    })
  }
})