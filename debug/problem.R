library(shiny)
    ui <- fluidPage(
      textOutput('text'),
      actionButton('button', 'OK')
    )
    
    server <- function(input, output, session){
      output$text <- renderText({'blabla1'})
      observeEvent(input$button, {
        output$text <- renderText({'blabla2'})
      })
    }
    
    shinyApp(ui=ui, server=server)
