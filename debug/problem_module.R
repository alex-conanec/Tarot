innerUI <- function(id){
  ns <- NS(id)
  textOutput(ns('text'))
}

outerUI <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns('button'), 'OK'),
    actionButton(ns('button2'), 'OK1')
  )
}

inner <- function(input, output, session, chain){
  
  output$text <- renderText({
    chain()
  })
}

outer <- function(input, output, session, value){
  
  observeEvent(input$button | input$button2, {
    value(rnorm(10)) 
    callModule(inner, 'test1', value)
  }, ignoreInit = T)
}

ui <- fluidPage(
  innerUI('test1'),
  outerUI('test2')
)

server <- function(input, output, session){
  
  value <- reactiveVal('blabla1')
  
  callModule(inner, 'test1', value)
  callModule(outer, 'test2', value)
  
}

shinyApp(ui=ui, server=server)
