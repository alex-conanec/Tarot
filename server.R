shinyServer(function(input, output, session) {
  
  ##onglet manche ----
  callModule(form, "main_form")
  callModule(validForm, "main_form")
  
  ##onglet scores ----
  value <- reactiveVal(scores)
  callModule(scoresDisplay, 'scores', value)
  callModule(scoresCalculation, "main_form", value)
  
  #onglet stats ----
  callModule(stats, "stats")
  callModule(makeGraphs, "stats", value)
  
})