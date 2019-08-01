shinyServer(function(input, output, session) {
  
  #onglet manche ----
  callModule(form, "main_form")
  callModule(validForm, "main_form")
  # 
  # ##onglet scores ----
  scores <- reactiveVal(load_scores(con))
  id <- reactiveVal(0)
  callModule(scoresDisplay, 'scores', scores = scores)
  callModule(scoresCalculation, "main_form", scores = scores)

  #onglet stats ----
  callModule(stats, "stats")
  callModule(makeGraphs, "stats", scores)

  session$onSessionEnded(function() {
    if (dbDisconnect(con) & dbUnloadDriver(drv)){
      cat("database succesfully disconected")
    }
  })
})