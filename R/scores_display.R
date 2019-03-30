scoresDisplayUI <- function(id) {
  
  ns <- NS(id)
  
  titlePanel(ns("Scores"))
  wellPanel(fluidRow(column(width = 10,
                            div(style = 'overflow-x: scroll',
                                DT::dataTableOutput(ns('scores_df'))
                                )
                            ),
                     column(width = 2,
                            verticalLayout( 
                              actionButton(inputId = ns("delete"), 
                                       label = "Supprimer",
                                       disabled = TRUE),
                              actionButton(inputId = ns("modify"), 
                                       label = "Modifier", 
                                       disabled = TRUE),
                              downloadLink(ns('downloadData'), 'Télécharger')
                              )
                            )
                     )
            )
}

scoresDisplay <- function(input, output, session, scores){
  output$scores_df <- DT::renderDataTable({
    DT::datatable(scores()[order(as.numeric(row.names(scores())), decreasing = TRUE), ],
                  extensions = 'Scroller',
                  filter = 'none',
                  options = list(deferRender = F, dom = 't',
                                 columnDefs = list(list(className = 'dt-center',
                                                        targets = 5)),
                                 scrollY = 300, scroller = TRUE, scrollX = T),
                  editable = FALSE, selection = 'single')      
  })
  
  # shinyjs::onclick('scores_df',{
  #   toggleState("modify", condition = (!is.null(input$scores_df_rows_selected)))
  #   toggleState("delete", condition = (!is.null(input$scores_df_rows_selected)))
  # })
  
  observe({
    # toggleState("modify", condition = (!is.null(input$scores_df_rows_selected)))
    toggleState("delete", condition = (!is.null(input$scores_df_rows_selected)))
    })
  
  observeEvent(input$delete,{
    showModal(
      modalDialog(
        tagList(
          paste('Est-vous sûre de vouloir supprimer la ligne',
                sort(1:nrow(scores()), decreasing = TRUE)[input$scores_df_rows_selected])
        ),
        title="Suppression",
        footer = tagList(actionButton(session$ns("confirmDelete"), "Oui"),
                         modalButton("Non")
        )
      )
    )
  })
  
  observeEvent(input$confirmDelete,{
    scores_tab <- scores()
    row_to_del <- sort(1:nrow(scores()), decreasing = TRUE)[input$scores_df_rows_selected]
    delta <- scores_tab[row_to_del, players_names_col] -
      scores_tab[row_to_del - 1, players_names_col]
    scores_tab <- scores_tab[-row_to_del,]
    rownames(scores_tab)[as.numeric(rownames(scores_tab))>row_to_del] <-
      as.numeric(rownames(scores_tab)[as.numeric(rownames(scores_tab))>row_to_del]) - 1
    
    if (row_to_del <= nrow(scores_tab)){
      scores_tab <- update_prev_scores(scores_tab, delta, row_to_del)
    }
      
    #refresh score dataframe after new score record ----
    scores(scores_tab)
    callModule(scoresDisplay, 'scores', scores)

    #save the new scores
    # write.csv2(scores(), file='www/scores.csv', sep="\t", row.names = FALSE)
    # saveRDS(scores(), file = "www/scores.RDS")
    drop_save_rds(scores(), "www/scores.RDS", output_dir = output_dir)
    
    #close modal dialog
    removeModal()
  })
  
  observeEvent(input$modify,{
    print('modify')
    print(paste('select', input$scores_df_rows_selected))
    annonces_targets <- c('poignee'='-poignee', 'double_poignee'='double_poignee',
                          'triple_poignee'='triple_poignee', 
                          'misere'='-misere','double_misere'='double_misere')
    
    row_selected <- reactive({
      sort(1:nrow(scores()), decreasing = TRUE)[input$scores_df_rows_selected]
    })
    
    active_players <- reactive(colnames(scores()[players_names_col])[which(
      t(presence(scores()[players_names_col])
        [row_selected(), ])
      )]
    )
    
    couleur <- reactive(scores()[row_selected(), 'Couleur'])
    contrat <- reactive(scores()[row_selected(),'Contrat'])

    annonces <- reactive({
      x <- scores()[row_selected(),'Annonces']
      res <- c()
      for (target in annonces_targets){
        res <- c(res, grepl(target, x))
      }
      if (is.na(scores()[row_selected(), 'Petit'])){
        names(annonces_targets)[res]
      }else if (scores()[row_selected(), 'Petit']==""){
        names(annonces_targets)[res]
      }else{
        c(names(annonces_targets)[res], 'petit_au_bout')
      }
      
    })
    
    bouts <- reactive(scores()[row_selected(), 'Bouts'])
    points <- reactive(scores()[row_selected(), 'Points'])
    preneur <- reactive(scores()[row_selected(), 'Preneur'])
    appele <- reactive(scores()[row_selected(), 'Appele'])
    
    annonce_players <- reactive({
      
      res <-list()
      for (string in unlist(strsplit(
        scores()[row_selected(), 'Annonces'],
        ';'))){

        for (target_name in names(annonces_targets)){
          if(grepl(annonces_targets[target_name], string)) {
            annonce <- target_name
            break
          }
        }
        
        players <- active_players()
        p <- c()
        for (target in players){
          p <- c(p, grepl(target, string))
        }
        
        res[[annonce]] <- players[p]
      }
      res
    })

    petit_au_bout_succes <- reactive({
      if (scores()[row_selected(), 'Petit'] != ''){
        petit_au_bout_succes <- unlist(strsplit(
          scores()[row_selected(), 'Petit'], ';'))[1]
      }else{
        petit_au_bout_succes <- NULL
      }
    })
    
    petit_au_bout_sens <- reactive({
      if (scores()[row_selected(), 'Petit'] != ''){
        petit_au_bout_sens <- unlist(strsplit(
          scores()[row_selected(), 'Petit'], ';'))[2]
      }else{
        petit_au_bout_succes <- NULL
      }
    })
    
    showModal(
      modalDialog(
        formUI(id=session$ns('modify_form'), players=players,
               active_players=active_players(),
               couleur=couleur(),
               contrat=contrat(),
               annonces=annonces(),
               bouts=bouts(),
               points=points()),
        title="Modifier",
        footer = modalButton("Annuler")
        )
      )

    callModule(form, "modify_form", preneur=preneur(), appele=appele(),
               annonce_players=annonce_players(), 
               petit_au_bout_succes=petit_au_bout_succes(), 
               petit_au_bout_sens=petit_au_bout_sens())
    
    callModule(validForm, 'modify_form', disabled_button=FALSE) 
    # print(row_selected())
    callModule(scoresCalculation, 'modify_form', scores=scores, 
               new=FALSE, row=row_selected())
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      scores_tab <- readRDS("www/scores.RDS")
      write.csv2(scores(), file, row.names = FALSE)
    }
  )
  
  
}
