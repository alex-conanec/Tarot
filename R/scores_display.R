scoresDisplayUI <- function(id) {
  
  ns <- NS(id)
  
  titlePanel(ns("Scores"))
  wellPanel(fluidRow(column(width = 10,
                            DT::dataTableOutput(ns('scores_df'))
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
                            ),
                     column(width = 5,
                            DT::dataTableOutput(ns('anonnce_df'))
                     )
                     )
            )
}

scoresDisplay <- function(input, output, session, scores){

  output$scores_df <- DT::renderDataTable({
    DT::datatable(scores()[order(as.numeric(row.names(scores())), 
                                 decreasing = TRUE), ],
                  extensions = 'Scroller',
                  filter = 'none',
                  rownames = FALSE,
                  options = list(deferRender = F, dom = 't',
                                 columnDefs = list(list(className = 'dt-center',
                                                        targets = 5)),
                                 scrollY = 300, scroller = TRUE, scrollX = T),
                  editable = FALSE, selection = 'single')      
  })

  id <- reactive({
    row <- sort(1:nrow(scores()), decreasing = TRUE)[input$scores_df_rows_selected]     
    scores()[row,]$id
  })
  
  output$anonnce_df <- DT::renderDataTable({
    
    if (!is.null(input$scores_df_rows_selected)){
      if (! scores() %>% filter(id == id()) %>% pull(annonce) %>% is.na()){
        DT::datatable(
          data = dbGetQuery(con, paste0("SELECT type AS Annonce,
                                                joueur_id AS Joueur
                                         FROM annonce
                                         WHERE partie_tarot_id=", id())),
          extensions = 'Scroller',
          filter = 'none',
          rownames = FALSE,
          options = list(dom = 't', scrollY = 300, scroller = TRUE),
          editable = FALSE, selection = 'single'
          )
      }
    }
    
  })

  observe({
    # toggleState("modify", condition = (!is.null(input$scores_df_rows_selected)))
    toggleState("delete", condition = (!is.null(input$scores_df_rows_selected)))
    })
  
  observeEvent(input$delete,{
      
    showModal(
      modalDialog(
        tagList(
          paste("Est-vous sûr de vouloir supprimer l'enregistrement", id())
          ),
        title="Suppression",
        footer = tagList(actionButton(session$ns("confirmDelete"), "Oui"),
                         modalButton("Non")
        )
      )
    )
  })

  observeEvent(input$confirmDelete,{
    delete_partie(con, id())
    
    #refresh score dataframe after new score record ----
    scores(load_scores(con))
    callModule(scoresDisplay, 'scores', scores)
    
    # #close modal dialog
    removeModal()
  })
  
  observeEvent(input$modify,{
    
    pt <- dbGetQuery(con, paste0("Select * FROM partie_tarot WHERE id=", id()))
    
    couleur <- pt$couleur
    contrat <- pt$contrat
    bouts <- pt$bouts
    points <- pt$points
    preneur <- pt$preneur
    appele <- pt$appele

    annonces_df <- dbGetQuery(con, paste0("Select * FROM annonce ", 
                                          "WHERE partie_tarot_id=", id()))
    
    if (NROW(annonces_df) > 0){
      annonces <- annonces_df %>% pull(type) %>% unique()
      active_players <- dbGetQuery(con, paste0("Select * FROM scores_tarot ", 
                                               "WHERE partie_tarot_id=", id())) %>% 
        pull(joueur_id)
      
      a <- unique(annonces_df$type)
      names(a) <- unique(annonces_df$type)
      annonce_players <- lapply(a, function(annonce){
        annonces_df %>% filter(type==annonce) %>% pull(joueur_id)
      })
    }else{
      annonce_players <- NULL
      annonces <- NULL
    }
    
    petit <- dbGetQuery(con, paste0("SELECT * FROM petit_au_bout ", 
                                    "WHERE partie_tarot_id=", id()))
    
    if (NROW(petit) > 0){
      annonces <- c(annonces, "petit_au_bout")
    }
    petit_au_bout_succes <- petit$succes
    petit_au_bout_sens <- petit$camps
    
    showModal(
      modalDialog(
        formUI(id=session$ns(paste0("modify_form", form_num)), players=players,
               active_players=active_players,
               couleur=couleur,
               contrat=contrat,
               annonces=annonces,
               bouts=bouts,
               points=points),
        title="Modifier",
        footer = modalButton("Annuler")
      )
    )
    
    callModule(form, paste0("modify_form", form_num), 
               preneur = preneur, appele = appele,
               annonce_players = annonce_players,
               petit_au_bout_succes = petit_au_bout_succes,
               petit_au_bout_sens = petit_au_bout_sens)
    

    cat("displ", id(), "\n")
    callModule(validForm, paste0("modify_form", form_num), 
               disabled_button = FALSE)
    
    callModule(scoresCalculation, paste0("modify_form", form_num), new=FALSE, 
               scores = scores, ID = id())

    form_num <- form_num + 1
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv2(scores(), file, row.names = FALSE)
    }
  )
  
}
