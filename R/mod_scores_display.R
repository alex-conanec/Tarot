# Module UI
  
#' @title   mod_scores_display_ui and mod_scores_display_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_scores_display
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_scores_display_ui <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(column(width = 10,
                      DT::dataTableOutput(ns('scores_df'))
                      ),
               column(width = 2, verticalLayout(
                 actionButton(inputId = ns("delete"),
                              label = "Supprimer",
                              disabled = TRUE),
                 actionButton(inputId = ns("modify"), 
                              label = "Modifier", 
                              disabled = TRUE),
                 downloadLink(ns('downloadData'), 'Télécharger'))
                      ),
               column(width = 5, DT::dataTableOutput(ns('anonnce_df'))
                      )
               )
      )
  )
}
    
# Module Server
    
#' @rdname mod_scores_display
#' @export
#' @keywords internal
    
mod_scores_display_server <- function(input, output, session, con_param, scores, 
                                      disabled_button){
  ns <- session$ns
  
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
        con <- do.call(db_con, con_param)
        data <- dbGetQuery(con, paste0("SELECT type AS Annonce,
                                        joueur_id AS Joueur
                                        FROM annonce
                                        WHERE partie_tarot_id=", id()))
        on.exit(dbDisconnect(con), add=TRUE)
        
        DT::datatable(
          data = data,
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
    con <- do.call(db_con, con_param)
    delete_partie(con, id())
    
    #refresh score dataframe after new score record ----
    scores(load_scores(con))
    on.exit(dbDisconnect(con), add=TRUE)
    
    callModule(mod_scores_display_server, "scores_display_ui_1", scores, 
               disabled_button)
    
    # #close modal dialog
    removeModal()
  })
  
  
  observeEvent(input$modify,{
    con <- do.call(db_con, con_param)
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
        mod_form_ui("modify",
               #      players = players,
               # active_players = active_players,
               couleur = couleur,
               contrat = contrat,
               annonces = annonces,
               bouts = bouts,
               points = points),
        title="Modifier",
        footer = modalButton("Annuler")
      )
    )
    # disabled_button(FALSE)
    callModule(mod_form_server, "modify", scores = scores, 
               preneur = preneur, appele = appele,
               annonce_players = annonce_players,
               petit_au_bout_succes = petit_au_bout_succes,
               petit_au_bout_sens = petit_au_bout_sens, 
               new = FALSE, ID = id(), disabled_button = disabled_button,
               players = players,
               active_players = active_players)
    
    on.exit(dbDisconnect(con), add=TRUE)
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
    
## To be copied in the UI
# mod_scores_display_ui("scores_display_ui_1")
    
## To be copied in the server
# callModule(mod_scores_display_server, "scores_display_ui_1")
 
