# Module UI
  
#' @title   mod_form_ui and mod_form_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_form
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_form_ui <- function(id, 
                        # players, active_players=NULL,
                        couleur=NULL,
                        contrat=NULL, annonces=NULL, bouts=NULL, points=NULL){
  ns <- NS(id)
  tagList(shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        # checkboxGroupInput(inputId=ns("active_players"),
        #                               label='Joueurs actifs',
        #                               choices=players,
        #                               selected=active_players)
        uiOutput(outputId=ns("active_players_ui")),
                   fluidRow(column(width = 6,
                                   actionButton(inputId = ns("add_player_form"), 
                                                label = "Ajouter")),
                            column(width = 6,
                                   actionButton(inputId = ns("del_player_form"), 
                                                label = "Supprimer")))
                   ),
      mainPanel(
        wellPanel(id = ns("main-panel"),
                  
                  fluidRow(column(width = 6, uiOutput(ns("preneur"))),
                           column(width = 6, uiOutput(ns("appele")))),
                  
                  fluidRow(column(width = 6, 
                                  radioButtons(inputId=ns("personnage_appele"), 
                                               label='Personnage appelé',
                                               choices=c('Roi', 'Dame', 
                                                         'Cavalier', 'Valet'),
                                               selected = 'Roi')),
                           
                           column(width = 6, 
                                  radioButtons(inputId = ns("couleur"), 
                                               label = "Couleur", 
                                               choices=c('Coeur', 'Carreau',
                                                         'Trèfle', 'Pique'),
                                               selected=couleur))
                           ),
                  
                  fluidRow(
                    column(width = 6,
                           selectizeInput(inputId = ns("contrat"),
                                          label = "Contrat",
                                          choices = c('Petite','Garde',
                                                      'Garde Sans', 
                                                      'Garde Contre'),
                                          selected = contrat)
                           ),
                    column(width = 6, 
                           checkboxGroupInput(inputId = ns("annonces"),
                                              label = 'Annonces',
                                              choiceNames = 
                                                list('Poignée', 'Double poignée',
                                                     'Triple poignée', 'Misère',
                                                     'Double Misère',
                                                     'Petit au bout'),
                                              choiceValues =
                                                list('poignee', 'double_poignee',
                                                     'triple_poignee', 'misere',
                                                     'double_misere',
                                                     'petit_au_bout'),
                                              selected=annonces))
                    ),
                  fluidRow(column(width = 6, 
                                  numericInput(inputId=ns('bouts'), 'Bouts', 
                                               value=bouts,
                                               min=0, max=3, step=1)),
                           column(width = 6, uiOutput(ns('players_annonce')))
                           ),
                  
                  fluidRow(column(width = 6, 
                                  numericInput(inputId=ns('point'), 'Points', 
                                               value=points,
                                               min=0, max=94, step=0.5))
                           ),
                  fluidRow(uiOutput(ns("validation")))
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_form
#' @export
#' @keywords internal
    
mod_form_server <- function(input, output, session, con_param, preneur=NULL, appele=NULL,
                            annonce_players = NULL, petit_au_bout_succes=NULL, 
                            petit_au_bout_sens=NULL, disabled_button,
                            new=TRUE, ID, scores, players, active_players=NULL){
  ns <- session$ns
  output$active_players_ui <- renderUI({
    if (is.null(active_players)){
      con <- do.call(db_con, con_param)
      players <- get_ordered_player_list(con)
      on.exit(dbDisconnect(con), add=TRUE)
      active_players <- players[1:5]
    }

    checkboxGroupInput(inputId=ns("active_players"),
                                  label='Joueurs actifs',
                                  choices=players,
                                  selected=active_players)
  })
  
  
  observeEvent(input$add_player_form, {
    
    showModal(
      modalDialog(
        mod_add_player_ui("add_player_ui_1"),
        title="Ajouter un joueur",
        footer = modalButton("Annuler")
      )
    )
  })
  
  observeEvent(input$del_player_form, {
    
    showModal(
      modalDialog(
        paste("Supprimer", input$active_players, collapse = ', '),
        title="Supprimer des joueurs",
        footer = tagList(
          fluidRow(column(width = 6, actionButton(inputId = ns("del_player"),
                                                  label = "OUI")),
                   column(width = 6, modalButton("NON")))
          )
      )
    )
  })
  
  observeEvent(input$del_player, {
    con <- do.call(db_con, con_param)
    del_player(con, input$active_players)
    players <- get_ordered_player_list(con)
    on.exit(dbDisconnect(con), add=TRUE)
    shinyjs::reset("active_players_ui")
    removeModal()
  })
  
  if (is.null(annonce_players)){
    annonce_players <- c('poignee'=NULL, 'double_poignee'=NULL,
                         'triple_poignee'=NULL, 'misere'=NULL,
                         'double_misere'=NULL)
  }
  
  output$preneur <- renderUI({
    selectizeInput(ns("preneur"), "Preneur", choices=input$active_players,
                   selected=preneur)
  })
  
  output$appele <- renderUI({
    selectizeInput(ns("appele"), "Appelé", choices=input$active_players,
                   selected=appele)
  })
  
  output$players_annonce <- renderUI({
    if (!is.null(input$annonces)){
      
      lapply(input$annonces, function(annonce) {
        
        if (annonce!='petit_au_bout'){
          tagList(checkboxGroupInput(inputId=ns(annonce),
                                     label=annonce,
                                     choices=input$active_players,
                                     selected=annonce_players[[annonce]]))
        }else{
          tagList(radioButtons(inputId=ns("petit_au_bout_succes"),
                               label='Petit au bout',
                               choiceNames=list('Réussi', 'Perdu'),
                               choiceValues=list(TRUE, FALSE),
                               selected = petit_au_bout_succes),
                  radioButtons(inputId=ns("petit_au_bout_sens"),
                               label='Par',
                               choiceNames=list('Attaque', 'Défense'),
                               choiceValues=list('Attaque', 'Defense'),
                               selected = petit_au_bout_sens)
          )
        }
      })
    }
  })
  
  output$validation <- renderUI({
    ns <- session$ns
    if (disabled_button()){
      column(width = 12, offset= 5, actionButton(inputId = ns("valid"),
                                                 label = "Valider", disabled = FALSE))
    }else{
      column(width = 12, offset= 5, actionButton(inputId = ns("valid"),
                                                 label = "Valider"))
    }
  })
  
  #voir si on peut pas rajouter un truc pour surveiller les miseres
  observe(
    toggleState("valid", condition = (
      (length(input$active_players) == 5) &
        (!is.na(input$bouts)) &
        (!is.na(input$point)) &
        input$point >= 0 &
        input$point < 95 &
        (input$point %% 1) %in% c(0, 0.5) &
        input$bouts %in% 0:3
    )
    )
  )
  
  observeEvent(input$valid, {
    con <- do.call(db_con, con_param)
    
    if (new){
      shinyjs::reset('main-panel')
      t <- lubridate::now("Europe/Paris")
      date = lubridate::today("Europe/Paris") %>% as.character()
      heure = paste(hour(t), minute(t), second(t) %>% round(0), sep = ':')
    }else{
      removeModal()
      date <- dbGetQuery(con, 
                         paste0("SELECT date FROM partie_tarot WHERE id=", ID))
      heure <- dbGetQuery(con,
                          paste0("SELECT heure FROM partie_tarot WHERE id=", ID))
    }
    
    #game settings
    partie_df <- data.frame(date = date,
                            heure = heure,
                            preneur = input$preneur,
                            appele = input$appele,
                            contrat = input$contrat,
                            couleur = input$couleur,
                            bouts = input$bouts,
                            points = input$point,
                            nb_joueur=length(input$active_players),
                            stringsAsFactors = F)
    
    #annonces
    if(!is.null(input$annonces)){
      annonce_df <- lapply(input$annonces, function(annonce){
        if (annonce != "petit_au_bout"){
          lapply(input[[annonce]], function(name){
            c(name, annonce)
          })
        }
      }) %>% unlist() 
      
      if (! is.null(annonce_df)){
        annonce_df <- annonce_df %>% matrix(nrow=2) %>% t() %>%   
          as.data.frame()
        colnames(annonce_df) <- c("joueur_id", "type")
      } #sinon annonce_df est deja null
    }else{ #si pas d'annonce
      annonce_df <- NULL
    }
    
    #marque calculation et petit au bout
    if ('petit_au_bout' %in% input$annonces){
      petit_au_bout_df <- data.frame(camps = input$petit_au_bout_sens,
                                     succes = input$petit_au_bout_succes)
      
      if ((as.logical(input$petit_au_bout_succes) & 
           input$petit_au_bout_sens == 'Attaque') |
          (! as.logical(input$petit_au_bout_succes) &
           input$petit_au_bout_sens == 'Defense')){
        bonus <- 10
        
      }else{
        bonus <- (-10)
      } 
    }else{
      petit_au_bout_df <- NULL
      bonus <- 0
    }
    
    ecart <- input$point - c(56, 51, 41, 36)[input$bouts + 1]
    marque <- (ifelse(ecart == 0, 1, sign(ecart)) *
                 (25 + abs(round(ecart/5)*5 )) + bonus) *
      switch(input$contrat, "Petite" = 1, "Garde" = 2, "Garde Sans" = 4,
             "Garde Contre" = 6)
    
    #complete the game settings with the marque result
    partie_df$marque <- marque
    
    if (input$preneur != input$appele){
      preneur <- data.frame(joueur_id = input$preneur, score = marque * 2)
      appele <- data.frame(joueur_id = input$appele, score = marque)
      defenseur <- data.frame(joueur_id = 
                                input$active_players[!input$active_players 
                                                     %in% c(input$preneur, 
                                                            input$appele)],
                              score = - marque)
      scores_partie_df <- rbind(preneur, appele, defenseur)
    }else{
      preneur <- data.frame(joueur_id = input$preneur, score = marque * 4)
      defenseur <- data.frame(joueur_id = 
                                input$active_players[!input$active_players 
                                                     %in% c(input$preneur, 
                                                            input$appele)],
                              score = - marque)
      scores_partie_df <- rbind(preneur, defenseur)
    }
    
    #Score treatement with annonces ----
    if (!is.null(input$annonces)){
      for (annonce in input$annonces){
        if (annonce != 'petit_au_bout'){
          scores_partie_df <- annonce_calculation(names=input[[annonce]],
                                                  scores_partie_df = scores_partie_df,
                                                  annonce = annonce,
                                                  active_players = input$active_players,
                                                  preneur = input$preneur,
                                                  appele = input$appele,
                                                  ecart = ecart)
        }
      }
    }
    
    if (new){
      if(insert_score(con, game = "tarot", partie_df, scores_partie_df,
                      petit_au_bout_df, annonce_df)){
        cat("Insertion de la partie ok \n")
      }
    }else{
      update_score(con, game = "tarot", partie_df, scores_partie_df,
                   petit_au_bout_df, annonce_df, id = ID)
    }
    
    scores(load_scores(con))
    callModule(mod_scores_display_server, "scores_display_ui_1", scores, disabled_button)
    on.exit(dbDisconnect(con), add=TRUE)
  })
  

}
    
