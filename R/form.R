formUI <- function(id, players, active_players=NULL, couleur=NULL,
                   contrat=NULL, annonces=NULL, bouts=NULL, points=NULL) {

  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(checkboxGroupInput(inputId=ns("active_players"), 
                                    label='Joueurs actifs',
                                    choices=players,
                                    selected=active_players)),
    mainPanel(
      wellPanel(shinyjs::useShinyjs(),
                id = ns("main-panel"),
                
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
                                             selected=couleur))),
                
                fluidRow(column(width = 6, 
                                selectizeInput(inputId = ns("contrat"), 
                                               label = "Contrat",
                                               choices=c('Petite', 'Garde', 
                                                         'Garde Sans', 'Garde Contre'),
                                               selected=contrat)
                ),
                column(width = 6, 
                       checkboxGroupInput(inputId=ns("annonces"), 
                                          label='Annonces',
                                          choiceNames=list('Poignée', 'Double poignée',
                                                        'Triple poignée','Misère', 
                                                        'Double Misère', 'Petit au bout'),
                                          choiceValues=list('poignee', 'double_poignee',
                                                         'triple_poignee', 'misere', 
                                                         'double_misere', 'petit_au_bout'),
                                          selected=annonces))),
                fluidRow(column(width = 6, 
                                numericInput(inputId=ns('bouts'), 'Bouts', 
                                             value=bouts,
                                             min=0, max=3, step=1)),
                         column(width = 6, uiOutput(ns('players_annonce')))),
                
                fluidRow(column(width = 6, 
                                numericInput(inputId=ns('point'), 'Points', 
                                             value=points,
                                             min=0, max=94, step=0.5))
                         ),
                fluidRow(uiOutput(ns("validation")))
      )
    )
  )
}

form <- function(input, output, session, preneur=NULL, appele=NULL,
                 annonce_players=c('poignee'=NULL, 'double_poignee'=NULL,
                                    'triple_poignee'=NULL, 'misere'=NULL, 
                                    'double_misere'=NULL),
                 petit_au_bout_succes=NULL, petit_au_bout_sens=NULL) {

  output$preneur <- renderUI({
    ns <- session$ns
    selectizeInput(ns("preneur"), "Preneur", choices=input$active_players,
                   selected=preneur)
  })

  output$appele <- renderUI({
    ns <- session$ns
    selectizeInput(ns("appele"), "Appelé", choices=input$active_players,
                   selected=appele)
  })

  output$players_annonce <- renderUI({
    ns <- session$ns
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
                               choiceValues=list('Reussi', 'Perdu'),
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
}

validForm <- function(input, output, session, disabled_button=TRUE) {
  
  output$validation <- renderUI({
    ns <- session$ns
    if (disabled_button){
      column(width = 12, offset= 5, actionButton(inputId = ns("valid"),
                                                 label = "Valider", disabled = FALSE))
    }else{
      column(width = 12, offset= 5, actionButton(inputId = ns("valid"),
                                                 label = "Valider"))
    }
  })
  
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
  
}