require(shiny)

scores <- read.csv("scores.csv", sep = ';', stringsAsFactors=FALSE, dec=',')
players <- colnames(scores)[11:ncol(scores)]

shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Manche",
             
             sidebarLayout(
               sidebarPanel(
                 wellPanel(checkboxGroupInput(inputId="active_players", 
                                                         label='Joueurs actifs',
                                                         choices=players))),
               mainPanel(
                 wellPanel(
                   fluidRow(column(width = 6, uiOutput("preneur")),
                            column(width = 6, uiOutput("appele"))),
                   
                   fluidRow(column(width = 6, 
                                   radioButtons(inputId="personnage_appele", 
                                                label='Personnage appelé',
                                                choices=c('Roi', 'Dame', 
                                                          'Cavalier', 'Valet'),
                                                selected = 'Roi')),
                            
                            column(width = 6, 
                                   radioButtons(inputId = "couleur", 
                                                label = "Couleur", 
                                                choices=c('Coeur', 'Carreau',
                                                          'Trèfle', 'Pique')))),
                   
                   fluidRow(column(width = 6, 
                                   selectizeInput(inputId = "contrat", 
                                                  label = "Contrat",
                                                  choices=c('Petite', 'Garde', 
                                                            'Garde Sans', 'Garde Contre'))
                   ),
                   column(width = 6, 
                          checkboxGroupInput(inputId="annonces", 
                                             label='Annonces',
                                             choices=c('Poignée', 'Double poignée',
                                                       'Triple poignée', 'Misère', 
                                                       'Double Misère', 'Petit au bout')))),
                   
                   fluidRow(column(width = 6, 
                                   numericInput(inputId='bouts', 'Bouts', 
                                                value=NULL,
                                                min=0, max=3, step=1)),
                            column(width = 6, uiOutput('players_annonce'))),
                   
                   fluidRow(column(width = 6, 
                                   numericInput(inputId='point', 'Points', 
                                                value=NULL,
                                                min=0, max=94, step=0.5)))
                 ),
                 
                 fluidRow(column(width = 12, offset= 5, actionButton(inputId = "valid", 
                                                         label = "Valider")))
               )
             )
             
             
             
             
    ),
    tabPanel("Scores",
             titlePanel("Scores"),
             tableOutput(outputId="scores")
    )
  )
))


