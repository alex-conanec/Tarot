require(shiny)

scores <- read.csv("scores.csv", sep = ';', stringsAsFactors=FALSE, dec=',')
players <- colnames(scores)[11:ncol(scores)]

shinyUI(fluidPage(theme = "bootstrap.css",
  HTML('<head>
        <meta charset="utf-8" />
        <title>Tarot</title>
        </head>
      
        <div id="entete">
        <h1 id="main_title">Compteur de points de Tarot</h1>
        </div>'),
  tabsetPanel(
    tabPanel("Manche",
             sidebarLayout(
               sidebarPanel(checkboxGroupInput(inputId="active_players", 
                                               label='Joueurs actifs',
                                               choices=players)),
               mainPanel(
                 wellPanel(shinyjs::useShinyjs(),
                           id = "main-panel",
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
             wellPanel(div(style = 'overflow-x: scroll', 
                           DT::dataTableOutput('scores'))
             )
    )
  ),
  HTML('<hr>
          <footer>
            <div id="social_network">
              <p>
                <a href="https://github.com/alex-conanec"><img src="github_logo.png" alt="logo_network" id="logo_github"/></a>
                <a href="https://www.linkedin.com/in/alexandre-conanec"><img src="in.png" alt="logo_network" id="logo_linkedin"/></a>
              </p>
            </div>')
))


