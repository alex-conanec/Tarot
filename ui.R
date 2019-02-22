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
             useShinyjs(debug = TRUE),
             formUI('main_form', players)
    ),
    tabPanel("Scores",
             scoresDisplayUI('scores')
             ),
      tabPanel("Stats",
               statsUI('stats')
               # titlePanel("Stats"),
               # sidebarLayout(
               #   sidebarPanel(radioButtons(inputId="choice_graph",
               #                             label="Choix graphique",
               #                             choices=c('Total',
               #                                       'Stats individuelle',
               #                                       'Stats collectives'),
               #                             selected='Total'),
               #                uiOutput("perf_perso_players")),
               #   mainPanel(wellPanel(shinyjs::useShinyjs(),
               #                       id = "main-panel2",
               #                       uiOutput("graph_principal")))
               #   )
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


