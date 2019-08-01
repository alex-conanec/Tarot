shinyUI(fluidPage(theme = "bootstrap.css",
  HTML('<head>
        <meta charset="utf-8" />
        <title>Tarot</title>
        </head>
      
        <div id="entete">
        <!-- <h1 id="main_title">Compteur de points de Tarot</h1> -->
        </div>'),
  tabsetPanel(id = "tabs",
    tabPanel("Manche",
             useShinyjs(debug = TRUE),
             formUI('main_form', players, active_players = active_players)
    ),
    tabPanel("Scores",
             scoresDisplayUI('scores')
             ),
      tabPanel("Stats",
               statsUI('stats')
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
