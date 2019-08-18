#' @import shiny
app_ui <- function() {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    
    fluidPage(
      shiny::htmlTemplate(
        system.file("templates", "template.html", package = "Tarot"),
        title = "Tarot",
        tabsetPanel = tabsetPanel(id = "tabs",
                       tabPanel("Manche",
                                mod_form_ui("form_ui_1"
                                            # ,
                                            # players = players,
                                            # active_players = active_players
                                )
                       ),
                       tabPanel("Scores",
                                mod_scores_display_ui("scores_display_ui_1")
                       ),
                       tabPanel("Stats",
                                mod_stat_ui("stat_ui_1")
                       )
         ),
        footer = htmlTemplate(
          system.file("templates", "footer.html", package = "Tarot")
        )

      ) #templateHTML
    ) #fluidPage             
  ) #taglist
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'Tarot')
  )
 
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
