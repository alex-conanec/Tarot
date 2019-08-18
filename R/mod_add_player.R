# Module UI
  
#' @title   mod_add_player_ui and mod_add_player_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_add_player
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_add_player_ui <- function(id){
  ns <- NS(id)
  tagList(
    textInput(inputId = ns("pseudo"), label = "Pseudo"),
    textInput(inputId = ns("firstname"), label = "Prénom"),
    textInput(inputId = ns("lastname"), label = "Nom"),
    textInput(inputId = ns("email"), label = "email"),
    actionButton(inputId = ns("add_player_validate"), label = "Valider")
  )
}
    
# Module Server
    
#' @rdname mod_add_player
#' @export
#' @keywords internal
    
mod_add_player_server <- function(input, output, session, con_param){
  ns <- session$ns
  observeEvent(input$add_player_validate, {
    player_df <- data.frame(nom = input$lastname, 
                            prenom = input$firstname, 
                            pseudo = input$pseudo, 
                            email = input$email)

    con <- do.call(db_con, con_param)
    add_player(con, player_df)
    players <- get_ordered_player_list(con)
    on.exit(dbDisconnect(con), add=TRUE)
    
    shinyjs::reset("active_players")
    removeModal()
    print(players)
  })
}
    
## To be copied in the UI
# mod_add_player_ui("add_player_ui_1")
    
## To be copied in the server
# callModule(mod_add_player_server, "add_player_ui_1")
 
