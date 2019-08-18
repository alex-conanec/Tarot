#' @import shiny

app_server <- function(input, output,session) {
  
  require(shiny)
  require(shinyjs)
  require(tidyverse)
  require(lubridate)
  require(RPostgreSQL)

  # con_param <- list(host = "localhost",
  #                   user = "alex",
  #                   pw = Sys.getenv("local_db_pw"))
  
  con_param <- list(host = "aws-db.c4kkextytsdt.us-east-2.rds.amazonaws.com",
                    user = "postgres",
                    pw = Sys.getenv("aws_db_pw"))
  
  # con <- db_con(host = con_param$host,
  #               user = con_param$user,
  #               pw = con_param$pw)
  
  con <- do.call(db_con, con_param)
  
  scores <- reactiveVal(load_scores(con))
  on.exit(dbDisconnect(con), add=TRUE)
  
  disabled_button <- reactiveVal(TRUE)
  
  callModule(mod_form_server, "form_ui_1", scores = scores, 
             disabled_button = disabled_button, con_param = con_param)
  
  callModule(mod_scores_display_server, "scores_display_ui_1", 
             con_param = con_param, 
             scores = scores,
             disabled_button = disabled_button)
  
  callModule(mod_stat_server, "stat_ui_1",
             con_param = con_param, 
             scores = scores)
  
  callModule(mod_add_player_server, "add_player_ui_1", 
             con_param = con_param)
  
}
