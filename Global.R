require(shiny)
require(shinyjs)
require(shinyBS)
require(rdrop2)
require(tidyverse)
require(lubridate)

token <- readRDS("www/droptoken.rds")
drop_acc(dtoken = token)
output_dir <- "Tarot"

eval(parse('R/db.R', encoding="UTF-8"))
eval(parse('R/form.R', encoding="UTF-8"))
eval(parse('R/scores_display.R', encoding="UTF-8"))
eval(parse('R/scores_calculation.R', encoding="UTF-8"))
eval(parse('R/stats.R', encoding="UTF-8"))

backup(dtoken = token)

scores <- drop_read_rds(file = "www/scores.RDS", output_dir = output_dir,
                        dtoken = token)


players_names_col <- seq(from=which(colnames(scores)=='Annonces') + 1, 
                         to= ncol(scores))

players <- colnames(scores)[players_names_col]
active_players <- players[presence(scores[, players_names_col])[NROW(scores),] %>% unlist()]
