require(shiny)
require(shinyjs)
require(shinyBS)

source('R/fonctions.R')
eval(parse('R/form.R', encoding="UTF-8"))
eval(parse('R/scores_display.R', encoding="UTF-8"))
eval(parse('R/scores_calculation.R', encoding="UTF-8"))
eval(parse('R/stats.R', encoding="UTF-8"))

scores <- read.csv("www/scores.csv", sep = ';', 
                   stringsAsFactors=FALSE, dec=',')

players_names_col <- seq(from=which(colnames(scores)=='Annonces') + 1, 
                         to= ncol(scores))

players <- colnames(scores)[players_names_col]

