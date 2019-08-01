require(shiny)
require(shinyjs)
require(shinyBS)
require(tidyverse)
require(lubridate)
require(RPostgreSQL)

eval(parse('R/db.R', encoding="UTF-8"))
eval(parse('R/form.R', encoding="UTF-8"))
eval(parse('R/scores_display.R', encoding="UTF-8"))
eval(parse('R/scores_calculation.R', encoding="UTF-8"))
eval(parse('R/stats.R', encoding="UTF-8"))

# # aws-db
# pw <- {"Xo8654eZHm8s"}
# drv <- dbDriver("PostgreSQL")
# con <- dbConnect(drv, dbname = "tarot",
#                  host = "aws-db.c4kkextytsdt.us-east-2.rds.amazonaws.com",
#                  port = 5432,
#                  user = "postgres", password = pw)
# rm(pw)

# localhost
pw <- {"6zNdWEu45"}
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "tarot",
                 host = "localhost", port = 5432,
                 user = "alex", password = pw)
rm(pw)

# form_num <- 1

# load_scores(con)
# # close the connection
# lapply(dbListConnections(PostgreSQL()), function(con){
#   dbDisconnect(con)
# })
# dbUnloadDriver(drv)
# dbListConnections(PostgreSQL())

players <- dbGetQuery(con, "SELECT pseudo FROM joueur") %>% pull(pseudo)
active_players <- dbGetQuery(con, "SELECT * FROM scores_tarot") 

if (NROW(active_players) > 0){
  players <- rbind(
    data.frame(joueur_id = active_players %>% pull(joueur_id) %>% unique(), n = 0),
    active_players %>% group_by(joueur_id) %>% summarise(n=n())
  ) %>% 
    group_by(joueur_id) %>% summarise(n = max(n)) %>% arrange(desc(n)) %>% 
    mutate(player = factor(joueur_id, levels = joueur_id)) %>% pull(player)
  
  active_players <- active_players %>%
    filter(partie_tarot_id == max(partie_tarot_id)) %>%
    pull(joueur_id)
  
}else{
  active_players <- NULL
}


# shinyOptions(cache = memoryCache(size = 0))
# getShinyOption("memoryCache")



