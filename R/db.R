# library(tidyverse)
# library(lubridate)
# 
# #test add_player
# player_df <- data.frame(nom=c("Durand", "Conanec", "Lee", "Cozzy",
#                               "Fayole", "Guiraud"),
#                         prenom=c("Guillaume", "Alexandre", "Alexandre",
#                                  "Thomas", "Etienne", "Thomas"),
#                         pseudo=c("dudu", "AlexC", "AlexL", "ThoC", "Etienne",
#                                  "ThoG"),
#                         email = "blabla@agro-bordeaux.fr")
# 
# add_player(con, player_df)
# player=dbGetQuery(con, "SELECT * from joueur") %>% pull(pseudo)
# player
# 
# #test insert and update partie saisie
# t <- lubridate::now("Europe/Paris")
# partie_df <- data.frame(date = lubridate::today("Europe/Paris") %>% as.character(),
#                         heure = paste(hour(t), minute(t),
#                                       second(t) %>% round(0), sep = ':'),
#                         preneur = "AlexC",
#                         appele = "ThoC",
#                         contrat = "Garde",
#                         couleur = "Coeur",
#                         bouts = 1,
#                         points = 33,
#                         nb_joueur=5,
#                         stringsAsFactors = F)
# 
# scores_partie_df <- data.frame(joueur_id = player[1:5],
#                            score = c(50, 25, -25, -25, -2))
# 
# annonce_df <- data.frame(joueur_id = "ThoC", type = "Poignee")
# petit_au_bout_df <- data.frame(camps = "Attaque", succes = FALSE)
# 
# #test insert
# insert_score(con, game = "tarot", partie_df, scores_partie_df,
#              petit_au_bout_df, annonce_df)
# dbGetQuery(con, "SELECT * from partie_tarot;")
# dbGetQuery(con, "SELECT * from scores_tarot;")
# dbGetQuery(con, "SELECT * from annonce;")
# dbGetQuery(con, "SELECT * from petit_au_bout;")
# 
# #test update /!\ ne prend pas en compte quand modif de deletion
# partie_df$bouts <- 2
# scores_partie_df$score[4] <- 23
# annonce_df$type = "misere"
# petit_au_bout_df$succes = FALSE
# 
# update_score(con, game = "tarot", partie_df, scores_partie_df,
#              petit_au_bout_df, annonce_df, id=5)
# dbGetQuery(con, "SELECT * from partie_tarot;")
# dbGetQuery(con, "SELECT * from scores_tarot;")
# dbGetQuery(con, "SELECT * from annonce;")
# dbGetQuery(con, "SELECT * from petit_au_bout;")
# 
# load_scores(con)

insert_score <- function(con, game = "tarot", partie_df, scores_partie_df, 
                         petit_au_bout_df, annonce_df){
  if (game == "tarot"){
    #partie settings
    dbWriteTable(con, "partie_tarot", 
                 value = partie_df, append = TRUE, row.names = FALSE)
    id <- dbGetQuery(con, "SELECT max(id) FROM partie_tarot;") %>% 
      as.numeric()

    #scores
    scores_partie_df <- bind_cols(
      partie_tarot_id = rep(id, NROW(scores_partie_df)), 
      scores_partie_df)
    dbWriteTable(con, "scores_tarot", 
                 value = scores_partie_df, append = TRUE, row.names = FALSE)
    
    #annonces
    annonce_df <- bind_cols(
      partie_tarot_id = rep(id, NROW(annonce_df)),
      annonce_df)
    dbWriteTable(con, "annonce",
                 value = annonce_df, append = TRUE, row.names = FALSE)

    #petit au bout #peut mieux faire car toujours qu'un seul ligne
    petit_au_bout_df <- bind_cols(
      partie_tarot_id = rep(id, NROW(petit_au_bout_df)),
      petit_au_bout_df)
    dbWriteTable(con, "petit_au_bout",
                 value = petit_au_bout_df, append = TRUE, row.names = FALSE)

  }
}

update_score <- function(con, game = "tarot", partie_df, scores_partie_df, 
                         petit_au_bout_df, annonce_df, id){
  
  #partie settings
  update_query1 <- paste0("UPDATE partie_tarot SET ",
                             "date='", partie_df[1,]$date, "',",
                             "heure='", partie_df[1,]$heure, "',",
                             "preneur='", partie_df[1,]$preneur, "',",
                             "appele='", partie_df[1,]$appele, "',",
                             "contrat='", partie_df[1,]$contrat, "',",
                             "couleur='", partie_df[1,]$couleur, "',",
                             "marque=", partie_df[1,]$marque, ",",
                             "bouts=", partie_df[1,]$bouts, ",",
                             "points=", partie_df[1,]$points, ",",
                             "nb_joueur=", partie_df[1,]$nb_joueur,
                             "WHERE id=", id, ";")
  dbSendStatement(con, update_query1)
  
  #scores
  lapply(seq_len(NROW(scores_partie_df)), function(i){
    update_query2 <- paste0("UPDATE scores_tarot SET ",
                            "partie_tarot_id=", id, ",",
                            "joueur_id='", scores_partie_df[i,]$joueur_id, "',",
                            "score=", scores_partie_df[i,]$score, " ",
                            "WHERE partie_tarot_id=", id, " ",
                            "AND joueur_id='", scores_partie_df[i,]$joueur_id, 
                            "';")
    
    dbSendStatement(con, update_query2)
  })
  
  #annonces
  annonce_df <- data.frame(joueur_id = "AlexC", type = "poignee")
  lapply(seq_len(NROW(annonce_df)), function(i){
    update_query3 <- paste0("UPDATE annonce SET ",
                            "partie_tarot_id=", id, ",",
                            "joueur_id='", annonce_df[i,]$joueur_id, "',",
                            "type='", annonce_df[i,]$type, "' ", 
                            "WHERE partie_tarot_id=", id, " ",
                            "AND joueur_id='", annonce_df[i,]$joueur_id, 
                            "';")
    
    dbSendStatement(con, update_query3)
  })
  
  
  #petit au bout
  last <- dbGetQuery(con, paste0("SELECT * 
                                  FROM petit_au_bout 
                                  WHERE partie_tarot_id=", id, ";"))

  if (NROW(last) > 0 & ! is.null(petit_au_bout_df)){
    update_query4 <- paste0("UPDATE petit_au_bout SET ",
                            "partie_tarot_id=", id, ",",
                            "camps='", petit_au_bout_df$camps, "',",
                            "succes=", petit_au_bout_df$succes, " ",
                            "WHERE partie_tarot_id=", id, ";")
    
    dbSendStatement(con, update_query4)
  }else if (NROW(last) > 0 & is.null(petit_au_bout_df)){
    dbSendStatement(con, paste0("DELETE FROM petit_au_bout 
                                 WHERE partie_tarot_id=", 
                                 last$partie_tarot_id))
  }else if (NROW(last) < 0 & !is.null(petit_au_bout_df)){
    dbWriteTable(con, "petit_au_bout",
                 value = cbind(partie_tarot_id = id, petit_au_bout_df),
                 append = TRUE, row.names = FALSE)
  }
}


add_player <- function(con, player_df){
  dbWriteTable(con, "joueur", 
               value = player_df, append = TRUE, row.names = FALSE)
}


load_scores <- function(con, game = "tarot"){
  
  if (game == "tarot"){
    df <- dbGetQuery(con, "SELECT *  
                           FROM scores_tarot as st
                           JOIN partie_tarot as pt 
                           ON st.partie_tarot_id=pt.id;")
    
    annonce <- dbGetQuery(con, "SELECT distinct(partie_tarot_id) 
                                FROM annonce;")
    petit <- dbGetQuery(con, "SELECT * 
                              FROM petit_au_bout
                              GROUP BY partie_tarot_id;")

    if (NROW(petit) > 0) {
      petit <- petit %>%
        mutate(petit = paste(camps,
                             c("Reussi", "Perdu")[factor(succes,
                                                         levels = c(TRUE,
                                                                    FALSE))])) %>% 
        select(-camps, -succes)
    }
    
    if (NROW(df)>0){
      spread(df, key=joueur_id, value = score) %>% 
        left_join(cbind(annonce, annonce = TRUE), 
                  by = c("id"="partie_tarot_id")) %>% 
        left_join(petit, by = c("id"="partie_tarot_id")) %>% 
        select(id, date, heure, preneur, appele, contrat, couleur, bouts, 
               points, marque, nb_joueur, petit, annonce, everything(), 
               -partie_tarot_id)
    }else NULL
  }
}

delete_partie <- function(con, id){
  
  dbSendStatement(con, paste0("DELETE FROM partie_tarot WHERE id=", id))
  dbSendStatement(con, paste0("DELETE FROM scores_tarot WHERE partie_tarot_id=",
                              id))
  dbSendStatement(con, paste0("DELETE FROM annonce WHERE partie_tarot_id=",
                              id))
  dbSendStatement(con, paste0("DELETE FROM petit_au_bout WHERE partie_tarot_id=",
                              id))
}
