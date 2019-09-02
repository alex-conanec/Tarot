#' @export
db_con <- function(host , user, pw, drv_type = "PostgreSQL", dbname = "tarot"){
  
  drv <- dbDriver(drv_type)
  dbConnect(drv, dbname = dbname, host = host, port = 5432, user = user, 
            password = pw)
  
}

#' @export
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

#' @export
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

#' @export
add_player <- function(con, player_df){
  dbWriteTable(con, "joueur", 
               value = player_df, append = TRUE, row.names = FALSE)
}

#' @export
del_player <- function(con, players){
  lapply(players, function(player){
    dbSendStatement(con, paste0("DELETE FROM joueur WHERE pseudo='", player,"'"))
  })
}

#' @export
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

    if (NROW(df) > 0){
      if (NROW(petit) > 0 & NROW(annonce) > 0) {
        petit <- petit %>%
          mutate(petit = paste(camps,
                               c("Reussi", "Perdu")[factor(succes,
                                                           levels = c(TRUE,
                                                                      FALSE))])) %>% 
          select(-camps, -succes)
        
        spread(df, key=joueur_id, value = score) %>% 
          left_join(cbind(annonce, annonce = TRUE), 
                    by = c("id"="partie_tarot_id")) %>% 
          left_join(petit, by = c("id"="partie_tarot_id")) %>% 
          select(id, date, heure, preneur, appele, contrat, couleur, bouts, 
                 points, marque, nb_joueur, petit, annonce, everything(), 
                 -partie_tarot_id)
      }else if (NROW(petit) > 0 & NROW(annonce) == 0){
        petit <- petit %>%
          mutate(petit = paste(camps,
                               c("Reussi", "Perdu")[factor(succes,
                                                           levels = c(TRUE,
                                                                      FALSE))])) %>% 
          select(-camps, -succes)
        
        spread(df, key=joueur_id, value = score) %>% 
          left_join(petit, by = c("id"="partie_tarot_id")) %>% 
          mutate(annonce = "false") %>%  
          select(id, date, heure, preneur, appele, contrat, couleur, bouts, 
                 points, marque, nb_joueur, petit, annonce, everything(), 
                 -partie_tarot_id)
      }else if (NROW(petit) == 0 & NROW(annonce) > 0){
        spread(df, key=joueur_id, value = score) %>%
          left_join(cbind(annonce, annonce = TRUE), 
                    by = c("id"="partie_tarot_id"))  %>% 
          mutate(petit = NA) %>%  
          select(id, date, heure, preneur, appele, contrat, couleur, bouts, 
                 points, marque, nb_joueur, petit, annonce, everything(), 
                 -partie_tarot_id)
      }else if (NROW(petit) == 0 & NROW(annonce) == 0){
        spread(df, key=joueur_id, value = score) %>%  
          mutate(annonce = "false", petit = NA) %>%  
          select(id, date, heure, preneur, appele, contrat, couleur, bouts, 
                 points, marque, nb_joueur, petit, annonce, everything(), 
                 -partie_tarot_id)
      }
    }else NULL
  }else NULL
}

#' @export
delete_partie <- function(con, id){
  
  dbSendStatement(con, paste0("DELETE FROM partie_tarot WHERE id=", id))
  dbSendStatement(con, paste0("DELETE FROM scores_tarot WHERE partie_tarot_id=",
                              id))
  dbSendStatement(con, paste0("DELETE FROM annonce WHERE partie_tarot_id=",
                              id))
  dbSendStatement(con, paste0("DELETE FROM petit_au_bout WHERE partie_tarot_id=",
                              id))
}

#' @export
get_ordered_player_list <- function(con){

  players <- dbGetQuery(con, "SELECT pseudo FROM joueur") 
  
  if (NROW(players) > 0){
    players <- pull(players, pseudo)
    
    game_count <- dbGetQuery(con, "SELECT joueur_id 
                                 FROM scores_tarot
                                 GROUP BY joueur_id
                                 ORDER BY joueur_id, COUNT(*)")
    
    if (NROW(game_count) > 0){
      game_count <- pull(game_count, joueur_id)
      
      none_game <- players[!players %in% game_count]
      if (NROW(none_game) > 0){
        res <- c(game_count, none_game)  
        factor(res, levels = res)
      }else{
        factor(game_count, levels = game_count)
      }
    }else{
      players
    }
    
  }else {
    NULL
  }

}
