scoresCalculation <- function(input, output, session, new=TRUE, ID, scores){
  observeEvent(input$valid, {
    if (new){
      shinyjs::reset('main-panel')
      t <- lubridate::now("Europe/Paris")
      date = lubridate::today("Europe/Paris") %>% as.character()
      heure = paste(hour(t), minute(t), second(t) %>% round(0), sep = ':')
    }else{
      cat("scoreCac", ID, "\n")
      removeModal()
      date <- dbGetQuery(con, 
                         paste0("SELECT date FROM partie_tarot WHERE id=", ID))
      heure <- dbGetQuery(con,
                          paste0("SELECT heure FROM partie_tarot WHERE id=", ID))
    }
    
    #game settings
    partie_df <- data.frame(date = date,
                            heure = heure,
                            preneur = input$preneur,
                            appele = input$appele,
                            contrat = input$contrat,
                            couleur = input$couleur,
                            bouts = input$bouts,
                            points = input$point,
                            nb_joueur=length(input$active_players),
                            stringsAsFactors = F)
    
    #annonces
    if(!is.null(input$annonces)){
      annonce_df <- lapply(input$annonces, function(annonce){
        if (annonce != "petit_au_bout"){
          lapply(input[[annonce]], function(name){
            c(name, annonce)
          })
        }
      }) %>% unlist() 
      
      if (! is.null(annonce_df)){
        annonce_df <- annonce_df %>% matrix(nrow=2) %>% t() %>%   
          as.data.frame()
        colnames(annonce_df) <- c("joueur_id", "type")
      } #sinon annonce_df est deja null
    }else{ #si pas d'annonce
      annonce_df <- NULL
    }
    
    #marque calculation et petit au bout
    if ('petit_au_bout' %in% input$annonces){
      petit_au_bout_df <- data.frame(camps = input$petit_au_bout_sens,
                                     succes = input$petit_au_bout_succes)
      
      if ((as.logical(input$petit_au_bout_succes) & 
           input$petit_au_bout_sens == 'Attaque') |
          (! as.logical(input$petit_au_bout_succes) &
           input$petit_au_bout_sens == 'Defense')){
        bonus <- 10
        
      }else{
        bonus <- (-10)
      } 
    }else{
      petit_au_bout_df <- NULL
      bonus <- 0
    }
    
    ecart <- input$point - c(56, 51, 41, 36)[input$bouts + 1]
    marque <- (ifelse(ecart == 0, 1, sign(ecart)) *
                 (25 + abs(round(ecart/5)*5 )) + bonus) *
      switch(input$contrat, "Petite" = 1, "Garde" = 2, "Garde Sans" = 4,
             "Garde Contre" = 6)
    
    #complete the game settings with the marque result
    partie_df$marque <- marque
    
    if (input$preneur != input$appele){
      preneur <- data.frame(joueur_id = input$preneur, score = marque * 2)
      appele <- data.frame(joueur_id = input$appele, score = marque)
      defenseur <- data.frame(joueur_id = 
                                input$active_players[!input$active_players 
                                                     %in% c(input$preneur, 
                                                            input$appele)],
                              score = - marque)
      scores_partie_df <- rbind(preneur, appele, defenseur)
    }else{
      preneur <- data.frame(joueur_id = input$preneur, score = marque * 4)
      defenseur <- data.frame(joueur_id = 
                                input$active_players[!input$active_players 
                                                     %in% c(input$preneur, 
                                                            input$appele)],
                              score = - marque)
      scores_partie_df <- rbind(preneur, defenseur)
    }
    
    #Score treatement with annonces ----
    if (!is.null(input$annonces)){
      for (annonce in input$annonces){
        if (annonce != 'petit_au_bout'){
          scores_partie_df <- annonce_calculation(names=input[[annonce]],
                                                  scores_partie_df = scores_partie_df,
                                                  annonce = annonce,
                                                  active_players = input$active_players,
                                                  preneur = input$preneur,
                                                  appele = input$appele,
                                                  ecart = ecart)
        }
      }
    }
    
    # push the score into the db
    if (new){
      if(insert_score(con, game = "tarot", partie_df, scores_partie_df,
                      petit_au_bout_df, annonce_df)){
        cat("Insertion de la partie ok \n")
      }
    }else{
      # print(annonce_df)
      # print(petit_au_bout_df)
      update_score(con, game = "tarot", partie_df, scores_partie_df,
                   petit_au_bout_df, annonce_df, id = ID)
      # cat("Update de la partie ok \n") #check if you can test the return value....
    }
    
    scores(load_scores(con))
    callModule(scoresDisplay, 'scores', scores = scores)
  })
}

succes_poignee <- function(name, preneur, appele, ecart){
  
  if ((name == appele | name == preneur) & 
      ecart > 0){
    res <- TRUE
  }else if ((name == appele | name == preneur) & 
            ecart < 0){
    res <- FALSE
  }else if ((name != appele & name != preneur) & 
            ecart > 0){
    res <- FALSE
  }else if ((name != appele & name != preneur) & 
            ecart < 0){
    res <- TRUE
  }
  res
}

annonce_calculation <- function(names, scores_partie_df, annonce, 
                                preneur, appele, ecart, active_players,
                                bonus = c(poignee=20, double_poignee=30, 
                                          triple_poignee=40, misere=10, 
                                          double_misere=20)){
  
  for (name in names){
    if (grepl(pattern='poignee', annonce)){
      if (! succes_poignee(name, preneur, appele, ecart)){
        bonus[annonce] <- (- bonus[annonce])
      }
    }
    
    name_row <- which(scores_partie_df$joueur_id == name)
    scores_partie_df[name_row, 2] <- scores_partie_df[name_row, 2] +
      bonus[annonce] * (length(active_players)-1)
    
    scores_partie_df[-name_row, 2] <- scores_partie_df[-name_row, 2] - 
      bonus[annonce]
  }
  scores_partie_df
}
