scoresCalculation <- function(input, output, session, scores, new=TRUE, row){
  observeEvent(input$valid, {
    if (new){
      row <- nrow(scores())+1
      shinyjs::reset('main-panel')
      
    }else{
      removeModal()
      prev_scores <- scores()[row, players_names_col]
    }
    scores_tab <- scores()
    scores_tab[row,]$Date <- date()
    scores_tab[row,]$Preneur <- input$preneur
    scores_tab[row,]$Contrat <- input$contrat
    scores_tab[row,]$Couleur <- input$couleur
    scores_tab[row,]$Appele <- input$appele
    scores_tab[row,]$Bouts <- input$bouts
    scores_tab[row,]$Points <- input$point
    scores_tab[row,]$Ecart <- input$point - c(56, 51, 41, 36)[input$bouts + 1]
    rownames(scores_tab)[row] <- row

    #marque calculation
    if ('petit_au_bout' %in% input$annonces){
      if (input$petit_au_bout_succes=='Reussi' & input$petit_au_bout_sens=='Attaque' |
          input$petit_au_bout_succes=='Perdu' & input$petit_au_bout_sens=='Defense'){
        bonus <- 10
      }else{
        bonus <- (-10)
      }

    }else{
      bonus <- 0
    }
    scores_tab[row,]$Marque <- (ifelse(scores_tab[row,]$Ecart==0, 1, sign(scores_tab[row,]$Ecart)) *
      (25 + abs(round(scores_tab[row,]$Ecart/5)*5 )) + bonus) *
      switch(input$contrat, "Petite"=1, "Garde"=2, "Garde Sans"=4, "Garde Contre"=6)

    #Petit registration ----
       if(!is.null(input$annonces)){
        res <- ''
          if('petit_au_bout' %in% input$annonces){
            res <- paste(input$petit_au_bout_succes, 
                         input$petit_au_bout_sens, sep=';')
          }
        scores_tab[row,]$Petit <- res
       }else{
         scores_tab[row,]$Petit <- ''
       }
    
    #Annonce registration ----
    if(!is.null(input$annonces)){
      res <- ''
      for (annonce in input$annonces){
        if(annonce != 'petit_au_bout'){
          res <- paste(res, annonce,
                       paste(input[[annonce]], collapse=', '),
                       ';', sep='-')
        }
      }
      scores_tab[row,]$Annonces <- res
    }else{
      scores_tab[row,]$Annonces <- ''
    }

    #Score treatement without annonces ----
    if (row==1){
      scores_tab[row,players_names_col] <- 0
    }else{
      scores_tab[row,players_names_col] <- scores_tab[row - 1, players_names_col]
    }

    if (input$preneur != input$appele){

      scores_tab[row, input$preneur] <-
        scores_tab[row, input$preneur] +
        scores_tab[row,]$Marque * 2

      scores_tab[row, input$appele] <-
        scores_tab[row, input$appele] +
        scores_tab[row,]$Marque

      scores_tab[row, which(colnames(scores_tab) != input$appele &
                          colnames(scores_tab) != input$preneur &
                          colnames(scores_tab) %in% input$active_players)] <-
        scores_tab[row, which(colnames(scores_tab) != input$appele &
                            colnames(scores_tab) != input$preneur &
                            colnames(scores_tab) %in% input$active_players)] -
        scores_tab[row,]$Marque

    }else{

      scores_tab[row, input$preneur] <-
        scores_tab[row, input$preneur] +
        scores_tab[row,]$Marque * 4

      scores_tab[row, which(colnames(scores_tab) != input$appele &
                          colnames(scores_tab) != input$preneur &
                          colnames(scores_tab) %in% input$active_players)] <-
        scores_tab[row, which(colnames(scores_tab) != input$appele &
                            colnames(scores_tab) != input$preneur &
                            colnames(scores_tab) %in% input$active_players)] -
        scores_tab[row,]$Marque 

    }

    #Score treatement with annonces ----
    if (!is.null(input$annonces)){
      for (annonce in input$annonces){
        if (annonce != 'petit_au_bout'){
          scores_tab[row,] <- annonce_calculation(names=input[[annonce]],
                                              scores_last_row=scores_tab[row,],
                                              annonce = annonce,
                                              active_players=input$active_players,
                                              preneur=input$preneur,
                                              appele=input$appele,
                                              ecart=scores_tab[row,'Ecart'])
        }
      }
    }

    if (! new){
      delta <- prev_scores - scores_tab[row, players_names_col] 
      if (row < nrow(scores_tab)){
        scores_tab <- update_prev_scores(scores_tab, delta, row_to_start = row + 1)
      }
    }
    
    scores(scores_tab)
    callModule(scoresDisplay, 'scores', scores)
    
    #save the new scores
    write.csv2(scores(), file='www/scores.csv', sep="\t", row.names = FALSE)
  }, ignoreInit = T)
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

annonce_calculation <- function(names, scores_last_row, annonce, 
                                preneur, appele, ecart, active_players,
                                bonus = c(poignee=20, double_poignee=30, 
                                          triple_poignee=40, misere=10, 
                                          double_misere=20)){
  rownames(scores_last_row) <- 1
  for (name in names){
    if (grepl(pattern='poignee', annonce)){
      if (! succes_poignee(name, preneur, appele, ecart)){
        bonus[annonce] <- (- bonus[annonce])
      }
    }
    
    scores_last_row[1, name] <- scores_last_row[1, name] + bonus[annonce] * (length(active_players)-1)
    
    scores_last_row[1, which(colnames(scores_last_row) != name &
                        colnames(scores_last_row) %in% active_players)] <-
      scores_last_row[1, which(colnames(scores_last_row) != name &
                                 colnames(scores_last_row) %in% active_players)] - 
      bonus[annonce]
  }
  scores_last_row
}

update_prev_scores <- function(scores, delta, row_to_start) {


  delta <- matrix(as.matrix(delta), 
                  nrow = nrow(scores) - row_to_start + 1, 
                  ncol=ncol(delta), byrow = T)

  scores[(as.numeric(rownames(scores)) >= row_to_start), players_names_col] <-
    scores[(as.numeric(rownames(scores)) >= row_to_start), players_names_col] - delta
 
  scores
  
}
