#' @export
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

#' @export
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
