#' @export
summary_stat_perso <- function(name, scores, list_contrats = 
                                 list(P = "Petite", G = "Garde",
                                      GS = "Garde Sans", GC = "Garde Contre")){
  
  res <- data.frame(matrix(NA, ncol = 4, nrow = 4))
  colnames(res) <- c('Parties jouées', 'PJ (en %)', 'Succes', 'Succes (en %)')
  rownames(res) <- c('Preneur', 'Appele','Défense', 'Total')
  
  #Parties jouées
  res['Preneur','Parties jouées'] <- scores %>% filter(preneur == name) %>%
                                        NROW()
  
  res['Appele','Parties jouées'] <- scores %>% filter(appele == name) %>%
                                        NROW()
  
  res['Défense', 'Parties jouées'] <- scores %>% filter(preneur !=name & 
                                                        appele != name &
                                                        ! is.na(UQ(as.name(name)))) %>%
                                        NROW()
  
  res['Total', 'Parties jouées'] <- sum(res[, 'Parties jouées'], na.rm = TRUE)
  
  res['Preneur','PJ (en %)'] <- round( 100 * res['Preneur','Parties jouées'] /
                                         (res['Preneur','Parties jouées'] + 
                                            res['Appele','Parties jouées'] + 
                                            res['Défense','Parties jouées']), 
                                       0)
  res['Appele','PJ (en %)'] <- round( 100 * res['Appele','Parties jouées'] /
                                        (res['Preneur','Parties jouées'] + 
                                           res['Appele','Parties jouées'] + 
                                           res['Défense','Parties jouées']),
                                      0)
  res['Défense','PJ (en %)'] <- 100 - res['Preneur','PJ (en %)'] - 
    res['Appele','PJ (en %)']
  
  res['Total','PJ (en %)'] <- weighted.mean(x = res[,'PJ (en %)'], 
                                            w = res[, 'Parties jouées'], 
                                            na.rm = TRUE) %>% round(0)
  
  #Succes total
  res['Preneur','Succes'] <- scores %>% filter(preneur == name &
                                                   marque > 0) %>%
    NROW()
  res['Appele','Succes'] <- scores %>% filter(appele == name &
                                                  marque > 0) %>%
    NROW()
  
  res['Défense','Succes'] <- scores %>% filter(preneur !=name & 
                                                   appele != name &
                                                   UQ(as.name(name)) > 0) %>%
    NROW()
  
  res['Total', 'Succes'] <- sum(res[, 'Succes'], na.rm = TRUE)
  
  res['Preneur','Succes (en %)'] <- round(100 * res['Preneur','Succes'] /
                                            res['Preneur','Parties jouées'], 
                                          0)
  res['Appele','Succes (en %)'] <- round(100 * res['Appele','Succes'] /
                                           res['Appele','Parties jouées'],
                                         0)
  res['Défense','Succes (en %)'] <- round(100 * res['Défense','Succes'] /
                                            res['Défense','Parties jouées'], 
                                          0)
  
  res['Total','Succes (en %)'] <- weighted.mean(x = res[,'Succes (en %)'], 
                                            w = res[, 'Succes'], 
                                            na.rm = TRUE) %>% round(0) 
  
  res2 <- lapply(list_contrats, function(cont){
    a = c(
      scores %>% filter(preneur == name & contrat == cont) %>% NROW(),
      scores %>% filter(appele == name & contrat == cont) %>% NROW(), 
      scores %>% filter(preneur !=name & appele != name & contrat == cont &
                          !is.na(UQ(as.name(name)))) %>% NROW(),
      NA
    )
    a[4] <- sum(a[1:3], na.rm = TRUE)
    
    b = c(
      (scores %>% filter(preneur == name & marque > 0 & contrat == cont) %>%
         NROW() / a[1]) %>% round(0) * 100,
      
      (scores %>% filter(appele == name & marque > 0 & contrat == cont) %>%
         NROW() / a[2]) %>% round(0) * 100,
      
      (scores %>% filter(preneur !=name & appele != name & contrat == cont &
                             UQ(as.name(name)) > 0) %>% NROW() / 
         a[3]) %>% round(0) * 100,
      NA
    )
    b[4] <- weighted.mean(x = b[1:3], w = a[1:3], na.rm = TRUE) %>% round(0)
    
    cbind(a, b)
    
  }) %>% unlist() %>% matrix(nrow = 4, byrow = FALSE) %>% as.data.frame() 
  
  colnames(res2) <- lapply(list_contrats, function(cont){
    c(cont, paste(cont, "réussi (en %)"))
  }) %>% unlist()
  
  res <- cbind(res, res2)
  rownames(res)[2] <- 'Appelé'
  
  res
}

