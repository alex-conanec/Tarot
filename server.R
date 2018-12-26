require(shiny)
require(shinyjs)
source('www/fonctions.R')

scores <- read.csv("www/scores.csv", sep = ';', stringsAsFactors=FALSE, dec=',')

shinyServer(function(input, output) {
  
  ##onglet manche
  output$preneur <- renderUI({
    selectizeInput(inputId = "preneur", label = "Preneur", 
                   choices=input$active_players)
    })
  
  output$appele <- renderUI({
    selectizeInput(inputId = "appele", label = "Appelé", 
                   choices=input$active_players)
  })
  
  
    output$players_annonce <- renderUI({
      if (!is.null(input$annonces)){
        
        lapply(input$annonces, function(annonce) {
          
          if (annonce!='Petit au bout'){
          checkboxGroupInput(inputId=annonce, 
                             label=annonce,
                             choices=input$active_players)
          }else{
            radioButtons(inputId="petit_au_bout", 
                         label='Petit au bout',
                         choices=c('Reussi', 'Chipé'),
                         selected = 'Reussi')
            }
          })
      }
    })
    
    #onglet scores
    observeEvent(input$valid, {
      scores <- read.csv("www/scores.csv", sep = ';', stringsAsFactors=FALSE, dec=',')
      row <- nrow(scores)+1
      scores[row,]$Date <- date() 
      scores[row,]$Preneur <- input$preneur
      scores[row,]$Contrat <- input$contrat
      scores[row,]$Couleur <- input$couleur
      scores[row,]$Appelé <- input$appele
      scores[row,]$Bouts <- input$bouts
      scores[row,]$Points <- input$point
      scores[row,]$Ecart <- input$point - c(56, 51, 41, 36)[input$bouts + 1]
      
      #marque calculation
      if ('Petit au bout' %in% input$annonces){
        bonus <- ifelse(input$petit_au_bout=='Reussi',10,-10)
      }else{
        bonus <- 0
      }
      scores[row,]$Marque <- sign(scores[row,]$Ecart) *
        (25 + abs(round(scores[row,]$Ecart/5)*5 + bonus)) *
      switch(input$contrat, "Petite"=1, "Garde"=2, "Garde Sans"=4, "Garde Contre"=6) 
      
      #Annonce registration
      scores[row,]$Annonces <- 
        
        if(!is.null(input$annonces)){
        res <- ''
        for (annonce in input$annonces){
          res <- paste(res, annonce, 
                       paste(input[[annonce]], collapse=', '),
                       ';', sep=' ')
        }
        res
      }
      
      #Score treatement without annonces
      if (row==1){
        scores[row,11:ncol(scores)] <- 0
      }else{
        scores[row,11:ncol(scores)] <- scores[row - 1, 11:ncol(scores)]
      }
      
      if (input$preneur != input$appele){
        
        scores[row, which(colnames(scores)==input$preneur)] <- 
          scores[row, which(colnames(scores)==input$preneur)] +
          scores[row,]$Marque * 2
        
        scores[row, which(colnames(scores)==input$appele)] <- 
          scores[row, which(colnames(scores)==input$appele)] +
          scores[row,]$Marque
        
        scores[row, which(colnames(scores) != input$appele & 
                            colnames(scores) != input$preneur &
                            colnames(scores) %in% input$active_players)] <-
          scores[row, which(colnames(scores) != input$appele & 
                              colnames(scores) != input$preneur &
                              colnames(scores) %in% input$active_players)] -
          scores[row,]$Marque
        
      }else{
        
        scores[row, which(colnames(scores)==input$preneur)] <- 
          scores[row, which(colnames(scores)==input$preneur)] +
          scores[row,]$Marque * 3
        
        scores[row, which(colnames(scores) != input$appele & 
                            colnames(scores) != input$preneur &
                            colnames(scores) %in% input$active_players)] <-
          scores[row, which(colnames(scores) != input$appele & 
                              colnames(scores) != input$preneur &
                              colnames(scores) %in% input$active_players)] -
          scores[row,]$Marque * 3/4
        
      }
      
      #Score treatement with annonces      
      if (!is.null(input$annonces)){
          
        if ('Misère' %in% input$annonces){
          names <- input[['Misère']]
          
          scores[row, which(colnames(scores) == names)] <-
            scores[row, which(colnames(scores) == names)] + 10
          
          scores[row, which(colnames(scores) != names &
                   colnames(scores) %in% input$active_players)] <-
            scores[row, which(colnames(scores) != names &
                     colnames(scores) %in% input$active_players)] - 
            10/(length(input$active_players)-1)
        }
        
        if ('Double Misère' %in% input$annonces){
          names <- input[['Double Misère']]
          
          scores[row, which(colnames(scores) == names)] <-
            scores[row, which(colnames(scores) == names)] + 20
          
          scores[row, which(colnames(scores) != names &
                              colnames(scores) %in% input$active_players)] <-
            scores[row, which(colnames(scores) != names &
                                colnames(scores) %in% input$active_players)] - 
            20/(length(input$active_players)-1)
        }        
          
        if ('Poignée' %in% input$annonces){
          
          names <- input[['Poignée']]
          
          if ((names == input$appele | names == input$preneur) & 
              scores[row,]$Marque > 0){
            sign <- 1
          }else if ((names == input$appele | names == input$preneur) & 
                    scores[row,]$Marque < 0){
            sign <- (-1)
          }else if (names != input$appele & names != input$preneur &
                    scores[row,]$Marque > 0){
            sign <- (-1)
          }else if (names != input$appele & names != input$preneur &
                    scores[row,]$Marque < 0){
            sign <- 1
          }

          scores[row, which(colnames(scores) == names)] <-
            scores[row, which(colnames(scores) == names)] + 20*sign

          scores[row, which(colnames(scores) != names &
                                   colnames(scores) %in% input$active_players)] <-
            scores[row, which(colnames(scores) != names &
                                colnames(scores) %in% input$active_players)] - 
            20*sign/(length(input$active_players)-1)

        }
        
        if ('Double poignée' %in% input$annonces){
          
          names <- input[['Double poignée']]
          
          if ((names == input$appele | names == input$preneur) & 
              scores[row,]$Marque > 0){
            sign <- 1
          }else if ((names == input$appele | names == input$preneur) & 
                    scores[row,]$Marque < 0){
            sign <- (-1)
          }else if (names != input$appele & names != input$preneur &
                    scores[row,]$Marque > 0){
            sign <- (-1)
          }else if (names != input$appele & names != input$preneur &
                    scores[row,]$Marque < 0){
            sign <- 1
          }
          
          scores[row, which(colnames(scores) == names)] <-
            scores[row, which(colnames(scores) == names)] + 30*sign
          
          scores[row, which(colnames(scores) != names &
                              colnames(scores) %in% input$active_players)] <-
            scores[row, which(colnames(scores) != names &
                                colnames(scores) %in% input$active_players)] - 
            30*sign/(length(input$active_players)-1)
          
        }
        
        if ('Triple poignée' %in% input$annonces){
          
          names <- input[['Triple poignée']]
          
          if ((names == input$appele | names == input$preneur) & 
              scores[row,]$Marque > 0){
            sign <- 1
          }else if ((names == input$appele | names == input$preneur) & 
                    scores[row,]$Marque < 0){
            sign <- (-1)
          }else if (names != input$appele & names != input$preneur &
                    scores[row,]$Marque > 0){
            sign <- (-1)
          }else if (names != input$appele & names != input$preneur &
                    scores[row,]$Marque < 0){
            sign <- 1
          }
          
          scores[row, which(colnames(scores) == names)] <-
            scores[row, which(colnames(scores) == names)] + 40*sign
          
          scores[row, which(colnames(scores) != names &
                              colnames(scores) %in% input$active_players)] <-
            scores[row, which(colnames(scores) != names &
                                colnames(scores) %in% input$active_players)] - 
            40*sign/(length(input$active_players)-1)
          
        }
      }
      
      #refresh score dataframe after new score record
      output$scores <- DT::renderDataTable({
        DT::datatable(scores,
                      extensions = 'Scroller',
                      options = list(deferRender = F, dom = 't',
                                     columnDefs = list(list(className = 'dt-center',
                                                            targets = 5)),
                                     scrollY = 300, scroller = TRUE, scrollX = T))      
        })
      
      #refresh form (only the main panel)
      shinyjs::reset('main-panel')
      
      #save the new dataframe
      write.csv2(scores, file='www/scores.csv', sep="\t", row.names = FALSE)
    })
    
    output$scores <- DT::renderDataTable({
      DT::datatable(scores,
                    extensions = 'Scroller',
                    options = list(deferRender = F, dom = 't',
                                   columnDefs = list(list(className = 'dt-center',
                                                          targets = 5)),
                                   scrollY = 300, scroller = TRUE, scrollX = T))
    })
    
    #onglet stats
    output$graph_principal <- renderUI({
      
      if (input$choice_graph=='Total'){
        plotOutput("graph_total")
      }else if (input$choice_graph=='Stats individuelle'){
        LL <- list()
        LL[[1]] <- plotOutput("graph_perso")
        LL[[2]] <- div(style = 'overflow-x: scroll', 
                       DT::dataTableOutput('perf_perso'))
        return(LL)
      }else if (input$choice_graph=='Stats collectives'){
        plotOutput("perf_collective")
      }
      
    })
    
    #Global overview of the score evolution
    output$graph_total <- renderPlot({

      par(bg="#D9F0A3")
      colors <- c(RColorBrewer::brewer.pal(n = 12, name = 'Paired'),
                  RColorBrewer::brewer.pal(n = ncol(scores) - 10 - 12 , name = 'Set1'))
      
      ylim=c(min(scores[11:ncol(scores)]),
             max(scores[11:ncol(scores)]))
      xlim = c(0, nrow(scores))
      
      plot(x=1:nrow(scores), y=scores[,11], 
           col=colors[1],
           type = 'l', lwd = 2,
           ylim=ylim, xlim = xlim,
           xlab='Parties', ylab='Scores')
      
      for (j in 12:ncol(scores)){
        lines(x=1:nrow(scores), y=scores[,j],
              col=colors[j-10],
              type = 'l', lwd = 2)
      }
      
      legend(x=-13,
       y=ylim[2] + (ylim[2]-ylim[1])*0.065, 
       legend = colnames(scores)[11:18], 
       lwd = 2, col = colors[1:(18-10)], cex = 1, seg.len = 1, 
       bty="n", x.intersp=0.5, y.intersp=1)
      
      legend(x=-13,
             y=(ylim[1]+ylim[2])/2 - (ylim[2]-ylim[1])*0.04, 
             legend = colnames(scores)[19:ncol(scores)], 
             lwd = 2, col = colors[(18-10+1):(ncol(scores)-10)], cex = 1, 
             seg.len = 1, bty="n", x.intersp=0.5, y.intersp=1)
    })
    
    output$perf_perso_players <- renderUI({
      if (input$choice_graph=='Stats individuelle'){
        radioButtons(inputId='perf_perso_player',
                     label='Joueur',
                     choices=colnames(scores)[11:ncol(scores)])
      }
    })
    
    #Overview of the individual performances
    output$graph_perso <- renderPlot({
      
      par(bg="#D9F0A3")
      player_j <- colnames(scores)==input$perf_perso_player
      ylim=c(min(scores[player_j]),
             max(scores[player_j]))
      xlim = c(0, nrow(scores))
      
      plot(x=1:nrow(scores), y=scores[,player_j], 
           col='red',
           type = 'l', lwd = 2,
           ylim=ylim, xlim = xlim,
           xlab='Parties', ylab='Scores')
      
    })
    
    output$perf_perso <- DT::renderDataTable({

      res <- data.frame(matrix(NA, ncol = 12, nrow = 3))
      colnames(res) <- c('Parties jouées', 'PJ (en %)', 'Succes', 'Succes (en %)',
                         'Petite', 'Petite Reussi (en %)', 'Garde', 'Garde Reussi (en %)',
                         'Garde S', 'Garde S Reussi (en %)', 'Garde C', 'Garde C Reussi (en %)')
      rownames(res) <- c('Attaque', 'Défense', 'Total')


      pres <- presence(scores[,11:ncol(scores)])
      name=input$perf_perso_player

      #Parties jouées
      res['Attaque','Parties jouées'] <- length(which(scores[,'Preneur'] == name |
                                                        scores[,'Appelé'] == name  ))
      res['Défense','Parties jouées'] <- length(which(pres[, colnames(pres)==name])) -
                                          res['Attaque','Parties jouées']
      res['Attaque','PJ (en %)'] <- round( 100 * res['Attaque','Parties jouées'] / 
        (res['Attaque','Parties jouées'] + res['Défense','Parties jouées']), 0) 
      res['Défense','PJ (en %)'] <- round( 100 * res['Défense','Parties jouées'] / 
        (res['Attaque','Parties jouées'] + res['Défense','Parties jouées']), 0)
      
      #Succes total
      res['Attaque','Succes'] <- length(which((scores[,'Preneur'] == name |
                                                scores[,'Appelé'] == name) & scores$Ecart > 0))
      res['Défense','Succes'] <- length(which(scores[! (which(pres[, colnames(pres)==name]) %in% 
        which(scores[,'Preneur'] == name | scores[,'Appelé'] == name)),]$Ecart < 0))   
      res['Attaque','Succes (en %)'] <- round( 100 * res['Attaque','Succes'] /
                                              res['Attaque','Parties jouées'], 0)
      res['Défense','Succes (en %)'] <- round( 100 * res['Défense','Succes'] /
                                              res['Défense','Parties jouées'], 0)
      
      #Petite
      res['Attaque','Petite'] <- length(which((scores[,'Preneur'] == name |
                                                 scores[,'Appelé'] == name) & scores$Contrat == 'Petite'))
      res['Défense','Petite'] <- length(which(scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                          which(scores[,'Preneur'] == name | 
                                                                  scores[,'Appelé'] == name)),]$Contrat == 'Petite'))
      res['Attaque','Petite Reussi (en %)'] <- round( 100 * length(which((scores[,'Preneur'] == name |
                                                                           scores[,'Appelé'] == name) & 
                                                                           scores$Contrat == 'Petite' &
                                                                           scores$Ecart > 0)) /
                                                 res['Attaque','Petite'], 0)
      res['Défense','Petite Reussi (en %)'] <- round( 100 * length(which(scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                                                     which(scores[,'Preneur'] == name | 
                                                                                             scores[,'Appelé'] == name)),]$Contrat == 'Petite' &
                                                                           scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                                                       which(scores[,'Preneur'] == name | 
                                                                                               scores[,'Appelé'] == name)),]$Ecart < 0)) /
                                                  res['Défense','Petite'], 0)
      
      #garde
      res['Attaque','Garde'] <- length(which((scores[,'Preneur'] == name |
                                                scores[,'Appelé'] == name) & scores$Contrat == 'Garde'))
      res['Défense','Garde'] <- length(which(scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                         which(scores[,'Preneur'] == name | 
                                                                 scores[,'Appelé'] == name)),]$Contrat == 'Garde'))
      res['Attaque','Garde Reussi (en %)'] <- round( 100 * length(which((scores[,'Preneur'] == name |
                                                                           scores[,'Appelé'] == name) & 
                                                                          scores$Contrat == 'Garde' &
                                                                          scores$Ecart > 0)) /
                                                       res['Attaque','Garde'], 0)
      res['Défense','Garde Reussi (en %)'] <- round( 100 * length(which(scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                                                    which(scores[,'Preneur'] == name | 
                                                                                            scores[,'Appelé'] == name)),]$Contrat == 'Garde' &
                                                                          scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                                                      which(scores[,'Preneur'] == name | 
                                                                                              scores[,'Appelé'] == name)),]$Ecart < 0)) /
                                                       res['Défense','Garde'], 0)
      
      #garde sans
      res['Attaque','Garde S'] <- length(which((scores[,'Preneur'] == name |
                                                  scores[,'Appelé'] == name) & scores$Contrat == 'Garde Sans'))
      res['Défense','Garde S'] <- length(which(scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                           which(scores[,'Preneur'] == name | 
                                                                   scores[,'Appelé'] == name)),]$Contrat == 'Garde Sans'))
      res['Attaque','Garde S Reussi (en %)'] <- round( 100 * length(which((scores[,'Preneur'] == name |
                                                                             scores[,'Appelé'] == name) & 
                                                                            scores$Contrat == 'Garde Sans' &
                                                                            scores$Ecart > 0)) /
                                                         res['Attaque','Garde S'], 0)
      res['Défense','Garde S Reussi (en %)'] <- round( 100 * length(which(scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                                                      which(scores[,'Preneur'] == name | 
                                                                                              scores[,'Appelé'] == name)),]$Contrat == 'Garde Sans' &
                                                                            scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                                                        which(scores[,'Preneur'] == name | 
                                                                                                scores[,'Appelé'] == name)),]$Ecart < 0)) /
                                                         res['Défense','Garde S'], 0)
      
      #garde contre
      res['Attaque','Garde C'] <- length(which((scores[,'Preneur'] == name |
                                                  scores[,'Appelé'] == name) & scores$Contrat == 'Garde Contre'))
      res['Défense','Garde C'] <- length(which(scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                           which(scores[,'Preneur'] == name | 
                                                                   scores[,'Appelé'] == name)),]$Contrat == 'Garde Contre'))
      res['Attaque','Garde C Reussi (en %)'] <- round( 100 * length(which((scores[,'Preneur'] == name |
                                                                             scores[,'Appelé'] == name) & 
                                                                            scores$Contrat == 'Garde Contre' &
                                                                            scores$Ecart > 0)) /
                                                         res['Attaque','Garde C'], 0)
      res['Défense','Garde C Reussi (en %)'] <- round( 100 * length(which(scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                                                      which(scores[,'Preneur'] == name | 
                                                                                              scores[,'Appelé'] == name)),]$Contrat == 'Garde Contre' &
                                                                            scores[! (which(pres[, colnames(pres)==name]) %in% 
                                                                                        which(scores[,'Preneur'] == name | 
                                                                                                scores[,'Appelé'] == name)),]$Ecart < 0)) /
                                                         res['Défense','Garde C'], 0)
      
      res['Total',c(1,2,3,5,7,9,11)] <- res[1,c(1,2,3,5,7,9,11)] + res[2,c(1,2,3,5,7,9,11)] 
      DT::datatable(res,
                    extensions = 'Scroller',
                    options = list(deferRender = F, dom = 't',
                                   columnDefs = list(list(className = 'dt-center',
                                                          targets = 5)),
                                   scrollY = 300, scroller = TRUE, scrollX = T))
    })
    
    #Main group stats 
    output$perf_collective <- renderPlot({

      par(mfrow=c(2,2), bg="#D9F0A3")
      
      #presence
      pres <- presence(scores[,11:ncol(scores)])
      stat_pres <- sort(apply(pres, MARGIN = 2, FUN=function(x){ length(which(x)) }),
                        decreasing = TRUE)

      #graph presence    
      b <- barplot(stat_pres, ylab='Nombre de parties jouées', 
                   col='#FC4E2A', xaxt = 'n')
      
      text(x = b-0.55, y=-50, par("usr")[3], 
           labels = names(stat_pres), 
           srt = 90, pos = 1, cex = 1, xpd = T)
      
      #graph roi appele
      pie(table(scores$Couleur)[-1], 
          col=RColorBrewer::brewer.pal(n = 4, name = 'Set1'))

      #graph contrat
      pie(table(scores$Contrat), 
          col=RColorBrewer::brewer.pal(n = 4, name = 'Set1'))
      
      #graph Succes
      pie(c(length(which(scores$Ecart > 0)), 
            length(which(scores$Ecart < 0))),
          labels = c('Succès Attaque', 'Succès Défense'), 
          col=RColorBrewer::brewer.pal(n = 3, name = 'Set1'))
      
    })
    
})