statsUI <- function(id) {
  
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(radioButtons(inputId=ns("choice_graph"),
                              label="Choix graphique",
                              choices=c('Total',
                                        'Stats individuelle',
                                        'Stats collectives'),
                              selected='Total'),
                 uiOutput(ns("perf_perso_players"))),
    mainPanel(wellPanel(shinyjs::useShinyjs(),
                        id = ns("main-panel2"),
                        uiOutput(ns("graph_principal"))
                        )
              )
    )
}

stats <- function(input, output, session){
  
  #choice of graph
  output$graph_principal <- renderUI({
    ns <- session$ns
    
    if (input$choice_graph=='Total'){
      plotOutput(ns("graph_total"))
    }else if (input$choice_graph=='Stats individuelle'){
      tagList(plotOutput(ns("graph_perso")),
              div(style = 'overflow-x: scroll', 
                  DT::dataTableOutput(ns('perf_perso')))) 
      
    }else if (input$choice_graph=='Stats collectives'){
      plotOutput(ns("perf_collective"))
    }
    
  })
  
  #plots for invidual perf, choice of the player
  output$perf_perso_players <- renderUI({

    ns <- session$ns
    if (input$choice_graph=='Stats individuelle'){
      radioButtons(inputId=ns('perf_perso_players'),
                   label='Joueur',
                   choices=players)
    }
  })
  
  
}

makeGraphs <- function(input, output, session, scores){
  
  #graph total
  output$graph_total <- renderPlot({
    
    tab_scores <- scores()
    
    par(bg="#D9F0A3")
    colors <- c(RColorBrewer::brewer.pal(n = 12, name = 'Paired'),
                RColorBrewer::brewer.pal(n = ncol(tab_scores) - 10 - 12 , name = 'Set1'))

    ylim=c(min(tab_scores[players_names_col]),
           max(tab_scores[players_names_col]))
    xlim = c(0, nrow(tab_scores))
    
    month <- stringr::str_extract(month.name, "^.{3}")
    date <- as.Date(
      paste(
        #jour
        stringr::str_extract(tab_scores$Date, "[0-9]{2}"),
        
        #mois
        stringr::str_pad(sapply(stringr::str_extract(tab_scores$Date, paste(month, collapse = '|')),
                                function(x){
                                  which(x==month)
                                }), 2, pad = "0"),
        
        #annee
        stringr::str_extract(tab_scores$Date, "[0-9]{4}"),
        
        sep = '/'),
      format = "%d/%m/%Y"
    )
    
    plot(x=1:nrow(tab_scores), y=tab_scores[,players_names_col[1]],
         col=colors[1],
         type = 'l', lwd = 2,
         ylim=ylim, xlim = xlim,
         xaxt = 'n',
         xlab='Date', ylab='Scores')
    
    max_date=10
    text(x=seq(from = 1, to = nrow(tab_scores), length.out = max_date),
         y=min - 0.1*(ylim[2]-ylim[1]),
         srt = 60,
         par("usr")[3],
         pos = 1,
         cex = 1,
         xpd = T,
         labels=format(seq.Date(min(date), max(date), 
                                length.out = max_date),
                       "%d/%m/%y")
         )
    
    for (j in players_names_col){
      lines(x=1:nrow(tab_scores), y=tab_scores[,j],
            col=colors[j-players_names_col[1] + 1],
            type = 'l', lwd = 2)
    }

    legend(x=-13,
           y=ylim[2] + (ylim[2]-ylim[1])*0.065,
           legend = colnames(tab_scores)[players_names_col[1:8]],
           lwd = 2, col = colors[1:8], cex = 1, seg.len = 1,
           bty="n", x.intersp=0.5, y.intersp=1)

    legend(x=-13,
           y=(ylim[1]+ylim[2])/2 - (ylim[2]-ylim[1])*0.04,
           legend = colnames(tab_scores)[players_names_col[9:length(players_names_col)]],
           lwd = 2, col = colors[9:length(players_names_col)], cex = 1,
           seg.len = 1, bty="n", x.intersp=0.5, y.intersp=1)
  })

  
  output$perf_perso <- DT::renderDataTable({
    
    tab_scores <- scores()
    res <- data.frame(matrix(NA, ncol = 12, nrow = 4))
    colnames(res) <- c('Parties jouées', 'PJ (en %)', 'Succes', 'Succes (en %)',
                       'Petite', 'Petite Reussi (en %)', 'Garde', 'Garde Reussi (en %)',
                       'Garde S', 'Garde S Reussi (en %)', 'Garde C', 'Garde C Reussi (en %)')
    rownames(res) <- c('Preneur', 'Appele','Défense', 'Total')
    
    pres <- presence(tab_scores[,players_names_col])
    name <- input$perf_perso_players

    #Parties jouées
    res['Preneur','Parties jouées'] <- length(which(tab_scores[,'Preneur'] == name))
    res['Appele','Parties jouées'] <- length(which(tab_scores[,'Appele'] == name  ))
    res['Défense','Parties jouées'] <- length(which(pres[, colnames(pres)==name])) -
    res['Preneur','Parties jouées'] - res['Appele','Parties jouées']
    res['Preneur','PJ (en %)'] <- round( 100 * res['Preneur','Parties jouées'] /
    (res['Preneur','Parties jouées'] + res['Appele','Parties jouées'] + res['Défense','Parties jouées']), 0)
    res['Appele','PJ (en %)'] <- round( 100 * res['Appele','Parties jouées'] /
                                          (res['Preneur','Parties jouées'] + res['Appele','Parties jouées'] + res['Défense','Parties jouées']), 0)
    res['Défense','PJ (en %)'] <- round( 100 * res['Défense','Parties jouées'] /
                                           (res['Preneur','Parties jouées'] + res['Appele','Parties jouées'] + res['Défense','Parties jouées']), 0)

    #Succes total
    res['Preneur','Succes'] <- length(which((tab_scores[,'Preneur'] == name) & tab_scores$Ecart > 0))
    res['Appele','Succes'] <- length(which((tab_scores[,'Appele'] == name) & tab_scores$Ecart > 0))

    res['Défense','Succes'] <- length(which(tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                      which(tab_scores[,'Preneur'] == name |
                                                                              tab_scores[,'Appele'] == name))),]$Ecart < 0))


    res['Preneur','Succes (en %)'] <- round( 100 * res['Preneur','Succes'] /
                                               res['Preneur','Parties jouées'], 0)
    res['Appele','Succes (en %)'] <- round( 100 * res['Appele','Succes'] /
                                              res['Appele','Parties jouées'], 0)
    res['Défense','Succes (en %)'] <- round( 100 * res['Défense','Succes'] /
                                               res['Défense','Parties jouées'], 0)

    #Petite
    res['Preneur','Petite'] <- length(which((tab_scores[,'Preneur'] == name) & tab_scores$Contrat == 'Petite'))
    res['Appele','Petite'] <- length(which((tab_scores[,'Appele'] == name) & tab_scores$Contrat == 'Petite'))
    res['Défense','Petite'] <- length(which(tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                      which(tab_scores[,'Preneur'] == name |
                                                                              tab_scores[,'Appele'] == name))),]$Contrat == 'Petite'))

    res['Preneur','Petite Reussi (en %)'] <- round( 100 * length(which((tab_scores[,'Preneur'] == name) &
                                                                         tab_scores$Contrat == 'Petite' &
                                                                         tab_scores$Ecart > 0)) /
                                                      res['Preneur','Petite'], 0)
    res['Appele','Petite Reussi (en %)'] <- round( 100 * length(which((tab_scores[,'Appele'] == name) &
                                                                        tab_scores$Contrat == 'Petite' &
                                                                        tab_scores$Ecart > 0)) /
                                                     res['Appele','Petite'], 0)
    res['Défense','Petite Reussi (en %)'] <- round( 100 * length(which(tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                                                 which(tab_scores[,'Preneur'] == name |
                                                                                                         tab_scores[,'Appele'] == name))),]$Contrat == 'Petite' &
                                                                         tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                                                   which(tab_scores[,'Preneur'] == name |
                                                                                                           tab_scores[,'Appele'] == name))),]$Ecart < 0)) /
                                                      res['Défense','Petite'], 0)

    #garde
    res['Preneur','Garde'] <- length(which((tab_scores[,'Preneur'] == name) & tab_scores$Contrat == 'Garde'))
    res['Appele','Garde'] <- length(which((tab_scores[,'Appele'] == name) & tab_scores$Contrat == 'Garde'))
    res['Défense','Garde'] <- length(which(tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                     which(tab_scores[,'Preneur'] == name |
                                                                             tab_scores[,'Appele'] == name))),]$Contrat == 'Garde'))
    res['Preneur','Garde Reussi (en %)'] <- round( 100 * length(which((tab_scores[,'Preneur'] == name) &
                                                                        tab_scores$Contrat == 'Garde' &
                                                                        tab_scores$Ecart > 0)) /
                                                     res['Preneur','Garde'], 0)
    res['Appele','Garde Reussi (en %)'] <- round( 100 * length(which((tab_scores[,'Appele'] == name) &
                                                                       tab_scores$Contrat == 'Garde' &
                                                                       tab_scores$Ecart > 0)) /
                                                    res['Appele','Garde'], 0)
    res['Défense','Garde Reussi (en %)'] <- round( 100 * length(which(tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                                                which(tab_scores[,'Preneur'] == name |
                                                                                                        tab_scores[,'Appele'] == name))),]$Contrat == 'Garde' &
                                                                        tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                                                  which(tab_scores[,'Preneur'] == name |
                                                                                                          tab_scores[,'Appele'] == name))),]$Ecart < 0)) /
                                                     res['Défense','Garde'], 0)

    #garde sans
    res['Preneur','Garde S'] <- length(which((tab_scores[,'Preneur'] == name) & tab_scores$Contrat == 'Garde Sans'))
    res['Appele','Garde S'] <- length(which((tab_scores[,'Appele'] == name) & tab_scores$Contrat == 'Garde Sans'))
    res['Défense','Garde S'] <- length(which(tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                       which(tab_scores[,'Preneur'] == name |
                                                                               tab_scores[,'Appele'] == name))),]$Contrat == 'Garde Sans'))
    res['Preneur','Garde S Reussi (en %)'] <- round( 100 * length(which((tab_scores[,'Preneur'] == name) &
                                                                          tab_scores$Contrat == 'Garde Sans' &
                                                                          tab_scores$Ecart > 0)) /
                                                       res['Preneur','Garde S'], 0)
    res['Appele','Garde S Reussi (en %)'] <- round( 100 * length(which((tab_scores[,'Appele'] == name) &
                                                                         tab_scores$Contrat == 'Garde Sans' &
                                                                         tab_scores$Ecart > 0)) /
                                                      res['Appele','Garde S'], 0)
    res['Défense','Garde S Reussi (en %)'] <- round( 100 * length(which(tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                                              which(tab_scores[,'Preneur'] == name |
                                                                                                      tab_scores[,'Appele'] == name))),]$Contrat == 'Garde Sans' &
                                                                          tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                                                which(tab_scores[,'Preneur'] == name |
                                                                                                        tab_scores[,'Appele'] == name))),]$Ecart < 0)) /
                                                       res['Défense','Garde S'], 0)

    #garde contre
    res['Preneur','Garde C'] <- length(which((tab_scores[,'Preneur'] == name) & tab_scores$Contrat == 'Garde Contre'))
    res['Appele','Garde C'] <- length(which((tab_scores[,'Appele'] == name) & tab_scores$Contrat == 'Garde Contre'))
    res['Défense','Garde C'] <- length(which(tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                   which(tab_scores[,'Preneur'] == name |
                                                                           tab_scores[,'Appele'] == name))),]$Contrat == 'Garde Contre'))
    res['Preneur','Garde C Reussi (en %)'] <- round( 100 * length(which((tab_scores[,'Preneur'] == name) &
                                                                          tab_scores$Contrat == 'Garde Contre' &
                                                                          tab_scores$Ecart > 0)) /
                                                       res['Preneur','Garde C'], 0)
    res['Appele','Garde C Reussi (en %)'] <- round( 100 * length(which((tab_scores[,'Appele'] == name) &
                                                                         tab_scores$Contrat == 'Garde Contre' &
                                                                         tab_scores$Ecart > 0)) /
                                                      res['Appele','Garde C'], 0)
    res['Défense','Garde C Reussi (en %)'] <- round( 100 * length(which(tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                                              which(tab_scores[,'Preneur'] == name |
                                                                                                      tab_scores[,'Appele'] == name))),]$Contrat == 'Garde Contre' &
                                                                          tab_scores[which(! (which(pres[, colnames(pres)==name]) %in%
                                                                                                which(tab_scores[,'Preneur'] == name |
                                                                                                        tab_scores[,'Appele'] == name))),]$Ecart < 0)) /
                                                       res['Défense','Garde C'], 0)

    res['Total',c(1,2,3,5,7,9,11)] <- res[1,c(1,2,3,5,7,9,11)] + res[2,c(1,2,3,5,7,9,11)]
    res['Total',c(1,2,3,5,7,9,11)] <- apply(res[-4,c(1,2,3,5,7,9,11)], MARGIN = 2, FUN = sum)
    rownames(res)[2] <- 'Appelé'
    DT::datatable(res,
                  extensions = 'Scroller',
                  options = list(deferRender = F, dom = 't',
                                 columnDefs = list(list(className = 'dt-center',
                                                        targets = 5)),
                                 scrollY = 300, scroller = TRUE, scrollX = T))
  })
  
  
  #Overview of the individual performances
  output$graph_perso <- renderPlot({

    tab_scores <- scores()
    pres <- presence(tab_scores[,players_names_col])[, input$perf_perso_players]
    scores_ind <- tab_scores[pres, input$perf_perso_players]
    
    par(bg="#D9F0A3")
    plot(x=1:length(scores_ind), y=scores_ind,
         col='red',
         type = 'l', lwd = 2,
         xlab='Parties', ylab='Scores')

  })


  #Main group stats ----
  output$perf_collective <- renderPlot({

    tab_scores <- scores()
    par(mfrow=c(2,2), bg="#D9F0A3")

    #presence
    pres <- presence(tab_scores[,players_names_col])
    stat_pres <- sort(apply(pres, MARGIN = 2, FUN=function(x){length(which(x))}),
                      decreasing = TRUE)

    #graph presence
    b <- barplot(stat_pres, ylab='Nombre de parties jouées',
                 col='#FC4E2A', xaxt = 'n')

    text(x = b-0.25, y=-max(stat_pres)/7, par("usr")[3],
         labels = names(stat_pres),
         srt = 90, pos = 1, cex = 1, xpd = T)
    
    #graph roi appele
    pie(table(tab_scores$Couleur),
        col=RColorBrewer::brewer.pal(n = 4, name = 'Set1'))

    #graph contrat
    pie(table(tab_scores$Contrat),
        col=RColorBrewer::brewer.pal(n = 4, name = 'Set1'))

    #graph Succes
    pie(c(length(which(tab_scores$Ecart > 0)),
          length(which(tab_scores$Ecart < 0))),
        labels = c('Succès Attaque', 'Succès Défense'),
        col=RColorBrewer::brewer.pal(n = 3, name = 'Set1'))

  })
  
}
  