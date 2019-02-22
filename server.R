shinyServer(function(input, output, session) {
  
  ##onglet manche ----
  callModule(form, "main_form")
  callModule(validForm, "main_form")
  
  ##onglet scores ----
  value <- reactiveVal(scores)
  callModule(scoresDisplay, 'scores', value)
  callModule(scoresCalculation, "main_form", value)
  
  #onglet stats ----
  callModule(stats, "stats")
  callModule(makeGraphs, "stats", value)
  
  # output$graph_principal <- renderUI({
  #   
  #   if (input$choice_graph=='Total'){
  #     plotOutput("graph_total")
  #   }else if (input$choice_graph=='Stats individuelle'){
  #     tagList(plotOutput("graph_perso"),
  #             div(style = 'overflow-x: scroll', 
  #                 DT::dataTableOutput('perf_perso'))) 
  #     
  #   }else if (input$choice_graph=='Stats collectives'){
  #     plotOutput("perf_collective")
  #   }
  #   
  # })
  # 
  # #Global overview of the score evolution ----
  # output$graph_total <- renderPlot({
  #   
  #   par(bg="#D9F0A3")
  #   colors <- c(RColorBrewer::brewer.pal(n = 12, name = 'Paired'),
  #               RColorBrewer::brewer.pal(n = ncol(scores) - 10 - 12 , name = 'Set1'))
  #   
  #   ylim=c(min(scores[players_names_col]),
  #          max(scores[players_names_col]))
  #   xlim = c(0, nrow(scores))
  #   
  #   plot(x=1:nrow(scores), y=scores[,players_names_col[1]], 
  #        col=colors[1],
  #        type = 'l', lwd = 2,
  #        ylim=ylim, xlim = xlim,
  #        xlab='Parties', ylab='Scores')
  #   
  #   for (j in players_names_col){
  #     lines(x=1:nrow(scores), y=scores[,j],
  #           col=colors[j-players_names_col[1] + 1],
  #           type = 'l', lwd = 2)
  #   }
  #   
  #   legend(x=-13,
  #          y=ylim[2] + (ylim[2]-ylim[1])*0.065, 
  #          legend = colnames(scores)[players_names_col[1:8]], 
  #          lwd = 2, col = colors[1:8], cex = 1, seg.len = 1, 
  #          bty="n", x.intersp=0.5, y.intersp=1)
  #   
  #   legend(x=-13,
  #          y=(ylim[1]+ylim[2])/2 - (ylim[2]-ylim[1])*0.04, 
  #          legend = colnames(scores)[players_names_col[9:length(players_names_col)]], 
  #          lwd = 2, col = colors[9:length(players_names_col)], cex = 1, 
  #          seg.len = 1, bty="n", x.intersp=0.5, y.intersp=1)
  # })
  # 
  # #plots for invidual perf ----
  # output$perf_perso_players <- renderUI({
  #   if (input$choice_graph=='Stats individuelle'){
  #     radioButtons(inputId='perf_perso_player',
  #                  label='Joueur',
  #                  choices=colnames(scores)[players_names_col])
  #   }
  # })
  # 
  # #Overview of the individual performances
  # output$graph_perso <- renderPlot({
  #   
  #   par(bg="#D9F0A3")
  #   player_j <- colnames(scores)==input$perf_perso_player
  #   ylim=c(min(scores[player_j]),
  #          max(scores[player_j]))
  #   xlim = c(0, nrow(scores))
  #   
  #   plot(x=1:nrow(scores), y=scores[,player_j], 
  #        col='red',
  #        type = 'l', lwd = 2,
  #        ylim=ylim, xlim = xlim,
  #        xlab='Parties', ylab='Scores')
  #   
  # })
  # 
  # output$perf_perso <- DT::renderDataTable({
  #   
  #   res <- data.frame(matrix(NA, ncol = 12, nrow = 4))
  #   colnames(res) <- c('Parties jouées', 'PJ (en %)', 'Succes', 'Succes (en %)',
  #                      'Petite', 'Petite Reussi (en %)', 'Garde', 'Garde Reussi (en %)',
  #                      'Garde S', 'Garde S Reussi (en %)', 'Garde C', 'Garde C Reussi (en %)')
  #   rownames(res) <- c('Preneur', 'Appelé','Défense', 'Total')
  #   
  #   
  #   pres <- presence(scores[,players_names_col])
  #   name=input$perf_perso_player
  #   # name='Guillaume'
  #   
  #   #Parties jouées
  #   res['Preneur','Parties jouées'] <- length(which(scores[,'Preneur'] == name))
  #   res['Appelé','Parties jouées'] <- length(which(scores[,'Appelé'] == name  ))
  #   res['Défense','Parties jouées'] <- length(which(pres[, colnames(pres)==name])) -
  #     res['Preneur','Parties jouées'] - res['Appelé','Parties jouées']
  #   res['Preneur','PJ (en %)'] <- round( 100 * res['Preneur','Parties jouées'] / 
  #                                          (res['Preneur','Parties jouées'] + res['Appelé','Parties jouées'] + res['Défense','Parties jouées']), 0)
  #   res['Appelé','PJ (en %)'] <- round( 100 * res['Appelé','Parties jouées'] / 
  #                                         (res['Preneur','Parties jouées'] + res['Appelé','Parties jouées'] + res['Défense','Parties jouées']), 0)
  #   res['Défense','PJ (en %)'] <- round( 100 * res['Défense','Parties jouées'] / 
  #                                          (res['Preneur','Parties jouées'] + res['Appelé','Parties jouées'] + res['Défense','Parties jouées']), 0)
  #   
  #   #Succes total
  #   res['Preneur','Succes'] <- length(which((scores[,'Preneur'] == name) & scores$Ecart > 0))
  #   res['Appelé','Succes'] <- length(which((scores[,'Appelé'] == name) & scores$Ecart > 0))
  #   
  #   res['Défense','Succes'] <- length(which(scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                             which(scores[,'Preneur'] == name | 
  #                                                                     scores[,'Appelé'] == name))),]$Ecart < 0))   
  #   
  #   
  #   res['Preneur','Succes (en %)'] <- round( 100 * res['Preneur','Succes'] /
  #                                              res['Preneur','Parties jouées'], 0)
  #   res['Appelé','Succes (en %)'] <- round( 100 * res['Appelé','Succes'] /
  #                                             res['Appelé','Parties jouées'], 0)
  #   res['Défense','Succes (en %)'] <- round( 100 * res['Défense','Succes'] /
  #                                              res['Défense','Parties jouées'], 0)
  #   
  #   #Petite
  #   res['Preneur','Petite'] <- length(which((scores[,'Preneur'] == name) & scores$Contrat == 'Petite'))
  #   res['Appelé','Petite'] <- length(which((scores[,'Appelé'] == name) & scores$Contrat == 'Petite'))
  #   res['Défense','Petite'] <- length(which(scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                             which(scores[,'Preneur'] == name | 
  #                                                                     scores[,'Appelé'] == name))),]$Contrat == 'Petite'))
  #   
  #   res['Preneur','Petite Reussi (en %)'] <- round( 100 * length(which((scores[,'Preneur'] == name) & 
  #                                                                        scores$Contrat == 'Petite' &
  #                                                                        scores$Ecart > 0)) /
  #                                                     res['Preneur','Petite'], 0)
  #   res['Appelé','Petite Reussi (en %)'] <- round( 100 * length(which((scores[,'Appelé'] == name) & 
  #                                                                       scores$Contrat == 'Petite' &
  #                                                                       scores$Ecart > 0)) /
  #                                                    res['Appelé','Petite'], 0)
  #   res['Défense','Petite Reussi (en %)'] <- round( 100 * length(which(scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                                                        which(scores[,'Preneur'] == name | 
  #                                                                                                scores[,'Appelé'] == name))),]$Contrat == 'Petite' &
  #                                                                        scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                                                          which(scores[,'Preneur'] == name | 
  #                                                                                                  scores[,'Appelé'] == name))),]$Ecart < 0)) /
  #                                                     res['Défense','Petite'], 0)
  #   
  #   #garde
  #   res['Preneur','Garde'] <- length(which((scores[,'Preneur'] == name) & scores$Contrat == 'Garde'))
  #   res['Appelé','Garde'] <- length(which((scores[,'Appelé'] == name) & scores$Contrat == 'Garde'))
  #   res['Défense','Garde'] <- length(which(scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                            which(scores[,'Preneur'] == name | 
  #                                                                    scores[,'Appelé'] == name))),]$Contrat == 'Garde'))
  #   res['Preneur','Garde Reussi (en %)'] <- round( 100 * length(which((scores[,'Preneur'] == name) & 
  #                                                                       scores$Contrat == 'Garde' &
  #                                                                       scores$Ecart > 0)) /
  #                                                    res['Preneur','Garde'], 0)
  #   res['Appelé','Garde Reussi (en %)'] <- round( 100 * length(which((scores[,'Appelé'] == name) & 
  #                                                                      scores$Contrat == 'Garde' &
  #                                                                      scores$Ecart > 0)) /
  #                                                   res['Appelé','Garde'], 0)
  #   res['Défense','Garde Reussi (en %)'] <- round( 100 * length(which(scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                                                       which(scores[,'Preneur'] == name | 
  #                                                                                               scores[,'Appelé'] == name))),]$Contrat == 'Garde' &
  #                                                                       scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                                                         which(scores[,'Preneur'] == name | 
  #                                                                                                 scores[,'Appelé'] == name))),]$Ecart < 0)) /
  #                                                    res['Défense','Garde'], 0)
  #   
  #   #garde sans
  #   res['Preneur','Garde S'] <- length(which((scores[,'Preneur'] == name) & scores$Contrat == 'Garde Sans'))
  #   res['Appelé','Garde S'] <- length(which((scores[,'Appelé'] == name) & scores$Contrat == 'Garde Sans'))
  #   res['Défense','Garde S'] <- length(which(scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                              which(scores[,'Preneur'] == name | 
  #                                                                      scores[,'Appelé'] == name))),]$Contrat == 'Garde Sans'))
  #   res['Preneur','Garde S Reussi (en %)'] <- round( 100 * length(which((scores[,'Preneur'] == name) & 
  #                                                                         scores$Contrat == 'Garde Sans' &
  #                                                                         scores$Ecart > 0)) /
  #                                                      res['Preneur','Garde S'], 0)
  #   res['Appelé','Garde S Reussi (en %)'] <- round( 100 * length(which((scores[,'Appelé'] == name) & 
  #                                                                        scores$Contrat == 'Garde Sans' &
  #                                                                        scores$Ecart > 0)) /
  #                                                     res['Appelé','Garde S'], 0)
  #   res['Défense','Garde S Reussi (en %)'] <- round( 100 * length(which(scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                                                         which(scores[,'Preneur'] == name | 
  #                                                                                                 scores[,'Appelé'] == name))),]$Contrat == 'Garde Sans' &
  #                                                                         scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                                                           which(scores[,'Preneur'] == name | 
  #                                                                                                   scores[,'Appelé'] == name))),]$Ecart < 0)) /
  #                                                      res['Défense','Garde S'], 0)
  #   
  #   #garde contre
  #   res['Preneur','Garde C'] <- length(which((scores[,'Preneur'] == name) & scores$Contrat == 'Garde Contre'))
  #   res['Appelé','Garde C'] <- length(which((scores[,'Appelé'] == name) & scores$Contrat == 'Garde Contre'))
  #   res['Défense','Garde C'] <- length(which(scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                              which(scores[,'Preneur'] == name | 
  #                                                                      scores[,'Appelé'] == name))),]$Contrat == 'Garde Contre'))
  #   res['Preneur','Garde C Reussi (en %)'] <- round( 100 * length(which((scores[,'Preneur'] == name) & 
  #                                                                         scores$Contrat == 'Garde Contre' &
  #                                                                         scores$Ecart > 0)) /
  #                                                      res['Preneur','Garde C'], 0)
  #   res['Appelé','Garde C Reussi (en %)'] <- round( 100 * length(which((scores[,'Appelé'] == name) & 
  #                                                                        scores$Contrat == 'Garde Contre' &
  #                                                                        scores$Ecart > 0)) /
  #                                                     res['Appelé','Garde C'], 0)
  #   res['Défense','Garde C Reussi (en %)'] <- round( 100 * length(which(scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                                                         which(scores[,'Preneur'] == name | 
  #                                                                                                 scores[,'Appelé'] == name))),]$Contrat == 'Garde Contre' &
  #                                                                         scores[which(! (which(pres[, colnames(pres)==name]) %in% 
  #                                                                                           which(scores[,'Preneur'] == name | 
  #                                                                                                   scores[,'Appelé'] == name))),]$Ecart < 0)) /
  #                                                      res['Défense','Garde C'], 0)
  #   
  #   res['Total',c(1,2,3,5,7,9,11)] <- res[1,c(1,2,3,5,7,9,11)] + res[2,c(1,2,3,5,7,9,11)] 
  #   res['Total',c(1,2,3,5,7,9,11)] <- apply(res[-4,c(1,2,3,5,7,9,11)], MARGIN = 2, FUN = sum)
  #   DT::datatable(res,
  #                 extensions = 'Scroller',
  #                 options = list(deferRender = F, dom = 't',
  #                                columnDefs = list(list(className = 'dt-center',
  #                                                       targets = 5)),
  #                                scrollY = 300, scroller = TRUE, scrollX = T))
  # })
  # 
  # #Main group stats ---- 
  # output$perf_collective <- renderPlot({
  #   
  #   par(mfrow=c(2,2), bg="#D9F0A3")
  #   
  #   #presence
  #   pres <- presence(scores[,players_names_col])
  #   stat_pres <- sort(apply(pres, MARGIN = 2, FUN=function(x){ length(which(x)) }),
  #                     decreasing = TRUE)
  #   
  #   #graph presence    
  #   b <- barplot(stat_pres, ylab='Nombre de parties jouées', 
  #                col='#FC4E2A', xaxt = 'n')
  #   
  #   text(x = b-0.55, y=-50, par("usr")[3], 
  #        labels = names(stat_pres), 
  #        srt = 90, pos = 1, cex = 1, xpd = T)
  #   
  #   #graph roi appele
  #   pie(table(scores$Couleur)[-1], 
  #       col=RColorBrewer::brewer.pal(n = 4, name = 'Set1'))
  #   
  #   #graph contrat
  #   pie(table(scores$Contrat), 
  #       col=RColorBrewer::brewer.pal(n = 4, name = 'Set1'))
  #   
  #   #graph Succes
  #   pie(c(length(which(scores$Ecart > 0)), 
  #         length(which(scores$Ecart < 0))),
  #       labels = c('Succès Attaque', 'Succès Défense'), 
  #       col=RColorBrewer::brewer.pal(n = 3, name = 'Set1'))
  #   
  # })
  
  
})