statsUI <- function(id) {
  
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(radioButtons(inputId=ns("choice_graph"),
                              label="Choix graphique",
                              choices=c('Classement',
                                        'Evolution',
                                        'Stats individuelle',
                                        'Stats collectives'),
                              selected='Classement'),
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
    
    if (input$choice_graph=='Evolution'){
      plotOutput(ns("graph_evolution"))
    }else if (input$choice_graph=='Stats individuelle'){
      tagList(plotOutput(ns("graph_perso")),
              DT::dataTableOutput(ns('perf_perso'))) 
      
    }else if (input$choice_graph=='Stats collectives'){
      # plotOutput(ns("perf_collective"))
      tagList(
        fluidRow(column(width = 6, plotOutput(ns("pres"))),
                 column(width = 6, plotOutput(ns("contract")))
        ),
        fluidRow(column(width = 6, plotOutput(ns("king"))),
                 column(width = 6, plotOutput(ns("success")))
        )
      )
      
    }else if (input$choice_graph=='Classement'){
      fluidRow(column(width = 6, plotOutput(ns("barClass"))),
               column(width = 6, DT::dataTableOutput(ns('tabClass')))
               )
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
  
  #graph evolution
  output$graph_evolution <- renderPlot({
    
    if (!is.null(scores())){
      cumul <- scores() %>% select(-date, -heure, -preneur, -appele, -contrat, -couleur, 
                                   -bouts, -points, -marque, -nb_joueur, -petit, -annonce)
      for (i in 2:NROW(cumul)){
        cumul[i, -1] <- colSums(rbind(cumul[i, -1], cumul[i - 1, -1]), 
                                na.rm = TRUE)
      }
      
      cumul %>% 
        gather(key = player, value = score, -id) %>% 
        ggplot() +
        geom_line(aes(x = id, y = score, colour = player)) +
        theme(plot.background = element_rect(fill = "#D9F0A3"))
    }
    
  })

  
  output$perf_perso <- DT::renderDataTable({
    
    if (!is.null(scores())){
      df <- summary_stat_perso(name = input$perf_perso_players,
                               scores = scores())
      
      DT::datatable(df, extensions = 'Scroller',
                    options = list(deferRender = F, dom = 't',
                                   columnDefs = list(list(className = 'dt-center',
                                                          targets = 5)),
                                   scrollY = 300, scroller = TRUE, scrollX = F))
      }
  })
  
  
  #Overview of the individual performances
  output$graph_perso <- renderPlot({

    if (!is.null(scores())){
      cumul <- scores() %>% select(id, score = input$perf_perso_players) %>% 
        filter(!is.na(score)) %>% 
        mutate(id = 1:NROW(.))

      if (NROW(cumul) > 1){
        for (i in 2:NROW(cumul)){
          cumul[i, -1] <- cumul[i, -1] + cumul[i - 1, -1]
        }
      }
      cumul %>%  
        ggplot() +
        geom_line(aes(x = id, y = score)) +
        theme(plot.background = element_rect(fill = "#D9F0A3"))
      
    }
  })


  #Main group stats ----
  output$pres <- renderPlot({

    #graph presence
    # s <- scores() #juste pour faire de la reactivite
    if (!is.null(scores())){
      dbGetQuery(con, paste0("SELECT joueur_id AS player",
                             ", count(partie_tarot_id) AS n ",
                             "FROM scores_tarot ",
                             "GROUP BY joueur_id ",
                             "ORDER BY count(partie_tarot_id) DESC")) %>% 
        mutate(player = factor(player, levels = player)) %>% 
        ggplot() +
        geom_bar(aes(x = player, y = n), stat="identity", fill = "red") +
        xlab('') +
        ylab('') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.35, 
                                         size = 15),
              axis.title=element_blank(),
              # aspect.ratio = 1,
              plot.background = element_rect(fill = "#D9F0A3")) 
    }
  })
  
  output$contract <- renderPlot({

    if (!is.null(scores())){
      par(bg="#D9F0A3")
      pie(table(scores()$contrat),
          col=RColorBrewer::brewer.pal(n = 4, name = 'Set1'))
    }

  })

  output$king <- renderPlot({
    
    if (!is.null(scores())){
      par(bg="#D9F0A3")
      pie(table(scores()$couleur),
          col=RColorBrewer::brewer.pal(n = 4, name = 'Set1'))
      }

  })  
  
  output$success <- renderPlot({
    
    if (!is.null(scores())){
      par(bg="#D9F0A3")
      pie(c(length(which(scores()$marque > 0)),
            length(which(scores()$marque < 0))),
          labels = c('Succès Attaque', 'Succès Défense'),
          col=RColorBrewer::brewer.pal(n = 3, name = 'Set1'))
    }
    
  })
  
  #Classement barplot
  output$barClass <- renderPlot({
    
    if (!is.null(scores())){
      cumul <- scores() %>% 
        select(-id, -date, -heure, -preneur, -appele, -contrat, -couleur,
               -bouts, -points, -marque, -nb_joueur, -petit, -annonce) %>% 
        apply(2, sum, na.rm = TRUE)
      
      data.frame(player = names(cumul), score = cumul) %>% 
        arrange(desc(score)) %>%
        mutate(player = factor(player, levels = player)) %>% 
        ggplot() +
        geom_bar(aes(x = player, y = score), stat="identity", fill = "red") +
        xlab('') +
        ylab('') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.35, 
                                         size = 15),
              axis.title=element_blank(),
              # aspect.ratio = 1,
              plot.background = element_rect(fill = "#D9F0A3"))
      } 
  })

  #Classement tableau
  output$tabClass <- DT::renderDataTable({
    
    if (!is.null(scores())){
      cumul <- scores() %>% 
        select(-id, -date, -heure, -preneur, -appele, -contrat, -couleur,
               -bouts, -points, -marque, -nb_joueur, -petit, -annonce) %>% 
        apply(2, sum, na.rm = TRUE)
      
      df <- data.frame(player = names(cumul), score = cumul) %>% 
        arrange(desc(score)) %>%
        mutate(player = factor(player, levels = player))
      
      DT::datatable(df, extensions = 'Scroller',
                    options = list(deferRender = F, dom = 't',
                                    scrollY = 300, 
                                    scroller = TRUE,
                                    scrollX = F))
      }
  })
  
}


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

