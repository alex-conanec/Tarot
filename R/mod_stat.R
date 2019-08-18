# Module UI
  
#' @title   mod_stat_ui and mod_stat_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_stat
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_stat_ui <- function(id){
  ns <- NS(id)
  tagList(
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
  )
}
    
# Module Server
    
#' @rdname mod_stat
#' @export
#' @keywords internal
    
mod_stat_server <- function(input, output, session, con_param, scores){
  ns <- session$ns
  
  #choice of graph
  output$graph_principal <- renderUI({

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
    
    if (input$choice_graph=='Stats individuelle'){
      con <- do.call(db_con, con_param)
      players <- get_ordered_player_list(con)
      on.exit(dbDisconnect(con), add=TRUE)
      
      radioButtons(inputId=ns('perf_perso_players'),
                   label='Joueur',
                   choices=players)
    }
  })
  
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
    if (!is.null(scores())){
      
      con <- do.call(db_con, con_param)
      
      data <- dbGetQuery(con, paste0("SELECT joueur_id AS player",
                               ", count(partie_tarot_id) AS n ",
                               "FROM scores_tarot ",
                               "GROUP BY joueur_id ",
                               "ORDER BY count(partie_tarot_id) DESC")) %>%
        mutate(player = factor(player, levels = player)) 
      on.exit(dbDisconnect(con), add=TRUE)
      ggplot(data = data) +
        geom_bar(aes(x = player, y = n), stat="identity", fill = "red") +
        xlab('') +
        ylab('') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.35,
                                         size = 15),
              axis.title=element_blank(),
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
    
## To be copied in the UI
# mod_stat_ui("stat_ui_1")
    
## To be copied in the server
# callModule(mod_stat_server, "stat_ui_1")
 
