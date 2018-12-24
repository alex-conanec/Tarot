require(shiny)
require(shinyjs)

scores <- read.csv("scores.csv", sep = ';', stringsAsFactors=FALSE, dec=',')

shinyServer(function(input, output) {
  
  ##onglet manche
  output$preneur <- renderUI({
    
    selectizeInput(inputId = "preneur", label = "Preneur", 
                   choices=input$active_players)
  
    })
  output$appele <- renderUI({
    
    selectizeInput(inputId = "appele", label = "Appelé", choices=input$active_players)
    
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
      scores <- read.csv("scores.csv", sep = ';', stringsAsFactors=FALSE, dec=',')
      row <- nrow(scores)+1
      scores[row,]$Date <- date() 
      scores[row,]$Preneur <- input$preneur
      scores[row,]$Contrat <- input$contrat
      scores[row,]$Couleur <- input$couleur
      scores[row,]$Appelé <- input$appele
      scores[row,]$Bouts <- input$bouts
      scores[row,]$Points <- input$point
      scores[row,]$Ecart <- input$point - c(56, 51, 41, 36)[input$bouts + 1]
      if ('Petit au bout' %in% input$annonces){
        bonus <- ifelse(input$petit_au_bout=='Reussi',10,-10)
      }
      scores[row,]$Marque <- sign(scores[row,]$Ecart) *
        (25 + abs(round(scores[row,]$Ecart/5)*5 + bonus)) *
      switch(input$contrat, "Petite"=1, "Garde"=2, "Garde Sans"=4, "Garde Contre"=6) 
      
      scores[row,]$Annonces <- if(!is.null(input$annonces)){
        
        res <- ''
        for (annonce in input$annonces){

          res <- paste(res, annonce, 
                       paste(input[[annonce]], collapse=', '),
                       ';', sep=' ')
        }
        res
      }
      
      #Traitement du score sans annonces
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
      
      #traitements score des annonces ----      
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
      
      output$scores <- DT::renderDataTable({
        DT::datatable(scores,
                      extensions = 'Scroller',
                      options = list(deferRender = F, dom = 't',
                                     columnDefs = list(list(className = 'dt-center',
                                                            targets = 5)),
                                     scrollY = 300, scroller = TRUE, scrollX = T))
      })
      
      shinyjs::reset('main-panel')
      
      write.csv2(scores, file='scores.csv', sep="\t", row.names = FALSE)
    })
    
    output$scores <- DT::renderDataTable({
      DT::datatable(scores,
                    extensions = 'Scroller',
                    options = list(deferRender = F, dom = 't',
                                          columnDefs = list(list(className = 'dt-center',
                                                                 targets = 5)),
                                          scrollY = 300, scroller = TRUE, scrollX = T))
    })
    
})