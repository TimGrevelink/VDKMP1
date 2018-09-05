function(input, output, session) {
  
  # UI------
  output$ui_sidebar <- renderMenu({
    tabs.out <- list() 
    tabs.out[[length(tabs.out)+1]] <- fileInput("file1", "Kies CSV File", placeholder = "Geen CSV geselecteerd")
    tabs.out[[length(tabs.out)+1]] <- menuItem("Ingelezen tabel", tabName = "dataTable", 
                                               icon = shiny::icon("fas fa-table", lib = "font-awesome"))
    tabs.out[[length(tabs.out)+1]] <- menuItem("Radarplot", tabName = "plot", 
                                               icon = shiny::icon("fas fa-chart-line", lib = "font-awesome"))
    
    return(sidebarMenu(
      .list = tabs.out,
      id = "sidebarmenu")
    )
  })
  
  output$test_ui <- renderUI({
    items <- c(
      list(tabItem(tabName = "dataTable",
                   fluidPage(
                     fluidRow(
                       div(class = "col-md-12 col-lg-10", 
                           box(title = "Antwoorden tabel", status = "primary", solidHeader = TRUE, width = "100%",
                               DT::dataTableOutput("csvData")
                           )
                           
                           
                       )
                     )
                   )
      )
      ),
      list(tabItem(tabName = "plot",
                   fluidPage(
                     fluidRow(
                       div(class = "col-md-12 col-lg-10", 
                           box(title = "Gemiddelde van de groep per domein", status = "primary", solidHeader = TRUE, width = "100%",
                               plotlyOutput("radarPlot")
                           )
                       )
                     ),
                     uiOutput("domainUI"),
                     fluidRow(
                       div(class = "col-md-12 col-lg-10", 
                           box(title = "Uitkomsten uitkomst per vraag", status = "primary", solidHeader = TRUE, width = "100%",
                               dataTableOutput("percTable")
                           )
                       )
                     )
                     
                   )
      )
      )
    )
    do.call(tabItems, items)
  })
  
  output$domainUI <- renderUI({
    s <- event_data("plotly_click", source = "radarplot")
    
    if (!is.null(s)){
      tmp <- fluidRow(
        div(class = "col-md-12 col-lg-10", 
            box(title = "Gemiddelde van de groep per vraag", status = "primary", solidHeader = TRUE, width = "100%",
                plotlyOutput("domainPlot")
            )
        )
      )
    } else {
      tmp<- NULL
    }
    
    return(tmp)
  })
    
  
  # Read data ----
  
  loadData <- reactive({
    if (is.null(input$file1)){
      return(NULL)
    } else{
      file <- input$file1$datapath[1]
      dt <- tryCatch(
        {
          dt <- as.data.table(read.csv(file, header = T, sep = ",", check.names = FALSE, colClasses = "character") )
        }, error = function(cond){
          stop("The file can not be read.")
        }, warning = function(cond){
          if (file.exists(file)){
            cat("\n", file = file, append = TRUE)
            dt <- as.data.table(read.csv(file, header = T, sep = ",", check.names = FALSE, colClasses = "character") )
          } else{
            stop("The file is not available.")
          }
        }
      )
      
      return(dt)
    }
  })
  
  output$csvData <- DT::renderDataTable({
    dt <- loadData()
    dt.new = datatable(dt, options = list(scrollX = T)) 
    if (!is.null(dt)){
      dt.new = dt.new %>%
        formatStyle(
          columns = colnames(dt),
          color = styleInterval(2, c('red', 'black')),
          backgroundColor = styleEqual("", 'red')
        )
    }
  })
  
  # Preprocess data ----
  
  preprocessData <- reactive({
    data <- loadData()
    
    if (!is.null(dt)){
      for (i in mappingAnswers$Antwoord){
        replaceValue <- mappingAnswers[Antwoord == i, Score]
        data[data == i] <- as.numeric(replaceValue)
      }
      
      data = data[, Tijdstempel := as.POSIXct(Tijdstempel)]
      dataT <- data.table(t(data))
      dataT = dataT[, questions := colnames(data)]
      colnames(dataT) <- c(data$Gebruikersnaam, "questions")
      columnsToNumeric <- data$Gebruikersnaam
      domainQuestions[, koppelKolom:= gsub(" ", "", Vraag, fixed = TRUE)]
      dataT <- dataT[, koppelKolom:= gsub(" ", "", questions, fixed = TRUE)]
      dataT = data.table(merge(dataT, domainQuestions, by.x = "koppelKolom", by.y = "koppelKolom"))[, `:=` (#questions = NULL,
                                                                                                            koppelKolom = NULL,
                                                                                                            Vraag = NULL)]
      dataT = dataT[, (columnsToNumeric) := lapply(.SD, as.numeric), .SDcols = columnsToNumeric] 
      
      verwijdert <- nrow(domainQuestions) - nrow(dataT)
      if (verwijdert > 0) {
        warning(paste0("Er zijn ", verwijdert, " vragen verwijdert, doordat hier geen score aan gekoppeld kan worden. Controleer dit bij Tim."))
      }
      # domainQuestions = domainQuestions[, `:=` (maxScore = 5,
      #                                           minScore = 1)]
      # info <- domainQuestions[, .(maxTotaalScore = sum(maxScore),
      #                             minTotaalScore = sum(minScore)), by = Domein]
      # scores <- dataT[, lapply(.SD, sum, na.rm=TRUE) , by = Domein]
      # 
      # dt <- merge(scores, info, by.x = "Domein", by.y = "Domein")
      dataT = dataT[order(Domein, questions)]
      return(dataT)
      
    }
  })
  
  calculateDomain <- reactive({
    
    dt <- copy(preprocessData())
    vragen <- unique(dt$questions)
    domeinen <- dt$Domein
    
    dataT <- dt[, Domein := NULL]
    columnsToNumeric <- loadData()$Gebruikersnaam
    dtPerc = data.table(t(dataT[, questions := NULL]))
    colnames(dtPerc) <- vragen
    # dtPerc = dtPerc[, Gebruikersnaam := columnsToNumeric][,c("Gebruikersnaam", colnames(dtPerc)[!colnames(dtPerc) %in% "Gebruikersnaam"]), with = F]
    dt.output <- dtPerc[, lapply(.SD, mean, na.rm=TRUE)]
    
    dt.output.T = data.table(t(dt.output))
    colnames(dt.output.T) <- "meanQuestion"
    
    dt.output.T = dt.output.T[, `:=` (Domein = domeinen,
                                      questions = vragen)]
    
    dt.output.T = dt.output.T[,c("Domein", "questions", "meanQuestion")]
    
  })
  
  # calculatePercentages <- reactive({
  #   
  #   dt <- preprocessData()
  #   
  #   if (!is.null(dt)) {
  #     columnsToNumeric <- loadData()$Gebruikersnaam
  #     cols <- c("Domein", paste0("percentage_", columnsToNumeric))
  #     dtPerc <- dt[, c(paste0("percentage_", columnsToNumeric)) := lapply(.SD, function(x){
  #       (x - minTotaalScore)/ maxTotaalScore *100
  #     }), .SDcols = columnsToNumeric][,..cols]
  #     
  #     dtPerc = data.table(t(dtPerc))
  #     colnames(dtPerc) <- unique(dt$Domein)
  #     dtPerc = dtPerc[-1,][, Gebruikersnaam := columnsToNumeric][,c("Gebruikersnaam", colnames(dtPerc)[!colnames(dtPerc) %in% "Gebruikersnaam"]), with = F]
  #   }
  # })
  
  output$percTable <- renderDataTable({
    # dt <- calculatePercentages()
    s <- event_data("plotly_click", source = "radarplot")
    
    if (!is.null(s)){
      dt <- getDomainData()
    } else{
      dt <- calculateDomain()
    }
    dt.new = datatable(dt, options = list(scrollX = T)) 
    if (!is.null(dt)){
      dt.new = dt.new %>%
        formatStyle(
          columns = colnames(dt),
          color = styleInterval(2, c('red', 'black')),
          backgroundColor = styleEqual("", 'red')
        )
    }
    return(dt.new)
  })
  
  avgPerc <- reactive({
    
    dt <- calculateDomain()
    
    if (!is.null(dt)) {
      dt.output <- dt[, lapply(.SD, mean, na.rm=TRUE), .SDcols = "meanQuestion", by = Domein]
      
      domeinen = unique(dt.output$Domein)
      dt = data.table(t(dt.output[, Domein := NULL]))
      colnames(dt) <- domeinen
      
    }
    
    return(dt)
  })
  
  # sdPerc <- reactive({
  #   
  #   dt <- calculateDomain()
  #   
  #   if (!is.null(dt)) {
  #     dt = dt[, Gebruikersnaam := NULL]
  #     dt = dt[, lapply(.SD, as.numeric), .SDcols = colnames(dt)]
  #     dt <- dt[, lapply(.SD, sd, na.rm=TRUE)]
  #   }
  #   
  #   return(dt)
  # })
  
  # quantilesPerc <- reactive({
  #   
  #   dt <- calculateDomain()
  #   
  #   if (!is.null(dt)) {
  #     dt = dt[, Gebruikersnaam := NULL]
  #     dt = dt[, lapply(.SD, as.numeric), .SDcols = colnames(dt)]
  #     tmp <- lapply(quantiles, function(x){
  #       dt[, lapply(.SD, function(y){ quantile(y, x)})]
  #       
  #     })
  #     dtReturn <- rbindlist(tmp, use.names = T) 
  #   }
  #   
  #   return(dt)
  # })
  
  # Make plot --------
  
  output$radarPlot <- renderPlotly({
    
    dt <- copy(avgPerc())
    
    if (!is.null(dt)) {
      
      dimensies <- c(colnames(dt), colnames(dt)[1])
      dt = dt[, eind := dt[,1]]
      
      lines <- data.table(t(matrix(rep(stdPercentages, each = length(dimensies)), ncol = length(stdPercentages) )))
      p <- plot_ly(source = "radarplot")
      for (i in 1:nrow(lines)) {
        p <- p %>%
          add_trace(type = 'scatterpolar',
                    mode = "lines",
                    line = list(color = colors[i]),
                    r = unlist(lines[i,]),
                    theta = dimensies,
                    fill = "tonext",
                    fillcolor = colors[i],
                    hoverinfo = "none",
                    showlegend = F
          ) 
      }
      
      p = p %>% add_trace( 
        type = 'scatterpolar',
        mode = "lines",
        r = unlist(dt[1,]),
        theta = dimensies,
        line = list(color = "#000000", width = 4),
        fill = "none",
        hoverinfo = "text",
        text = paste0("Op het domein ", dimensies, " is de score ", unlist(dt[1,]))
      ) 
      
      
      p = p %>%
        layout(
          polar = list(
            angularaxis = list(
              tickwidth = 2,
              linewidth = 1,
              visible = T,
              tickfont = list(
                family = 'Times New Roman',
                size = 16,
                color = '#000'
              )
            ),
            radialaxis = list(
              linewidth = 2,
              tickwidth = 2,
              # layer = 'below traces',
              visible = T,
              range = c(0,5)
            )
          ),
          showlegend = F
          
        )
    } else {
      p <- plot_ly()
    }
    
    return(p)
    
  })
  
  getDomainData <- reactive({
    s <- event_data("plotly_click", source = "radarplot")
    
    if (!is.null(s)){
      numberSelected <- s$pointNumber
      domeinen <- unique(domainQuestions$Domein)
      selectedDomain <- domeinen[c(2:5,1)][numberSelected]
      dt <- copy(preprocessData())
      dt.new <- dt[Domein == selectedDomain]
      vragen <- unique(dt.new$questions)
      
      dataT <- dt.new[, Domein := NULL]
      columnsToNumeric <- loadData()$Gebruikersnaam
      dtPerc = data.table(t(dataT[, questions := NULL]))
      colnames(dtPerc) <- vragen
      dtPerc = dtPerc[, Gebruikersnaam := columnsToNumeric][,c("Gebruikersnaam", colnames(dtPerc)[!colnames(dtPerc) %in% "Gebruikersnaam"]), with = F]
      
    } else{
      dtPerc <- NULL
    }
    
    return(dtPerc)
  })
  
  output$domainPlot <- renderPlotly({
    dt <- copy(getDomainData())
    
    if (!is.null(dt)){
      dt = dt[, Gebruikersnaam := NULL]
      dt = dt[, lapply(.SD, as.numeric), .SDcols = colnames(dt)]
      dt <- dt[, lapply(.SD, mean, na.rm=TRUE)]
      
      dt.tmp <- data.table(t(dt))[,questions := colnames(dt)]
      
      sublength <- 22
      teksten <- rbindlist(lapply(dt.tmp$questions, function(x){
        
        # total length of string
        num.chars <- nchar(x)
        
        # the indices where each substr will start
        starts <- seq(1,num.chars, by=sublength+1)
        
        # chop it up
        subs <- sapply(starts, function(ii) {
          substr(x, ii, ii+sublength)
        })
        
        new.string <- paste0(subs, collapse = " <br> ")
        return(list(new.string))
      })
      )
      
      dt.tmp = dt.tmp[, hoverInfo := paste0(questions, " Score: ", V1)][, tekst := teksten]
      f1 <- list(
        size = 10,
        color = "black"
      )
      a <- list(
        titlefont = f1,
        showticklabels = TRUE,
        tickangle = 0,
        tickfont = f1
      )
     
      
      p <- plot_ly(dt.tmp, x = ~tekst, y = ~V1, type = 'bar', text = ~hoverInfo,
                   hoverinfo = 'text',
                   marker = list(color = 'rgb(158,202,225)',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1.5))) %>%
        layout(title = "",
               xaxis = a,
               yaxis = list(title = "", domain = c(0.3, 1), range = c(0, 5)))
      
    } else{
    
      p <- plot_ly()
    }
   
    
    return(p)
  })
}