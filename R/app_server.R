#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 
#' 
#

library(readr)
library(magrittr)
library(lazytrade)
library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)
library(DT)
library(plotly)
library(randomcoloR)


app_server <- function( input, output, session ) {
  
  
  #-----------DATA MANAGEMENT-------------- 
  file_path <- reactive({
    
    #  Terminals <- normalizePath(Sys.getenv('PATH_T1'), winslash = '/')
    #  file_path <- paste0(Terminals,"/OrdersResultsT",1,".csv")      
    Terminals <- normalizePath(Sys.getenv(paste0('PATH_T',input$Terminal)), winslash = '/')
    file_path <- paste0(Terminals,"/OrdersResultsT",input$Terminal,".csv")
  })
  #-----------------------------------------
  DF_Stats <- reactive({ 
    
    if(file.exists(file_path())){
      DF_Stats <- read.csv(file_path(), col.names = c("MagicNumber","Ticket","EntryTime","ExitTime","Profit","Symbol","Type"))
      
      DF_Stats <- data.frame(MagicNumber = DF_Stats$MagicNumber,
                             Ticket = DF_Stats$Ticket,
                             EntryTime = as.POSIXct(DF_Stats$EntryTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo"),
                             ExitTime = as.POSIXct(DF_Stats$ExitTime, format = "%Y.%m.%d %H:%M:%S", tz = "Africa/Cairo"),
                             Profit = DF_Stats$Profit,
                             Symbol = DF_Stats$Symbol,
                             Type = DF_Stats$Type)
    }else{"NO DATA"}
  })
  #---------------------------------------      
  magicNumber <- reactive({
    if(file.exists(file_path())){
      unique(DF_Stats()$MagicNumber)
    }else{"NO DATA"}
  })
  #---------------------------------------    
  symbol <- reactive({
    if(file.exists(file_path())){  
      symbol <-  unique(DF_Stats()$Symbol)
      symbol[order(symbol)]
    }else{"NO DATA"}
  })
  #---------------------------------------
  pair <- reactive({
    if(file.exists(file_path())){    
      as.vector(unique(DF_Balance()$Symbol))
    }else{"NO DATA"}
  })
  
  
  #---------------------------------------    
  
  #-----------MANAGE SIDEBAR-------------    
  #Refresh data 
  observeEvent(input$Refresh,{
    updateSelectInput(session, inputId = "MagicNum", label = NULL, choices = c("All",magicNumber()), selected = NULL)
    updateSelectInput(session, inputId = "Symbol", label = NULL, choices = c("All",symbol()), selected = NULL)
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    
    
    
    Terminals <- normalizePath(Sys.getenv(paste0('PATH_T',input$Terminal)), winslash = '/')
    
    acc_file_T1 <- file.path(path_user, "_DATA/AccountResultsT1.rds")
    acc_file_T3 <- file.path(path_user, "_DATA/AccountResultsT3.rds")
    
    
    if(input$Terminal == 1 ){
      DFT1Act <- read_csv(file.path(Terminals, 'AccountResultsT1.csv'),
                          col_names = c("DateTime", "Balance", "Equity","Profit"),
                          col_types = "cddd")
      DFT1Act$DateTime <- ymd_hms(DFT1Act$DateTime)
      if (!file.exists(acc_file_T1)) {
        write_rds(DFT1Act, acc_file_T1)
      } else {
        read_rds(acc_file_T1) %>% 
          bind_rows(DFT1Act) %>% 
          arrange(DateTime) %>% 
          head(1000) %>% 
          write_rds(acc_file_T1)
      }
    }
    if(input$Terminal == 3 ){
      DFT3Act <- read_csv(file.path(Terminals, 'AccountResultsT3.csv'),
                          col_names = c("DateTime", "Balance", "Equity","Profit"),
                          col_types = "cddd")
      DFT3Act$DateTime <- ymd_hms(DFT3Act$DateTime)
      
      if (!file.exists(acc_file_T3)) {
        write_rds(DFT3Act, acc_file_T3)
      }else {
        read_rds(acc_file_T3) %>% 
          bind_rows(DFT3Act) %>% 
          arrange(DateTime) %>% 
          head(1000) %>% 
          write_rds(acc_file_T3)
      }    
    }
    
    
  })
  
  
  observeEvent(input$Terminal,{
    updateSelectInput(session, inputId = "MagicNum", label = NULL, choices = c("All",magicNumber()), selected = NULL)
    updateSelectInput(session, inputId = "Symbol", label = NULL, choices = c("All",symbol()), selected = NULL)
  }) 
  
  #update Symbol choices
  observeEvent(input$MagicNum,{
    if(input$MagicNum == "All"){
      updateSelectInput(session, "Symbol",label = "Select the symbol",choices = c("All",symbol()),selected = "All")
    }else{
      pair <- DF_Stats()%>%group_by(Symbol)%>%filter(MagicNumber == input$MagicNum)%>%select(Symbol)%>%unique()
      updateSelectInput(session, "Symbol",label = "Select the symbol",choices = as.character(pair),selected = pair)
    }
  })
  #update Magic Number
  observeEvent(input$Symbol,{
    if(input$Symbol == "All" || input$Symbol == 1)
    {
      updateSelectInput(session, "MagicNum",label = "Select Magic Number",choices = c("All",magicNumber()),selected = "All")
    }else
    {
      MN <- DF_Stats()%>%group_by(MagicNumber)%>%filter(Symbol==input$Symbol)%>%select(MagicNumber)%>%unique()
      updateSelectInput(session, "MagicNum",label = "Select Magic Number",choices =  c(as.integer(unlist(MN))) ,selected = as.integer(unlist(MN)[1]))
    }
  })
  
  #---------DATA TAB---------------  
  Stats <- reactive({
    if(input$MagicNum == "All"){
      DF_Stats <- DF_Stats()%>%filter(ExitTime >= input$From, ExitTime <= paste0(input$To," 23:59:59"))
    }
    else{
      
      DF_Stats <- DF_Stats()%>%filter(MagicNumber == input$MagicNum, ExitTime >= input$From, ExitTime <= input$To)
      
    }
  })
  
  output$data <- DT::renderDataTable({
    if(file.exists(file_path())) {
      Stats <- data.frame(MagicNumber = Stats()$MagicNumber,
                          Ticket = Stats()$Ticket,
                          EntryTime = as.character(Stats()$EntryTime),
                          ExitTime = as.character(Stats()$ExitTime),
                          Profit = Stats()$Profit,
                          Symbol = Stats()$Symbol,
                          Type = Stats()$Type)
      
      
      
      switch(input$Sort,
             "MagicNumber" =  Stats[order(Stats$MagicNumber,decreasing = T),],
             "Ticket" =  Stats[order(Stats$Ticket,decreasing = T),],
             "EntryTime" =  Stats[order(Stats$EntryTime,decreasing = T),],
             "ExitTime" =  Stats[order(Stats$ExitTime,decreasing = T),],
             "Profit"=  Stats[order(Stats$Profit,decreasing = T),],
             "Symbol"=  Stats[order(Stats$Symbol,decreasing = T),])
      
      datatable(Stats,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
        pageLength = 100, autoWidth = TRUE))
      
    }else{"NO DATA"}
  })
  
  
  
  DF_Balance <- reactive({
    
    Balance <- c()
    DF_Balance <- Stats()
    #mutate(DF_Balance,ExitTime =  as.POSIXct(DF_Balance$ExitTime, format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Cairo"))
    DF_Balance <- DF_Balance %>% subset(select = -c(MagicNumber,Ticket,EntryTime,Type)) 
    DF_Balance <- DF_Balance[order(DF_Balance$Symbol,decreasing = F),]
    #group_by(DF_Balance,Stats()$Symbol)
    #pair <- as.vector(unique(Stats()$Symbol))
    
    for(i in  1:nrow(DF_Balance)){
      if (i==1){
        Balance[i] <- DF_Balance$Profit[i]
      }else if(DF_Balance$Symbol[i] != DF_Balance$Symbol[i-1] && i>1){
        Balance[i] <- DF_Balance$Profit[i]
      }else{
        Balance[i] <- DF_Balance$Profit[i]+ Balance[i-1]
      }
    }
    
    DF_Balance <- DF_Balance %>%  mutate(Balance) 
    # DF_Balance[order(DF_Balance$Symbol,decreasing = F),]
    #  DF_Balance[order(DF_Balance$ExitTime,decreasing = F),]
    
    DF_Balance
  })
  
  DF_Balance_All <- reactive({
    DF_Balance <- Stats()
    DF_Balance <- DF_Balance %>% subset(select = -c(MagicNumber,Ticket,EntryTime,Type)) 
    DF_Balance <- DF_Balance[order(DF_Balance$ExitTime,decreasing = F),]
    
    Balance <- c()
    for(i in  1:nrow(DF_Balance)){
      if (i==1){
        Balance[i] <- DF_Balance$Profit[i]
        
      }else{
        Balance[i] <- DF_Balance$Profit[i]+ Balance[i-1]
      }
    }
    
    DF_Balance <- DF_Balance %>%  mutate(Balance) 
  })
  
  
  output$balance <- renderTable({
    if(file.exists(file_path())){
      
      if(input$MagicNum != "All"){
        DF_Balance <- data.frame(ExitTime = as.character(DF_Balance()$ExitTime),
                                 Profit = DF_Balance()$Profit,
                                 Symbol = DF_Balance()$Symbol,
                                 Balance = DF_Balance()$Balance)
        
        "ExitTime" =  DF_Balance[order(DF_Balance$ExitTime,decreasing = F),]
      }else{
        
        DF_Balance <- data.frame(ExitTime = as.character(DF_Balance_All()$ExitTime),
                                 Profit = DF_Balance_All()$Profit,
                                 Symbol = DF_Balance_All()$Symbol,
                                 Balance = DF_Balance_All()$Balance)
      }
    }
    else{"NO DATA"}
  })
  
  #----------GRAPH TAB-----------------
  output$profitGraph <- renderPlotly({
    if(file.exists(file_path())){
      
      color <- c("red", "black", "blue","green","orange","purple", "pink","cornflowerblue", "darkgreen","indianred3","magenta","mediumpurple3", "midnightblue","orchid4","palegreen","skyblue","slateblue4", "tomato1")
      colorList <- vector("list",length(pair()))
      
      for (i in 1 : length(pair())){
        Ps <- list(target = pair()[i], value = list(marker =list(color = sample(color,1))))
        colorList[[i]] <- Ps
      }
      
      if(input$MagicNum == "All"){
        graph <-  plot_ly(
          type = 'scatter',
          x = Stats()$ExitTime,
          y = Stats()$Profit,
          text = paste("<br>Time: ", Stats()$ExitTime,
                       "<br>Profit: ", Stats()$Profit,
                       "<br>Symbol: ", Stats()$Symbol),
          hoverinfo = 'text',
          mode = 'markers',
          transforms = list(
            list(
              type = 'groupby',
              groups = Stats()$Symbol,
              styles = colorList)))
      }else
      {
        plot_ly(
          x = Stats()$ExitTime,
          y = Stats()$Profit,
          type = "scatter",
          mode = 'markers',
          marker = list(color = sample(color,1)),
          name = paste0(input$Symbol," PROFIT"))
      }
    }
  })
  
  
  output$balanceGraph <- renderPlotly({
    
    if(file.exists(file_path())){
      color <- c("red", "black", "blue","green","orange","purple", "pink","cornflowerblue", "darkgreen","indianred3","magenta","mediumpurple3", "midnightblue","orchid4","palegreen","skyblue","slateblue4", "tomato1")
      colorList <- vector("list",length(pair()))
      
      for (i in 1 : length(pair())){
        Ps <- list(target = pair()[i], value = list(line =list(color = sample(color,1))))
        colorList[[i]] <- Ps
      }
      
      graph <-  plot_ly(
        type = 'scatter',
        x = DF_Balance()$ExitTime,
        y = DF_Balance()$Balance,
        text = paste("<br>Time: ", DF_Balance()$ExitTime,
                     "<br>Balance: ", DF_Balance()$Balance,
                     "<br>Symbol: ", DF_Balance()$Symbol),
        hoverinfo = 'text',
        mode = 'lines',
        transforms = list(
          list(
            type = 'groupby',
            groups = DF_Balance()$Symbol,
            styles = colorList)))
    }
  })  
  
  
  #------------ACCOUNT TAB--------------  
  
  accountResults <- reactive({
    
    
    if(file.exists(account_path())){
      df_AR <- readr::read_rds(account_path())
      
      df_AR <-  data.frame(DateTime =df_AR$DateTime,
                           Balance = df_AR$Balance,
                           Equity = df_AR$Equity,
                           Profit = df_AR$Profit
      )
      df_AR <- df_AR%>%filter(DateTime >= input$From, DateTime <= paste0(input$To," 23:59:59"))
    } else{"NO DATA"}
  })
  
  output$watchDogReport <- DT::renderDataTable({
    if(file.exists(account_path())){
      df_AR <-  data.frame(DateTime =as.character(accountResults()$DateTime),
                           Balance = accountResults()$Balance,
                           Equity = accountResults()$Equity,
                           Profit = accountResults()$Profit)
      
      datatable(df_AR,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
        pageLength = 100, autoWidth = TRUE))
      
    }else{"NO DATA"}
  }) 
  
  output$equityGraph <- renderPlotly({
    
    if(file.exists(account_path())){
      
      
      graph <-  plot_ly(
        type = 'scatter',
        x = accountResults()$DateTime,
        y = accountResults()$Balance,
        text = paste("<br>Time: ", accountResults()$DateTime,
                     "<br>Balance: ", accountResults()$Balance),
        hoverinfo = 'text',
        mode = 'lines',
        name = 'Balance',
        line = list(color = 'blue'))
      
      graph <- graph %>% add_trace(
        type = 'scatter',
        x = accountResults()$DateTime,
        y = accountResults()$Equity,
        text = paste("<br>Time: ", accountResults()$DateTime,
                     "<br>Equity: ", accountResults()$Equity),
        hoverinfo = 'text',
        mode = 'lines',
        name = 'Equity',
        line = list(color = 'red')
      )
    }
  }) 
  
  
  
  
  #-------REPORT TAB-----------------
  buyProfit <- reactive({
    
    if(input$MagicNum == "All"){
      allProfit <-  Stats()  %>%
        group_by(Type) %>% 
        filter(Type == 0) %>%
        summarise( Profit = sum(Profit)) %>%
        select(-c(Type)) %>%
        mutate(Symbol = "ALL PAIR")
      
      
      buyProfit <-  Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 0) %>% 
        summarise( Profit = sum(Profit)) %>% 
        subset(select = c(Symbol,Profit)) %>%
        rbind(allProfit)
      
    } else{
      buyProfit <-  Stats()  %>%
        group_by(Symbol, Type) %>% 
        summarise( Profit = sum(Profit)) %>% 
        filter(Type == 0) %>% 
        subset(select = c(Symbol,Profit))
    }
  })
  
  
  sellProfit <- reactive({
    
    if(input$MagicNum == "All"){
      allProfit <-  Stats()  %>%
        group_by(Type) %>% 
        filter(Type == 1) %>%
        summarise( Profit = sum(Profit)) %>%
        select(-c(Type)) %>%
        mutate(Symbol = "ALL PAIR")
      
      sellProfit <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 1) %>% 
        summarise( Profit = sum(Profit)) %>% 
        subset(select = c(Symbol,Profit)) %>%
        rbind(allProfit)
      
    } else{
      sellProfit <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 1) %>% 
        summarise( Profit = sum(Profit)) %>% 
        subset(select = c(Symbol,Profit))
    }
  })
  
  buyTrade <- reactive({
    
    if(input$MagicNum == "All"){
      allTrade <-  Stats()  %>%
        group_by(Type) %>% 
        filter(Type == 0) %>%
        summarise( Buy_Trade = n()) %>%
        select(-c(Type)) %>%
        mutate(Symbol = "ALL PAIR")
      
      buyTrade <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 0) %>% 
        summarise( Buy_Trade = n()) %>% 
        subset(select = c(Symbol,Buy_Trade)) %>%
        rbind(allTrade)
      
    } else{
      buyTrade <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        summarise(Buy_Trade = n()) %>% 
        filter(Type == 0) %>% 
        subset(select = c(Symbol,Buy_Trade))
    }
  })
  
  sellTrade <- reactive({
    
    if(input$MagicNum == "All"){
      allTrade <-  Stats()  %>%
        group_by(Type) %>% 
        filter(Type == 1) %>%
        summarise( Sell_Trade = n()) %>%
        select(-c(Type)) %>%
        mutate(Symbol = "ALL PAIR")
      
      sellTrade <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 1) %>% 
        summarise( Sell_Trade = n()) %>% 
        subset(select = c(Symbol,Sell_Trade)) %>%
        rbind(allTrade)
    } else{
      sellTrade <- Stats()  %>%
        group_by(Symbol, Type) %>% 
        filter(Type == 1) %>% 
        summarise(Sell_Trade = n()) %>% 
        subset(select = c(Symbol,Sell_Trade))
    }
  })
  
  output$result <- DT::renderDataTable({
    
    if(file.exists(file_path())){
      DF_allPair <- DF_Balance_All()
      allPair <- round(DF_allPair[nrow(DF_allPair),4],2)
      final_Balance  <- vector("numeric", length(pair()))
      
      for (i in 1: length(pair())){
        pairBalance <-  DF_Balance() %>% filter(Symbol == pair()[i])
        final_Balance[i]  <- round(pairBalance[nrow(pairBalance),4],2)
      }
      
      Final_Balance <- data.frame(Symbol = pair(),
                                  Final_Balance = final_Balance) 
      
      if(input$MagicNum == "All"){
        All_Pair <- c("ALL PAIR", allPair)
        
        Final_Balance <- rbind(Final_Balance,All_Pair)  
        Final_Balance <- left_join(x = Final_Balance,y = buyProfit(), by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance,y = sellProfit(), by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance, y = buyTrade(),by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance, y = sellTrade(),by = "Symbol") 
        
        
        
      }else{
        Final_Balance <- left_join(x = Final_Balance,y = buyProfit(), by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance,y = sellProfit(), by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance, y = buyTrade(),by = "Symbol")
        Final_Balance <- left_join(x = Final_Balance, y = sellTrade(),by = "Symbol") 
      }
      FB <- as.data.frame(Final_Balance)
      FB[is.na(FB)] <- 0
      FB <- FB %>% cbind(FB$Buy_Trade + FB$Sell_Trade) %>%
        set_names(c("Symbol","Final_Balance","Buy_Profit","Sell_Profit","Buy_Trade","Sell_Trade", "Total_Trade"))
      
      datatable(FB,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
        pageLength = 28, autoWidth = TRUE))
    }
    else{"NO DATA"}
  })
  
  output$profitFactor <- renderText({
    if(file.exists(file_path())){
      negProfit <- Stats()%>%filter(Profit<0)%>%select(Profit)%>%summarise(Loss = abs(sum(Profit)))
      posProfit <- Stats()%>%filter(Profit>0)%>%select(Profit)%>%summarise(Gain = abs(sum(Profit)))
      
      
      if(negProfit == 0 ){"Only Gain"}
      else if (posProfit == 0){"Only Loss"}
      else{as.double(round( posProfit/(0.001+negProfit),2))}
    }else{
      "NO DATA"
    }
  })
  
  output$maxProfit <- renderText({
    if(file.exists(file_path())){max(Stats()$Profit)}
    else{"NO DATA"}
  })
  
  output$minProfit <- renderText({
    if(file.exists(file_path())){min(Stats()$Profit)}
    else{"NO DATA"}
  })
  
  output$totalTrade <- renderText({
    if(file.exists(file_path())){
      nrow(Stats())}
    else{"NO DATA"}
  })
  
  account_path <- reactive({
    
    #  Terminals <- normalizePath(Sys.getenv('PATH_T1'), winslash = '/')
    #  file_path <- paste0(Terminals,"/OrdersResultsT",1,".csv")      
    pathDSS <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    paste0(pathDSS,"/_DATA/AccountResultsT",input$Terminal,".rds")
  })
  
  
  ###################################################################
  ####################### - MT INSPECTION - #########################
  ###################################################################
  
  
  macd_ai <- reactive({
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_data <- file.path(path_user, "_DATA")
    readr::read_rds(file.path(path_data, 'macd_ai_classified.rds'))
  })  
  
  n_rows <- reactiveValues(c = nrow(macd_ai))
  
  #update slider max
  observeEvent(input$BOOM, {
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_data <- file.path(path_user, "_DATA")
    macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds')) 
    n_rows$c <- nrow(macd_ai)
    updateSliderInput(session, inputId = "rows",min = 1, max = n_rows$c,value = 1)
  })
  
  #current market type value
  mt_analysed <- reactive({
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_data <- file.path(path_user, "_DATA")
    macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds'))
    
    mt_analysed <- macd_ai[input$rows, 65]
  })
  #current data row
  ln_analysed <- reactive({ ln_analysed <- macd_ai()[input$rows, ]})
  
  
  #function to write data
  write_data <- function(x){
    readr::write_rds(x, file.path(path_data, 'macd_ai_classified.rds'))
  }
  
  #output data from this app
  file_checked <- file.path(path_data, "macd_checked_60M.rds")
  
  # function that writes data to rds file 
  storeData <- function(data, fileName) {
    
    nonDuplicate <- data[!duplicated(data), ]
    
    if(file.exists(fileName)){
      ex_data <- readr::read_rds(fileName)
      agr_data <- dplyr::bind_rows(ex_data, nonDuplicate)
      readr::write_rds(x = agr_data, file = fileName)
    }else{
      # Write the file to the local system
      readr::write_rds(x = nonDuplicate, file = fileName)
    }
  }
  
  observeEvent(input$BUN, {
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_data <- file.path(path_user, "_DATA")
    macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds'))   
    df <- tibble::tibble(M_T = 'BUN')
    ln_new <- dplyr::bind_cols(macd_ai[input$rows, 1:64], df)
    storeData(ln_new, file_checked)
    macd_ai <<- macd_ai[-input$rows, ]
    write_data(macd_ai)
  })     
  
  observeEvent(input$BUV, {
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_data <- file.path(path_user, "_DATA")
    macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds')) 
    df <- tibble::tibble(M_T = 'BUV')
    ln_new <- dplyr::bind_cols(macd_ai[input$rows, 1:64], df)
    storeData(ln_new, file_checked)
    macd_ai <<- macd_ai[-input$rows, ]
    write_data(macd_ai)
  })
  
  observeEvent(input$BEN, {
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_data <- file.path(path_user, "_DATA")
    macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds'))   
    df <- tibble::tibble(M_T = 'BEN')
    ln_new <- dplyr::bind_cols(macd_ai[input$rows, 1:64], df)
    storeData(ln_new, file_checked)
    macd_ai <<- macd_ai[-input$rows, ]
    write_data(macd_ai)
  })
  
  observeEvent(input$BEV, {
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_data <- file.path(path_user, "_DATA")
    macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds')) 
    df <- tibble::tibble(M_T = 'BEV')
    ln_new <- dplyr::bind_cols(macd_ai[input$rows, 1:64], df)
    storeData(ln_new, file_checked)
    macd_ai <<- macd_ai[-input$rows, ]
    write_data(macd_ai)
  })
  
  observeEvent(input$RAV, {
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_data <- file.path(path_user, "_DATA")
    macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds'))   
    df <- tibble::tibble(M_T = 'RAV')
    ln_new <- dplyr::bind_cols(macd_ai[input$rows, 1:64], df)
    storeData(ln_new, file_checked)
    macd_ai <<- macd_ai[-input$rows, ]
    write_data(macd_ai)
  })
  
  observeEvent(input$RAN, {
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_data <- file.path(path_user, "_DATA")
    macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds')) 
    df <- tibble::tibble(M_T = 'RAN')
    ln_new <- dplyr::bind_cols(macd_ai[input$rows, 1:64], df)
    storeData(ln_new, file_checked)
    macd_ai <<- macd_ai[-input$rows, ]
    write_data(macd_ai)
  })
  
  
  #graphs and outputs
  output$AI_Data <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_data <- file.path(path_user, "_DATA")
    macd_ai <-readr::read_rds(file.path(path_data, 'macd_ai_classified.rds'))
    
    
    plot(x = 1:64, y = macd_ai[input$rows, 1:64], main = mt_analysed())
    abline(h=0, col="blue")
    
    #plot(x = 1:64, y = macd_ai[3, 1:64])
    
    
  })
  
  
  ###################################################################
  ####################### - MODEL INSPECTION - ######################
  ###################################################################
  
  
  readResult <- reactive({
    
    pathDSS <- normalizePath(Sys.getenv("PATH_DSS"), winslash = '/')
    file_path <- paste0(pathDSS,"/_DATA/analyse_resultM60.csv")
    DF_Result <- read.csv(file_path, col.names = c("TR_Level","NB_hold","Symbol","MaxPerf","FrstQntlPerf"))
    
    
    
  })
  
  output$AnalyseResult <- DT::renderDataTable({
    
    DF_Result <- data.frame(TR_Level = readResult()$TR_Level,
                            NB_hold = readResult()$NB_hold,
                            Symbol =readResult()$Symbol,
                            MaxPerf = format(round(readResult()$MaxPerf,2),nsmall=2),
                            FrstQntlPerf = format(round(readResult()$FrstQntlPerf,2),nsmall=2))
    
    datatable(DF_Result,class = 'cell-border stripe', rownames = FALSE, filter = 'top', options = list(
      pageLength = 28, autoWidth = TRUE))
    
  })
  
  dataResult <- reactive({
    pathDSS <- normalizePath(Sys.getenv("PATH_DSS"), winslash = '/')
    file_path <- paste0(pathDSS,"/_data/analyse_resultM60_data.csv")
    DF_DataResult <- read.csv(file_path, col.names = c("PnL_NB","TotalTrades","TR_Level","NB_hold","Symbol","FinalOutCome"))
    
  })
  
  output$strategyTestResults <- renderPlot({
    ggplot(dataResult(), aes(x = NB_hold, y = PnL_NB,
                             #size = TotalTrades, 
                             col = as.factor(Symbol)))+geom_point()+
      ggtitle("Strategy Test results")
  })
  
  output$modelPerformance <- renderPlot({
    ggplot(readResult(), aes(x = MaxPerf, y = Symbol,
                             col = TR_Level, 
                             size = NB_hold))+geom_point()+
      geom_vline(xintercept=0.001)+ 
      scale_x_continuous()+
      ggtitle("Model Performance")
  })
  
  observeEvent(input$RefreshM60,{
    
    path_user <- normalizePath(Sys.getenv('PATH_DSS'), winslash = '/')
    path_logs <- file.path(path_user, "_MODELS")
    
    # file names
    filesToAnalyse <-list.files(path = path_logs,
                                pattern = "-60.rds",
                                full.names=TRUE)
    
    # aggregate all files into one
    for (VAR in filesToAnalyse) {
      # VAR <- filesToAnalyse[1]
      if(!exists("dfres")){dfres <<- readr::read_rds(VAR)}  else {
        dfres <<- readr::read_rds(VAR) %>% dplyr::bind_rows(dfres)
      }
    }
    ## Analysis of model quality records
    # file names
    filesToAnalyse1 <-list.files(path = path_logs,
                                 pattern = "M60.csv",
                                 full.names=TRUE)
    
    # aggregate all files into one
    for (VAR in filesToAnalyse1) {
      # VAR <- filesToAnalyse1[1]
      if(!exists("dfres1")){dfres1 <<- readr::read_csv(VAR)}  else {
        dfres1 <<- readr::read_csv(VAR) %>% dplyr::bind_rows(dfres1)
      }
    }
    
    write.csv(dfres1,paste0(path_user,"/_DATA/analyse_resultM60.csv"), row.names=FALSE)
    write.csv(dfres,paste0(path_user,"/_DATA/analyse_resultM60_data.csv"), row.names=FALSE)
    readResult()
    dataResult()
    
  })
  
  
  
  
  ##################################################################################
  ############################# PERFORMANCE ########################################
  ##################################################################################
  
  perf_log <- reactive({
    pathDSS <- normalizePath(Sys.getenv("PATH_DSS"), winslash = '/')
    file_path <- paste0(pathDSS,"/_LOGS/perf_logs60.rds")
    perf_log <- readr::read_rds(file_path)
    
  })
  
  output$perfLog <- renderTable({
    perfLog <- data.frame(TimeTest = as.character(perf_log()$TimeTest),
                          MeanPerf = perf_log()$MeanPerf,
                          Quantil = perf_log()$Quantil)
  })
  
  
  output$perfGraph <- renderPlotly({
    
    
    perfLog <- data.frame(TimeTest = as.Date(perf_log()$TimeTest,"%Y-%m-%d", tz="GMT"),
                          MeanPerf = perf_log()$MeanPerf,
                          Quantil = perf_log()$Quantil)
    
    fig <- perfLog %>% plot_ly(x = perfLog$TimeTest, type="candlestick",
                               open = perfLog$MeanPerf, close = perfLog$Quantil,
                               high = perfLog$MeanPerf, low =perfLog$Quantil,
                               text = paste("<br>Time: ", perfLog$TimeTest,
                                            "<br>MeanPerf: ", perfLog$MeanPerf,
                                            "<br>Quantile:",perfLog$Quantil)
    ) 
    fig <- fig %>% layout(title = list(text = "Open = MeanPerf - Close = Quantile", x = 0),
                          xaxis = list(rangeslider = list(visible = F))) 
    fig
    
    
  }) 
  
  
  
  #---------------END CODE------------------------------------------
  output$console <- renderPrint({
    
    data.frame(ExitTime = as.character(DF_Balance()$ExitTime),
               Profit = DF_Balance()$Profit,
               Symbol = DF_Balance()$Symbol,
               Balance = DF_Balance()$Balance)
    
  })
  
  
}
