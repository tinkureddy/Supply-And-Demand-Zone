server <- function(input, output) {
  
  supply_zone_detection <- function(df,stock,df_supply_and_demand){
    # browser()
    if(nrow(df) >= 3){
      for (ind in 2:(nrow(df)-1)) {
        # browser()
        if(df[ind-1,'Open'] < df[ind-1,'Close'] && ##Green Candle
           (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5 *(df[ind-1,'High'] - df[ind-1,'Low'])) && #Im Balance Candle
           (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
           (df[ind+1,'Open'] > df[ind+1,'Close']) && # Red Candle
           (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))
        ){
          # browser()
          df_supply_and_demand[ind,'stock'] = stock
          df_supply_and_demand[ind,'pattern'] = "Supply Reversal Pattern"
          df_supply_and_demand[ind,'Date'] = df[ind,'Date']
          df_supply_and_demand[ind,'zone_1'] = round(min(df[ind,'Open'],df[ind,'Close']),2)
          df_supply_and_demand[ind,'zone_2'] = round(max(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
          
          if(df[ind+1,'Open'] > df[ind,'Open']){
            df_supply_and_demand[ind,'strength'] = "Strong"
          }else{
            df_supply_and_demand[ind,'strength'] = "Normal"
          }
          
        }
        else if((df[ind-1,'Open'] > df[ind-1,'Close']) && # Red Candle
                (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5*(df[ind-1,'High'] - df[ind-1,'Low'])) && #Im-Balance Candle
                
                (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
                (df[ind+1,'Open'] > df[ind+1,'Close']) && # Red Candle
                (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))  #Im-Balance Candle
                
        ){
          df_supply_and_demand[ind,'stock'] = stock
          df_supply_and_demand[ind,'pattern'] = "Supply Continuous Pattern"
          df_supply_and_demand[ind,'Date'] = df[ind,'Date']
          df_supply_and_demand[ind,'zone_1'] = round(min(df[ind,'Open'],df[ind,'Close']),2)
          df_supply_and_demand[ind,'zone_2'] = round(max(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
          if(df[ind+1,'Open'] < df[ind,'Open']){
            df_supply_and_demand[ind,'strength'] = "Strong"
          }else{
            df_supply_and_demand[ind,'strength'] = "Normal"
          }
          
        }
        # print(ind)
      }
      
    }
    
    return(df_supply_and_demand)
  }
  
  demand_zone_detection <- function(df,stock,df_supply_and_demand){
    if(nrow(df) >= 3){
    for (ind in 2:(nrow(df)-1)) {
      # browser()
      if((df[ind-1,'Open'] > df[ind-1,'Close']) &&
         (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5*(df[ind-1,'High'] - df[ind-1,'Low'])) &&
         (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
         (df[ind+1,'Open'] < df[ind+1,'Close']) && # Green Candle
         (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))
      ){
        df_supply_and_demand[ind,'stock'] = stock
        df_supply_and_demand[ind,'pattern'] = "Demand Reversal Pattern"
        df_supply_and_demand[ind,'Date'] = df[ind,'Date']
        df_supply_and_demand[ind,'zone_1'] = round(max(df[ind,'Open'],df[ind,'Close']),2)
        df_supply_and_demand[ind,'zone_2'] = round(min(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
        
        if(df[ind+1,'Open'] > df[ind,'Open']){
          df_supply_and_demand[ind,'strength'] = "Strong"
        }else{
          df_supply_and_demand[ind,'strength'] = "Normal"
        }
        
      }
      else if((df[ind-1,'Open'] < df[ind-1,'Close']) && # Green Candle
              (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5*(df[ind-1,'High'] - df[ind-1,'Low'])) && #Im-Balance Candle
              (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
              (df[ind+1,'Open'] < df[ind+1,'Close']) && # Green Candle
              (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))  #Im-Balance Candle
              
      ){
        df_supply_and_demand[ind,'stock'] = stock
        df_supply_and_demand[ind,'pattern'] = "Demand Continuous Pattern"
        df_supply_and_demand[ind,'Date'] = df[ind,'Date']
        df_supply_and_demand[ind,'zone_1'] = round(max(df[ind,'Open'],df[ind,'Close']),2)
        df_supply_and_demand[ind,'zone_2'] = round(min(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
        
        if(df[ind+1,'Open'] > df[ind,'Open']){
          df_supply_and_demand[ind,'strength'] = "Strong"
        }else{
          df_supply_and_demand[ind,'strength'] = "Normal"
        }
      }
      # print(ind)
    }
    }
    return(df_supply_and_demand)
  }
  
  
  observeEvent(input$bot_submit,{
  
  output$demand_daily_screener <-  DT::renderDataTable({
    
    # if(input$nifty_selection == "nifty_50"){
      nifty_df <- read.csv(paste0(getwd(),"/Nifty50_Stocks.csv", sep = ""))
    # }else{
    #   nifty_df <- read.csv(paste0(getwd(),"/ind_nifty500list.csv", sep = ""))
    # }
    
    
    df_supply_and_demand_final = data.frame(stock=character(),pattern=character(),strength=character(0),Date=as.POSIXct(character()),zone_1=numeric(),zone_2=numeric(),stringsAsFactors=FALSE)
      
   for (i in 1:nrow(nifty_df)) {
     stock <- nifty_df[i,2]
      print(stock)
     # data <- na.omit(getSymbols(as.character(stock),
     #                            src = "yahoo",
     #                            from = as.Date(input$date_range[1]),
     #                            to = as.Date(input$date_range[2]),
     #                            periodicity = "daily",
     #                            auto.assign = FALSE))
     # 
     # data <- data.frame(data)
      
      if(input$bot_timeframe == "1m"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1m&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "2m"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=2m&range=2d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "5m"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "15m"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&range=60d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "1h"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1h&range=60d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "4h"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=4h&range=15d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      else if(input$bot_timeframe == "1d"){
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=2mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
     else if(input$bot_timeframe == "1wk"){
       response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=36mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
     }
     else if(input$bot_timeframe == "1mo"){
       response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&range=36mo&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
     }
      # else if(input$candle_stick_range == "1y"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1d&range=1y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      # else if(input$candle_stick_range == "5y"){
      #   response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1wk&range=5y&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      # }
      else {
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=1mo&range=max&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      }
      
      # browser()
      
      
      # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
      
      stock_timestamp <- response_data$chart$result[[1]]$timestamp
      Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
      High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
      Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
      Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
      Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume

      final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
      
      # stock_timestamp <- response_data$chart$result[[2]][[1]]
      # Close <- response_data$chart$result[[3]]$quote[[1]]$close
      # High <- response_data$chart$result[[3]]$quote[[1]]$high
      # Low <- response_data$chart$result[[3]]$quote[[1]]$low
      # Open <- response_data$chart$result[[3]]$quote[[1]]$open
      # Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
      # 
      # final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
      # # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
      # colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
      
      if(typeof(final_data$V1) == "list"){
        final_data <- final_data[-c(which(final_data$Close == "NULL")),]
        new_stock_timestamp <- unlist(final_data$V1)
        Close <- unlist(final_data$Close)
        High <- unlist(final_data$High)
        Open <- unlist(final_data$Open)
        Low <- unlist(final_data$Low)
        Volume <- unlist(final_data$Volume)
        
        final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
        
        final_data$Date <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
        
        final_data <- final_data %>% select(Date, Open, High, Low, Close,Volume)
      }else{
        final_data$Date <- as.POSIXct(final_data$V1, origin="1970-01-01")
        
        final_data <- final_data %>% select(Date, Open, High, Low, Close,Volume)
      }
      final_data <- na.omit(final_data)

     
    # browser()

      data <- as.data.frame(final_data)
     if(nrow(data) >= 3){
       # colnames(data) <- c('Open','High','Low','Close','Volume','Adjusted')
       # 
       # data <- cbind(Date = rownames(data), data)
       # rownames(data) <- 1:nrow(data)
       # # browser()
       # colnames(data) <- c('Date','Open','High','Low','Close','Volume','Adjusted')
       
       df_supply_and_demand = data.frame(stock=character(),pattern=character(),strength=character(0),Date=as.POSIXct(character()),zone_1=numeric(),zone_2=numeric(),stringsAsFactors=FALSE)
       
       
       # supply_zone_df = supply_zone_detection(df,stock)
       
       supply_zone_df = supply_zone_detection(data,stock,df_supply_and_demand)
       
       df_supply_and_demand_final <- rbind(df_supply_and_demand_final, supply_zone_df)
       
       demand_zone_df = demand_zone_detection(data,stock,df_supply_and_demand)
       
       df_supply_and_demand_final <- rbind(df_supply_and_demand_final, demand_zone_df)
     }

     df_supply_and_demand_final = df_supply_and_demand_final[!rowSums((is.na(df_supply_and_demand_final))),]
     
   }
    
    if(nrow(df_supply_and_demand_final)>=1){
      rownames(df_supply_and_demand_final) <- 1:nrow(df_supply_and_demand_final)
    }
    
    # df_supply_and_demand_final$Date <- as.POSIXct(as.numeric(as.character(df_supply_and_demand_final$Date)),origin="1970-01-01")
    
    df_supply_and_demand_final$Date <- df_supply_and_demand_final$Date + hm("5:30") 
    
    
    
    df_supply_and_demand_final <- df_supply_and_demand_final[order(df_supply_and_demand_final$Date, decreasing = TRUE),]
    
    write.csv(df_supply_and_demand_final,paste0(getwd(),"/demand_and_supply_zones.csv", sep = ""))
    
    row.names(df_supply_and_demand_final) <- NULL
    
    DT::datatable(df_supply_and_demand_final,extensions = c('FixedColumns'),selection = "single",
                  options = list(scrollX = TRUE,
                                 pageLength=10,
                                 searchHighlight = TRUE,
                                 filter = 'top',
                                 rowCallback=JS(
                                   'function(row,data) {
     if($(row)["0"]["_DT_RowIndex"] % 2 <1)
            $(row).css("background","#D7DBDD")
   }')
                  ))
    
  })
  
  # output$demand_weekly_screener <-  DT::renderDataTable({
  #   
  #   nifty_df <- read.csv(paste0(getwd(),"/Nifty50_Stocks.csv", sep = ""))
  #   
  #   df_supply_and_demand_final = data.frame(stock=character(),pattern=character(),Date=as.Date(character()),zone_1=numeric(),zone_2=numeric(),stringsAsFactors=FALSE)
  #   
  #   for (i in 1:nrow(nifty_df)) {
  #     stock <- nifty_df[i,2]
  #     print(stock)
  #     # browser()
  #     # stock_data <- na.omit(getSymbols(as.character(stock),
  #     #                            src = "yahoo", 
  #     #                            from = as.Date(input$date_range[1]), 
  #     #                            to = as.Date(input$date_range[2]), 
  #     #                            return.class = 'xts',
  #     #                            periodicity = "weekly",
  #     #                            auto.assign = FALSE))
  #     
  #     stock_data <- na.omit(getSymbols(as.character(stock),
  #                                      src = "yahoo", 
  #                                      return.class = 'xts',
  #                                      periodicity = "weekly",
  #                                      auto.assign = FALSE))
  #     
  #     stock_data <- data.frame(Date = index(stock_data), coredata(stock_data) )
  #     
  #     stock_data <- stock_data[stock_data$Date>= as.Date(input$date_range[1]) & stock_data$Date<= as.Date(input$date_range[2]),]
  #     
  #     
  #     colnames(stock_data) <- c('Date','Open','High','Low','Close','Volume','Adjusted')
  #     # stock_data <- as.data.frame(stock_data)
  #     # stock_data <- cbind(Date = rownames(stock_data), stock_data)
  #     # rownames(stock_data) <- 1:nrow(stock_data)
  #     # # browser()
  #     # colnames(stock_data) <- c('Date','Open','High','Low','Close','Volume','Adjusted')
  #     
  #     df_supply_and_demand = data.frame(stock=character(),pattern=character(),Date=as.Date(character()),zone_1=numeric(),zone_2=numeric(),stringsAsFactors=FALSE)
  #     
  #     
  #     # supply_zone_df = supply_zone_detection(df,stock)
  #     
  #     supply_zone_df = supply_zone_detection(stock_data,stock,df_supply_and_demand)
  #     
  #     df_supply_and_demand_final <- rbind(df_supply_and_demand_final, supply_zone_df)
  #     
  #     demand_zone_df = demand_zone_detection(stock_data,stock,df_supply_and_demand)
  #     
  #     df_supply_and_demand_final <- rbind(df_supply_and_demand_final, demand_zone_df)
  #     
  #     df_supply_and_demand_final = df_supply_and_demand_final[!rowSums((is.na(df_supply_and_demand_final))),]
  #     
  #   }
  #   
  #   if(nrow(df_supply_and_demand_final)>=1){
  #     rownames(df_supply_and_demand_final) <- 1:nrow(df_supply_and_demand_final)
  #   }
  #   
  #   
  #   df_supply_and_demand_final
  #   
  # })
  
  output$Stocks_Screener <-  DT::renderDataTable({
    
    # browser()
    demand_zones <- read.csv(paste0(getwd(),"/demand_and_supply_zones.csv", sep = ""))
    
    demand_zones <- subset(demand_zones, select = -c(X) )

    today = Sys.Date()
    f <- function(d)if(format(d - 1, '%w') %in% c(0, 5)) Recall(d - 1) else d
    previousWorkingDay <- f(today) - 1
    
    current_zones <- demand_zones[demand_zones$Date < previousWorkingDay,]
    
    # browser()
    
    if(input$bot_timeframe == "1h"){
      current_zones <- demand_zones[demand_zones$Date < previousWorkingDay - 1,]
    }
    
    rownames(current_zones) <- NULL
    
    satisfied_df_supply_and_demand = data.frame(stock=character(),pattern=character(),strength=character(0),Date=as.POSIXct(character()),zone_1=numeric(),zone_2=numeric(),calltime=as.POSIXct(character()),stringsAsFactors=FALSE)
    
    # satisfied_df = data.frame(dates = as.POSIXct(character()),Open = numeric(),High=numeric(),Low = numeric(),Close = numeric(),Volume = numeric())
    
    #######    Iterate over every row in Demand and Supply created zones  #########
    
    
   
    
    for (i in 1:nrow(current_zones)) {
      
      
      current_stock <- current_zones[i,"stock"]
      
      current_date <- current_zones[i,"Date"]
      current_date <- as.Date(current_date)
      
      
      today = Sys.Date()
      f <- function(d)if(format(d - 1, '%w') %in% c(0, 5)) Recall(d - 1) else d
      previousWorkingDay <- f(today) - 2
      previousWorkingDay <- as.Date(previousWorkingDay)
      
      if(current_date == previousWorkingDay){
        print(current_stock)
        print("same date")
        next
      }else{


      # if(current_stock == "HINDALCO.NS"){
        
        # browser()
        
        response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",current_stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
        
        stock_timestamp <- response_data$chart$result[[1]]$timestamp
        Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
        High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
        Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
        Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
        Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume

        final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
        
        # stock_timestamp <- response_data$chart$result[[2]][[1]]
        # Close <- response_data$chart$result[[3]]$quote[[1]]$close
        # High <- response_data$chart$result[[3]]$quote[[1]]$high
        # Low <- response_data$chart$result[[3]]$quote[[1]]$low
        # Open <- response_data$chart$result[[3]]$quote[[1]]$open
        # Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
        # 
        # final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
        # # if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){
        # colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
        
        if(typeof(final_data$V1) == "list"){
          final_data <- final_data[-c(which(final_data$Close == "NULL")),]
          new_stock_timestamp <- unlist(final_data$V1)
          Close <- unlist(final_data$Close)
          High <- unlist(final_data$High)
          Open <- unlist(final_data$Open)
          Low <- unlist(final_data$Low)
          Volume <- unlist(final_data$Volume)
          
          final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
          
          final_data$Date <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
          
          final_data <- final_data %>% select(Date, Open, High, Low, Close,Volume)
        }else{
          final_data$Date <- as.POSIXct(final_data$V1, origin="1970-01-01")
          
          final_data <- final_data %>% select(Date, Open, High, Low, Close,Volume)
        }
        final_data <- na.omit(final_data)
        
        # browser()
        
        temp_df <- data.frame()
        
        for(ind in 1:nrow(final_data)){
          
          ######     Get the zone from the file    #######
          zone_1 <- max(current_zones[i,"zone_1"],current_zones[i,"zone_2"])
          zone_2 <- min(current_zones[i,"zone_1"],current_zones[i,"zone_2"])
          
          if((final_data[ind,"Close"] <= zone_1 && final_data[ind,"Close"] >= zone_2) && (final_data[ind,"Open"] <= zone_1 && final_data[ind,"Open"] >= zone_2)){
            temp_df <- current_zones[i,]
            temp_df$calltime <- as.POSIXct(final_data[ind,"Date"])
            satisfied_df_supply_and_demand <- rbind(satisfied_df_supply_and_demand,temp_df)
            # print(final_data[ind,"Date"])
            # satisfied_df_supply_and_demand[nrow(satisfied_df_supply_and_demand),"calltime"] <- final_data[ind,"Date"]
            # satisfied_df <- rbind(satisfied_df,final_data) 
            print(current_stock)
            break
          }else{
            next
          }
          
        }
        
      # }
      
    }
      
    }
    # browser()
    
    # print(satisfied_df_supply_and_demand)
    
    
    
    satisfied_df_supply_and_demand$Date <- as.Date(satisfied_df_supply_and_demand$Date)
    
    satisfied_df_supply_and_demand$calltime <- satisfied_df_supply_and_demand$calltime + hm("5:30") 
    
    satisfied_df_supply_and_demand <- satisfied_df_supply_and_demand[order(satisfied_df_supply_and_demand$calltime, decreasing = TRUE),]
    
    row.names(satisfied_df_supply_and_demand) <- NULL
    
    write.csv(satisfied_df_supply_and_demand,paste0(getwd(),"/current_zone_stocks.csv", sep = ""))
    
    
    DT::datatable(satisfied_df_supply_and_demand,extensions = c('FixedColumns'),selection = "single",
                  options = list(scrollX = TRUE,
                                 pageLength=10,
                                 searchHighlight = TRUE,
                                 filter = 'top',
                                 rowCallback=JS(
                                   'function(row,data) {
     if($(row)["0"]["_DT_RowIndex"] % 2 <1)
            $(row).css("background","#D7DBDD")
   }')
                  ))
    
    
    
   
    
  })
  
  output$Execution_Screener <-  DT::renderDataTable({
    
    stock_row <- input$Stocks_Screener_rows_selected
    current_zones <- read.csv(paste0(getwd(),"/current_zone_stocks.csv", sep = ""))
    current_zones <- subset(current_zones, select = -c(X) )
    
    stock <- current_zones[stock_row,"stock"]
    
    call_date <- current_zones[stock_row,"Date"]
    
    call_date <- as.Date(call_date)
    
    print(stock)
    
    nextWorkingDay <- adjust(calendar="TARGET", dates=call_date + 2, bdc = 0)
    
    # print(nextWorkingDay)
    
    stock_data <- getSymbols(stock, src = "yahoo", from = nextWorkingDay, to = Sys.Date() + 1 , auto.assign = FALSE)
    
    stock_data <- data.frame(Date = index(stock_data), coredata(stock_data))
    
    colnames(stock_data) <- c("Date","Open","High","Low","Close","Volume","Adjusted")
    
    final_historic_data <- data.frame()
    
    # print(stock_data)
    
    for (row_ind in 1:nrow(stock_data)) {
      
      max_zone <- max(current_zones[stock_row,"zone_1"],current_zones[stock_row,"zone_2"])
      min_zone <- min(current_zones[stock_row,"zone_1"],current_zones[stock_row,"zone_2"])
      
      if((current_zones[stock_row, "pattern"] == "Supply Reversal Pattern") || (current_zones[stock_row, "pattern"] == "Supply Continuous Pattern")){
        if((stock_data[row_ind,"High"] <= max_zone) && (stock_data[row_ind,"High"] >= min_zone)){
          # print(stock)
          # print(current_zones[stock_row,])
          # print(stock_data[row_ind,])
          final_historic_data <- rbind(final_historic_data,stock_data[row_ind,])
        }
      }else{
        if((stock_data[row_ind,"Low"] <= max_zone) && (stock_data[row_ind,"Low"] >= min_zone)){
          # print(stock)
          # print(current_zones[stock_row,])
          # print(stock_data[row_ind,])
          final_historic_data <- rbind(final_historic_data,stock_data[row_ind,])
        }
      }
      
      
    }
    
    final_historic_data
    
  })
  
  })
  
  
}

# 
# 
# stock_row <- input$bot_buy_and_sell_rows_selected
# 
# Signal_df <- read.csv(paste0(getwd(),"/data/Signal_df.csv", sep = ""))
# 
# stock <- Signal_df[stock_row,"Stock"]
# 
# current_zones <- read.csv(paste0(getwd(),"/current_zone_stocks.csv", sep = ""))
# current_zones <- subset(current_zones, select = -c(X) )
# 
# for (ind in 1:nrow(current_zones)) {
# 
#   call_date <- current_zones[ind,"Date"]
#   stock <- current_zones[ind,"stock"]
# 
#   # print(stock)
#   call_date <- as.Date(call_date)
# 
#   # print(call_date)
# 
#   nextWorkingDay <- adjust(calendar="TARGET", dates=call_date + 2, bdc = 0)
# 
#   # print(nextWorkingDay)
# 
#   stock_data <- getSymbols(stock, src = "yahoo", from = nextWorkingDay, to = Sys.Date() + 1 , auto.assign = FALSE)
# 
#   stock_data <- data.frame(Date = index(stock_data), coredata(stock_data))
# 
#   colnames(stock_data) <- c("Date","Open","High","Low","Close","Volume","Adjusted")
# 
#   # print(stock_data)
# 
#   for (row_ind in 1:nrow(stock_data)) {
# 
#     max_zone <- max(current_zones[ind,"zone_1"],current_zones[ind,"zone_2"])
#     min_zone <- min(current_zones[ind,"zone_1"],current_zones[ind,"zone_2"])
# 
#     if((current_zones[ind, "pattern"] = "Supply Reversal Pattern") || (current_zones[ind, "pattern"] = "Supply Continuous Pattern")){
#       if((stock_data[row_ind,"High"] <= max_zone) && (stock_data[row_ind,"High"] >= min_zone)){
#         print(stock)
#         print(current_zones[ind,])
#         print(stock_data[row_ind,])
#       }
#     }else{
#       if((stock_data[row_ind,"Low"] <= max_zone) && (stock_data[row_ind,"Low"] >= min_zone)){
#         print(stock)
#         print(current_zones[ind,])
#         print(stock_data[row_ind,])
#       }
#     }
# 
# 
#   }
# 
# }


