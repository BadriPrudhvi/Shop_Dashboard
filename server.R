library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(highcharter)
library(googlesheets)
library(xts)
library(dygraphs)

# shopData <- gs_title("Shop_Sales") %>%  gs_read()
# Stock_Data <- gs_title("Stock_Data") %>%  gs_read()
# Sales_Data <- gs_title("Sale_Transactions") %>% gs_read()
# Payment_Data <- gs_title("Payment_Transactions") %>% gs_read()
# Expense_Data <- gs_title("Expense_Transactions") %>% gs_read()
# 
# saveRDS(shopData,"~/Documents/ShopSales.rds")
# saveRDS(Stock_Data,"~/Documents/Stock_Data.rds")
# saveRDS(Sales_Data,"~/Documents/Sales_Data.rds")
# saveRDS(Payment_Data,"~/Documents/Payment_Data.rds")
# saveRDS(Expense_Data,"~/Documents/Expense_Data.rds")

Full_shopData <- reactiveFileReader(60000, session = NULL
                           , filePath = '~/Documents/ShopSales.rds', readRDS)

Stock_Data <- reactiveFileReader(60000, session = NULL
                                 , filePath = '~/Documents/Stock_Data.rds', readRDS)

Sales_Data <- reactiveFileReader(60000, session = NULL
                                 , filePath = '~/Documents/Sales_Data.rds', readRDS)

Payment_Data <- reactiveFileReader(60000, session = NULL
                                 , filePath = '~/Documents/Payment_Data.rds', readRDS)

Expense_Data <- reactiveFileReader(60000, session = NULL
                                 , filePath = '~/Documents/Expense_Data.rds', readRDS)

server <- function(input, output, session) {

shopData <- reactive({ 
    Full_shopData() %>% 
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] )  
}) 

output$OverallSales <- renderText({
  paste("₹",format(sum(shopData()$Sale,na.rm=T), big.mark=","))
})

output$OverallProfit <- renderText({
  paste("₹",format(sum(shopData()$Profit,na.rm=T), big.mark=","))
})

output$OverallExpense <- renderText({
  paste("₹",format(sum(shopData()$Expenses,na.rm=T), big.mark=","))
})

output$OverallPayment <- renderText({
  paste("₹",format(sum(shopData()$Payment,na.rm=T), big.mark=","))
})

output$AverageSales <- renderText({
  paste("₹",format(round(mean(shopData()$Sale,na.rm=T)), nsmall=0, big.mark=","))
})

output$AverageProfit <- renderText({
  paste("₹",format(round(mean(shopData()$Profit,na.rm=T)), nsmall=0, big.mark=","))
})

output$AverageExpense <- renderText({
  paste("₹",format(round(mean(shopData()$Expenses,na.rm=T)), nsmall=0, big.mark=","))
})

output$AveragePayment <- renderText({
  paste("₹",format(round(mean(shopData()$Payment,na.rm=T)), nsmall=0, big.mark=","))
})

output$MyInvestment <- renderText({
  paste("₹",format(round(sum(Full_shopData()$Investment,na.rm=T)), nsmall=0, big.mark=","))
})

output$ProfitPercent <- renderText({
  paste(round((sum(shopData()$Profit,na.rm=T)/sum(shopData()$Sale,na.rm=T))*100,2)," %")
})

output$ExpensePercent <- renderText({
  paste(round((sum(shopData()$Expenses,na.rm=T)/sum(shopData()$Sale,na.rm=T))*100,2)," %")
})

output$StockWorth <- renderText({
  paste("₹",format(round(sum(Stock_Data()$Worth,na.rm=T)), nsmall=0, big.mark=","))
})

shop_Summary <- reactive({
  shopData() %>%
  group_by(
    Date = floor_date(as.Date(Date,"%m/%d/%Y"), tolower(input$timeslice))
  ) %>%
  summarise( Days = n(),
             Total_Sale = sum(Sale,na.rm=T),
             Total_Profit = sum(Profit,na.rm=T),
             Total_Expense = sum(Expenses,na.rm=T),
             Total_Payment = sum(Payment,na.rm=T),
             Avg_Sale = round(mean(Sale,na.rm=T),2),
             Avg_Profit = round(mean(Profit,na.rm=T),2),
             Avg_Expense = round(mean(Expenses,na.rm=T),2),
             Avg_Payment = round(mean(Payment,na.rm=T),2),
             Profit_Percent = round((Total_Profit/Total_Sale),2),
             Expense_Percent = round((Total_Expense/Total_Sale),2),
             Payment_Percent = round((Total_Payment/Total_Sale),2),
             No_Sale_Days = sum(ifelse(Sale == 0, 1, 0),na.rm=T)
  )
})

output$SummaryTable <- renderDataTable({
  datatable(
    shop_Summary() %>% arrange(desc(Date))
    # , class = 'table-bordered'
    ,rownames=FALSE
    ,filter = "top"
    ,extensions = c("Buttons","FixedColumns")
    , options = list(
      dom = 'Bfrtip'
      , buttons = c('pageLength', 'colvis', 'csv')
      , lengthMenu = list(c(10, 15, 30, -1), c('10', '15', '30','All'))
      , pageLength = 10
      , searchHighlight = TRUE
      , width="100%"
      , scrollX = TRUE
      , fixedColumns = list(leftColumns = 1)
    )
  ) %>% formatCurrency(3:10,currency="₹ ") %>%
    formatPercentage(11:13)
})

output$Overall_Trends_Plot <- renderDygraph({
  pdf(NULL)
  g <- dygraph(xts(shop_Summary()[,3:6],order.by = shop_Summary()$Date)
               ,ylab = "Rupees (₹)") %>% 
       dyRangeSelector() %>%
       dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
       dyOptions(colors = c("blue","green", "red","orange"),strokeWidth = 2) %>%
       dyLegend(width = 800)
  dev.off()
  g
})

output$Average_Trends_Plot <- renderDygraph({
  pdf(NULL)
  g <- dygraph(xts(shop_Summary()[,7:10],order.by = shop_Summary()$Date)
               ,ylab = "Rupees (₹)") %>% 
    dyRangeSelector() %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
    dyOptions(colors = c("blue","green", "red","orange"),strokeWidth = 2) %>%
    dyLegend(width = 800)
  dev.off()
  g
})

output$Percent_Trends_Plot <- renderDygraph({
  pdf(NULL)
  g <- dygraph(xts(shop_Summary() %>% transmute(Profit_Percent = Profit_Percent*100,
                                                Expense_Percent = Expense_Percent*100)
                   ,order.by = shop_Summary()$Date)
               ,ylab = "Percentage (%)") %>% 
    dyRangeSelector() %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
    dyOptions(colors = c("green", "red"),strokeWidth = 2) %>%
    dyLegend(width = 800)
  dev.off()
  g
})



################################ Sale Analysis Tab #######################################

output$ItemList <- renderUI({
  selectizeInput("Items"
                 , "Items:"
                 , sort(unique(as.character(Sales_Data()$Item)))
                 , NULL
                 , multiple =TRUE)
})

Sale_report <- reactive({
  if(is.null(input$Items)){
    ItemsO <- unique(Sales_Data()$Item)
  }else{
    ItemsO <- input$Items
  }
  
  Sales_Data() %>%
    filter(
      Item %in% ItemsO
    )
})

Sale_Summary <- reactive({
  Sale_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Sale = sum(Selling_Price,na.rm=T)) %>%
    group_by(Item) %>%
    summarise( Transactions = n(),
               Total_Sale = sum(Selling_Price,na.rm=T),
               Avg_Sale = round(mean(Selling_Price,na.rm=T),2),
               Sale_Percent = sum(Selling_Price,na.rm=T)/mean(Entire_Sale,na.rm=T)
    )
})

output$SalesTable <- renderDataTable({
  datatable(
    Sale_Summary() %>% arrange(desc(Total_Sale))
    ,rownames=FALSE
    ,filter = "top"
    ,extensions = c("Buttons","FixedColumns")
    , options = list(
      dom = 'Bfrtip'
      , buttons = c('pageLength', 'colvis', 'csv')
      , lengthMenu = list(c(10, 15, 30, -1), c('10', '15', '30','All'))
      , pageLength = 10
      , searchHighlight = TRUE
      , width="100%"
      , scrollX = TRUE
      , fixedColumns = list(leftColumns = 1)
    )
  ) %>% formatCurrency(3:4,currency="₹ ") %>%
    formatPercentage(5,digits=2)
})

################################ Stock House Tab ##########################################

output$DealerList <- renderUI({
  selectizeInput("Dealers"
                 , "Dealers:"
                 , sort(unique(as.character(Payment_Data()$Dealer)))
                 , NULL
                 , multiple =TRUE)
})

Stock_report <- reactive({
  if(is.null(input$Dealers)){
    DealersO <- unique(Stock_Data()$Dealer)
  }else{
    DealersO <- input$Dealers
  }
  
  Stock_Data() %>%
    filter(
      Dealer %in% DealersO
    )
})

Stock_Summary <- reactive({
  Stock_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Worth = sum(Worth,na.rm=T)) %>%
    group_by(Dealer) %>%
    summarise( Transactions = n(),
               Total_Stock_Price = sum(Worth,na.rm=T),
               Avg_Stock_Price = round(mean(Worth,na.rm=T),2),
               Stock_Percent = sum(Worth,na.rm=T)/mean(Entire_Worth,na.rm=T)
    )
})

output$StockTable <- renderDataTable({
  datatable(
    Stock_Summary() %>% arrange(desc(Total_Stock_Price))
    ,rownames=FALSE
    ,filter = "top"
    ,extensions = c("Buttons","FixedColumns")
    , options = list(
      dom = 'Bfrtip'
      , buttons = c('pageLength', 'colvis', 'csv')
      , lengthMenu = list(c(10, 15, 30, -1), c('10', '15', '30','All'))
      , pageLength = 10
      , searchHighlight = TRUE
      , width="100%"
      , scrollX = TRUE
      , fixedColumns = list(leftColumns = 1)
    )
  ) %>% formatCurrency(3:4,currency="₹ ") %>%
    formatPercentage(5,digits=2)
})


#### Payments View Tab #####

Payment_report <- reactive({
  if(is.null(input$Dealers)){
    DealersO <- unique(Payment_Data()$Dealer)
  }else{
    DealersO <- input$Dealers
  }
  
  Payment_Data() %>%
    filter(
      Dealer %in% DealersO
    )
})

Payment_Summary <- reactive({
  Payment_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Payment = sum(Payment_Amount,na.rm=T)) %>%
    group_by(Dealer) %>%
    summarise( Transactions = n(),
               Total_Payment = sum(Payment_Amount,na.rm=T),
               Avg_Payment = round(mean(Payment_Amount,na.rm=T),2),
               Payment_Percent = sum(Payment_Amount,na.rm=T)/mean(Entire_Payment,na.rm=T)
    )
})

output$PaymentTable <- renderDataTable({
  datatable(
    Payment_Summary() %>% arrange(desc(Total_Payment))
    ,rownames=FALSE
    ,filter = "top"
    ,extensions = c("Buttons","FixedColumns")
    , options = list(
      dom = 'Bfrtip'
      , buttons = c('pageLength', 'colvis', 'csv')
      , lengthMenu = list(c(10, 15, 30, -1), c('10', '15', '30','All'))
      , pageLength = 10
      , searchHighlight = TRUE
      , width="100%"
      , scrollX = TRUE
      , fixedColumns = list(leftColumns = 1)
    )
  ) %>% formatCurrency(3:4,currency="₹ ") %>%
    formatPercentage(5,digits=2)
})

################################ Expense Analysis Tab #####################################

output$Expense_SourceList <- renderUI({
  selectizeInput("Expenses_Source"
                 , "Expenses Source:"
                 , sort(unique(as.character(Expense_Data()$Expense_Type)))
                 , NULL
                 , multiple =TRUE)
})

Expenses_report <- reactive({
  if(is.null(input$Expenses_Source)){
    TypeO <- unique(Expense_Data()$Expense_Type)
  }else{
    TypeO <- input$Expenses_Source
  }
  
  Expense_Data() %>%
    filter(
      Expense_Type %in% TypeO
    )
})

Expenses_Summary <- reactive({
  Expenses_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Expense = sum(Expense_Amount,na.rm=T)) %>%
    group_by(Expense_Type) %>%
    summarise( Transactions = n(),
               Total_Expense = sum(Expense_Amount,na.rm=T),
               Avg_Expense = round(mean(Expense_Amount,na.rm=T),2),
               Expense_Percent = sum(Expense_Amount,na.rm=T)/mean(Entire_Expense,na.rm=T)
    )
})

output$ExpensesTable <- renderDataTable({
  datatable(
    Expenses_Summary() %>% arrange(desc(Total_Expense))
    ,rownames=FALSE
    ,filter = "top"
    ,extensions = c("Buttons","FixedColumns")
    , options = list(
      dom = 'Bfrtip'
      , buttons = c('pageLength', 'colvis', 'csv')
      , lengthMenu = list(c(10, 15, 30, -1), c('10', '15', '30','All'))
      , pageLength = 10
      , searchHighlight = TRUE
      , width="100%"
      , scrollX = TRUE
      , fixedColumns = list(leftColumns = 1)
    )
  ) %>% formatCurrency(3:4,currency="₹ ") %>%
    formatPercentage(5,digits=2)
})

}