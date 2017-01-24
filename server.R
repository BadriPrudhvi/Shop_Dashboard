library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(highcharter)
library(googlesheets)
library(xts)
library(ggplot2)
library(plotly)
library(dygraphs)

shopData <- gs_title("Shop_Sales") %>%  gs_read()
Stock_Data <- gs_title("Stock_Data") %>%  gs_read()
Sales_Data <- gs_title("Sale_Transactions") %>% gs_read()
Payment_Data <- gs_title("Payment_Transactions") %>% gs_read()
Expense_Data <- gs_title("Expense_Transactions") %>% gs_read()

saveRDS(shopData,"/Users/bprudhvi/Documents/Shop_Dashboard/ShopSales.rds")
saveRDS(Stock_Data,"/Users/bprudhvi/Documents/Shop_Dashboard/Stock_Data.rds")
saveRDS(Sales_Data,"/Users/bprudhvi/Documents/Shop_Dashboard/Sales_Data.rds")
saveRDS(Payment_Data,"/Users/bprudhvi/Documents/Shop_Dashboard/Payment_Data.rds")
saveRDS(Expense_Data,"/Users/bprudhvi/Documents/Shop_Dashboard/Expense_Data.rds")

# library(rsconnect)
# packrat:::appDependencies()
# install.packages("shinydashboard")
# rsconnect::deployApp('/Users/bprudhvi/Documents/Shop_Dashboard')

Full_shopData <- reactiveFileReader(60000, session = NULL
                           , filePath = 'ShopSales.rds', readRDS)

Stock_Data <- reactiveFileReader(60000, session = NULL
                                 , filePath = 'Stock_Data.rds', readRDS)

Sales_Data <- reactiveFileReader(60000, session = NULL
                                 , filePath = 'Sales_Data.rds', readRDS)

Payment_Data <- reactiveFileReader(60000, session = NULL
                                 , filePath = 'Payment_Data.rds', readRDS)

Expense_Data <- reactiveFileReader(60000, session = NULL
                                 , filePath = 'Expense_Data.rds', readRDS)

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
  Invest <- Full_shopData() %>% 
    mutate(Deposit_Date = as.Date(Deposit_Date,"%m/%d/%Y")) %>%
    filter( Deposit_Date >= input$daterange[1]
            & Deposit_Date <= input$daterange[2] )
  paste("₹",format(round(sum(Invest$Investment,na.rm=T)), nsmall=0, big.mark=","))
})

output$ProfitPercent <- renderText({
  paste(round((sum(shopData()$Profit,na.rm=T)/sum(shopData()$Sale,na.rm=T))*100,2)," %")
})

output$ExpensePercent <- renderText({
  paste(round((sum(shopData()$Expenses,na.rm=T)/sum(shopData()$Profit,na.rm=T))*100,2)," %")
})

output$StockWorth <- renderText({
  stocks <- Stock_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] )
  paste("₹",format(round(sum(stocks$Worth,na.rm=T)), nsmall=0, big.mark=","))
})

output$NetSale <- renderText({
  paste("₹",format(sum(shopData()$Sale,na.rm=T)-sum(shopData()$Profit,na.rm=T), big.mark=","))
})

output$EarnedMoney <- renderText({
  paste("₹",format(sum(shopData()$Profit,na.rm=T)-sum(shopData()$Expenses,na.rm=T), big.mark=","))
})

output$PaymentPercentwithProfit <- renderText({
  paste(round(sum(shopData()$Payment,na.rm=T)/((sum(shopData()$Sale,na.rm=T)-sum(shopData()$Profit,na.rm=T)) + (sum(shopData()$Profit,na.rm=T) - sum(shopData()$Expenses,na.rm=T)))*100,2),"%")
})

output$PaymentPercentwithoutProfit <- renderText({
  paste(round(sum(shopData()$Payment,na.rm=T)/((sum(shopData()$Sale,na.rm=T)-sum(shopData()$Profit,na.rm=T)))*100,2),"%")
})

output$TotalDays <- renderText({
  as.character(nrow(shopData()))
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
             Net_Sale = Total_Sale - Total_Profit,
             Earned_Money = Total_Profit - Total_Expense,
             Avg_Sale = round(mean(Sale,na.rm=T),2),
             Avg_Profit = round(mean(Profit,na.rm=T),2),
             Avg_Expense = round(mean(Expenses,na.rm=T),2),
             Avg_Payment = round(mean(Payment,na.rm=T),2),
             Profit_Percent = round((Total_Profit/Total_Sale),2),
             Expense_Percent = round((Total_Expense/Total_Profit),2),
             Payment_Percent = round((Total_Payment/Net_Sale),2),
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
  ) %>% formatCurrency(3:12,currency="₹ ") %>%
    formatPercentage(13:15)
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
                 , sort(unique(tolower(as.character(Sales_Data()$Item))))
                 , NULL
                 , multiple =TRUE)
})

Sale_report <- reactive({
  if(is.null(input$Items)){
    ItemsO <- unique(tolower(Sales_Data()$Item))
  }else{
    ItemsO <- input$Items
  }
  
  Sales_Data() %>%
    filter(
      tolower(Item) %in% ItemsO
    )
})

Sale_Summary <- reactive({
  Sale_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Sale = sum(Selling_Price,na.rm=T)) %>%
    group_by(Item = tolower(Item)) %>%
    summarise( Transactions = n(),
               Total_Sale = sum(Selling_Price,na.rm=T),
               Avg_Sale = round(mean(Selling_Price,na.rm=T),2),
               Sale_Percent = sum(Selling_Price,na.rm=T)/mean(Entire_Sale,na.rm=T)
    )
})

Sales_By_Time <- reactive({
  Sale_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Sale = sum(Selling_Price,na.rm=T)) %>%
    group_by(Date = floor_date(Date, tolower(input$saletimeslice))
    ) %>%
    summarise( Transactions = n(),
               Total_Sale = sum(Selling_Price,na.rm=T),
               Avg_Sale = round(mean(Selling_Price,na.rm=T),2)
               # ,Sale_Percent = sum(Selling_Price,na.rm=T)/mean(Entire_Sale,na.rm=T)
    )
})

output$SaleMoney <- renderText({
  paste("₹",format(round(sum(Sales_By_Time()$Total_Sale,na.rm=T)), nsmall=0, big.mark=","))
})

output$AvgSaleMoney <- renderText({
  paste("₹",format(round(mean(Sales_By_Time()$Total_Sale,na.rm=T)), nsmall=0, big.mark=","))
})

output$Sale_Plot_Time <- renderHighchart({
  pdf(NULL)
  g <-  highchart() %>% 
    hc_yAxis(labels = list(format = "{value:,.0f}")) %>%
    hc_add_series_times_values(Sales_By_Time()$Date, Sales_By_Time()$Total_Sale
                               ,name = "Sale Money"
                               ,colorByPoint=TRUE
                               ,type = "column"
                               ,showInLegend = FALSE) %>%
    hc_chart(options3d = list(enabled = TRUE,beta=15,alpha=5))
  dev.off()
  g
})

output$Sale_distribution <- renderHighchart({
  Sales_Pie <- Sale_Summary() %>% mutate(Sale_Percent = round(Sale_Percent *100,2))
  pdf(NULL)
  g <-  highchart() %>%
    hc_add_series_labels_values(Sales_Pie$Item, Sales_Pie$Sale_Percent
                                , type = "pie"
                                , name = "Sale Percent"
                                , colorByPoint = TRUE
                                , size = 300
    )
  dev.off()
  g
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

output$Sales_Time_Table <- renderDataTable({
  datatable(
    Sales_By_Time() %>% arrange(desc(Date))
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
  ) %>% formatCurrency(3:4,currency="₹ ") 
  # %>% formatPercentage(5,digits=2)
})

################################ Profit Analysis Tab #######################################
output$ProductList <- renderUI({
  selectizeInput("Products"
                 , "Products:"
                 , sort(unique(tolower(as.character(Sales_Data()$Item))))
                 , NULL
                 , multiple =TRUE)
})

Profit_report <- reactive({
  if(is.null(input$Products)){
    ProductsO <- unique(tolower(Sales_Data()$Item))
  }else{
    ProductsO <- input$Products
  }
  
  Sales_Data() %>%
    filter(
      tolower(Item) %in% ProductsO
    )
})

Profit_Summary <- reactive({
  Profit_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Profit = sum(Profit,na.rm=T)) %>%
    group_by(Item = tolower(Item)) %>%
    summarise( Transactions = n(),
               Total_Profit = sum(Profit,na.rm=T),
               Avg_Profit = round(mean(Profit,na.rm=T),2),
               Profit_Percent = sum(Profit,na.rm=T)/mean(Entire_Profit,na.rm=T)
    )
})

Profit_By_Time <- reactive({
  Profit_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Profit_Sale = sum(Profit,na.rm=T)) %>%
    group_by(Date = floor_date(Date, tolower(input$profittimeslice))
    ) %>%
    summarise( Transactions = n(),
               Total_Profit = sum(Profit,na.rm=T),
               Avg_Profit = round(mean(Profit,na.rm=T),2)
               # ,Sale_Percent = sum(Selling_Price,na.rm=T)/mean(Entire_Sale,na.rm=T)
    )
})

output$ProfitMoney <- renderText({
  paste("₹",format(round(sum(Profit_By_Time()$Total_Profit,na.rm=T)), nsmall=0, big.mark=","))
})

output$AvgProfitMoney <- renderText({
  paste("₹",format(round(mean(Profit_By_Time()$Total_Profit,na.rm=T)), nsmall=0, big.mark=","))
})

output$Profit_Plot_Time <- renderHighchart({
  pdf(NULL)
  g <-  highchart() %>% 
    hc_yAxis(labels = list(format = "{value:,.0f}")) %>%
    hc_add_series_times_values(Profit_By_Time()$Date, Profit_By_Time()$Total_Profit
                               ,name = "Profit Money"
                               ,colorByPoint=TRUE
                               ,type = "column"
                               ,showInLegend = FALSE) %>%
    hc_chart(options3d = list(enabled = TRUE,beta=15,alpha=5))
  dev.off()
  g
})

output$Profit_distribution <- renderHighchart({
  Profit_Pie <- Profit_Summary() %>% mutate(Profit_Percent = round(Profit_Percent *100,2))
  pdf(NULL)
  g <-  highchart() %>%
    hc_add_series_labels_values(Profit_Pie$Item, Profit_Pie$Profit_Percent
                                , type = "pie"
                                , name = "Profit Percent"
                                , colorByPoint = TRUE
                                , size = 300
    )
  dev.off()
  g
})

output$ProfitTable <- renderDataTable({
  datatable(
    Profit_Summary() %>% arrange(desc(Total_Profit))
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

output$Profit_Time_Table <- renderDataTable({
  datatable(
    Profit_By_Time() %>% arrange(desc(Date))
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
  ) %>% formatCurrency(3:4,currency="₹ ") 
  # %>% formatPercentage(5,digits=2)
})

################################ Expense Analysis Tab #####################################

output$Expense_SourceList <- renderUI({
  selectizeInput("Expenses_Source"
                 , "Expenses Source:"
                 , sort(unique(tolower(as.character(Expense_Data()$Expense_Type))))
                 , NULL
                 , multiple =TRUE)
})

Expenses_report <- reactive({
  if(is.null(input$Expenses_Source)){
    TypeO <- unique(tolower(Expense_Data()$Expense_Type))
  }else{
    TypeO <- input$Expenses_Source
  }
  
  Expense_Data() %>%
    filter(
      tolower(Expense_Type) %in% TypeO
    )
})

Expenses_Summary <- reactive({
  Expenses_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Expense = sum(Expense_Amount,na.rm=T)) %>%
    group_by(Expense_Type = tolower(Expense_Type)) %>%
    summarise( Transactions = n(),
               Total_Expense = sum(Expense_Amount,na.rm=T),
               Avg_Expense = round(mean(Expense_Amount,na.rm=T),2),
               Expense_Percent = sum(Expense_Amount,na.rm=T)/mean(Entire_Expense,na.rm=T)
    )
})

Expenses_By_Time <- reactive({
  Expenses_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Expense = sum(Expense_Amount,na.rm=T)) %>%
    group_by(Date = floor_date(Date, tolower(input$expensetimeslice))
             ) %>%
    summarise( Transactions = n(),
               Total_Expense = sum(Expense_Amount,na.rm=T),
               Avg_Expense = round(mean(Expense_Amount,na.rm=T),2)
               # ,Expense_Percent = sum(Expense_Amount,na.rm=T)/mean(Entire_Expense,na.rm=T)
    )
})

output$TotExpense <- renderText({
  paste("₹",format(round(sum(Expenses_By_Time()$Total_Expense,na.rm=T)), nsmall=0, big.mark=","))
})

output$AvgExpenseMoney <- renderText({
  paste("₹",format(round(mean(Expenses_By_Time()$Total_Expense,na.rm=T)), nsmall=0, big.mark=","))
})

output$Expense_Plot_Time <- renderHighchart({
  pdf(NULL)
  g <-  highchart() %>% 
    hc_yAxis(labels = list(format = "{value:,.0f}")) %>%
    hc_add_series_times_values(Expenses_By_Time()$Date, Expenses_By_Time()$Total_Expense
                               ,name = "Expense Money"
                               ,colorByPoint=TRUE
                               ,type = "column"
                               ,showInLegend = FALSE) %>%
    hc_chart(options3d = list(enabled = TRUE,beta=15,alpha=5))
  dev.off()
  g
})

output$Expense_distribution <- renderHighchart({
  Expenses_Pie <- Expenses_Summary() %>% mutate(Expense_Percent = round(Expense_Percent *100,2))
  pdf(NULL)
  g <-  highchart() %>%
    hc_add_series_labels_values(Expenses_Pie$Expense_Type, Expenses_Pie$Expense_Percent
                                , type = "pie"
                                , name = "Expense Percent"
                                , colorByPoint = TRUE
                                , size = 300
    )
  dev.off()
  g
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

output$Expenses_Time_Table <- renderDataTable({
  datatable(
    Expenses_By_Time() %>% arrange(desc(Date))
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
  ) %>% formatCurrency(3:4,currency="₹ ") 
  # %>% formatPercentage(6,digits=2)
})

####################################### Payments Tab ##################################################

output$Payment_Dealers_List <- renderUI({
  selectizeInput("paymentDealers"
                 , "Dealers:"
                 , sort(unique(tolower(as.character(Payment_Data()$Dealer))))
                 , NULL
                 , multiple =TRUE)
})

Payment_report <- reactive({
  if(is.null(input$paymentDealers)){
    DealersO <- unique(tolower(Payment_Data()$Dealer))
  }else{
    DealersO <- input$paymentDealers
  }
  
  Payment_Data() %>%
    filter(
      tolower(Dealer) %in% DealersO
    )
})

Payment_Summary <- reactive({
  Payment_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Payment = sum(Payment_Amount,na.rm=T)) %>%
    group_by(Dealer = tolower(Dealer)) %>%
    summarise( Transactions = n(),
               Total_Payment = sum(Payment_Amount,na.rm=T),
               Avg_Payment = round(mean(Payment_Amount,na.rm=T),2),
               Payment_Percent = sum(Payment_Amount,na.rm=T)/mean(Entire_Payment,na.rm=T)
    )
})

Payment_By_Time <- reactive({
  Payment_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Payment = sum(Payment_Amount,na.rm=T)) %>%
    group_by(Date = floor_date(Date, tolower(input$paymenttimeslice))
    ) %>%
    summarise( Transactions = n(),
               Total_Payment = sum(Payment_Amount,na.rm=T),
               Avg_Payment = round(mean(Payment_Amount,na.rm=T),2)
               # ,Payment_Percent = sum(Payment_Amount,na.rm=T)/mean(Entire_Payment,na.rm=T)
    )
})

output$TotPayment <- renderText({
  paste("₹",format(round(sum(Payment_By_Time()$Total_Payment,na.rm=T)), nsmall=0, big.mark=","))
})

output$AvgPaymentMoney <- renderText({
  paste("₹",format(round(mean(Payment_By_Time()$Total_Payment,na.rm=T)), nsmall=0, big.mark=","))
})

output$Payment_Plot_Time <- renderHighchart({
  pdf(NULL)
  g <-  highchart() %>% 
    hc_yAxis(labels = list(format = "{value:,.0f}")) %>%
    hc_add_series_times_values(Payment_By_Time()$Date, Payment_By_Time()$Total_Payment
                               ,name = "Payment Money"
                               ,colorByPoint=TRUE
                               ,type = "column"
                               ,showInLegend = FALSE) %>%
    hc_chart(options3d = list(enabled = TRUE,beta=15,alpha=5))
  dev.off()
  g
})

output$Payment_distribution <- renderHighchart({
  Payments_Pie <- Payment_Summary() %>% mutate(Payment_Percent = round(Payment_Percent *100,2))
  pdf(NULL)
  g <-  highchart() %>%
    hc_add_series_labels_values(Payments_Pie$Dealer, Payments_Pie$Payment_Percent
                                , type = "pie"
                                , name = "Payment Percent"
                                , colorByPoint = TRUE
                                , size = 300
    )
  dev.off()
  g
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

output$Payment_Time_Table <- renderDataTable({
  datatable(
    Payment_By_Time() %>% arrange(desc(Date))
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
  ) %>% formatCurrency(3:4,currency="₹ ") 
  # %>% formatPercentage(5,digits=2)
})


################################ Stocks Tab ##########################################

output$DealerList <- renderUI({
  selectizeInput("Dealers"
                 , "Dealers:"
                 , sort(unique(tolower(as.character(Payment_Data()$Dealer))))
                 , NULL
                 , multiple =TRUE)
})

Stock_report <- reactive({
  if(is.null(input$Dealers)){
    DealersO <- unique(tolower(Stock_Data()$Dealer))
  }else{
    DealersO <- input$Dealers
  }
  
  Stock_Data() %>%
    filter(
      tolower(Dealer) %in% DealersO
    )
})

Stock_Summary <- reactive({
  Stock_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Worth = sum(Worth,na.rm=T)) %>%
    group_by(Dealer = tolower(Dealer)) %>%
    summarise( Transactions = n(),
               Total_Stock_Price = sum(Worth,na.rm=T),
               Avg_Stock_Price = round(mean(Worth,na.rm=T),2),
               Stock_Percent = sum(Worth,na.rm=T)/mean(Entire_Worth,na.rm=T)
    )
})

Stock_By_Time <- reactive({
  Stock_report() %>%
    mutate(Date = as.Date(Date,"%m/%d/%Y")) %>%
    filter( Date >= input$daterange[1]
            & Date <= input$daterange[2] ) %>%
    mutate(Entire_Worth = sum(Worth,na.rm=T)) %>%
    group_by(Date = floor_date(Date, tolower(input$stocktimeslice))
    ) %>%
    summarise( Transactions = n(),
               Total_Stock_Price = sum(Worth,na.rm=T),
               Avg_Stock_Price = round(mean(Worth,na.rm=T),2)
               # ,Stock_Percent = sum(Worth,na.rm=T)/mean(Entire_Worth,na.rm=T)
    )
})

output$TotStock <- renderText({
  paste("₹",format(round(sum(Stock_By_Time()$Total_Stock_Price,na.rm=T)), nsmall=0, big.mark=","))
})

output$AvgStockMoney <- renderText({
  paste("₹",format(round(mean(Stock_By_Time()$Total_Stock_Price,na.rm=T)), nsmall=0, big.mark=","))
})

output$Stock_Plot_Time <- renderHighchart({
  pdf(NULL)
  g <-  highchart() %>% 
    hc_yAxis(labels = list(format = "{value:,.0f}")) %>%
    hc_add_series_times_values(Stock_By_Time()$Date, Stock_By_Time()$Total_Stock_Price
                               ,name = "Sale Money"
                               ,colorByPoint=TRUE
                               ,type = "column"
                               ,showInLegend = FALSE) %>%
    hc_chart(options3d = list(enabled = TRUE,beta=15,alpha=5))
  dev.off()
  g
})

output$Stock_distribution <- renderHighchart({
  Stocks_Pie <- Stock_Summary() %>% mutate(Stock_Percent = round(Stock_Percent *100,2))
  pdf(NULL)
  g <-  highchart() %>%
    hc_add_series_labels_values(Stocks_Pie$Dealer, Stocks_Pie$Stock_Percent
                                , type = "pie"
                                , name = "Stock Percent"
                                , colorByPoint = TRUE
                                , size = 275
    )
  dev.off()
  g
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

output$Stock_Time_Table <- renderDataTable({
  datatable(
    Stock_By_Time() %>% arrange(desc(Date))
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
  ) %>% formatCurrency(3:4,currency="₹ ") 
  # %>% formatPercentage(5,digits=2)
})

}