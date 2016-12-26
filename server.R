library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(highcharter)
library(googlesheets)
library(xts)
library(dygraphs)

# shopData <- gs_title("Shop_Sales") %>%
#   gs_read()
# 
# saveRDS(shopData,"~/Documents/ShopSales.rds")

Full_shopData <- reactiveFileReader(60000, session = NULL
                           , filePath = '~/Documents/ShopSales.rds', readRDS)
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
    ,extensions = c("Buttons")
    , options = list(
      dom = 'Bfrtip'
      , buttons = c('colvis', 'csv', 'excel', 'pdf')
      , lengthMenu = list(c(10,-1), c('10','All'))
      , pageLength = -1
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

}