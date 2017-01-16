library(shinydashboard)
library(shiny)
library(DT)
library(dygraphs)
library(ggplot2)
library(plotly)
library(highcharter)

ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard")
  ,skin = 'yellow'
  ,dashboardSidebar(
    sidebarMenu(
       menuItem("Portfolio", tabName = "Portfolio", icon = icon("dashboard"))
       ,menuItem("Sale Analysis", tabName = "SaleAnalysis", icon = icon("line-chart"))
       # ,menuItem("Stocks & Payments", tabName = "Stocks_Payments", icon = icon("inr"))
       ,menuItem("Payments", tabName = "payments", icon = icon("briefcase"))
       ,menuItem("Expense Analysis", tabName = "ExpenseAnalysis", icon = icon("pie-chart"))
       ,menuItem("Stocks", tabName = "stocks", icon = icon("cubes"))
    )
    ,dateRangeInput("daterange", "Date Range:", start = '2016-09-15', min = '2016-09-15')
  )
  ,dashboardBody(
    tabItems(
       tabItem(
        tabName = "Portfolio"
        ,fluidRow(
          valueBox(
            uiOutput("OverallSales")
            , "Overall Sales"
            , color = "olive"
            , width = 3
          )
          ,valueBox(
            uiOutput("OverallProfit")
            , "Overall Profit"
            , color = "green"
            , width = 3
          )
          ,valueBox(
            uiOutput("OverallExpense")
            , "Overall Expense"
            , color = "maroon"
            , width = 3
          )
          ,valueBox(
            uiOutput("OverallPayment")
            , "Overall Payment"
            , color = "orange"
            , width = 3
          )
        )
        ,fluidRow(
          valueBox(
            uiOutput("AverageSales")
            , "Average Sales"
            , color = "light-blue"
            , width = 3
          )
          ,valueBox(
            uiOutput("AverageProfit")
            , "Average Profit"
            , color = "blue"
            , width = 3
          )
          ,valueBox(
            uiOutput("AverageExpense")
            , "Average Expense"
            , color = "red"
            , width = 3
          )
          ,valueBox(
            uiOutput("AveragePayment")
            , "Average Payment"
            , color = "yellow"
            , width = 3
          )
        )
        ,fluidRow(
           valueBox(
            uiOutput("MyInvestment")
            , "Money Invested"
            , color = "aqua"
            , width = 3
          )
          ,valueBox(
            uiOutput("ProfitPercent")
            , "Profit Percent"
            , color = "green"
            , width = 3
          )
          ,valueBox(
            uiOutput("ExpensePercent")
            , "Expense Percent"
            , color = "red"
            , width = 3
          )
          ,valueBox(
            uiOutput("StockWorth")
            , "Stock Transacted Worth"
            , color = "purple"
            , width = 3
          )
        )
      ,fluidRow(
        box(
          title = "Trends Over Time"
           ,fluidRow(
             box(div(style="display:block;width: 100%;float: left;margin: 0 2px;"
                     ,selectInput('timeslice', 'Time View'
                                  , c('Day', 'Week', 'Month', 'Quarter', 'Year')
                                  , selected = 'Month'))
                 ,status = "primary"
                 ,width = 2)
           )
          ,fluidRow(
            tabBox(
              tabPanel("Overall Trends"

                         , dygraphOutput("Overall_Trends_Plot")
                         , width = 12
                         , status = "primary"

              )
              ,tabPanel("Average Trends"
                          , dygraphOutput("Average_Trends_Plot")
                          , width = 12
                          , status = "primary"
              )
              ,tabPanel("Percentage Trends"
                        , dygraphOutput("Percent_Trends_Plot")
                        , width = 12
                        , status = "primary"
              )
              ,width = 12
            )
          )
          ,dataTableOutput('SummaryTable')
          ,width=12
          ,status = "primary"
          ,collapsible = TRUE
         )
        )
      )
      ,tabItem(
        tabName = "SaleAnalysis"
        ,fluidRow(
          box(
          uiOutput('ItemList')
          ,status = "success"
          ,collapsible = TRUE
          ,width = 2
          )
        )
          ,fluidRow(
            tabBox(
              tabPanel("Product View"
                       ,fluidRow(
                         box(
                           title = "Sales By Product"
                           , collapsible = TRUE
                           , dataTableOutput("SalesTable")
                           , width = 6
                           , status = 'success'
                         )
                         ,box(
                           title = "Sale Distribution"
                           , collapsible = TRUE
                           , highchartOutput("Sale_distribution")
                           , width = 6
                           , status = 'success'
                         )
                       )
              )
               ,tabPanel("Time View"
                        ,fluidRow(
                          box(div(style="display:block;width: 100%;float: left;margin: 0 2px;"
                                  ,selectInput('saletimeslice', 'Time View'
                                               , c('Day', 'Week', 'Month', 'Quarter', 'Year')
                                               , selected = 'Week'))
                              ,status = "success"
                              ,width = 2)
                          ,valueBox(
                            uiOutput("SaleMoney")
                            , "Sale Value"
                            , color = "green"
                            , width = 3
                          )
                        )
                        ,fluidRow(
                        box(
                          title = "Sale Data"
                          , collapsible = TRUE
                          , dataTableOutput("Sales_Time_Table")
                          , width = 6
                          , status = 'success'
                        )
                        ,box(
                          title = "Sale Plot"
                          , collapsible = TRUE
                          , highchartOutput("Sale_Plot_Time")
                          , width = 6
                          , status = 'success'
                        )
                        )
                     
                        , width = 12
                        , status = "success"
              )
              ,width = 12
            )
        )
      )
      ,tabItem(
        tabName = "payments"
        ,fluidRow(
          box(
            uiOutput('Payment_Dealers_List')
            ,status = "warning"
            ,collapsible = TRUE
            ,width = 2
          )
        )
        ,fluidRow(
          tabBox(
            tabPanel("Overall View"
                     ,fluidRow(
                       box(
                         title = "Payments By Dealer"
                         , collapsible = TRUE
                         , dataTableOutput("PaymentTable")
                         , width = 6
                         , status = 'warning'
                       )
                       ,box(
                         title = "Payment Distribution"
                         , collapsible = TRUE
                         , highchartOutput("Payment_distribution")
                         , width = 6
                         , status = 'warning'
                       )
                     )
            )
            ,tabPanel("Time View"
                      ,fluidRow(
                        box(div(style="display:block;width: 100%;float: left;margin: 0 2px;"
                                ,selectInput('paymenttimeslice', 'Time View'
                                             , c('Day', 'Week', 'Month', 'Quarter', 'Year')
                                             , selected = 'Week'))
                            ,status = "warning"
                            ,width = 2)
                        ,valueBox(
                          uiOutput("TotPayment")
                          , "Total Payment"
                          , color = "orange"
                          , width = 3
                        )
                      )
                      ,fluidRow(
                        box(
                          title = "Payment Data"
                          , collapsible = TRUE
                          , dataTableOutput("Payment_Time_Table")
                          , width = 6
                          , status = 'warning'
                        )
                        ,box(
                          title = "Payment Plot"
                          , collapsible = TRUE
                          , highchartOutput("Payment_Plot_Time")
                          , width = 6
                          , status = 'warning'
                        )
                      )
                      , width = 12
                      , status = "warning"
            )
            ,width = 12
          )
        )
      )
      ,tabItem(
        tabName = "ExpenseAnalysis"
        ,fluidRow(
          box(
            uiOutput('Expense_SourceList')
            ,status = "danger"
            ,collapsible = TRUE
            ,width = 2
          )
        )
        ,fluidRow(
          tabBox(
            tabPanel("Overall View"
                     ,fluidRow(
                       box(
                         title = "Expenses By Type"
                         , collapsible = TRUE
                         , dataTableOutput("ExpensesTable")
                         , width = 6
                         , status = 'danger'
                       )
                       ,box(
                         title = "Expense Distribution"
                         , collapsible = TRUE
                         , highchartOutput("Expense_distribution")
                         , width = 6
                         , status = 'danger'
                       )
                     )
            )
             ,tabPanel("Time View"
                      ,fluidRow(
                        box(div(style="display:block;width: 100%;float: left;margin: 0 2px;"
                                ,selectInput('expensetimeslice', 'Time View'
                                             , c('Day', 'Week', 'Month', 'Quarter', 'Year')
                                             , selected = 'Week'))
                            ,status = "danger"
                            ,width = 2)
                        ,valueBox(
                          uiOutput("TotExpense")
                          , "Total Expense"
                          , color = "red"
                          , width = 3
                        )
                      )
                      ,fluidRow(
                        box(
                          title = "Expense Data"
                          , collapsible = TRUE
                          , dataTableOutput("Expenses_Time_Table")
                          , width = 6
                          , status = 'danger'
                        )
                        ,box(
                          title = "Expense Plot"
                          , collapsible = TRUE
                          , highchartOutput("Expense_Plot_Time")
                          , width = 6
                          , status = 'danger'
                        )
                      )
                      , width = 12
                      , status = "danger"
            )
            ,width = 12
          )
        )
      )
      ,tabItem(
        tabName = "stocks"
        ,fluidRow(
          box(
            uiOutput('DealerList')
            ,status = "primary"
            ,collapsible = TRUE
            ,width = 2
          )
        )
        ,fluidRow(
          tabBox(
            tabPanel("Overall View"
                     ,fluidRow(
                       box(
                         title = "Stocks By Dealer"
                         , collapsible = TRUE
                         , dataTableOutput("StockTable")
                         , width = 6
                         , status = 'primary'
                       )
                       ,box(
                         title = "Stock Distribution"
                         , collapsible = TRUE
                         , highchartOutput("Stock_distribution")
                         , width = 6
                         , status = 'primary'
                       )
                     )
            )
            ,tabPanel("Time View"
                      ,fluidRow(
                        box(div(style="display:block;width: 100%;float: left;margin: 0 2px;"
                                ,selectInput('stocktimeslice', 'Time View'
                                             , c('Day', 'Week', 'Month', 'Quarter', 'Year')
                                             , selected = 'Week'))
                            ,status = "primary"
                            ,width = 2)
                        ,valueBox(
                          uiOutput("TotStock")
                          , "Total Stock Worth"
                          , color = "blue"
                          , width = 3
                        )
                      )
                      ,fluidRow(
                        box(
                          title = "Stock Data"
                          , collapsible = TRUE
                          , dataTableOutput("Stock_Time_Table")
                          , width = 6
                          , status = 'primary'
                        )
                        ,box(
                          title = "Stock Plot"
                          , collapsible = TRUE
                          , highchartOutput("Stock_Plot_Time")
                          , width = 6
                          , status = 'primary'
                        )
                      )
                      , width = 12
                      , status = "primary"
            )
            ,width = 12
          )
        )
      )
      
    )
  )
)