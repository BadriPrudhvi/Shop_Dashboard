library(shinydashboard)
library(shiny)
library(plotly)
library(DT)
library(dygraphs)

ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard")
  ,skin = 'yellow'
  ,dashboardSidebar(
    sidebarMenu(
       menuItem("Portfolio", tabName = "Portfolio", icon = icon("dashboard"))
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
    )
  )
)