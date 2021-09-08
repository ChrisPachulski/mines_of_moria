library(shiny)
library(shinydashboard)
shinyUI(
  dashboardPage(
    dashboardHeader(title = "This is the header",
                    dropdownMenu(type = "message",
                      messageItem(from = "Finance Update", message ="We are on threshold")
                    
                      )
                      ),
    
    dashboardSidebar(
      sliderInput("bins","Number of Breaks",1,100,50),
      sidebarMenu(
      menuItem("Menu Item 1 (Buylist)",tabName = "dashboard", icon = icon("coins")),
        menuSubItem("Dashboard FInance", tabName = "finance"),
        menuSubItem("Dashboard Sales", tabName = "Sales"),
      menuItem("Menu Item 2 (CK Market)"),
      menuItem("Menu Item 3 (TCG Market)"),
      menuItem("Menu Item 4 (TCG Vendors)"),
      menuItem("Menu Item 5 (TCG Best Sellers)"),
      menuItem("Menu Item 6 (CK Best Sellers)")
    )),
    dashboardBody(
      tabItems(
        tabItem(tabName ="dashboard",
                fluidRow(
                  box(plotOutput("histogram"))
                  )),
        tabItem(tabName = "finance",
                 h1("Oh My....")
                ),
        tabItem(tabName = "Sales",
                h2("Sales tab"))
      )
    )
  )
)
