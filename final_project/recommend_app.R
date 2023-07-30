library(tidyverse)
library(shiny)
library(shinydashboard)


#### Data
netflix <- read.csv("netflix_titles.csv")



ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)


server <- function(input, output) {
  
}

shinyApp(ui, server)
