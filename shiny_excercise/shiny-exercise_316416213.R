
# Setup -------------------------------------------------------------------

library(dplyr)
library(shiny)
library(shinydashboard) # make powerfull dashboards 
# https://rstudio.github.io/shinydashboard/index.html





# Data --------------------------------------------------------------------

data <- readxl::read_xlsx("C:/Users/tomer/OneDrive/מסמכים/GitHub/DS-skills/shiny_excercise/file_a3664f94-0441-4e67-bc94-d4ada374a1db.xlsx", 
                          range = "A19:I39") |> 
  rename(Year = 1) |> 
  mutate(across(.fns = as.numeric)) |> 
  tidyr::pivot_longer(-Year, names_to = "Type", values_to = "Tons") |>
  mutate(Type = factor(stringr::str_remove_all(data$Type, "\r")))







# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Recycling In Israel"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Point Plot", tabName = "itm_plot_point", icon = icon("chart-line")),
      menuItem("Line Plot", tabName = "itm_plot_line", icon = icon("chart-line")),
      menuItem("Table", tabName = "itm_tab", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    # Inputs
    selectInput("gar_type", "Select type to highlight",
                choices = unique(data$Type)),
    sliderInput("yr_range",
                "Year Range",
                min = min(data$Year),
                max = max(data$Year),
                value = range(data$Year)),
    actionButton("render", "Change Colors"),
    
    #h3("Output"),
    
    # Tabbed outputs 
    tabItems(
      tabItem("itm_plot_point",
              fluidRow(
                plotOutput("plot_gar_point")
              )),
      
      tabItem("itm_plot_line",
              fluidRow(
                plotOutput("plot_gar_line")
              )),
      
      
      tabItem("itm_tab",
              fluidRow(
                tableOutput("tab_summ")
              )) 
    )
  ), skin = "green"
)


# server ------------------------------------------------------------------

# vars = "gar_type", "yr_range"

server <- function(input, output) {
  library(ggplot2)
  
  r <- reactiveValues(point_col = "black")
  
  observe({
    r$point_col <- sample(colors(), 1)
    }
  ) |> bindEvent(input$render, ignoreInit = T)
  
  output$plot_gar_point <- renderPlot({
    data %>%
      filter(Type == input$gar_type & Year >= input$yr_range[1] & Year <= input$yr_range[2]) %>%
      
      ggplot(aes(x = Year, y = Tons / 1000)) +
      geom_point(color = r$point_col) +
      labs(title = as.character(input$gar_type), x = "Year", y = "Amount") +
      scale_x_binned(breaks = seq(input$yr_range[1], input$yr_range[2], 1),
                     labels = seq(input$yr_range[1], input$yr_range[2], 1)) +
      scale_y_continuous(n.breaks = 10) +
      theme_classic() +
      geom_smooth(method = "lm", color = "#9649cb") +
      theme(plot.title = element_text(hjust = 0.5, size = 25),
            panel.background = element_rect(color = "#ECF0F5", fill = "#ECF0F5"),
            plot.background = element_rect(color = "#ECF0F5", fill = "#ECF0F5"))
  })
  
  output$plot_gar_line <- renderPlot({
    data %>%
      filter(Type == input$gar_type & Year >= input$yr_range[1] & Year <= input$yr_range[2]) %>%
      
      ggplot(aes(x = Year, y = Tons / 1000)) +
      geom_line(color = r$point_col) +
      labs(title = as.character(input$gar_type), x = "Year", y = "Amount") +
      scale_x_binned(breaks = seq(input$yr_range[1], input$yr_range[2], 1),
                     labels = seq(input$yr_range[1], input$yr_range[2], 1)) +
      scale_y_continuous(n.breaks = 10) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 25),
            panel.background = element_rect(color = "#ECF0F5", fill = "#ECF0F5"),
            plot.background = element_rect(color = "#ECF0F5", fill = "#ECF0F5"))
  })
  
  
  output$tab_summ <- renderTable({
    data.frame("Year" = scales::number(seq(input$yr_range[1], input$yr_range[2], 1), accuracy = 1, big.mark = ""),
               "Garbage in Tons" = data$Tons[data$Type == input$gar_type
                                            & data$Year %in% seq(input$yr_range[1], input$yr_range[2], 1)] / 1000,
               "Change from last year" = scales::number(data$Tons[data$Type == input$gar_type
                                                   & data$Year %in% seq(input$yr_range[1], input$yr_range[2], 1)] / 1000 -
                 lag(data$Tons[data$Type == input$gar_type & data$Year %in% seq(input$yr_range[1], input$yr_range[2], 1)] / 1000, 1), style_positive = "plus", big.mark = ""),
               check.names = F)
  })
}








# Run ---------------------------------------------------------------------

shinyApp(ui, server)






# Exercise ----------------------------------------------------------------

# 1. Make the outputs:
# 1.1. "plot_gar" should be a plot with time (x) and tons (y). It should react
#     to "gar_type" - highlighting (somehow) the selected type.
# 1.2. "tab_summ" should be a table of summary statistics for each 'type' of
#     waste. It should be reactive to "yr_range" - showing info only for the
#     selected range of years.
# 
# 2. Add an action-button. Make some reactivity dependent on pressing it.
# 
# 3. Can you think of any other type of output to add?
#
# 4. Play around with the layout.






