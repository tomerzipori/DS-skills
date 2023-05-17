library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  # Application title
  titlePanel("Poly reg"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("text", "Title"),
      sliderInput("deg",
                  "Poly degree",
                  min = 1,
                  max = 10,
                  value = 1),
      sliderInput("xrange",
                  "HP range", 
                  min = 50,
                  max = 350,
                  value = c(50, 350))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("title"),
      plotOutput("xydots", click = "click"),
      actionButton("render", "Submit!!")
    )
  )
)

server <- function(input, output) {
  r <- reactiveValues(col = colors()[1], # white
                      col2 = "green") 
  
  observe({
    r$col <- sample(colors(), 1)
  }) |> bindEvent(input$text, ignoreInit = TRUE)
  
  observe({
    r$col2 <- sample(colors(), 1)
  }) |> bindEvent(input$click)
  
  output$xydots <- renderPlot({
    ggplot(mtcars, aes(hp, mpg)) + 
      geom_smooth(method = "lm", se = FALSE, 
                  formula = substitute(y ~ poly(x, i), list(i = input$deg)),
                  color = r$col2) + 
      geom_point(fill = r$col, shape = 21, size = 3) + 
      theme_bw() + 
      coord_cartesian(ylim = c(10, 35),
                      xlim = input$xrange)
  }) |> 
    bindEvent(input$render, ignoreNULL = FALSE)
  
  output$title <- renderText({
    glue::glue("{title} ... and random die: {die}",
               title = input$text,
               die = sample(1:6, 1))
  })
}


shinyApp(ui = ui, server = server)

# 1. Make the color (r$col) change every time someone changes the title!
# 2. Make the color change every time someone clicks ON the plot!
# 3. Make the plot re-render only when you press an action button.
