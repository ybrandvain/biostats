library(shiny)
library(munsell)
library(bslib)
library(readr)

# Define UI for app that draws a histogram ----

ui <- fluidPage(
  titlePanel("Bin size can mislead!"),
  
  fluidRow(
    column(4),  # Empty column for spacing
    column(4,
           numericInput("bins", "Number of bins", value = 3,
                        min = 2, max = 200, step = 1,width = "22%")
    ),
    column(4)   # Empty column for spacing
  ),
  
  fluidRow(
    column(12,
      plotOutput("plot", width = "100%", height = "430px")
    )
  )
)

server <- function(input, output, session) {
      salmon <- read.csv('https://raw.githubusercontent.com/ybrandvain/datasets/refs/heads/master/salmon_body_size.csv')
  output$plot <- renderPlot({
    library(ggplot2)

    # Create ggplot histogram
    ggplot(salmon, aes(x = mass_kg)) +
      geom_histogram(bins = as.numeric(input$bins)+2, 
                     fill = "salmon", 
                     color = "black", 
                     alpha = 0.7) +
      labs(title = "Distribution of Body Mass in Salmon",
           x = "Body Mass (kg)",
           y = "Count") +
      scale_x_continuous(limits = c(0.9,3.6))
  }, res = 150)
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)
