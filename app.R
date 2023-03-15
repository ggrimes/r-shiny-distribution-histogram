ui <- fluidPage(
  titlePanel("Distribution Histogram"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("distribution", "Distribution:",
                  choices = c("Binomial", "Poisson", "Normal")),
      sliderInput("size", "Sample size:", min = 1, max = 1000, value = 100),
      
      conditionalPanel(
        condition = "input.distribution == 'Binomial'",
        sliderInput("prob", "Probability of success:", min = 0, max = 1, step = 0.01, value = 0.5),
        sliderInput("trials", "Number of trials:", min = 1, max = 1000, value = 100)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'Poisson'",
        sliderInput("lambda", "Lambda:", min = 0, max = 10, step = 0.1, value = 1)
      ),
      
      conditionalPanel(
        condition = "input.distribution == 'Normal'",
        sliderInput("mean", "Mean:", min = -10, max = 10, step = 0.1, value = 0),
        sliderInput("sd", "Standard deviation:", min = 0.1, max = 10, step = 0.1, value = 1)
      ),
      
      checkboxInput("log_transform", "Log-transform counts", FALSE),
      checkboxInput("zero_inflated", "Zero-inflated distribution", FALSE)
    ),
    
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

server <- function(input, output) {
  output$histPlot <- renderPlot({
    if (input$zero_inflated) {
      zero_inflated <- function(sample_data) {
        zero_inflated_sample <- ifelse(runif(length(sample_data)) < 0.5, 0, sample_data)
        return(zero_inflated_sample)
      }
    } else {
      zero_inflated <- function(sample_data) {
        return(sample_data)
      }
    }
    
    if (input$distribution == "Binomial") {
      data <- rbinom(input$size, input$trials, input$prob)
    } else if (input$distribution == "Poisson") {
      data <- rpois(input$size, input$lambda)
    } else {
      data <- rnorm(input$size, input$mean, input$sd)
    }
    
    data <- zero_inflated(data)
    
    if (input$log_transform) {
      data <- log(data + 1)
    }
    
    hist(data, main = paste(input$distribution, "Distribution Histogram"), xlab = "Value", ylab = "Frequency", col = "lightblue", border = "black")
  })
}

shinyApp(ui = ui, server = server)
