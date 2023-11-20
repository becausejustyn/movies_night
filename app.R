# Install and load necessary packages if not already installed
if (!require(shiny)) install.packages("shiny", dependencies = TRUE)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Movie Order App"),
  sidebarLayout(
    sidebarPanel(
      numericInput("seed", "Enter Seed:", value = 1, min = 1, max = 100000),
      p("Seed is to specify the RNG."),
      textInput("item1", "Name 1:", ""),
      textInput("item2", "Name 2:", ""),
      textInput("item3", "Name 3:", ""),
      p("Name of the people selecting movies."),
      numericInput("numSamples", "Number of Samples:", value = 1000, min = 1),
      actionButton("sampleBtn", "Sample"),
      checkboxInput("equallyLikelyCheckbox", "Equally Likely Probabilities", FALSE),
      h4("Movie Selector"),
      numericInput("movieSeed", "Enter Movie Seed:", value = 1, min = 1, max = 100000),
      numericInput("movieNumSamples", "Number of Movie Samples:", value = 1, min = 1),
      textInput("movie1", "Movie 1:", ""),
      numericInput("prob1", "Probability 1:", value = 1, min = 0, max = 1, step = 0.1),
      textInput("movie2", "Movie 2:", ""),
      numericInput("prob2", "Probability 2:", value = 1, min = 0, max = 1, step = 0.1),
      textInput("movie3", "Movie 3:", ""),
      numericInput("prob3", "Probability 3:", value = 1, min = 0, max = 1, step = 0.1),
      actionButton("movieBtn", "Select Movie")
    ),
    mainPanel(
      h4("Order of Selector"),
      tableOutput("resultTable"),
      h4("Selected Movie:"),
      tableOutput("selectedMovie")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$sampleBtn, {
    # Set seed for reproducibility
    set.seed(input$seed)
    
    # Store items in a vector
    items <- c(input$item1, input$item2, input$item3)
    
    # Sample items based on the specified number of samples
    samples <- sample(items, size = input$numSamples, replace = TRUE) |>
      as.data.frame() 
    
    # Rename column
    colnames(samples) <- "Count"
    
    sample_count <- samples |>
      count(Count) |>
      dplyr::arrange(desc(n)) |>
      dplyr::mutate(Order = dplyr::row_number())
    
    output$resultTable <- renderTable(sample_count)
  })
  
  observeEvent(input$movieBtn, {
    # Set seed for reproducibility
    set.seed(input$movieSeed)
    
    # Store movies and probabilities in a data frame
    movies <- data.frame(
      movie = c(input$movie1, input$movie2, input$movie3),
      prob = c(input$prob1, input$prob2, input$prob3)
    )
    
    if (input$equallyLikelyCheckbox) {
      selected_movie <- sample(movies$movie, input$movieNumSamples, replace = TRUE)
    } else {
      selected_movie <- sample(movies$movie, input$movieNumSamples, replace = TRUE, prob = movies$prob)
    }
    
    movie_count <- selected_movie |>
      as.data.frame() 
    
    colnames(movie_count) <- "Count"
    
    movie_count <- movie_count |>
      count(Count) |>
      dplyr::arrange(desc(n)) |>
      dplyr::mutate(Order = dplyr::row_number())
    
    # Display selected movie
    output$selectedMovie <- renderTable(movie_count)
  })
}

# Run the application
shinyApp(ui, server)
