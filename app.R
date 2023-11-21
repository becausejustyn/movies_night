# Install and load necessary packages if not already installed
if (!require(shiny)) install.packages("shiny", dependencies = TRUE)
library(dplyr)
library(shinythemes)

# Define UI
ui <- shiny::fluidPage(
  shiny::titlePanel("Movie Order App"),
  # Use tabsetPanel to create tabs
  shiny::tabsetPanel(
    # First tab
    shiny::tabPanel(
      "Movie Selection",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::numericInput("seed", "Enter Seed:", value = 1, min = 1, max = 100000),
          p("Seed is to specify the RNG."),
          shiny::textInput("item1", "Name 1:", ""),
          shiny::textInput("item2", "Name 2:", ""),
          shiny::textInput("item3", "Name 3:", ""),
          p("Name of the people selecting movies."),
          shiny::numericInput("num_samples", "Number of Samples:", value = 1000, min = 1),
          shiny::actionButton("sampleBtn", "Sample")
        ),
        shiny::mainPanel(
          h4("Order of Selector"),
          shiny::tableOutput("resultTable")
        )
      )
    ),
    # Second tab
    shiny::tabPanel(
      "Movie Selector",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::checkboxInput("equallyLikelyCheckbox", "Equally Likely Probabilities", FALSE),
          shiny::numericInput("movie_seed", "Enter Movie Seed:", value = 1, min = 1, max = 100000),
          shiny::numericInput("movie_n_samples", "Number of Movie Samples:", value = 1, min = 1),
          shiny::textInput("movie1", "Movie 1:", ""),
          shiny::numericInput("prob1", "Probability 1:", value = .5, min = 0, max = 1, step = 0.05),
          shiny::textInput("movie2", "Movie 2:", ""),
          shiny::numericInput("prob2", "Probability 2:", value = .25, min = 0, max = 1, step = 0.05),
          shiny::textInput("movie3", "Movie 3:", ""),
          shiny::numericInput("prob3", "Probability 3:", value = .25, min = 0, max = 1, step = 0.05),
          shiny::actionButton("movieBtn", "Select Movie")
        ),
        shiny::mainPanel(
          h4("Selected Movie:"),
          shiny::tableOutput("selectedMovieMovieTab"),
          shiny::tableOutput("selectedMovie")
        )
      )
    ),
    shiny::tabPanel( # here
      "Additional Info",
      shiny::verbatimTextOutput("additionalInfo")
    )
  ), theme = shinythemes::shinytheme("yeti")
)

# Define server logic
server <- function(input, output) {
  shiny::observeEvent(input$sampleBtn, {
    # Set seed for reproducibility
    base::set.seed(input$seed)
    
    # Store items in a vector
    items <- c(input$item1, input$item2, input$item3)
    
    # Sample items based on the specified number of samples
    samples <- base::sample(items, size = input$num_samples, replace = TRUE) |>
      base::as.data.frame() 
    
    # Rename column
    base::colnames(samples) <- "Count"
    
    sample_count <- samples |>
      dplyr::count(Count) |>
      dplyr::arrange(desc(n)) |>
      dplyr::mutate(Order = dplyr::row_number())
    
    output$resultTable <- shiny::renderTable(sample_count)
    output$selectedMovie <- shiny::renderTable(NULL)  # Clear the selected movie table in the first tab
  })
  
  observeEvent(input$movieBtn, {
    # Set seed for reproducibility
    base::set.seed(input$movie_seed)
    
    # Store movies and probabilities in a data frame
    movies <- base::data.frame(
      movie = c(input$movie1, input$movie2, input$movie3),
      prob = c(input$prob1, input$prob2, input$prob3)
    )
    
    if (input$equallyLikelyCheckbox) {
      selected_movie <- base::sample(movies$movie, input$movie_n_samples, replace = TRUE)
    } else {
      selected_movie <- base::sample(movies$movie, input$movie_n_samples, replace = TRUE, prob = movies$prob)
    }
    
    movie_count <- selected_movie |>
      base::as.data.frame() 
    
    base::colnames(movie_count) <- "Count"
    
    movie_count <- movie_count |>
      dplyr::count(Count) |>
      dplyr::arrange(desc(n)) |>
      dplyr::mutate(Order = dplyr::row_number())
    
    # Display selected movie
    output$selectedMovieMovieTab <- shiny::renderTable(movie_count)
    output$resultTable <- shiny::renderTable(NULL)  # Clear the result table in the first tab
  })
  
  # Render additional text in the third tab
  output$additionalInfo <- shiny::renderText({
    "This app is simply a RNG helper for selecting movies. It is not meant to help you solve your indecisiveness in life."
  })

}

# Run the application
shiny::shinyApp(ui, server)
