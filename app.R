library(shiny)
library(rinat)
library(ggplot2)
library(dplyr)

# Function to fetch iNaturalist data
get_inat_data <- function(project_slug, max_results = 10000) {
  obs <- get_inat_obs_project(grpid = project_slug, type = "observations")
  
  # Check for empty/nonexistent project
  if (is.null(obs) || nrow(obs) == 0) {
    warning("No observations found for this project.")
    return(NULL)
  }
  
  obs <- head(obs, max_results)
  return(obs)
}

# UI: Takes project slug input and displays a plot
ui <- fluidPage(
  # Application title
  titlePanel("iNaturalist Observations Per Day"),
  
  sidebarLayout(
    # Sidebar with text input, slider, and update button
    sidebarPanel(
      # Input for project slug
      textInput("project_slug", "Enter Project Slug:", value = "crows-in-vermont"),
      
      # Slider for max_results
      sliderInput("max_results", 
                  "Max Results:", 
                  min = 100, 
                  max = 10000, 
                  value = 1000, 
                  step = 100),
      
      actionButton("update", "Fetch Data")
    ),
    
    # Data plot
    mainPanel(
      plotOutput("obs_per_day_plot")
    )
  )
)

# Server: Fetch and plot data
server <- function(input, output, session) {
  # Reactive value (adjusts when updated)
  observations <- reactiveVal(NULL)
  
  # When user clicks update
  observeEvent(input$update, {
    req(input$project_slug)
    
    # Fetch data
    obs_data <- get_inat_data(input$project_slug, max_results = input$max_results)
    
    # If valid data, store it
    if (!is.null(obs_data)) {
      # Convert observed_on (normalized dates) to Date type
      obs_data$observed_on <- as.Date(obs_data$observed_on)
      observations(obs_data)
    }
  })
  
  # Plot observation dates
  output$obs_per_day_plot <- renderPlot({
    # Only plot if not null
    req(observations())
    
    # Count observations per day + arrange in ascending order
    obs_per_day <- observations() %>%
      count(observed_on) %>%
      arrange(observed_on)
    
    # Plot observations per day
    ggplot(data=obs_per_day, aes(x = observed_on, y = n)) +
      geom_line(color = "steelblue") +
      geom_point(color = "red", linewidth = 2) +
      labs(title = "Number of Observations Per Day",
           x = "Date",
           y = "Observations") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)