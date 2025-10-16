library(shiny)
library(rvest)
library(tidyverse)
library(polite)
library(dplyr)
library(stringr)
library(ggplot2)
library(dplyr)
library(plotly)
library(rsconnect)
library(shinylive)
library(httpuv)

# URL for the road bike reviews page
base_url <- "https://www.bicyclerollingresistance.com/road-bike-reviews"
start_url <- "https://www.bicyclerollingresistance.com/road-bike-reviews"

extract_tire_info <- purrr::possibly(
  function(tire) {
    tibble(
      brand = tire %>%
        html_element(".brand") %>%
        html_text(trim = TRUE) %>%
        # Ensure that even if a brand is missing, the column exists
        replace_na(""),
      model = tire %>%
        html_element(".model") %>%
        html_text(trim = TRUE) %>%
        replace_na(""),
      rolling_resistance_watts = tire %>%
        html_element(".rrM") %>%
        html_text(trim = TRUE) %>%
        replace_na(""),
      width_claimed = tire %>%
        html_element(".widths") %>%
        html_text(trim = TRUE) %>%
        replace_na(""),
      weight_claimed = tire %>%
        html_element(".sweight") %>%
        html_text(trim = TRUE) %>%
        replace_na(""),
      wet_grip = tire %>%
        html_element(".wga") %>%
        html_text(trim = TRUE) %>%
        replace_na(""),
      puncture_score = tire %>%
        html_element(".pr") %>%
        html_text(trim = TRUE) %>%
        replace_na(""),
      thickness = tire %>%
        html_element(".thick") %>%
        html_text(trim = TRUE) %>%
        replace_na(""),
      knob_center = tire %>%
        html_element(".knob") %>%
        html_text(trim = TRUE) %>%
        replace_na(""),
      knob_edge = tire %>%
        html_element(".knobedge") %>%
        html_text(trim = TRUE) %>%
        replace_na("")
      
    )
  },
  otherwise = NULL
)

session <- polite::bow(start_url)
page_html <- polite::scrape(session)

message(paste("Scraping page:", start_url))

# Extract 
tire_elements <- page_html %>%
  html_elements("tr") %>%
  head(180) # indicate the number of tires

# Check if there are any tire elements
if (length(tire_elements) == 0) {
  stop("No tire elements found on the page.")
}

# Use purrr::map to safely parse the data from the limited list of elements
tire_data_list <- purrr::map(tire_elements, extract_tire_info)

# Remove any NULL entries from the list before binding rows
tire_data_list <- purrr::compact(tire_data_list)

# Combine the list of tibbles into a single data frame
data_raw <- bind_rows(tire_data_list)

# Convert the character column to a numeric data type
data_clean <- data_raw %>%
  # Use mutate() to modify the existing column
  mutate(
    # Simply convert the character string to a numeric data type
    rolling_resistance_watts = as.numeric(rolling_resistance_watts),
    width_claimed = as.numeric(width_claimed),
    weight_claimed = as.numeric(weight_claimed),
    wet_grip = as.numeric(wet_grip),
    puncture_score = as.numeric(puncture_score),
    thickness = as.numeric(thickness),
    knob_center = as.numeric(knob_center),
    knob_edge = as.numeric(knob_edge)
  )

# View the cleaned data
message("\nCleaned and converted data:")


# Get a list of the numeric columns available for plotting
plot_vars <- data_clean %>%
  select(where(is.numeric)) %>%
  names()

# --- User Interface (UI) ---
ui <- fluidPage(
  titlePanel("bicyclerollingresistance.com Tire Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X-Axis Variable:",
                  choices = plot_vars, selected = "rolling_resistance_watts"),
      selectInput("y_var", "Y-Axis Variable:",
                  choices = plot_vars, selected = "puncture_score"),
      hr(),
      h4("Correlation (r-value):"),
      textOutput("correlation_output")
    ),
    mainPanel(
      plotlyOutput("interactive_plot", width = "100%", height = "600px")
    )
  )
)

# --- Server Logic ---
server <- function(input, output) {

  correlation_value <- reactive({
    req(input$x_var, input$y_var)
    x_data <- data_clean[[input$x_var]]
    y_data <- data_clean[[input$y_var]]
    clean_data <- na.omit(data.frame(x = x_data, y = y_data))
    cor_val <- cor(clean_data$x, clean_data$y, use = "complete.obs")
    round(cor_val, 2)
  })

  output$interactive_plot <- renderPlotly({
    req(input$x_var, input$y_var)

    p <- ggplot(data_clean, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point(aes(color = brand,
                     text = paste("Brand:", brand, "<br>Model:", model))) +
      labs(title = paste(input$y_var, "vs.", input$x_var),
           x = input$x_var,
           y = input$y_var) +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })

  output$correlation_output <- renderText({
    paste("r =", correlation_value())
  })
}

# --- Run the application ---
shinyApp(ui = ui, server = server)