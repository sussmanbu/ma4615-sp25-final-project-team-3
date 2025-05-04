library(shiny)
library(ggplot2)
library(dplyr)
library(maps)
library(scales)

housing_data <- readRDS("housing_cleaned.rds")

housing_data$RACE <- factor(housing_data$RACE,
                            levels = 1:9,
                            labels = c(
                              "White",
                              "Black/African American",
                              "American Indian or Alaska Native",
                              "Chinese",
                              "Japanese",
                              "Other Asian or Pacific Islander",
                              "Other race, nec",
                              "Two major races",
                              "Three or more major races"
                            )
)

state_avgs <- housing_data %>%
  filter(VALUEH > 0) %>%
  group_by(STATEICP, RACE) %>%
  summarise(avg_value = mean(VALUEH, na.rm = TRUE), .groups = "drop")

states_map <- map_data("state") %>%
  mutate(state_name = region)

state_name_map <- tibble::tibble(
  STATEICP = c(
    1, 2, 3, 4, 5, 6, 11, 12, 13, 14,
    21, 22, 23, 24, 25, 31, 32, 33, 34, 35,
    36, 37, 40, 41, 42, 43, 44, 45, 46, 47,
    48, 49, 51, 52, 53, 54, 56, 61, 62, 63,
    64, 65, 66, 67, 68, 71, 72, 73, 81, 82
  ),
  state_name = tolower(c(
    "connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont",
    "delaware", "new jersey", "new york", "pennsylvania", "illinois", "indiana", "michigan",
    "ohio", "wisconsin", "iowa", "kansas", "minnesota", "missouri", "nebraska",
    "north dakota", "south dakota", "virginia", "alabama", "arkansas", "florida",
    "georgia", "louisiana", "mississippi", "north carolina", "south carolina", "texas",
    "kentucky", "maryland", "oklahoma", "tennessee", "west virginia", "arizona",
    "colorado", "idaho", "montana", "nevada", "new mexico", "utah", "wyoming",
    "california", "oregon", "washington", "alaska", "hawaii"
  ))
)

state_avgs <- state_avgs %>%
  left_join(state_name_map, by = "STATEICP") %>%
  filter(!is.na(state_name))

ui <- fluidPage(
  titlePanel("Average Home Value by State and Race (2023)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("race_input", "Select a racial group:",
                  choices = levels(housing_data$RACE),
                  selected = "White")
    ),
    mainPanel(
      plotOutput("choropleth", height = "600px")
    )
  )
)

server <- function(input, output) {
  output$choropleth <- renderPlot({
    df <- state_avgs %>%
      filter(RACE == input$race_input) %>%
      right_join(states_map, by = "state_name")
    
    ggplot(df, aes(long, lat, group = group, fill = avg_value)) +
      geom_polygon(color = "white") +
      coord_fixed(1.3) +
      scale_fill_viridis_c(
        option = "plasma",
        na.value = "grey90",
        name = "Avg Home Value",
        labels = dollar_format(scale = 1)
      ) +
      theme_minimal() +
      labs(
        title = paste("Average Home Value by State —", input$race_input),
        x = "", y = ""
      ) +
      theme(axis.text = element_blank(), axis.ticks = element_blank())
  })
}

shinyApp(ui, server)
