# Title: Shiny APP - Exploring iNaturalst Observations in WNC
# Date:November 2025
# Author: Elvira McIntyre
#########################################

# Load packages

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)

# load data
df <- read_csv("data/inat_summary.csv", show_col_types = FALSE)
                 
# county choices and All
county_levels <- sort(unique(df$NAME))
county_choices <- c("All WNC" = "_ALL_", county_levels)

ui <- fluidPage(
  titlePanel("Top Species by Observations"),
  sidebarLayout(
    sidebarPanel(
      selectInput("county", "County:", choices = county_choices, selected = "_ALL_"),
      sliderInput("min_obs", "Minimum observations (species must have at least):",
                  min = 1, max = max(df$sum_sp, na.rm = TRUE), value = 10, step = 1),
      numericInput("top_n", "Show top N species:", value = 15, min = 5, max = 50, step = 1),
      actionButton("apply", "Apply")
    ),
    mainPanel(
      plotOutput("bar", height = 450)
    )
  )
)

server <- function(input, output, session){
  
  filtered <- eventReactive(input$apply, {
    d <- df
    # pick which count column to use
    if (input$county == "_ALL_") {
      d <- d %>%
        filter(sum_sp >= input$min_obs) %>%
        select(common_name, sum_sp) %>%
        distinct()
      d <- d %>%
        arrange(desc(sum_sp)) %>%
        slice_head(n = input$top_n)
      d$label <- d$common_name
      d$count <- d$sum_sp
    } else {
      d <- d %>%
        filter(NAME == input$county, sum_spcty >= input$min_obs) %>%
        select(common_name, sum_spcty) %>%
        distinct()
      d <- d %>%
        arrange(desc(sum_spcty)) %>%
        slice_head(n = input$top_n)
      d$label <- d$common_name
      d$count <- d$sum_spcty
    }
    d
  }, ignoreInit = TRUE)
  
  output$bar <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, "No species meet the current filters."))
    ggplot(d, aes(x = reorder(label, count), y = count)) +
      geom_col() +
      coord_flip() +
      labs(
        title = if (input$county == "_ALL_")
          "Top Species in Western NC"
        else
          paste("Top Species in", input$county,"County"),
        x = "Species (common name)", y = "Number of observations"
      ) +
      theme_minimal(base_size = 12)
  })
}

shinyApp(ui, server)