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
library(tidyr)

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
      sliderInput("month_range", "Observation months:",
                  min = 1, max = 12, value = c(1, 12), step = 1),
      sliderInput("min_obs", "Minimum observations (species must have at least):",
                  min = 1, max = max(df$sum_sp, na.rm = TRUE), value = 10, step = 1),
      numericInput("top_n", "Show top N species:", value = 15, min = 5, max = 50, step = 1),
      actionButton("apply", "Apply")
    ),
    mainPanel(
      DT::dataTableOutput("crosstab1yr"), # output for observation by year table
      tags$hr(),
      plotOutput("bar", height = 450) # output for top species bar chart
      
      
    )
  )
)

server <- function(input, output, session){
  
 
  filtered <- eventReactive(input$apply, {
    # month filter
    d <- df |>
      filter(between(observed_month, input$month_range[1], input$month_range[2]))
    
    if (input$county == "_ALL_") {
      # recalculate species counts after month filter
      d <- d |>
        count(common_name, name = "count") |>
        filter(count >= input$min_obs) |>
        arrange(desc(count)) |>
        slice_head(n = input$top_n) |>
        mutate(label = common_name)
    } else {
      # restrict to county, then recalculate species counts
      d <- d |>
        filter(NAME == input$county) |>
        count(common_name, name = "count") |>
        filter(count >= input$min_obs) |>
        arrange(desc(count)) |>
        slice_head(n = input$top_n) |>
        mutate(label = common_name)
    }
    d
  }, ignoreInit = TRUE)
  
  ###########################################
  # output top species bar chart
  output$bar <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, "No species meet the current filters."))
    ggplot(d, aes(x = reorder(label, count), y = count)) +
      geom_col() +
      coord_flip() +
      labs(
        title = if (input$county == "_ALL_")
          "Top Species in Western NC, 2020 - 2025"
        else
          paste("Top Species in", input$county,"County, 2020 - 2025"),
        x = "Species (common name)", y = "Number of observations"
      ) +
      theme_minimal(base_size = 12)
  })
  
  ###########################################
  # REACTIVE: 1-way contingency table for observations by year
  crosstab1yr_data <- eventReactive(input$apply, {
    d <- df |>
      filter(between(observed_month, input$month_range[1], input$month_range[2]))
    
    if (input$county != "_ALL_") {
      d <- d |> filter(NAME == input$county)
    }
    
    d |>
      count(observed_year, name = "n_obs") |>
      arrange(observed_year) |>
      mutate(Year = "Total Observations") |>
      pivot_wider(
        names_from = observed_year, 
        values_from = n_obs, 
        values_fill = 0)
  }, ignoreInit = TRUE)
  
  # RENDER: crosstab1yr
  output$crosstab1yr <- DT::renderDataTable({
    ct <- crosstab1yr_data()
    validate(need(nrow(ct) > 0, "No observations for this selection."))
    DT::datatable(
      ct,
      rownames = FALSE,
      options = list(
        ordering = FALSE,
        searching = FALSE, # Remove column filter boxes
        paging = FALSE,    # Turn off pagination entirely
        info = FALSE 
      ),
    )
  })
  
  ########################################
}

shinyApp(ui, server)