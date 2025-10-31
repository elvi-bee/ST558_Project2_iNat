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
library(leaflet)
library(leaflet.extras)

# load data
df <- read_csv("data/inat_summary.csv", show_col_types = FALSE)

# county choices and All
county_levels <- sort(unique(df$NAME))
county_choices <- c("All WNC" = "_ALL_", county_levels)
# species choices
species_choices <- sort(unique(df$common_name))

###############################################################################
#UI
###############################################################################
ui <- fluidPage(
  titlePanel("iNaturalist Observations in Western North Carolina"),
  sidebarLayout(
    sidebarPanel(
      
      
      selectInput("county", "County:", # change to selectizeInput?
                  choices = county_choices, 
                  selected = "_ALL_"),
      sliderInput("month_range", "Observation months:",
                  min = 1, max = 12, value = c(1, 12), step = 1),
      sliderInput("min_obs", "Minimum observations (species must have at least):",
                  min = 1, 
                  max = max(df$sum_sp, na.rm = TRUE), 
                  value = 10, 
                  step = 1),
      numericInput("top_n", "Show top N species:", value = 15, min = 5, max = 50, step = 1),
      actionButton("apply", "Apply"),
      
      selectizeInput(
        "species", "Species:",
        choices = c("", sort(unique(df$common_name))),  # alphabetized + blank default
        options = list(placeholder = "Start typing a bird name..."),
        multiple = FALSE
      )
      
    ), # sidebar panel
    ############################################################################
    ############################################################################
    mainPanel(
      DT::dataTableOutput("crosstab1yr"),
      DT::dataTableOutput("year_species_stats"),
      plotOutput("year_species_box", height = 300),
      plotOutput("bar", height = 450),
      DT::dataTableOutput("species_year_table"),
      leafletOutput("species_map", height =420)
    ) # main panel
  ) # side bar layout
) #fluid page
###############################################################################
# SERVER
###############################################################################
server <- function(input, output, session){
  
  # change to reactive later so something loads on start up?
  filtered <- eventReactive( {input$apply; TRUE}, {
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
  }, ignoreInit = FALSE)
  
  ###########################################
  # RENDER ---BAR CHART ---top species
  output$bar <- renderPlot({
    #req(input$apply > 0) # render card
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
  # REACTIVE: ---1-WAY TABLE --- for observations by year
  
  crosstab1yr_data <- eventReactive({input$apply; TRUE}, {
    d <- df |>
      filter(between(observed_month, input$month_range[1], input$month_range[2]))
    
    if (input$county != "_ALL_") {d <- d |> filter(NAME == input$county)}
    d |>
      count(observed_year, name = "n_obs") |>
      arrange(observed_year) |>
      mutate(Year = "Total Observations") |>
      pivot_wider(
        names_from = observed_year, 
        values_from = n_obs, 
        values_fill = 0)
  }, ignoreInit = FALSE)
  
  # RENDER
  output$crosstab1yr <- DT::renderDataTable({
    #req(input$apply > 0) # render card
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
  }) # crosstab1yr
  
  ###########################################
  # REACTIVE: ---SUMMARY STATS--- by Year Per-Species Observation 2-way contingency table
  
  # For each year, shows total number of unique species observed and the min/med/mean/max observations per species
  # add descriptive text later?? something like: how frequently observed species were within each year
  # min - fewest obs any species had; med - middle species when sorted by count; 
  # mean - avg obs per species; max - most observed species
  year_species_stats_data <- eventReactive({input$apply; TRUE}, {
    d <- df |>
      dplyr::filter(dplyr::between(observed_month, input$month_range[1], input$month_range[2]))
    
    if (input$county != "_ALL_") {
      d <- d |> dplyr::filter(NAME == input$county)
    }
    
    # per-species counts within each year, then year-level summaries of the counts
    d |>
      dplyr::count(observed_year, common_name, name = "n_species_obs") |> # each row is 1 species observed in a year
      dplyr::group_by(observed_year) |> # group above by observed year
      dplyr::summarise(
        species_n = dplyr::n(),
        min = min(n_species_obs, na.rm = TRUE),
        median = stats::median(n_species_obs, na.rm = TRUE),
        mean = mean(n_species_obs, na.rm = TRUE),
        max = max(n_species_obs, na.rm = TRUE),
        sd = stats::sd(n_species_obs, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(observed_year) |>
      dplyr::mutate(
        mean = round(mean, 2),
        sd   = round(sd, 2)
      )
  }, ignoreInit = FALSE)
  
  # RENDER
  output$year_species_stats <- DT::renderDataTable({
    ct <- year_species_stats_data()
    validate(need(nrow(ct) > 0, "No observations for this selection."))
    DT::datatable(
      ct,
      rownames = FALSE,
      options = list(
        dom = 't',
        ordering = FALSE,
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  ###########################################
  #REACTIVE: ---BOX PLOT--- for per-species observation stats
  # one row per species × year with its observation count
  per_species_year <- eventReactive({input$apply; TRUE}, {
    d <- df |>
      dplyr::filter(dplyr::between(observed_month, input$month_range[1], input$month_range[2]))
    
    if (input$county != "_ALL_") 
      d <- d |> 
        dplyr::filter(NAME == input$county)
    
    d |>
      dplyr::count(observed_year, common_name, name = "n_species_obs") |>
      dplyr::arrange(observed_year)
  }, ignoreInit = FALSE)
  
  # RENDER
  output$year_species_box <- renderPlot({
    #req(input$apply > 0)
    d <- per_species_year()
    validate(need(nrow(d) > 0, "No observations for this selection."),
             # validate(need(nrow(ct) > 0, "No observations for this selection."))
             need(all(c("observed_year", "n_species_obs") %in% names(d)),
                  paste("Missing required columns for boxplot. Got:", paste(names(d), collapse=", ")))
    )
    d$observed_year <- as.factor(d$observed_year)
    
    ggplot(d, aes(x = factor(observed_year), y = n_species_obs)) +
      geom_boxplot(outlier.alpha = 0.25) +
      labs(
        x = "Year",
        y = "Observations per species",
        title = "Distribution of per-species observation counts by year"
      ) +
      geom_jitter(width = 0.15, height = 0, alpha = 0.2) +
      theme_minimal(base_size = 12)
  })
  
  
  ###########################################
  # REACTIVE subset for the chosen species, keeping month and county filters
  
  # REACTIVE: 2-way contingency table for the selected species (County × Year)
  species_year_county_data <- reactive({
    #req(input$species)              # must pick a species first
    
    d <- species_data()             # already filtered by species + month + (maybe) county
    req(nrow(d) > 0)
    
    tab <- d |>
      count(NAME, observed_year, name = "n_obs") |>     # County × Year counts
      tidyr::pivot_wider(
        names_from  = observed_year,                     # Years become columns
        values_from = n_obs,
        values_fill = 0
      ) %>%
      dplyr::rename(County = NAME) |>
      dplyr::mutate(Total = rowSums(dplyr::across(where(is.numeric)), na.rm = TRUE)) |>
      dplyr::arrange(dplyr::desc(Total))                 # optional: sort by total desc
    
    tab
  })
  
  # REACTIVE
  species_data <- reactive({
    #req(input$species) # ensure a species is chosen / not empty
    d <- df |>
      dplyr::filter(
        common_name == input$species,
        dplyr::between(observed_month, input$month_range[1], input$month_range[2])
      )
    if (input$county != "_ALL_") {
      d <- d |> dplyr::filter(NAME == input$county)
    }
    d |> dplyr::filter(!is.na(latitude), !is.na(longitude))
  })
  
  # RENDER
  output$species_year_table <- DT::renderDataTable({
    #req(input$species)  # don’t render until a species is chosen
    ct <- species_year_county_data()
    DT::datatable(
      ct,
      rownames = FALSE,
      options = list(
        dom = 't',        # no search/paging/info
        ordering = TRUE,
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  
  
  # RENDER
  output$species_map <- renderLeaflet({
    #req(input$species)
    d <- species_data()
    validate(need(nrow(d) > 0, "No observations for this species with current filters."))
    
    # base map
    m <- leaflet(d) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addHeatmap(
        lng = ~longitude, lat = ~latitude,
        blur = 20, radius = 15, max = 1, minOpacity = 0.2
      )
    
    # fit to data extent
    m |> fitBounds(
      lng1 = min(d$longitude, na.rm = TRUE),
      lat1 = min(d$latitude,  na.rm = TRUE),
      lng2 = max(d$longitude, na.rm = TRUE),
      lat2 = max(d$latitude,  na.rm = TRUE)
    )
  })
  
  
  
  
  
  ########################################
}


###########################################
shinyApp(ui, server)