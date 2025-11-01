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
library(plotly)

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
      sliderInput("min_obs", "Minimum observations (species must have at least this many observations):",
                  min = 1, 
                  max = max(df$sum_sp, na.rm = TRUE), 
                  value = 1, 
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
      plotlyOutput("county_scatter"), #NEW
      plotOutput("bar", height = 450),
      plotOutput("phenology", height = 320),
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
      )
    )
  })
  
  ###########################################
  # REACTIVE: ---SUMMARY STATS--- by Year Per-Species Observation contingency table
  
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
  # REACTIVE: --- SCATTER PLOT ---
  # SCATTER PLOT — updates immediately when month_range changes
  
  scatter_base <- reactive({
    df |>
      dplyr::filter(dplyr::between(observed_month, input$month_range[1], input$month_range[2])) |>
      dplyr::group_by(NAME) |>
      dplyr::summarise(
        observations   = dplyr::n(),
        unique_species = dplyr::n_distinct(common_name),
        .groups = "drop"
      )
  })
  
  # RENDER
  output$county_scatter <- renderPlotly({
    d <- scatter_base()
    validate(need(nrow(d) > 0, "No counties meet the current month selection."))
    
    d$highlight <- if (is.null(input$county) || input$county == "_ALL_") FALSE else d$NAME == input$county
    
    p <- ggplot(d, aes(
      x = observations,
      y = unique_species,
      text = paste0(
        "<b>", NAME, "</b><br>",
        "Observations: ", observations, "<br>",
        "Unique species: ", unique_species
      )
    )) +
      geom_point(aes(alpha = !highlight), size = 3) +
      geom_point(data = subset(d, highlight), color = "blue", size = 4, stroke = 1.2) +
      labs(
        title = "County Species Richness vs Observation Effort",
        subtitle = paste0("Months: ", input$month_range[1], "–", input$month_range[2]),
        x = "Total Observations",
        y = "Number of Unique Species"
      ) +
      coord_flip() +
      theme_minimal(base_size = 12) +
      guides(alpha = "none")
    
    ggplotly(p, tooltip = "text") |>
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 70, r = 30, t = 60, b = 60)
      )
  })

  ###########################################
  #--- CIRCULAR PHENOLOGY ---
  # Circular phenology plot to show by month observations for a bird
  # can be all birds in WNC, all/species specific by county, or one species for all counties
  # REACTIVE
  phenology_df <- reactive({
    d <- df # start with all bird observations
    # county filter
    if (input$county != "_ALL_") {
      d <- d |>
        dplyr::filter(NAME == input$county)
    }
    # selected species if chosen
    if (nzchar(input$species)) {
      d <- d |>
        dplyr::filter(common_name == input$species)
    }
    
    d |>
      dplyr::count(observed_month, name = "n") |>
      dplyr::mutate(
        month_lab = factor(month.abb[observed_month], levels = month.abb)
      )

  })
  
  # RENDER
  output$phenology <- renderPlot({
    d <- phenology_df()
    validate(need(sum(d$n) > 0, "No observations available for this selection."))
    
    ggplot(d, aes(x = month_lab, y = n)) +
      geom_col(width = 1) +
      coord_polar() +
      labs(
        title = if (nzchar(input$species))
          paste0("Seasonality of Observations: ", input$species)
        else
          "Seasonality of Observations: All birds",
        subtitle = if (input$county == "_ALL_") "All WNC" else paste("County:", input$county),
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank()
      )
  })
  ###########################################
  # REACTIVE --- 2-WAY CONTINGENCY TABLE --- 

  # REACTIVE 1
  # triggers with a species after apply, filters df for chosen common_name, selected month range, and county (optional)
  species_data <- eventReactive({input$apply}, {
    req(nzchar(input$species))

    d <- df |>
      dplyr::filter(
        common_name == input$species,
        dplyr::between(observed_month, input$month_range[1], input$month_range[2])
      )
    if (input$county != "_ALL_") {
      d <- d |> dplyr::filter(NAME == input$county)
    }
    d |> dplyr::filter(!is.na(latitude), !is.na(longitude))
  }, ignoreInit = TRUE) # doesn't run on app start
  
  
  # REACTIVE 2
  # runs after REACTIVE 1 triggers, builds summary table with county and year total
  species_year_county_data <- reactive({
    # recalculates when species_data updates
    d <- species_data()  # filtered by species,month, county
    req(nrow(d) > 0)
    
    d |>
      count(NAME, observed_year, name = "n_obs") |> # row per county, year counts
      dplyr::mutate(observed_year = as.integer(observed_year)) |>
      tidyr::pivot_wider(
        names_from  = observed_year, # Years become columns
        values_from = n_obs,
        values_fill = 0
      ) |>
      dplyr::rename(County = NAME) |>
      dplyr::select(County, sort(tidyselect::peek_vars())) |>  # reorder columns numerically
      dplyr::mutate(Total = rowSums(dplyr::across(where(is.numeric)), na.rm = TRUE)) |> # total/county
      dplyr::arrange(dplyr::desc(Total)) # sort by total desc
  })
  
  # RENDER
  output$species_year_table <- DT::renderDataTable({
    req(nzchar(input$species)) # species must be selected to run
    ct <- species_year_county_data()
    DT::datatable(
      ct,
      rownames = FALSE,
      options = list(
        dom = 't',    
        ordering = TRUE,
        searching = FALSE,
        paging = FALSE,
        info = FALSE
      )
    )
  })
  
  ###########################################
  #--- MAP ---
  # RENDER
  output$species_map <- renderLeaflet({
    req(nzchar(input$species)) 
    d <- species_data()
    validate(need(nrow(d) > 0, "Select a bird species to see distribution map."))
    
    # base map
    m <- leaflet(d) |>
      addProviderTiles(providers$CartoDB.Positron) |> # light gray Carto DB basemap
      addHeatmap( 
        lng = ~longitude, lat = ~latitude,
        blur = 20, radius = 15, max = 1, minOpacity = 0.2
      )
    
    # fit map to data extent
    m |> fitBounds(
      lng1 = min(d$longitude, na.rm = TRUE),
      lat1 = min(d$latitude,  na.rm = TRUE),
      lng2 = max(d$longitude, na.rm = TRUE),
      lat2 = max(d$latitude,  na.rm = TRUE)
    )
  })
  
  ###########################################
 
 
 
  
  
  
  
  
  
  
  ###########################################
  #--- FACETED DENSITY PLOT ---
  #Faceted density plot of time of day (by bird)
  
  
  ########################################
}


###########################################
shinyApp(ui, server)