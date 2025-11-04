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
library(lubridate)
library(shinycssloaders)

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
ui <- bslib::page_sidebar(
  title = "iNaturalist Observations in Western North Carolina",
  sidebar = bslib::sidebar(
    
    p("Make a selection below to see summaries."),  
    selectInput("county", "Region/County:",
                  choices = county_choices, 
                  selected = "_ALL_"),
      sliderInput("month_range", "Observation Months:",
                  min = 1, max = 12, value = c(1, 12), step = 1),
      sliderInput("min_obs", "Minimum Observations:",
                  min = 1, 
                  max = max(df$sum_sp, na.rm = TRUE), 
                  value = 1, 
                  step = 1),
      numericInput("top_n", "Show top N species:", value = 15, min = 5, max = 50, step = 1),
      
      tags$hr(),
      p("Select a species to see county summary and species heatmap."),
      selectizeInput(
        "species", "Species:",
        choices = c("", sort(unique(df$common_name))),
        options = list(placeholder = "Start typing a bird name..."),
        multiple = FALSE
      ),
      actionButton("apply", "Apply Species Filter"),
      actionButton("reset", "Reset to Defaults", icon = icon("refresh"))
      
    ), # sidebar panel
    ############################################################################
    ############################################################################
  navset_tab(
    id = "tabs",
    selected = "About",
    
    # Overview tab
    nav_panel(
      
      "About",
      h3("About this App"),
      p("This Shiny web application is a tool for exploring iNaturalist data. 
      The app focuses on bird observations (Aves) recorded 
        through the iNaturalist platform between 2020 and 2025 across counties 
        in Western North Carolina (WNC). It enables users to visualize quantitative, 
        spatial, temporal, and taxonomic patterns in the data and examine how 
        observation effort and data quality vary over time."),
      
      p("This app contains three tabs:"),
      p("1. About Tab: Includes information about this App and iNaturalist."),
      p("2. Data Download Tab: You may download a subset of the data used in this App."),
      p("3. Data Exploration Tab: The main App where you can explore the data."),
      p("The side bar: Contains various filters you can use to subset and visualize the data on 
      the Data Exploration tab, as well as to subset data on the Data Download tab."),
      p("How to use this App: Start by navigating to the Data Exploration tab. The app 
        loads with data for all of Western North Carolina (WNC) and all observations months (Jan - Dec).
        You can start by adjusting the filters for Region/County and Observation Months 
        and see how the visuals change. You can then also scroll down on the left side bar, 
        and select a species, to see additional visuals that are species-specific, including 
        a heat map of the distribution of observations for your chosen species."),
      p("More about the data: iNaturalist data consist of data points or observations submitted 
      through the iNaturalist App by registered users. Each observation or data 
      points contains variables like the time, date, and coordinates of the 
      observation, as well quality grade and species details.
      Learn more  about iNaturalist at their website: "),
      tags$a(href = "https://www.inaturalist.org/", "iNaturalist", target = "_blank"),
      tags$img(src = "INaturalist_logo.png", width = "100px", alt = "Description of the image"),
    ),
    
    # Data Download Tab
    nav_panel(
      "Data Download",
      h3("Data Download"),
      p("Use the filters to preview and download a subset of the iNaturalist data."),
      
      DT::dataTableOutput("download_table"),
      br(),
      downloadButton("download_subset", "Download Filtered Data")
    ),
    
    # 3) APP tab
    nav_panel(
      "Data Exploration",
      tags$h4("Total Observations by Year"),
      tags$p("Adjust Region/County and/or Observation Month to see total observations by year for selected filters."),
      withSpinner(DT::dataTableOutput("crosstab1yr")),
      
      tags$h4("Total Observations by Quality Grade and Year"),
      tags$p("Adjust Region/County and/or Observation Month to see total observations by quality grade for selected filters."),
      withSpinner(DT::dataTableOutput("year_quality_table")),
      
      tags$h4("Per-Species Statistics"),
      tags$p("Adjust Region/County and/or Observation Month to see per-species statistics for selected filters.This shows the number of species observed, as well as the minimum, mean, median, and maximum observed for a species by year."),      
      withSpinner(DT::dataTableOutput("year_species_stats")),
      
      tags$h4("Per-Species Statistics Box Plot"),
      tags$p("Adjust Region/County and/or Observation Month to visually explore per-species statistics for selected filters. Optionally, also select a species from the Species dropdown to see where it falls in the distribution."),
      withSpinner(plotOutput("year_species_box", height = 300)),
      
      tags$h4("County-Level Species Richness vs Observation Effort"),
      tags$p("Select a Region/County and/or Observation Month to see where the county falls (for the selected time period) in terms of species richness and observation effort."),
      withSpinner(plotlyOutput("county_scatter")),
      
      tags$h4("Top Species"),
      tags$p("Select a Region/County and Observation Month to see most frequently observed species for selected filters. Adjust 'Show top N species' to change number of species shown in chart."),
      withSpinner(plotOutput("bar", height = 450)),
      
      tags$h4("Seasonality of Observations"),
      tags$p("Adjust Region/County and Species to see observations by month."),
      withSpinner(plotOutput("phenology", height = 320)),
      
      tags$h4("Time of Day of Observations"),
      tags$p("Adjust Region/County and Species to see observations by time of day"),
      withSpinner(plotOutput("tod_facets", height = 420)),
      
      tags$h4(""),
      tags$p(""),
      withSpinner(DT::dataTableOutput("species_year_table")),
      
      tags$h4(textOutput("map_title")),
      tags$p(textOutput("map_subtitle"), style = "font-size: 14px; color: #555; margin-top: -10px;"),
      withSpinner(leafletOutput("species_map", height =420))

    )))
   
###############################################################################
# SERVER
###############################################################################
server <- function(input, output, session){
  
  filtered <- reactive({
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
  })
  
  ###########################################
  # RENDER ---BAR CHART ---top species
  output$bar <- renderPlot({
    
    d <- filtered()
    validate(need(nrow(d) > 0, "No species meet the current filters."))
    ggplot(d, aes(x = reorder(label, count), y = count)) +
      geom_col() +
      coord_flip() +
      labs(
        title = if (input$county == "_ALL_")
          "Western NC, 2020 - 2025"
        else
          paste(input$county,"County, 2020 - 2025"),
        x = "Species (common name)", y = "Number of observations"
      ) +
      theme_minimal(base_size = 12)
  })
  
  ###########################################
  # REACTIVE: ---1-WAY TABLE --- for observations by year
  
  crosstab1yr_data <- reactive({
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
  })
  
  # RENDER
  output$crosstab1yr <- DT::renderDataTable({
    
    ct <- crosstab1yr_data()
    validate(need(nrow(ct) > 0, "No observations for this selection."))
    DT::datatable(
      ct,
      rownames = FALSE,
      options = list(
        ordering = FALSE,
        searching = FALSE,
        paging = FALSE,   
        info = FALSE 
      )
    )
  })
  
  
  ###########################################
  # --- 2 WAY YR QUALITY_GRADE TABLE ---
  # observations by year by quality grade
  year_quality_data <- reactive({
    grades <- c("research", "needs_id", "casual") 
    
    d <- df |>
      dplyr::filter(dplyr::between(observed_month, input$month_range[1], input$month_range[2]))
    
    if (input$county != "_ALL_") {
      d <- d |> dplyr::filter(NAME == input$county)
    }
    
    d |>
      dplyr::mutate(quality_grade = factor(quality_grade, levels = grades)) |>
      dplyr::count(observed_year, quality_grade, name = "n") |>
      tidyr::complete(
        observed_year,
        quality_grade = factor(grades, levels = grades),
        fill = list(n = 0L)
      ) |>
      tidyr::pivot_wider(
        names_from  = quality_grade,
        values_from = n,
        values_fill = 0
      ) |>
      dplyr::arrange(observed_year) |>
      dplyr::mutate(
        Total = rowSums(dplyr::across(tidyselect::any_of(grades)), na.rm = TRUE)
      )
  })
  
  output$year_quality_table <- DT::renderDataTable({
    ct <- year_quality_data()
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
  # REACTIVE: ---SUMMARY STATS--- by Year Per-Species Observation contingency table
  
  year_species_stats_data <- reactive({
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
  })
  
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
  per_species_year <- reactive({
    d <- df |>
      dplyr::filter(dplyr::between(observed_month, input$month_range[1], input$month_range[2]))
    
    if (input$county != "_ALL_") 
      d <- d |> 
        dplyr::filter(NAME == input$county)
    
    d |>
      dplyr::count(observed_year, common_name, name = "n_species_obs") |>
      dplyr::arrange(observed_year)
  })
  
  # RENDER
  output$year_species_box <- renderPlot({
    d <- per_species_year()
    validate(need(nrow(d) > 0, "No observations for this selection."),
             need(all(c("observed_year", "n_species_obs") %in% names(d)),
                  paste("Missing required columns for boxplot. Got:", paste(names(d), collapse=", ")))
    )
    d$observed_year <- as.factor(d$observed_year)
    
    p <- ggplot(d, aes(x = factor(observed_year), y = n_species_obs)) +
      geom_boxplot(outlier.alpha = 0.2) +
      geom_jitter(width = 0.15, height = 0, alpha = 0.2, size = 1.6) +
      labs(
        x = "Year",
        y = "Observations per species"
        #title = "Distribution of per-species observation counts by year"
      ) +
      theme_minimal(base_size = 12)
    
    # add highlight for selected bird
    if (nzchar(input$species) && any(d$common_name == input$species)) {
      sel <- d %>% dplyr::filter(common_name == input$species)
      
      p <- p +
        geom_point(
          data = sel,
          aes(x = observed_year, y = n_species_obs),
          inherit.aes = FALSE,
          size = 3.5,
          shape = 21, # filled circle with outline
          fill  = "#FDB863", # highlight fill
          color = "black", # thin outline so it pops on the box
          stroke = 0.4,
          alpha = 0.95
        )
    }
    p
  })
  
  ###########################################
  # REACTIVE: --- SCATTER PLOT ---
  # SCATTER PLOT — updates when month_range changes
  
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
      geom_point(aes(alpha = !highlight), color = "#5B5B5B", size = 2) +
      geom_point(data = subset(d, highlight), color = "#FDB863", size = 4, stroke = 1.2) +
      labs(
        #title = "County Species Richness vs Observation Effort",
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
        title = paste0(
          if (nzchar(input$species)) paste0("Species: ", input$species) else "All birds",
          " - ",
          if (input$county == "_ALL_") "All WNC" else paste("County:", input$county),
          " (Month range ignored)"
        ),
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank()
      )
      
  })
  
  ###########################################
  #--- TIME FACETS ---
  # faceted time-of-day observations by month - ignores month_range
  tod_month_period_df <- reactive({
    d <- df
    if (input$county != "_ALL_") d <- d |> dplyr::filter(NAME == input$county)
    if (nzchar(input$species))   d <- d |> dplyr::filter(common_name == input$species)
    
    d <- d |>
      dplyr::filter(!is.na(observed_month), !is.na(day_period)) |>
      dplyr::mutate(
        month_fac  = factor(month.abb[observed_month], levels = month.abb, ordered = TRUE),
        day_period = factor(as.character(day_period),
                            levels = c("Night","Dawn","Morning","Afternoon","Dusk"),
                            ordered = TRUE)
      )
    
    # aggregate to counts per month × day_period, fill zeros, calculate % per month
    agg <- d |>
      dplyr::count(month_fac, day_period, name = "n") |>
      tidyr::complete(
        month_fac  = factor(month.abb, levels = month.abb, ordered = TRUE),
        day_period = factor(c("Night","Dawn","Morning","Afternoon","Dusk"),
                            levels = c("Night","Dawn","Morning","Afternoon","Dusk"),
                            ordered = TRUE),
        fill = list(n = 0L)
      ) |>
      dplyr::group_by(month_fac) |>
      dplyr::mutate(
        month_total = sum(n, na.rm = TRUE),
        percent     = dplyr::if_else(month_total > 0, n / month_total, 0)
      ) |>
      dplyr::ungroup()
    
    agg
  })
  
  output$tod_facets <- renderPlot({
    d <- tod_month_period_df()
    validate(need(nrow(d) > 0, "No observations available for this selection."))
    
    # put n= in facet labels
    facet_labs <- d |>
      dplyr::distinct(month_fac, month_total) |>
      dplyr::mutate(label = paste0(month_fac, " (n=", month_total, ")"))
    lab_map <- stats::setNames(facet_labs$label, facet_labs$month_fac)
    
    ggplot(d, aes(x = "", y = percent, fill = day_period)) +
      geom_col(width = 0.8, color = "white", size = 0.3) +
      facet_wrap(~ month_fac, ncol = 4, labeller = ggplot2::labeller(month_fac = lab_map)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual(
        values = c(
          Night = "#5B5B5B",   # dark gray
          Dawn = "#FDB863",    # orange
          Morning = "#80B1D3", # blue
          Afternoon = "#8DD3C7", # teal
          Dusk = "#B3CDE3"     # light blue
        ),
        drop = FALSE,
        name = "Time of day"
      ) +
      labs(
        title = paste0(
          if (nzchar(input$species)) paste0("Species: ", input$species) else "All birds",
          " — ",
          if (input$county == "_ALL_") "All WNC" else paste0("County: ", input$county),
          " — Month range ignored"
        ),
        x = NULL, y = "Share of monthly observations"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        strip.text = element_text(face = "bold")
      )
  })
  
  ###########################################
  # REACTIVE --- CONTINGENCY TABLE --- 

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
  output$map_title <- renderText({
    if (nzchar(input$species)) {
      paste(input$species, "Distribution Map")
    } else {
      "Select species to see map."
    }
  })
  
  output$map_subtitle <- renderText({
    if (nzchar(input$species)) {
      if (input$county == "_ALL_") "All WNC"
      else paste("Observations within", input$county, "County")
    } else ""
  })
  
  output$species_map <- renderLeaflet({
    req(nzchar(input$species)) 
    d <- species_data()
    req(!is.null(d), nrow(d) > 0)
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
  # --- REFRESH ACTIONS ---
  # OBSERVE
  observeEvent(input$reset, {
    # reset all filters to defaults
    updateSelectInput(session, "county", selected = "_ALL_")
    updateSliderInput(session, "month_range", value = c(1, 12))
    updateSliderInput(session, "min_obs", value = 1)
    updateNumericInput(session, "top_n", value = 15)
    updateSelectInput(session, "species", selected = "")
    leafletProxy("species_map") %>% clearShapes() %>% clearControls() %>% clearMarkers()
  })
  
###########################################
  # --- DATA DOWNLOAD ---

  # REACTIVE
  download_data <- reactive({
    d <- df
    
    # County filter (All WNC vs specific county)
    if (input$county != "_ALL_") {
      d <- d |> dplyr::filter(NAME == input$county)
    }
    
    # month range filter
    d <- d |>
      dplyr::filter(
        dplyr::between(observed_month, input$month_range[1], input$month_range[2])
      )
    
    # Optional species filter - only if user selects
    if (nzchar(input$species)) {
      d <- d |> dplyr::filter(common_name == input$species)
    }
    
    d
  })
  
  # RENDER
  output$download_table <- DT::renderDataTable({
    d <- download_data()
    validate(need(nrow(d) > 0, "No observations for this selection."))
    
    DT::datatable(
      d,
      options = list(
        pageLength = 20,
        scrollX    = TRUE
      )
    )
  })
  
  output$download_subset <- downloadHandler(
    filename = function() {
      paste0("inat_subset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      readr::write_csv(download_data(), file)
    }
  )
  ###########################################
  
  
}
###########################################
shinyApp(ui, server)