#-Load libraries----------------------------------------------------------------
library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(plotly)
library(leaflet)
library(sf)
library(lubridate)

#-read in app data once on app start up-----------------------------------------
app_data <- 
  read.csv("./data-outputs/data_dhis2_laos_ammnet_cleaned.csv") 

# re-classify period as a date 
app_data$period <- lubridate::as_date(app_data$period)

#-Read in shapefile for provinces-----------------------------------------------
adm1_shp <- st_read("./data-outputs/adm1_shapefile.shp")

#-Define UI for application - User Interface------------------------------------
ui <- page_fluid(
  layout_sidebar(
    sidebar = sidebar(
      tags$img(src = "AMMnet-logo.png"),
      p("Dashboard for examining province level malaria indicators in Laos.
         Select a province and data series to view the trends over time."),
      selectInput(
        inputId = "province_select",  
        label = "Select a Province:",
        choices = c("", sort(unique(app_data$organisation_unit))),
        selected = "" 
      ), 

      checkboxGroupInput(
        inputId = "series_select",
        label = "Select data series:",
        choices = c("Confirmed Cases" = "malaria_cases",
                    "Malaria Tests"  = "malaria_tests"),
        selected = NULL
      ), 
      
      # <<< NEW >>> Updated condiditional UI for TPR checkbox 
      uiOutput("tpr_checkbox_ui"), 
    ),
    
    h1("Malaria Indicator Dashboard"), 
    
    uiOutput("value_boxes"), 
    
    layout_columns(
      card(
        height = 400, 
          card_header("Monthly Malaria Indicators"),
          card_body(plotlyOutput("ts_plot")),
          card_footer("Data source: DHIS2 export")
        ), 
      
      card(
        height = 200, 
        card_header("Monthly Malaria Test Positivity"), 
        card_body(plotlyOutput("tpr_plot")), 
        card_footer("Data source: DHIS2 export")
      )
    ), 
    
    layout_columns(
      card(
        height = 400,
        card_header("Malaria Incidence Trends"),
        card_body(plotlyOutput("inc_plot", height = "350px")),
        card_footer("Data source: DHIS2 export")
      ), 
      card(
        height = 400,
        card_header("Average Annual Incidence Map"),
        card_body(leafletOutput("inc_map", height = "350px")),
        card_footer("Data source: HumData for shape files, DHIS2 for malaria data")
      )
    ),
    
    card(
      card_header("Summary Table"),
      card_body(
        downloadButton("download_summary", "Download CSV"),
        DTOutput("summary_data")
      ),
      card_footer("Aggregated Annual Values per Province")
    )
  ),   
  

  theme = bs_theme(
    version = 5, 
    fg = "rgb(100, 16, 59)", 
    bg = "rgb(255, 255, 255)", 
    primary = "#7C204F", 
    secondary = "#F7CBE6"
  )
)

#-Define server logic - Computations-------------------------------------------- 
server <- function(input, output) {
 
  # bs_themer()
  # Reactive data filter
  province_data <- reactive({
    req(input$province_select != "")  # Only run if province selected
    
    app_data %>% 
      filter(organisation_unit == input$province_select)
  })
  
  # Compute total cases from filtered data
  output$total_cases_box <- renderText({
    df <- province_data()
    
    total_cases <- df %>%
      filter(var_name == "malaria_cases") %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      pull(total)
    
    total_cases
  })
  
  # Compute total tests from filtered data
  output$total_tests_box <- renderText({
    df <- province_data()
    
    total_tests <- df %>%
      filter(var_name == "malaria_tests") %>%
      summarise(total = sum(value, na.rm = TRUE)) %>%
      pull(total)
    
    total_tests
  })
  
  # Render the interactive time series plot 
  # Make the plot using GGplot and use Plotly to make it interactive
  # Add tests to the plot
  output$ts_plot <- 
    renderPlotly({
      # Add placeholders for input validation
      validate(
        need(input$province_select != "", "Please select a province to view data."),
        need(length(input$series_select) > 0, "Please select at least one data elemtent to generate the plot.")
      )
      
      df <- province_data()  |> 
        filter(var_name %in% input$series_select) # Get the filtered data as per input
      
      req(nrow(df) > 0)              # Ensure there are rows to plot
      
      # Create a ggplot time series
      p <- ggplot(df, aes(x = period, y = value, col=var_name)) +
        geom_line() +
        geom_point()+
        labs(
          title = paste("Malaria Cases in:", input$province_select),
          x = "Month",
          y = "Value", 
          color = "Indicator Variable"
        ) + 
        scale_color_manual(values = c("malaria_cases" = "#571845", "malaria_tests" = "#C42847"))+
        theme_minimal()
      
      # Convert ggplot to interactive Plotly object
      ggplotly(p)
    })
  
  # Conditional UI for TPR checkbox
  output$tpr_checkbox_ui <- renderUI({
    if (all(c("malaria_cases", "malaria_tests") %in% input$series_select)) {
      checkboxInput(
        inputId = "include_tpr",
        label = "Include Test Positivity Rate (TPR)",
        value = FALSE
      )
    } else {
      NULL
    }
  })
  
  # Conditional Plot for TPR 
  output$tpr_plot <- renderPlotly({
    
    # Use validate + need to provide informative user messages
    validate(
      need(input$include_tpr, "Enable TPR toggle to display plot."),
      need(input$province_select != "", "Please select a province to view data."),
      need(length(input$series_select) == 2, "Please select both malaria cases and malaria tests to calculate TPR.")
    )
    
    # calculate TPR
    df <- province_data() |>
      pivot_wider(names_from = var_name, values_from = value) |>
      arrange(period) |>
      mutate(
        tpr = round((malaria_cases / malaria_tests) * 100, 2)
      )
    
    # Plot
    p <- 
      ggplot(df, aes(x = period, y = tpr)) +
      geom_point(color = "#FECC01") +
      labs(
        title = paste("Test Positivity Rate (TPR) in:", input$province_select),
        x = "Month",
        y = "TPR (%)"
      ) +
      theme_minimal() +
      scale_y_continuous(limits = c(0, 100))
    
    ggplotly(p)
  })
  
  # Compute average TPR when toggle is active
  output$tpr_value <- renderText({
    req(input$include_tpr)
    
    df <- province_data() |>
      pivot_wider(names_from = var_name, values_from = value) |>
      summarise(tpr = mean((malaria_cases / malaria_tests) * 100, na.rm=TRUE))
    
    avg_tpr <- round(df$tpr, 1)
    avg_tpr
  })
  
  # Dynamically render full value box row based on toggle
  output$value_boxes <- renderUI({
    
    # Always include these two
    boxes <- list(
      value_box(
        title = "Total Cases Reported",
        value = textOutput("total_cases_box"),
        showcase = bsicons::bs_icon("journal-medical"),
        showcase_layout = "top right", 
        theme = "secondary"
      ),
      value_box(
        title = "Total Tests Reported", 
        value = textOutput("total_tests_box"),
        showcase = bsicons::bs_icon("calendar"), 
        showcase_layout = "top right", 
        theme = "secondary" 
      )
    )
    
    # Conditionally add TPR box only if checkbox is ON
    if (isTRUE(input$include_tpr)) {
      boxes <- append(boxes, list(
        value_box(
          title = "Average TPR (%)",
          value = textOutput("tpr_value"),
          showcase = bsicons::bs_icon("percent"),
          showcase_layout = "top right",
          theme = "secondary"
        )
      ))
    }
    
    # Wrap all boxes inside layout_column_wrap
    layout_column_wrap(!!!boxes)
  })
  
  # Incidence plot 
  output$inc_plot <- renderPlotly({
    # Add placeholders for input validation
    validate(
      need(input$province_select != "", "Please select a province to view data.")
    )
    
    # filter dataset
    df <- province_data() |>
      filter(var_name == "malaria_cases_per_1000") 
    
    # Ensure there is data to plot
    validate(
      need(nrow(df) > 0, "No incidence data available for this province.")
    )
    
    # Plot
    p <- 
      ggplot(df, aes(x = period, y = value)) +
      geom_line(color = "#FECC01") +
      geom_point(color = "#FECC01") +
      labs(
        title = paste("Malaria Incidence in:", input$province_select),
        x = "Month",
        y = "Incidence (per 1,000)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$inc_map <- renderLeaflet({
    
    # Add placeholders for input validation
    validate(
      need(input$province_select != "", "Please select a province to view data.")
    )
    
    # Summarise incidence by province
    df <- app_data |>
      filter(var_name == "malaria_cases_per_1000") |>
      group_by(organisation_unit) |>
      summarise(inc = mean(value, na.rm = TRUE))
    
    # Join data to spatial file
    df_shape <- left_join(adm1_shp, df, by = c("org_unit" = "organisation_unit"))
    
    # Create color palette
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = df_shape$inc,
      na.color = "#f0f0f0"
    )
    
    leaflet(df_shape) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fillColor = ~pal(inc),
        weight = ~ifelse(org_unit == input$province_select, 3, 1),
        color = ~ifelse(org_unit == input$province_select, "#7C204F", "black"),
        opacity = 1,
        fillOpacity = 0.8,
        label = ~paste0(org_unit, ": ", round(inc, 1), " per 1,000"),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#333",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = pal,
        values = df_shape$inc,
        title = "Avg Incidence (per 1,000)",
        position = "bottomright"
      )
  })
  
  output$summary_data <- renderDT({
    validate(
      need(input$province_select != "", "Please select a province to view data.")
    )
    
    df <- app_data |>
      filter(var_name %in% c("malaria_cases", "malaria_tests", "malaria_cases_per_1000")) |>
      pivot_wider(
        names_from = var_name,
        values_from = value,
      ) |> 
      group_by(organisation_unit) |> 
      mutate(tpr = round((malaria_cases / malaria_tests) * 100, 2)) |> 
      summarise(
        total_cases = sum(malaria_cases, na.rm = TRUE),
        total_tests = sum(malaria_tests, na.rm=TRUE), 
        avg_tpr = round(mean(tpr, na.rm=TRUE), 1),
        avg_inc = round(mean(malaria_cases_per_1000, na.rm=TRUE),1), 
        reporting_months_cases = sum(!is.na(malaria_cases)),
        reporting_months_tests = sum(!is.na(malaria_tests))
      ) |> 
      rename(
        Province = organisation_unit,
        `Total Cases` = total_cases,
        `Total Tests` = total_tests,
        `Average TPR (%)` = avg_tpr,
        `Avg Incidence` = avg_inc,
        `Case Reporting Months` = reporting_months_cases,
        `Test Reporting Months` = reporting_months_tests
      )
    
    # Build DT object
    dt <- datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = "tip",  
        autoWidth = TRUE
      )
    )
    
    # Highlight selected province
    if (input$province_select != "") {
      selected <- input$province_select
      dt <- dt %>%
        formatStyle(
          "Province",
          target = "row",
          fontWeight = styleEqual(selected, "bold")
        )
    }
    
    dt
  })
  
  # The same data prep code as before, but put into a reactive function to reuse:
  summary_table_data <- reactive({
    app_data |>
      filter(var_name %in% c("malaria_cases", "malaria_tests", "malaria_cases_per_1000")) |>
      pivot_wider(names_from = var_name, values_from = value) |> 
      group_by(organisation_unit) |> 
      mutate(tpr = round((malaria_cases / malaria_tests) * 100, 2)) |> 
      summarise(
        total_cases = sum(malaria_cases, na.rm = TRUE),
        total_tests = sum(malaria_tests, na.rm=TRUE), 
        avg_tpr = round(mean(tpr, na.rm=TRUE), 1),
        avg_inc = round(mean(malaria_cases_per_1000, na.rm=TRUE),1), 
        reporting_months_cases = sum(!is.na(malaria_cases)),
        reporting_months_tests = sum(!is.na(malaria_tests))
      ) |> 
      rename(
        Province = organisation_unit,
        `Total Cases` = total_cases,
        `Total Tests` = total_tests,
        `Average TPR (%)` = avg_tpr,
        `Avg Incidence` = avg_inc,
        `Case Reporting Months` = reporting_months_cases,
        `Test Reporting Months` = reporting_months_tests
      )
  })
  
  
  # Enable file download
  output$download_summary <- downloadHandler(
    filename = function() { paste0("summary_data_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(summary_table_data(), file, row.names = FALSE)
    }
  )
  
  
}

#-Run the application-----------------------------------------------------------
shinyApp(ui = ui, server = server)