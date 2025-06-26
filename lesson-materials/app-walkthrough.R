#-Load libraries----------------------------------------------------------------
library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(plotly)
library(leaflet)
library(sf)
library(lubridate)
library(bsicons)
library(scales)

#-read in app data once on app start up-----------------------------------------
app_data <- 
  read.csv("./data-outputs/data_dhis2_laos_ammnet_cleaned.csv") 

# re-classify period as a date 
app_data$period <- lubridate::as_date(app_data$period)

#-Read in the shapefile for the provinces---------------------------------------
adm1_shp <- st_read("./data-outputs/adm1_shapefile.shp")

#-Define UI for application - User Interface------------------------------------
ui <- page_fluid(
  
  #-layout------------------------
  layout_sidebar(
    
    #-sidebar---------
    sidebar = sidebar(
      
      # add a logo 
      tags$img(src = "AMMnet-logo.png"), 
      
      # informative description
      p("Dashboard for examining malaria indicators at national and province levels in Laos.
     Select reporting level and indicators to view trends over time."),
      
      # add dark/light switch 
      input_dark_mode(mode = "light"),
      
      # Reporting Level selection - National or Province
      radioButtons(
        inputId = "reporting_level",
        label = "Select reporting level:",
        choices = c("National", "Province"),
        selected = "National"  # Default to National on app launch
      ),
      
      # Conditionally render Province dropdown only if Province level is selected
      uiOutput("province_selector")
      
    ),
    
    #-main pannel content----
    h1("Malaria Indicators Dashboard"), # Main title of the app
    
    # Value Boxes in a single row that wraps responsive to screen size and data selection
    uiOutput("value_boxes"), 
   
    # Card with a sidebar for input selection 
    card(
      full_screen = TRUE, 
      # global card header
      card_header("Monthyl Malaria Indicator Trends"),
      # sidebar layout with sidebar and card body
      layout_sidebar(
        
        # add the sidebar with input elements
        sidebar = list(
          # include original data series checkboes
          checkboxGroupInput(
            inputId  = "series_select",
            label    = "Select data series:",
            choices  = c(
              "Confirmed Cases" = "malaria_cases",
              "Malaria Tests"   = "malaria_tests"
            ), 
            selected = "malaria_cases"
          ),
          # condiditionally render TPR checkbox
          uiOutput("tpr_checkbox_ui")
        ),
        
        # include the card body with the plots
        card_body(
          # UI output that will dynamically render different layout
          uiOutput("ts_plots_ui")
        ),
        card_footer("Data source: DHIS2 export") # informative card footer
      )
    ), 
    
    # incidence plot card - including map 
    layout_columns(
      # width - adjust the width to give some more space to the line chart
      col_widths = c(7,5), 
      card(
        height = 500,
        full_screen = TRUE,
        card_header("Monthly Malaria Incidence Trends"),
        card_body(plotlyOutput("inc_plot", height = "350px")),
        card_footer("Data source: DHIS2 export")
      ), 
      card(
        full_screen = TRUE, 
        card_header("Average Annual Incidence Map"),
        card_body(leafletOutput("inc_map", height = "350px")),
        card_footer("Data source: HumData for shape files, DHIS2 for malaria data")
      )
    ), 
    
    # DT output 
    card(
      card_header("Summary Table"), # card header
      card_body(
        downloadButton("download_summary", "Download CSV"), # download button
        DTOutput("summary_data") # DT output
        ),  
      card_footer("Aggregated Annual Values per Province") # card footer
    )
  ),

  # <<< NEW >>> Defining the theme of your app 
  theme = bs_theme(
    version = 5, 
    fg = "rgb(100, 16, 59)",   # set the foreground colour
    bg = "rgb(253, 251, 252)", # set the background colour 
    primary = "#7C204F",       # set the primary colour 
    secondary = "#F7CBE6"      # set the secondary colour
  )
  
)


#-Define server logic - Computations------------------------------------------------------------------
server <- function(input, output) {
  
   # bs_themer()
   
  #-DATA---------------------------------------------------------------------------------------------
  #-Reactive data filter: adjust for national or province selection------
  filtered_data <- reactive({
    
    # If Province is selected, ensure province dropdown has a selection
    if (input$reporting_level == "Province") {
      req(input$province_select != "")
      app_data %>% filter(organisation_unit == input$province_select)
    } else {
      # If National, use full dataset and sum over all provinces
      app_data %>% 
        group_by(period, var_name) %>% # group by period and variable name
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") # sum values over all provinces
      
    }
  })
  
  #-UI dynamics------------------------------------------------------------------------------------
  
  #-Dynamically show province selection if Province level is selected ----------
  output$province_selector <- renderUI({
    
    # Check if the reporting level is set to Province
    if (input$reporting_level == "Province") {
      # Create a dropdown for province selection
      selectInput(
        inputId = "province_select", # ID for the province selection,
        label = "Select a Province:",# UI side title
        choices = c("", sort(unique(app_data$organisation_unit))), # empty first value followed by all of our unique org_unit names
        selected = "" # default value that the application starts with as selected - in our case we want this to be blank.
      )
    } else {
      NULL # If National level is selected, return NULL to not display the dropdown
    }
  })
  
  #-Conditional UI element for TPR checkbox-------------------------------------
  output$tpr_checkbox_ui <- renderUI({
    
    if (all(c("malaria_cases", "malaria_tests") %in% input$series_select)) {
      # Only show the checkbox if both series are selected
      checkboxInput(
        inputId = "include_tpr", # name ID
        label = "Calculate and diaplay Test Positivity Rate (TPR)", # User side title
        value = FALSE # set to be unclicked as default
      )
    } else {
      # If not both series selected, return NULL to not display the checkbox
      NULL
    }
  })
  
  #-Render the value box for TPR if checkbox is selected------------------------
  output$tpr_value_box <- renderUI({
    # Check if the TPR checkbox is selected
    if (isTRUE(input$include_tpr))  { # requires TPR check to be active
      
      # standard value box set up 
      value_box(
        title = "Average TPR (%)",       # Title
        value = textOutput("tpr_value"), # using condiditional value 
        showcase = bsicons::bs_icon("percent"), #Icon
        showcase_layout = "top right",   # placement
        theme = "secondary"              # colour theme
      )
    } else {
      NULL # if not checked then nothing is displayed
    }
  })
  
  #-Value Box dynamic number based on TPR toggle--------------------------------
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
        showcase = bsicons::bs_icon("journal-medical"), 
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
  
  #-conditional layout for time series plots------------------------------------
  output$ts_plots_ui <- renderUI({
    
    # If TPR is selected, show both plots side by side
    if (isTRUE(input$include_tpr)) { 
      # Show both plots side by side
      layout_columns(
        col_widths = c(6, 6),  # 50-50 width
        plotlyOutput("ts_plot"),
        plotlyOutput("tpr_plot")
      )
      
      # If TPR is not selected, show only the main time series plot
    } else {
      # Show single plot full width
      layout_columns(
        col_widths = c(12),  # full width
        plotlyOutput("ts_plot")
      )
    }
    
  })
  
  
  #-VALUE BOXES DATA--------------------------------------------------------------------------------------
  #-Compute total cases from filtered data--------------------------------------
  output$total_cases_box <- renderText({
    
    # Only validate if Province level is selected
    if (input$reporting_level == "Province") {
      validate(
        need(input$province_select != "", "Select a province")
      )
    }
   
     df <- filtered_data() # this act like a function and pulls through our filtered data to an object inside renderText called 'df'
    
    total_cases <- df %>%
      filter(var_name == "malaria_cases") %>% # filters to malaria cases data only
      summarise(total = sum(value, na.rm = TRUE)) %>% # sums values over all periods included
      pull(total) # pulls out the value from summary calculation into named vector
    
    # Apply formatting here
    scales::comma(total_cases) # final value with nice formatting 
  })
  
  #-Compute total tests from filtered data--------------------------------------
  output$total_tests_box <- renderText({
    
    # Only validate if Province level is selected
    if (input$reporting_level == "Province") {
      validate(
        need(input$province_select != "", "Select a province")
      )
    }
    
    df <- filtered_data() # pull filtered data
    
    total_tests <- df %>%
      filter(var_name == "malaria_tests") %>%
      summarise(total = sum(value, na.rm = TRUE)) %>% #sum total tesats
      pull(total)
    
    scales::comma(total_tests) # final value with nice formatting 
    
  })
  
  #-average TPR value box-------------------------------------------------------
  output$tpr_value <- renderText({
    
    req(input$include_tpr) # requires the TPR check to be TRUE 
    
    # data processing
    df <- 
      filtered_data() %>% # pull filtered data
      pivot_wider(names_from = var_name, values_from = value) %>% #widens data frame to have a column per indicator 
      mutate(
        tpr = round((malaria_cases / malaria_tests) * 100, 2) # calculates TPR as a percentage
      ) %>%
      summarise(
        tpr = mean(tpr, na.rm=TRUE) # takes average of TPR on % scale 
      ) 
    
    avg_tpr <- round(df$tpr, 1) # rounds to one decimal place
    
    avg_tpr # returns value 
  })
  
 
  #-PLOTS------------------------------------------------------------------------------------------------------
  
  #-Plot of reported values over time selected by province----------------------
  output$ts_plot <- renderPlotly({
    
    # only validate if Province level is selected
    if (input$reporting_level == "Province") {
      # condiditional messaging -  Check if province selection is made and if not display the following message 
      # Check if data selected and prompt to select data series
      validate(
        need(input$province_select != "", "Please select a province to view data."),
        need(length(input$series_select) > 0, "Please select at least one data elemtent to generate the plot.")
      )
    }
    
    # title value 
    title_val = 
      ifelse(input$reporting_level == "Province", 
             paste("Malaria Indicators in:", input$province_select), 
             "Malaria Indicators at National Level")
    
    
    df <- 
      filtered_data()  %>%  # get province data
      filter(var_name %in% input$series_select) # Get the filtered data as per input 
    
    req(nrow(df) > 0)              # Ensure there are rows to plot
    
    # Create a ggplot time series of time on the xaxis cases on the y, colour by data type
    p <- 
      ggplot(df, aes(x = period, y = value, col=var_name)) +
      geom_line() +  #add line style
      geom_point() + # add points to the line
      labs(
        title = title_val,
        x = "Month",
        y = "Value", 
        col="Indicator"
      ) +
      scale_color_manual(values = c("malaria_cases" = "#571845", "malaria_tests" = "#C42847"))+ # set the colour values
      theme_minimal()
    
    # Convert ggplot to interactive Plotly object
    ggplotly(p)
    
  })
  
  #-Conditional rendering of TPR plot-------------------------------------------
  output$tpr_plot <- renderPlotly({
    
    # validate if Province level is selected
    if (input$reporting_level == "Province") {
      # condiditional messaging -  Check if province selection is made and if not display the following message 
      # Check if data selected and prompt to select data series
      validate(
        need(input$province_select != "", "Please select a province to view data."),
        need(input$include_tpr, "Enable TPR toggle to display plot."),
        need(length(input$series_select) == 2, "Please select both malaria cases and malaria tests to calculate TPR.")
      )
    }
    
    # Validate for national level 
    # Use validate + need to provide informative user messages
    validate(
      need(input$include_tpr, "Enable TPR toggle to display plot."),
      need(length(input$series_select) == 2, "Please select both malaria cases and malaria tests to calculate TPR.")
    )
    
    # title value 
    title_val = 
      ifelse(input$reporting_level == "Province", 
             paste("Test Positivity Rate (TPR) in:", input$province_select), 
             "Test Positivity Rate (TPR) at National Level")
    
    # calculate TPR - Test Positivity Rate
    df <- 
      filtered_data() %>% # get the filtered province data
      pivot_wider(names_from = var_name, values_from = value) %>% # pivot the data wider to have cases and tests in separate columns
      arrange(period) %>% # arrange by period
      mutate(
        tpr = round((malaria_cases / malaria_tests) * 100, 2) # calculate TPR as a percentage - rounded to 2 decimal places
      )
    
    # Plot the TPR over time
    p <- 
      ggplot(df, aes(x = period, y = tpr)) + 
      geom_point(color = "#FECC01") +
      labs(
        title =  title_val,
        x = "Month",
        y = "TPR (%)"
      ) +
      theme_minimal() +
      scale_y_continuous(limits = c(0, 100)) # set y-axis limits from 0 to 100%
    
    ggplotly(p) # convert ggplot to plotly object
    
  })
  
  
  #-Incidence plot----------------------------------------------------------------
  output$inc_plot <- renderPlotly({
  
  # Add messages to inform user to select a province
  if (input$reporting_level == "Province") {
    validate(
      need(input$province_select != "", "Please select a province to view data.")
      )
  }
  
  # filter dataset if province level otherwise use the full dataset
  if(input$reporting_level == "Province"){
    df <- 
      filtered_data() %>% # filtr province data
      filter(var_name == "malaria_cases_per_1000") # select inc var
    
  }else{
    df <- app_data %>% 
      filter(var_name == "malaria_cases_per_1000") # select inc var
  }
  
  
  # title value 
  title_val = 
    ifelse(input$reporting_level == "Province", 
           paste("Malaria Incidence in:", input$province_select), 
           "Malaria Incidence at National Level")
  
  # Plot the incidence over time 
  p <- 
    ggplot(df, aes(x = period, y = value, col=organisation_unit)) +
    geom_line( ) +
    geom_point( ) +
    labs(
      title = title_val,
      x = "Month",
      y = "Incidence (per 1,000)"
    ) +
    theme_minimal()
  
  ggplotly(p)
})

# Incidence leaflet map----------------------------------------------------------------------
output$inc_map <- renderLeaflet({
  
  # Summarise incidence by province - no need to use filtered data as 
  # we want all provinces shown on the map
  df <- 
    app_data %>%
    filter(var_name == "malaria_cases_per_1000") %>% # filter to incidence data
    group_by(organisation_unit) %>%  # group by organisation unit
    summarise(inc = mean(value, na.rm = TRUE)) # calculate mean incidence per 1,000
  
  # Join data to spatial file
  df_shape <- left_join(adm1_shp, df, by = c("org_unit" = "organisation_unit"))
  
  # Create color palette 
  pal <- colorNumeric(
    palette = "YlOrRd",    # yellow orange red scale 
    domain = df_shape$inc, # use the incidence values for color mapping
    na.color = "#f0f0f0"   # set the NA value colour
  )
  
  # Determine which province (if any) is selected to highlight
  selected_province <- if (input$reporting_level == "Province" && input$province_select != "") {
    input$province_select
  } else {
    "national"
  }
  
  # Create the leaflet map 
  leaflet(df_shape) %>% # use the spatial data
    addProviderTiles(providers$CartoDB.Positron) %>% # adds base map layer
    addPolygons( 
      # add polygons for each province
      fillColor = ~pal(inc), # fill color based on incidence
      weight = ~ifelse(org_unit == selected_province, 3, 1), # highlight selected province with thicker border
      color = ~ifelse(org_unit == selected_province, "#7C204F", "black"), # set border color
      opacity = 1, # set opacity of the border
      fillOpacity = 0.8, # set fill opacity 
      label = ~paste0(org_unit, ": ", round(inc, 1), " per 1,000"), # add labels with province name and incidence
      highlightOptions = highlightOptions(
        # highlight options for interactivity
        weight = 3, # thicker border on hover
        color = "#333", # darker border on hover
        fillOpacity = 0.9, # fill opacity on hover
        bringToFront = TRUE # bring polygon to front on hover
      )
    ) |>
    # Add the color legend
    addLegend(
      pal = pal,
      values = df_shape$inc,
      title = "Avg Incidence (per 1,000)",
      position = "bottomright"
    )
})

#-Summary table of annual values-------------------------------------------------------------
output$summary_data <- renderDT({
  
  # Filter and summarise the app_data to create a summary table using all provinces 
  df <- 
    app_data %>%
    # Filter to relevant variables
    filter(var_name %in% c("malaria_cases", "malaria_tests", "malaria_cases_per_1000")) %>%
    # Pivot the data to wide format
    pivot_wider(
      names_from = var_name,
      values_from = value,
    ) %>% 
    # Group by organisation unit (province)
    group_by(organisation_unit) %>% 
    # Calculate TPR and summarise the data
    mutate(tpr = round((malaria_cases / malaria_tests) * 100, 1)) %>% 
    # Summarise the data to get total cases, total tests, average TPR, and average incidence
    # And two new variables - reporting months for cases and tests
    summarise(
      total_cases = sum(malaria_cases, na.rm = TRUE),
      total_tests = sum(malaria_tests, na.rm=TRUE), 
      avg_tpr = round(mean(tpr, na.rm=TRUE), 1),
      avg_inc = round(mean(malaria_cases_per_1000, na.rm=TRUE),1), 
      reporting_months_cases = sum(!is.na(malaria_cases)),
      reporting_months_tests = sum(!is.na(malaria_tests))
    ) %>% 
    # Rename columns for nice table headings
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
    df, # Create a datatable from the summarised data
    rownames = FALSE,  # disable row names
    options = list(
      pageLength = 10, # number of rows per page
      dom = "tip",     # table controls
      autoWidth = TRUE # auto width for columns
    )
  )
  
  # Highlight selected province
  if (input$reporting_level == "Province" && input$province_select != "") {
    # Apply row styling to highlight the selected province
    selected <- input$province_select 
    # Use formatStyle to apply bold font weight to the selected province row
    dt <- dt %>%
      formatStyle(
        "Province",
        target = "row",
        fontWeight = styleEqual(selected, "bold")
      )
  }
  
  # Return the datatable object
  dt
})

#-Code for handling data downloads--------------------------------------------------------------

# Create a reactive data frame for the summary table to pull data from same logic as in the
# data rendering code
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
  # Define the filename for the downloaded file
  filename = function() { paste0("summary_data_", Sys.Date(), ".csv") }, # This will create a file name with the current date
  
  # Define the content of the file 
  content = function(file) {
    write.csv(summary_table_data(), file, row.names = FALSE) # Write the summary table data to a CSV file
  }
)

}


#-Run the application-----------------------------------------------------------
shinyApp(ui = ui, server = server)