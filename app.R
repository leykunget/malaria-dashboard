# Load necessary libraries
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)
library(tidyverse)
library(readr)
library(plotly)
library(DT)
library(sf)           # For spatial features
library(leaflet)      # For interactive maps
library(scales)       # For number formatting
library(RColorBrewer) # For color palettes

# Load regional malaria data
malaria_final_reg <- read_rds("data/malaria_final_reg.rds")
merged_data <- read_rds("data/merged_data.rds")  # Load spatial data
merged_data2023 <- read_rds("data/merged_data2023.rds") # load merged data 2023

# Ensure spatial data is in WGS84 (EPSG:4326) for Leaflet
merged_data <- st_transform(merged_data, crs = 4326)
merged_data2023 <- st_transform(merged_data2023, crs = 4326)

# Ensure month is properly ordered as a factor
if(!is.factor(malaria_final_reg$month)) {
  malaria_final_reg$month <- factor(malaria_final_reg$month, levels = month.name)
}

# Automatically generate the national data
malaria_final_eth <- malaria_final_reg %>%
  group_by(year, month) %>%
  summarise(total_cases = sum(total_cases, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, match(month, month.name), "01", sep = "-")))

# Define UI for the Shiny app
ui <- page_navbar(
  title = "Malaria Forecasting Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Controls",
    selectInput("data_source", "Select Data Source:",
                choices = c("Regional Data" = "regional", 
                            "Ethiopia National Data" = "national"),
                selected = "regional"),
    conditionalPanel(
      condition = "input.data_source == 'regional'",
      selectInput("region", "Select Region:", 
                  choices = sort(unique(malaria_final_reg$region_name)))
    ),
    selectInput("forecast_period", "Forecast Period (months):",
                choices = c(6, 12, 24, 36), selected = 24),
    numericInput("confidence_level", "Confidence Level (%)",
                 value = 95, min = 50, max = 99, step = 5),
    # radioButtons("view_type", "Data View:",
    #              choices = c("Monthly" = "monthly", 
    #                          "Quarterly" = "quarterly", 
    #                          "Yearly" = "yearly"),
    #              selected = "monthly"),
    # hr(),
    helpText("This dashboard provides historical malaria data analysis and forecasting for Ethiopia.")
  ),
  
  nav_panel(
    title = "Dashboard",
    icon = bsicons::bs_icon("graph-up"),
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Total Cases",
        value = textOutput("total_cases"),
        showcase = bsicons::bs_icon("clipboard-data"),
        theme = value_box_theme(bg = "#007bff", fg = "white")
      ),
      value_box(
        title = "Average Monthly Cases",
        value = textOutput("avg_monthly_cases"),
        showcase = bsicons::bs_icon("calendar-month"),
        theme = value_box_theme(bg = "#17a2b8", fg = "white")
      ),
      value_box(
        title = "Trend",
        value = textOutput("trend_indicator"),
        showcase = bsicons::bs_icon("arrow-right"),
        theme = value_box_theme(bg = "#28a745", fg = "white")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Malaria Cases Over Time and Forecast"),
      plotlyOutput("forecast_plot", height = "500px")
    ),
    layout_columns(
      card(
        card_header("Seasonal Pattern"),
        plotOutput("seasonal_plot", height = "300px")
      ),
      card(
        card_header("Year-over-Year Comparison"),
        plotOutput("yoy_plot", height = "300px")
      )
    )
  ),
  
  nav_panel(
    title = "Forecast Details",
    icon = bsicons::bs_icon("table"),
    card(
      full_screen = TRUE,
      card_header("Forecasted Values"),
      DTOutput("forecast_table")
    ),
    card(
      card_header("Model Information"),
      verbatimTextOutput("model_info")
    )
  ),
  
  nav_panel(
    title = "Analysis",
    icon = bsicons::bs_icon("bar-chart"),
    card(
      full_screen = TRUE,
      card_header("Decomposition Analysis"),
      plotOutput("decomp_plot", height = "600px")
    )
  ),
  
  # Adding a new navigation panel for the regional monthly trends
  nav_panel(
    title = "Regional Trends",
    icon = bsicons::bs_icon("globe"),
    card(
      full_screen = TRUE,
      card_header("Monthly Trends by Region"),
      layout_sidebar(
        sidebar = sidebar(
          checkboxGroupInput(
            "regions_to_compare", 
            "Regions to Compare:", 
            choices = sort(unique(malaria_final_reg$region_name)),
            # selected = sample(unique(malaria_final_reg$region_name), min(5, length(unique(malaria_final_reg$region_name,
            selected = unique(malaria_final_reg$region_name) 
          ),
          selectInput(
            "year_filter", 
            "Filter by Year:", 
            choices = c("All Years", sort(unique(malaria_final_reg$year))),
            selected = "All Years"
          ),
          hr(),
          radioButtons(
            "scale_type", 
            "Y-axis Scale:",
            choices = c("Original" = "original", "Normalized" = "normalized"),
            selected = "original"
          ),
          helpText("Normalized view shows each region's trends as a percentage of their maximum monthly cases.")
        ),
        card(
          full_screen = TRUE,
          card_header("Monthly Trends by Region"),
          plotlyOutput("regional_trends_plot", height = "500px")
        ),
        card(
          full_screen = TRUE,
          card_header("Regional Comparison Statistics"),
          DTOutput("regional_stats_table")
        )
      )
    )
  ),
  
  # Navigation panel for regional distribution treemap
  nav_panel(
    title = "Case Distribution",
    icon = bsicons::bs_icon("pie-chart"),
    card(
      full_screen = TRUE,
      card_header("Malaria Cases Distribution by Region"),
      layout_sidebar(
        sidebar = sidebar(
          selectInput(
            "dist_year", 
            "Select Year:", 
            choices = sort(unique(malaria_final_reg$year)),
            selected = max(unique(malaria_final_reg$year))
          ),
          hr(),
          helpText("This treemap shows the distribution of malaria cases across different regions for the selected year.")
        ),
        plotlyOutput("distribution_treemap", height = "600px")
      )
    )
  ),
  
  # New navigation panel for Ethiopia National data
  nav_panel(
    title = "Ethiopia National Trends",
    icon = bsicons::bs_icon("flag"),
    card(
      full_screen = TRUE,
      card_header("National Malaria Trends for Ethiopia"),
      plotlyOutput("ethiopia_national_plot", height = "500px")
    ),
    layout_columns(
      card(
        card_header("National Seasonal Pattern"),
        plotOutput("ethiopia_seasonal_plot", height = "300px")
      ),
      card(
        card_header("National Year-over-Year Comparison"),
        plotOutput("ethiopia_yoy_plot", height = "300px")
      )
    )
  ),
  
  # NEW: Spatial Distribution navigation panel
  nav_panel(
    title = "Spatial Distribution",
    icon = bsicons::bs_icon("map"),
    layout_sidebar(
      sidebar = sidebar(
        radioButtons(
          "map_type",
          "Map Type:",
          choices = c(
            "Total Malaria Cases" = "total_cases",
            "Incidence per 1,000 (2023)" = "incidence_rate"
          ),
          selected = "total_cases"
        ),
        radioButtons(
          "map_view",
          "Map View:",
          choices = c(
            "Static Map" = "static",
            "Interactive Map" = "interactive"
          ),
          selected = "interactive"
        ),
        hr(),
        helpText("This spatial visualization shows the distribution of malaria cases across Ethiopian regions.")
      ),
      
      card(
        full_screen = TRUE,
        card_header(textOutput("map_title")),
        uiOutput("spatial_map_ui")
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  # Create a function to get the national data
  get_national_data <- reactive({
    # We're using the malaria_final_eth data created at the start
    national_data <- malaria_final_eth
    
    # Create a time series object for forecasting
    ts_data <- ts(national_data$total_cases, 
                  frequency = 12, 
                  start = c(min(national_data$year), 
                            which(month.name == as.character(national_data$month[which.min(national_data$date)]))))
    
    return(list(
      data = national_data,
      time_series = ts_data
    ))
  })
  
  # Reactive function to get the selected data
  selected_data <- reactive({
    if(input$data_source == "regional") {
      # Regional data
      data <- malaria_final_reg %>%
        filter(region_name == input$region) %>%
        arrange(year, month)
      
      # Create time series object
      ts_data <- ts(data$total_cases, 
                    frequency = 12, 
                    start = c(min(data$year), 
                              which(month.name == as.character(data$month[which.min(data$date)]))))
      
      return(list(
        data = data,
        time_series = ts_data
      ))
    } else {
      # National data (Ethiopia)
      return(get_national_data())
    }
  })
  
  # Reactive function to aggregate data based on view type
  aggregated_data <- reactive({
    data <- selected_data()$data
    
    if(input$view_type == "monthly") {
      return(data)
    } else if(input$view_type == "quarterly") {
      data %>%
        mutate(quarter = paste0(year, "-Q", quarter(date))) %>%
        group_by(quarter) %>%
        summarize(
          date = min(date),
          total_cases = sum(total_cases, na.rm = TRUE),
          year = first(year)
        )
    } else if(input$view_type == "yearly") {
      data %>%
        group_by(year) %>%
        summarize(
          date = min(date),
          total_cases = sum(total_cases, na.rm = TRUE)
        )
    }
  })
  
  # Reactive function to generate forecast
  forecast_result <- reactive({
    ts_data <- selected_data()$time_series
    fit <- auto.arima(ts_data)
    forecast(fit, h = as.numeric(input$forecast_period), level = c(input$confidence_level))
  })
  
  # Model information
  output$model_info <- renderPrint({
    result <- forecast_result()
    cat("ARIMA Model Information:\n\n")
    print(result$model)
    cat("\nAccuracy Metrics:\n")
    print(accuracy(result))
  })
  
  # Reactive function to create forecast data frame
  forecast_values <- reactive({
    result <- forecast_result()
    
    data <- selected_data()$data
    forecast_dates <- seq(max(data$date) + months(1), 
                          by = "month", 
                          length.out = as.numeric(input$forecast_period))
    
    data.frame(
      date = forecast_dates,
      forecasted_cases = as.numeric(result$mean),
      lower_ci = as.numeric(result$lower),
      upper_ci = as.numeric(result$upper),
      year = year(forecast_dates),
      month = month(forecast_dates, label = TRUE)
    )
  })
  
  # Summary statistics
  output$total_cases <- renderText({
    format(sum(selected_data()$data$total_cases, na.rm = TRUE), big.mark = ",")
  })
  
  output$avg_monthly_cases <- renderText({
    format(round(mean(selected_data()$data$total_cases, na.rm = TRUE), 1), big.mark = ",")
  })
  
  output$trend_indicator <- renderText({
    data <- selected_data()$data
    model <- lm(total_cases ~ as.numeric(date), data = data)
    coef <- coef(model)[2]
    
    if(coef > 0) {
      "Rising ▲"
    } else if(coef < 0) {
      "Falling ▼"
    } else {
      "Stable ◆"
    }
  })
  
  # Plot historical and forecasted data
  output$forecast_plot <- renderPlotly({
    historical <- selected_data()$data
    forecast <- forecast_values()
    
    title_text <- if(input$data_source == "regional") {
      paste("Malaria Cases and Forecast for", input$region)
    } else {
      "National Malaria Cases and Forecast for Ethiopia"
    }
    
    p <- ggplot() +
      # Historical data
      geom_line(data = historical, 
                aes(x = date, y = total_cases, color = "Historical"),
                size = 1) +
      # Forecast line
      geom_line(data = forecast, 
                aes(x = date, y = forecasted_cases, color = "Forecast"),
                size = 1) +
      # Confidence interval
      geom_ribbon(data = forecast,
                  aes(x = date, ymin = lower_ci, ymax = upper_ci),
                  fill = "blue", alpha = 0.2) +
      scale_color_manual(values = c("Historical" = "#007bff", "Forecast" = "#dc3545")) +
      labs(title = title_text,
           x = "Date",
           y = "Number of Cases",
           color = "Data Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })
  
  # Seasonal pattern plot
  output$seasonal_plot <- renderPlot({
    data <- selected_data()$data
    
    data %>%
      group_by(month) %>%
      summarize(avg_cases = mean(total_cases, na.rm = TRUE)) %>%
      ggplot(aes(x = month, y = avg_cases, group = 1)) +
      geom_line(color = "#007bff", size = 1) +
      geom_point(color = "#007bff", size = 3) +
      labs(title = "Seasonal Pattern of Malaria Cases",
           x = "Month",
           y = "Average Cases") +
      theme_minimal()
  })
  
  # Year-over-year comparison
  output$yoy_plot <- renderPlot({
    selected_data()$data %>%
      ggplot(aes(x = month, y = total_cases, color = as.factor(year), group = year)) +
      geom_line() +
      geom_point() +
      labs(title = "Year-over-Year Comparison",
           x = "Month",
           y = "Total Cases",
           color = "Year") +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  # Decomposition plot
  output$decomp_plot <- renderPlot({
    ts_data <- selected_data()$time_series
    
    # Check if the time series has enough data points
    if(length(ts_data) >= 24) {  # Minimum 2 years for seasonal decomposition
      decomp <- decompose(ts_data)
      
      par(mfrow = c(4, 1))
      plot(decomp$x, main = "Observed", xlab = "")
      plot(decomp$trend, main = "Trend", xlab = "")
      plot(decomp$seasonal, main = "Seasonal", xlab = "")
      plot(decomp$random, main = "Random", xlab = "")
    } else {
      # If not enough data, show a message
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Not enough data points for decomposition.\nAt least 2 years of data required.", cex = 1.5)
    }
  })
  
  # Display forecasted values in a table
  output$forecast_table <- renderDT({
    forecast_df <- forecast_values() %>%
      mutate(
        date = format(date, "%b %Y"),
        forecasted_cases = round(forecasted_cases, 1),
        lower_ci = round(lower_ci, 1),
        upper_ci = round(upper_ci, 1)
      ) %>%
      select(date, forecasted_cases, lower_ci, upper_ci)
    
    colnames(forecast_df) <- c("Date", "Forecasted Cases", 
                               paste0("Lower ", input$confidence_level, "% CI"), 
                               paste0("Upper ", input$confidence_level, "% CI"))
    
    datatable(
      forecast_df,
      options = list(
        pageLength = 12,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      rownames = FALSE,
      extensions = 'Buttons'
    )
  })
  
  # Regional trends plot
  output$regional_trends_plot <- renderPlotly({
    # Filter data based on selected regions and year
    filtered_data <- malaria_final_reg %>%
      filter(region_name %in% input$regions_to_compare)
    
    if(input$year_filter != "All Years") {
      filtered_data <- filtered_data %>%
        filter(year == as.numeric(input$year_filter))
    }
    
    # Normalize if selected
    if(input$scale_type == "normalized") {
      filtered_data <- filtered_data %>%
        group_by(region_name) %>%
        mutate(total_cases = total_cases / max(total_cases, na.rm = TRUE) * 100) %>%
        ungroup()
      
      y_title <- "Normalized Cases (%)"
    } else {
      y_title <- "Number of Cases"
    }
    
    # Create plot
    p <- plot_ly() 
    
    # Add a line for each region
    for(region in unique(filtered_data$region_name)) {
      region_data <- filtered_data %>% 
        filter(region_name == region) %>%
        arrange(date)
      
      p <- p %>% add_trace(
        data = region_data,
        x = ~date,
        y = ~total_cases,
        type = "scatter",
        mode = "lines+markers",
        name = region,
        marker = list(size = 3)
      )
    }
    
    # Add layout
    p %>% layout(
      title = paste("Monthly Trends by Region", 
                    ifelse(input$year_filter != "All Years", 
                           paste("(", input$year_filter, ")", sep=""), "")),
      xaxis = list(title = "Date"),
      yaxis = list(title = y_title),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.2)
    )
  })
  
  # Regional comparison statistics table
  output$regional_stats_table <- renderDT({
    # Reuse the same filtering logic from the plot
    filtered_data <- malaria_final_reg %>%
      filter(region_name %in% input$regions_to_compare)
    
    if(input$year_filter != "All Years") {
      filtered_data <- filtered_data %>%
        filter(year == as.numeric(input$year_filter))
    }
    
    # Calculate statistics
    data <- filtered_data %>%
      group_by(region_name) %>%
      summarize(
        Total_Cases = sum(total_cases, na.rm = TRUE),
        Average_Monthly = round(mean(total_cases, na.rm = TRUE), 1),
        Maximum_Month = month.name[which.max(tapply(total_cases, match(month, month.name), mean, na.rm = TRUE))],
        Minimum_Month = month.name[which.min(tapply(total_cases, match(month, month.name), mean, na.rm = TRUE))],
        Seasonality_Index = round(sd(tapply(total_cases, match(month, month.name), mean, na.rm = TRUE), na.rm = TRUE) / 
                                    mean(total_cases, na.rm = TRUE), 2)
      ) %>%
      arrange(desc(Total_Cases))
    
    # Handle the case when there's no data
    if(nrow(data) == 0) {
      return(datatable(
        data.frame(Message = "No data available for the selected filters"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Format the table
    datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'lftip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      caption = paste("Regional Comparison Statistics", 
                      if(input$year_filter != "All Years") paste("for", input$year_filter) else "for All Years")
    ) %>%
      formatStyle(
        'Seasonality_Index',
        background = styleColorBar(c(0, max(data$Seasonality_Index, na.rm = TRUE)), '#d9f0d3'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Total_Cases',
        background = styleColorBar(c(0, max(data$Total_Cases, na.rm = TRUE)), '#f5b7b1'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatCurrency('Total_Cases', currency = "", interval = 3, mark = ",", digits = 0) %>%
      formatCurrency('Average_Monthly', currency = "", interval = 3, mark = ",", digits = 1)
  })
  
  # Reactive data for the distribution treemap
  distribution_data <- reactive({
    # Filter data for the selected year
    malaria_final_reg %>%
      filter(year == input$dist_year) %>%
      group_by(region_name) %>%
      summarize(case_count = sum(total_cases, na.rm = TRUE)) %>%
      mutate(label = paste0(region_name, "\n", format(case_count, big.mark = ","))) %>%
      arrange(desc(case_count))
  })
  
  # Distribution treemap plot
  output$distribution_treemap <- renderPlotly({
    data <- distribution_data()
    
    plot_ly(
      data = data,
      type = "treemap",
      labels = ~region_name,
      parents = rep("", nrow(data)),
      values = ~case_count,
      text = ~label,
      textinfo = "label+value+percent",
      hoverinfo = "text",
      marker = list(
        colorscale = "Viridis",
        colors = ~case_count
      )
    ) %>%
      layout(
        title = paste0("Malaria Cases Distribution by Region (", input$dist_year, ")")
      )
  })
  
  # Ethiopia national time series plot
  output$ethiopia_national_plot <- renderPlotly({
    # Get the Ethiopia national data
    ethiopia_data <- get_national_data()$data
    ethiopia_ts <- get_national_data()$time_series
    
    # Create forecast for Ethiopia national data
    ts_fit <- auto.arima(ethiopia_ts)
    ts_forecast <- forecast(ts_fit, h = as.numeric(input$forecast_period), level = c(input$confidence_level))
    
    # Create forecast dataframe
    forecast_dates <- seq(max(ethiopia_data$date) + months(1), 
                          by = "month", 
                          length.out = as.numeric(input$forecast_period))
    
    forecast_df <- data.frame(
      date = forecast_dates,
      forecasted_cases = as.numeric(ts_forecast$mean),
      lower_ci = as.numeric(ts_forecast$lower),
      upper_ci = as.numeric(ts_forecast$upper)
    )
    
    p <- ggplot() +
      # Historical data
      geom_line(data = ethiopia_data, 
                aes(x = date, y = total_cases, color = "Historical"),
                size = 1) +
      # Forecast line
      geom_line(data = forecast_df, 
                aes(x = date, y = forecasted_cases, color = "Forecast"),
                size = 1) +
      # Confidence interval
      geom_ribbon(data = forecast_df,
                  aes(x = date, ymin = lower_ci, ymax = upper_ci),
                  fill = "blue", alpha = 0.2) +
      scale_color_manual(values = c("Historical" = "#007bff", "Forecast" = "#dc3545")) +
      labs(title = "National Malaria Cases and Forecast for Ethiopia",
           x = "Date",
           y = "Number of Cases",
           color = "Data Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })
  
  # Ethiopia national seasonal pattern
  output$ethiopia_seasonal_plot <- renderPlot({
    # Get national data
    ethiopia_data <- get_national_data()$data
    
    # Calculate seasonal pattern
    ethiopia_data %>%
      group_by(month) %>%
      summarize(avg_cases = mean(total_cases, na.rm = TRUE)) %>%
      ggplot(aes(x = month, y = avg_cases, group = 1)) +
      geom_line(color = "#007bff", size = 1) +
      geom_point(color = "#007bff", size = 3) +
      labs(title = "National Seasonal Pattern of Malaria Cases",
           x = "Month",
           y = "Average Cases") +
      theme_minimal()
  })
  
  # Ethiopia national YoY comparison
  output$ethiopia_yoy_plot <- renderPlot({
    # Get national data
    ethiopia_data <- get_national_data()$data
    
    # Year-over-year comparison
    ethiopia_data %>%
      ggplot(aes(x = month, y = total_cases, color = as.factor(year), group = year)) +
      geom_line() +
      geom_point() +
      labs(title = "National Year-over-Year Comparison",
           x = "Month",
           y = "Total Cases",
           color = "Year") +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  # NEW: Spatial visualization outputs
  # Map title based on selection
  output$map_title <- renderText({
    if(input$map_type == "total_cases") {
      "Spatial Distribution of Malaria Cases Across Ethiopian Regions"
    } else {
      "Malaria Incidence Rates Across Ethiopian Regions (2023)"
    }
  })
  
  # Spatial map UI output
  output$spatial_map_ui <- renderUI({
    if(input$map_view == "interactive") {
      leafletOutput("interactive_map", height = "600px")
    } else {
      plotOutput("static_map", height = "600px")
    }
  })
  
  # Static map output
  output$static_map <- renderPlot({
    if(input$map_type == "total_cases") {
      # Calculate centroids for labels
      centroids <- st_centroid(merged_data)
      
      # Total cases map
      ggplot(merged_data) +
        geom_sf(aes(fill = total_cases), color = "white", size = 0.2, alpha = 0.9) +
        geom_sf_text(data = centroids, 
                     aes(label = region_name), 
                     size = 3.5,
                     color = "black", 
                     fontface = "bold",
                     check_overlap = TRUE) +
        scale_fill_distiller(
          palette = "YlOrRd",
          direction = 1,
          name = "Total Malaria Cases",
          labels = scales::comma
        ) +
        labs(
          title = "Spatial Variation of Malaria Cases Across Ethiopian Regions"
        ) +
        theme_void() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5),
          plot.caption = element_text(size = 10, hjust = 1),
          legend.position = c(0.85, 0.85),
          legend.justification = c(1, 1),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        )
    } else {
      # Calculate centroids for incidence map
      centroids <- st_centroid(merged_data2023)
      
      # Incidence rate map
      ggplot(merged_data2023) +
        geom_sf(aes(fill = incidence_per_1000), 
                color = "gray40",
                size = 0.2, 
                alpha = 0.9) +
        geom_sf_text(data = centroids,
                     aes(label = paste0(region_name, "\n", round(incidence_per_1000, 1))),
                     size = 3.2, color = "black", fontface = "bold") +
        scale_fill_distiller(
          palette = "YlOrRd",
          direction = 1,
          name = "Malaria Incidence\n(per 1,000 population)"
        ) +
        labs(
          title = "Spatial Variation of Malaria Incidence Across Ethiopian Regions (2023)"
        ) +
        theme_void() +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
          plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
          plot.caption = element_text(size = 9, hjust = 1, color = "gray40"),
          legend.position = c(0.85, 0.85),
          legend.justification = c(1, 1),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 11)
        )
    }
  })
  
  # Interactive map output
  output$interactive_map <- renderLeaflet({
    if(input$map_type == "total_cases") {
      # Color palette for total cases
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = merged_data$total_cases
      )
      
      # Create popup content for total cases
      popup_content <- paste0(
        "<strong>Region: </strong>", merged_data$region_name, "<br>",
        "<strong>Total Cases: </strong>", scales::comma(merged_data$total_cases)
      )
      
      # Create the interactive map for total cases
      leaflet(merged_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(total_cases),
          weight = 1,
          opacity = 1,
          color = "darkgray",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#667",
            dashArray = "",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          popup = popup_content,
          label = ~region_name
        ) %>%
        addLegend(
          pal = pal,
          values = ~total_cases,
          opacity = 0.7,
          title = "Total Malaria Cases",
          position = "topright",
          labFormat = labelFormat(big.mark = ",")
        )
    } else {
      # Color palette for incidence rate
      pal_inc <- colorNumeric(
        palette = "YlOrRd",
        domain = merged_data2023$incidence_per_1000
      )
      
      # Create popup content for incidence rate
      popup_content_inc <- paste0(
        "<strong>Region: </strong>", merged_data2023$region_name, "<br>",
        "<strong>Incidence per 1,000: </strong>", round(merged_data2023$incidence_per_1000, 2), "<br>"
        # "<strong>Incidence per 1,000: </strong>", round(merged_data2023$incidence_per_1000, 2), "<br>",
        # "<strong>Population: </strong>", scales::comma(merged_data2023$population), "<br>",
        # "<strong>Cases (2023): </strong>", scales::comma(merged_data2023$total_cases_2023)
      )
      
      # Create the interactive map for incidence rate
      leaflet(merged_data2023) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal_inc(incidence_per_1000),
          weight = 1,
          opacity = 1,
          color = "darkgray",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#667",
            dashArray = "",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          popup = popup_content_inc,
          label = ~region_name
        ) %>%
        addLegend(
          pal = pal_inc,
          values = ~incidence_per_1000,
          opacity = 0.7,
          title = "Incidence per 1,000",
          position = "topright"
        )
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)      