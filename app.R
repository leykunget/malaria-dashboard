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

# Load regional malaria data
malaria_final_reg <- read_rds("data/malaria_final_reg.rds")

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
        plotlyOutput("regional_trends_plot", height = "500px"),
        card(
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
  
  # Regional trends data preparation
  regional_trends_data <- reactive({
    data <- malaria_final_reg %>%
      filter(region_name %in% input$regions_to_compare)
    
    # Apply year filter if selected
    if(input$year_filter != "All Years") {
      data <- data %>% filter(year == input$year_filter)
    }
    
    # Apply normalization if requested
    if(input$scale_type == "normalized") {
      data <- data %>%
        group_by(region_name) %>%
        mutate(normalized_cases = total_cases / max(total_cases) * 100) %>%
        ungroup()
    }
    
    data
  })
  
  # Interactive monthly trends by region plot
  output$regional_trends_plot <- renderPlotly({
    data <- regional_trends_data()
    
    if(nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected filters") +
               theme_void())
    }
    
    y_var <- if(input$scale_type == "normalized") "normalized_cases" else "total_cases"
    y_label <- if(input$scale_type == "normalized") "Normalized Cases (%)" else "Total Cases"
    
    p <- data %>%
      ggplot(aes(x = month, y = !!sym(y_var), color = region_name, group = interaction(region_name, year))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(
        title = "Monthly Malaria Cases by Region",
        subtitle = if(input$year_filter != "All Years") paste("Year:", input$year_filter) else "All Years",
        x = "Month",
        y = y_label,
        color = "Region"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    if(input$year_filter != "All Years") {
      ggplotly(p) %>% layout(hovermode = "x unified")
    } else {
      # For all years, facet by year
      p <- p + facet_wrap(~year, scales = "free_y")
      ggplotly(p) %>% layout(hovermode = "x unified")
    }
  })
  
  # Regional comparison statistics table
  output$regional_stats_table <- renderDT({
    data <- regional_trends_data() %>%
      group_by(region_name) %>%
      summarize(
        Total_Cases = sum(total_cases, na.rm = TRUE),
        Average_Monthly = round(mean(total_cases, na.rm = TRUE), 1),
        Maximum_Month = month.name[which.max(tapply(total_cases, month, mean, na.rm = TRUE))],
        Minimum_Month = month.name[which.min(tapply(total_cases, month, mean, na.rm = TRUE))],
        Seasonality_Index = round(sd(tapply(total_cases, month, mean, na.rm = TRUE), na.rm = TRUE) / 
                                    mean(total_cases, na.rm = TRUE), 2)
      ) %>%
      arrange(desc(Total_Cases))
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'lftip'
      ),
      rownames = FALSE,
      caption = "Regional Comparison Statistics"
    ) %>%
      formatStyle(
        'Seasonality_Index',
        background = styleColorBar(c(0, max(data$Seasonality_Index)), '#d9f0d3'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Total_Cases',
        background = styleColorBar(c(0, max(data$Total_Cases)), '#f5b7b1'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
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
}

# Run the Shiny app
shinyApp(ui, server)      