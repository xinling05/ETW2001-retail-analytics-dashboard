# ETW2001 Assignment 2 - R Shiny Dashboard
# Author: 35664029 Xin Ling Chong

# Problem: Identify which U.S. Superstore markets should be prioritised for profitable growth
# using internal sales/profit data and external state-level GDP, population, and income data.

library(shiny)
library(tidyverse)
library(plotly)
library(scales)

# Load cleaned datasets
superstore <- read.csv("superstore_us_clean.csv", stringsAsFactors = FALSE)
state_economic <- read.csv("state_economic_clean.csv", stringsAsFactors = FALSE)

# Additional formatting
superstore <- superstore %>%
  mutate(
    Order_Date = as.Date(Order_Date),
    Ship_Date = as.Date(Ship_Date),
    Year = as.integer(Year),
    Profit_Margin = ifelse(Sales > 0, Profit / Sales, NA),
    Discount_Band = case_when(
      Discount == 0 ~ "No discount",
      Discount <= 0.20 ~ "0-20%",
      Discount <= 0.40 ~ "20-40%",
      TRUE ~ "40%+"
    )
  )

state_economic <- state_economic %>%
  mutate(
    Year = as.integer(Year),
    GDP_Billion = GDP_Million / 1000
  )

# User Interface
ui <- fluidPage(
  
  titlePanel("U.S. Superstore Market Opportunity Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 2,
      
      selectInput(
        inputId = "region",
        label = "Region",
        choices = c("All", sort(unique(superstore$Region)))
      ),
      
      selectInput(
        inputId = "category",
        label = "Category",
        choices = c("All", sort(unique(superstore$Category)))
      ),
      
      selectInput(
        inputId = "segment",
        label = "Segment",
        choices = c("All", sort(unique(superstore$Segment)))
      ),
      
      sliderInput(
        inputId = "year",
        label = "Year range",
        min = min(superstore$Year),
        max = max(superstore$Year),
        value = c(min(superstore$Year), max(superstore$Year)),
        sep = ""
      ),
      
      sliderInput(
        inputId = "discount",
        label = "Discount range",
        min = min(superstore$Discount),
        max = max(superstore$Discount),
        value = c(min(superstore$Discount), max(superstore$Discount)),
        step = 0.05
      ),
      
      selectInput(
        inputId = "state_metric",
        label = "State ranking metric",
        choices = c(
          "Sales",
          "Profit",
          "Profit_Margin",
          "Sales_per_100k_Population",
          "Sales_to_GDP_ppm"
        ),
        selected = "Sales"
      )
    ),
    
    mainPanel(
      width = 10,
      
      fluidRow(
        column(3, h4("Sales"), textOutput("kpi_sales")),
        column(3, h4("Profit"), textOutput("kpi_profit")),
        column(3, h4("Profit Margin"), textOutput("kpi_margin")),
        column(3, h4("Avg State GDP"), textOutput("kpi_gdp"))
      ),
      
      hr(),
      
      fluidRow(
        column(
          6,
          h4("Sales and Profit Trend"),
          plotlyOutput("trend_chart", height = "280px")
        ),
        column(
          6,
          h4("Top States by Selected Metric"),
          plotlyOutput("state_bar", height = "280px")
        )
      ),
      
      fluidRow(
        column(
          6,
          h4("Income vs Sales per 100k Population"),
          plotlyOutput("income_scatter", height = "280px")
        ),
        column(
          6,
          h4("Profit Margin Heatmap"),
          plotlyOutput("heatmap", height = "280px")
        )
      ),
      
      fluidRow(
        column(
          12,
          h4("Discount Band and Profit Margin"),
          plotlyOutput("discount_box", height = "300px")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Reactive filtered Superstore data
  filtered_orders <- reactive({
    
    data <- superstore %>%
      filter(
        Year >= input$year[1],
        Year <= input$year[2],
        Discount >= input$discount[1],
        Discount <= input$discount[2]
      )
    
    if (input$region != "All") {
      data <- data %>% filter(Region == input$region)
    }
    
    if (input$category != "All") {
      data <- data %>% filter(Category == input$category)
    }
    
    if (input$segment != "All") {
      data <- data %>% filter(Segment == input$segment)
    }
    
    data
  })
  
  # Reactive state-level summary joined with external data
  state_summary <- reactive({
    
    filtered_orders() %>%
      group_by(State, Year) %>%
      summarize(
        Sales = sum(Sales, na.rm = TRUE),
        Profit = sum(Profit, na.rm = TRUE),
        Orders = n_distinct(Order_ID),
        .groups = "drop"
      ) %>%
      left_join(state_economic, by = c("State", "Year")) %>%
      group_by(State) %>%
      summarize(
        Sales = sum(Sales, na.rm = TRUE),
        Profit = sum(Profit, na.rm = TRUE),
        Orders = sum(Orders, na.rm = TRUE),
        Profit_Margin = Profit / Sales,
        Per_Capita_Income = mean(Per_Capita_Income, na.rm = TRUE),
        Population = mean(Population, na.rm = TRUE),
        GDP_Million = mean(GDP_Million, na.rm = TRUE),
        Sales_per_100k_Population = Sales / Population * 100000,
        Sales_to_GDP_ppm = Sales / (GDP_Million * 1000000) * 1000000,
        .groups = "drop"
      )
  })
  
  # KPI outputs
  output$kpi_sales <- renderText({
    dollar(sum(filtered_orders()$Sales, na.rm = TRUE))
  })
  
  output$kpi_profit <- renderText({
    dollar(sum(filtered_orders()$Profit, na.rm = TRUE))
  })
  
  output$kpi_margin <- renderText({
    sales_total <- sum(filtered_orders()$Sales, na.rm = TRUE)
    profit_total <- sum(filtered_orders()$Profit, na.rm = TRUE)
    percent(profit_total / sales_total, accuracy = 0.1)
  })
  
  output$kpi_gdp <- renderText({
    selected_states <- unique(filtered_orders()$State)
    
    avg_gdp <- mean(
      state_economic$GDP_Million[state_economic$State %in% selected_states],
      na.rm = TRUE
    )
    
    dollar(avg_gdp * 1000000)
  })
  
  # Chart 1: Sales and profit trend
  output$trend_chart <- renderPlotly({
    
    trend <- filtered_orders() %>%
      group_by(Year) %>%
      summarize(
        Sales = sum(Sales, na.rm = TRUE),
        Profit = sum(Profit, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = c(Sales, Profit),
        names_to = "Metric",
        values_to = "Value"
      )
    
    p <- ggplot(trend, aes(x = Year, y = Value, color = Metric, group = Metric)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = dollar) +
      scale_x_continuous(breaks = sort(unique(trend$Year))) +
      labs(
        x = "Year",
        y = "Amount",
        color = "Metric"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Chart 2: Top states by selected metric
  output$state_bar <- renderPlotly({
    
    metric <- input$state_metric
    
    states <- state_summary() %>%
      arrange(desc(.data[[metric]])) %>%
      slice_head(n = 10)
    
    p <- ggplot(
      states,
      aes(
        x = reorder(State, .data[[metric]]),
        y = .data[[metric]],
        fill = Profit_Margin
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        x = "State",
        y = metric,
        fill = "Profit Margin"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Chart 3: Income vs sales per 100k population
  output$income_scatter <- renderPlotly({
    
    p <- ggplot(
      state_summary(),
      aes(
        x = Per_Capita_Income,
        y = Sales_per_100k_Population,
        size = Population,
        color = Profit_Margin,
        text = State
      )
    ) +
      geom_point(alpha = 0.75) +
      scale_x_continuous(labels = dollar) +
      scale_y_continuous(labels = dollar) +
      labs(
        x = "Per Capita Income",
        y = "Sales per 100k Population",
        size = "Population",
        color = "Profit Margin"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("text", "x", "y", "size", "color"))
  })
  
  # Chart 4: Profit margin heatmap
  output$heatmap <- renderPlotly({
    
    heat <- filtered_orders() %>%
      group_by(Region, Category) %>%
      summarize(
        Profit_Margin = sum(Profit, na.rm = TRUE) / sum(Sales, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(heat, aes(x = Category, y = Region, fill = Profit_Margin)) +
      geom_tile(color = "white") +
      scale_fill_continuous(labels = percent) +
      labs(
        x = "Category",
        y = "Region",
        fill = "Profit Margin"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Chart 5: Discount band and profit margin
  output$discount_box <- renderPlotly({
    
    box_data <- filtered_orders() %>%
      filter(!is.na(Profit_Margin), is.finite(Profit_Margin))
    
    p <- ggplot(
      box_data,
      aes(
        x = Discount_Band,
        y = Profit_Margin,
        fill = Discount_Band
      )
    ) +
      geom_boxplot(outlier.alpha = 0.2) +
      scale_y_continuous(labels = percent) +
      labs(
        x = "Discount Band",
        y = "Order Profit Margin"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)