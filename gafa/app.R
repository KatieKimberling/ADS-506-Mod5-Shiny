library(shiny)
library(bslib)
library(fpp3)
library(ggplot2)
library(dplyr)

# Load the data
data("gafa_stock")

# Get unique symbols and date range
stock_symbols <- unique(gafa_stock$Symbol)
date_range <- range(gafa_stock$Date)

ui <- page_sidebar(
  title = "GAFA Stock Analysis",
  sidebar = sidebar(
    checkboxGroupInput(
      "stocks",
      "Select Stocks:",
      choices = setNames(stock_symbols, stock_symbols),
      selected = stock_symbols[1:2]  # Default to first two stocks
    ),
    dateRangeInput(
      "date_range",
      "Select Date Range:",
      start = date_range[1],
      end = date_range[2],
      min = date_range[1],
      max = date_range[2]
    ),
    selectInput(
      "price_type",
      "Price Type:",
      choices = list(
        "Close" = "Close",
        "Open" = "Open",
        "High" = "High",
        "Low" = "Low"
      ),
      selected = "Close"
    )
  ),
  card(
    card_header("Stock Price Trends"),
    plotOutput("stock_plot", height = "600px")
  )
)

server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(input$stocks, input$date_range, input$price_type)
    
    gafa_stock %>%
      filter(
        Symbol %in% input$stocks,
        Date >= input$date_range[1],
        Date <= input$date_range[2]
      )
  })
  
  # Generate the faceted plot
  output$stock_plot <- renderPlot({
    req(filtered_data())
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      # Show message if no data available
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                label = "No data available for selected criteria", 
                size = 6) +
        theme_void()
    } else {
      # Create the faceted plot
      ggplot(data, aes(x = Date, y = .data[[input$price_type]])) +
        geom_line(color = "steelblue", size = 0.7) +
        facet_wrap(~Symbol, scales = "free_y") +
        labs(
          title = paste("GAFA Stock", input$price_type, "Prices"),
          subtitle = paste("From", input$date_range[1], "to", input$date_range[2]),
          x = "Date",
          y = paste(input$price_type, "Price ($)")
        ) +
        theme_minimal() +
        theme(
          strip.text = element_text(size = 12, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
  })
}

shinyApp(ui = ui, server = server)
