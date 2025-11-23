library(shiny)
library(bslib)
library(fpp3)
library(gt)
library(readr)

# Read and prepare data
Aus_wine_raw <- read_csv(
  "AustralianWines.csv",
  col_types = cols(
    Month = col_character(),
    Fortified = col_double(),
    Red = col_double(),
    sparkling = col_double(),
    `Sweet white` = col_double(),
    `Dry white` = col_double(),
    Rose = col_double()
  ),
  na = c("", "NA", "*")
)

wine_data <- Aus_wine_raw |>
  mutate(Month = yearmonth(my(Month))) |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  as_tsibble(index = Month, key = Varietal)

# Get date range
min_date <- min(wine_data$Month) |> as.Date()
max_date <- max(wine_data$Month) |> as.Date()
default_train_cutoff <- max_date - lubridate::years(1)

# UI
ui <- page_navbar(
  title = "Australian Wine Varietals Forecast",
  theme = bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = "#4285F4",
    secondary = "#FF9900"
  ),
  
  # Tab 1: Visualize Data
  nav_panel(
    "Visualize Data",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        checkboxGroupInput(
          "varietals",
          "Select Varietal(s)",
          choices = unique(wine_data$Varietal),
          selected = unique(wine_data$Varietal)
        ),
        dateInput(
          "train_cutoff_date",
          "Training Cutoff Date",
          value = default_train_cutoff,
          min = min_date,
          max = max_date
        ),
        dateInput(
          "horizon_end_date",
          "Forecast Horizon End",
          value = max_date,
          min = min_date
        ),
        checkboxInput(
          "show_decomposition",
          "Show STL Decomposition (first varietal only)",
          value = FALSE
        )
      ),
      plotOutput("viz_plot", height = "600px"),
      plotOutput("decomp_plot", height = "500px")
    )
  ),
  
  # Tab 2: Model Building
  nav_panel(
    "Model Building",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        actionButton("fit_models", "Fit Models", class = "btn-primary"),
        hr(),
        checkboxInput("show_model_spec", "Show Model Specifications", value = FALSE),
        checkboxInput("show_train_acc", "Show Training Accuracy", value = TRUE),
        numericInput(
          "forecast_horizon",
          "Forecast Horizon (months)",
          value = 12,
          min = 1,
          max = 36
        )
      ),
      verbatimTextOutput("out_model_spec"),
      gt_output("out_ets_spec"),
      gt_output("out_arima_spec"),
      gt_output("out_train_acc"),
      gt_output("out_forecast_acc")
    )
  ),
  
  # Tab 3: Forecasts
  nav_panel(
    "Forecasts",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput(
          "selected_model",
          "Select Model",
          choices = c("tslm", "ets", "arima"),
          selected = "ets"
        ),
        checkboxGroupInput(
          "forecast_varietals",
          "Select Varietal(s)",
          choices = unique(wine_data$Varietal),
          selected = unique(wine_data$Varietal)
        ),
        sliderInput(
          "confidence_level",
          "Prediction Interval Levels (%)",
          min = 50,
          max = 99,
          value = c(80, 95),
          step = 5
        )
      ),
      plotOutput("forecast_plot", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: filtered data for visualization
  filtered_data <- reactive({
    req(input$varietals)
    wine_data |>
      filter(Varietal %in% input$varietals)
  })
  
  # Reactive: training cutoff as yearmonth
  train_cutoff_ym <- reactive({
    yearmonth(input$train_cutoff_date)
  })
  
  # Reactive: training data
  training_data <- reactive({
    wine_data |>
      filter(Month <= train_cutoff_ym())
  })
  
  # Reactive: validation data
  validation_data <- reactive({
    wine_data |>
      filter(Month > train_cutoff_ym())
  })
  
  # Tab 1: Visualization plot
  output$viz_plot <- renderPlot({
    req(filtered_data())
    
    filtered_data() |>
      autoplot(Sales) +
      geom_vline(
        xintercept = as.Date(train_cutoff_ym()),
        linetype = "dashed",
        color = "red",
        linewidth = 0.6
      ) +
      labs(y = "Sales (kL)", title = "Australian Wine Sales by Varietal") +
      facet_wrap(vars(Varietal), ncol = 1, scales = "free_y") +
      theme_minimal()
  })
  
  # Tab 1: STL Decomposition plot with interpolation
  output$decomp_plot <- renderPlot({
    req(input$show_decomposition)
    req(input$varietals)
    
    # Get the selected varietal data
    selected_varietal <- input$varietals[1]
    
    # Check if there are missing values before filtering
    varietal_data <- wine_data |>
      filter(Varietal == selected_varietal)
    
    has_missing <- any(is.na(varietal_data$Sales))
    
    # Fill gaps if needed
    if (has_missing) {
      varietal_data <- varietal_data |>
        fill_gaps() |>
        mutate(Sales = if_else(is.na(Sales), mean(Sales, na.rm = TRUE), Sales))
    }
    
    varietal_data |>
      model(stl = STL(Sales)) |>
      components() |>
      autoplot() +
      labs(
        title = paste("STL Decomposition:", selected_varietal, "Wine Sales"),
        subtitle = if(has_missing) "Note: Missing values were filled with mean" else NULL
      ) +
      theme_minimal()
  })
  
  # Tab 2: Fit models
  fitted_models <- eventReactive(input$fit_models, {
    req(training_data())
    
    withProgress(message = "Fitting models...", value = 0, {
      incProgress(0.3, detail = "Fitting TSLM...")
      incProgress(0.3, detail = "Fitting ETS...")
      incProgress(0.3, detail = "Fitting ARIMA...")
      
      training_data() |>
        model(
          tslm = TSLM(Sales ~ trend() + season()),
          ets = ETS(Sales),
          arima = ARIMA(Sales)
        )
    })
  })
  
  # Tab 2: Model specification output (full)
  output$out_model_spec <- renderPrint({
    if (input$fit_models == 0) return(NULL)
    req(fitted_models())
    if (!input$show_model_spec) return(NULL)
    fitted_models()
  })
  
  # Tab 2: ETS specifications table
  output$out_ets_spec <- render_gt({
    if (input$fit_models == 0) return(NULL)
    req(fitted_models())
    if (!input$show_model_spec) return(NULL)
    
    fitted_models() |>
      filter(.model == "ets") |>
      mutate(ets_spec = format(ets)) |>
      select(Varietal, ets_spec) |>
      as_tibble() |>
      gt() |>
      tab_header(title = "ETS Model Specifications") |>
      cols_label(ets_spec = "ETS Components")
  })
  
  # Tab 2: ARIMA specifications table
  output$out_arima_spec <- render_gt({
    if (input$fit_models == 0) return(NULL)
    req(fitted_models())
    if (!input$show_model_spec) return(NULL)
    
    fitted_models() |>
      filter(.model == "arima") |>
      mutate(arima_spec = format(arima)) |>
      select(Varietal, arima_spec) |>
      as_tibble() |>
      gt() |>
      tab_header(title = "ARIMA Model Specifications") |>
      cols_label(arima_spec = "ARIMA Orders")
  })
  
  # Tab 2: Training accuracy table
  output$out_train_acc <- render_gt({
    if (input$fit_models == 0) return(NULL)
    req(fitted_models())
    if (!input$show_train_acc) return(NULL)
    
    fitted_models() |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2) |>
      tab_header(title = "Training Accuracy by Model and Varietal")
  })
  
  # Tab 2: Generate forecasts
  forecasts <- reactive({
    req(fitted_models())
    req(input$forecast_horizon)
    
    fitted_models() |>
      forecast(h = input$forecast_horizon)
  })
  
  # Tab 2: Forecast accuracy table
  output$out_forecast_acc <- render_gt({
    if (input$fit_models == 0) return(NULL)
    req(forecasts())
    
    forecasts() |>
      accuracy(wine_data) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2) |>
      tab_header(title = "Validation Accuracy by Model and Varietal")
  })
  
  # Tab 3: Forecast plot
  output$forecast_plot <- renderPlot({
    req(forecasts())
    req(input$selected_model)
    req(input$forecast_varietals)
    
    trn_start <- yearmonth(as.Date(train_cutoff_ym()) - lubridate::years(2))
    
    forecasts() |>
      filter(.model == input$selected_model, Varietal %in% input$forecast_varietals) |>
      autoplot(
        wine_data |> filter(Month >= trn_start),
        level = input$confidence_level
      ) +
      geom_vline(
        xintercept = as.Date(train_cutoff_ym()),
        linetype = "dashed",
        color = "red",
        linewidth = 0.6
      ) +
      labs(
        y = "Sales (kL)",
        title = paste("Wine Sales Forecasts -", toupper(input$selected_model))
      ) +
      facet_grid(Varietal ~ .model, scales = "free_y") +
      theme_minimal()
  })
}

shinyApp(ui, server)