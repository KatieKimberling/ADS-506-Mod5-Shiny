library(shiny)
library(bslib)
library(fpp3)
library(gt)

# Prepare data
data("tourism")
tourism_data <- tourism |>
  group_by(Purpose) |>
  summarise(Trips = sum(Trips, na.rm = TRUE), .groups = "drop")

max_date <- max(tourism_data$Quarter) |> as.Date()
min_date <- min(tourism_data$Quarter) |> as.Date()
default_train_cutoff <- max_date - lubridate::years(1)

# UI
ui <- page_navbar(
  title = "Australian Tourism Forecast",
  theme = bs_theme(
    version = 5,
    preset = "bootstrap"
  ),
  
  # Tab 1: Visualize Data
  nav_panel(
    "Visualize Data",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        checkboxGroupInput(
          "purposes",
          "Select Purpose(s)",
          choices = unique(tourism_data$Purpose),
          selected = unique(tourism_data$Purpose)
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
        )
      ),
      plotOutput("viz_plot", height = "700px")
    )
  ),
  
  # Tab 2: Model Building
  nav_panel(
    "Model Building",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        actionButton("fit_models", "Fit Models", class = "btn-primary"),
        checkboxInput("show_model_spec", "Show Model Specification", value = FALSE),
        checkboxInput("show_train_acc", "Show Training Accuracy", value = FALSE),
        numericInput("forecast_horizon", "Forecast Horizon (quarters)", value = 4, min = 1, max = 20)
      ),
      verbatimTextOutput("model_spec"),
      gt_output("train_accuracy_table"),
      gt_output("forecast_accuracy_table")
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
          choices = c("arima", "ets"),
          selected = "arima"
        ),
        checkboxGroupInput(
          "forecast_purposes",
          "Select Purpose(s)",
          choices = unique(tourism_data$Purpose),
          selected = unique(tourism_data$Purpose)
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
    req(input$purposes)
    tourism_data |>
      filter(Purpose %in% input$purposes)
  })
  
  # Reactive: training cutoff as yearquarter
  train_cutoff_yq <- reactive({
    yearquarter(input$train_cutoff_date)
  })
  
  # Reactive: training data
  training_data <- reactive({
    tourism_data |>
      filter(Quarter < train_cutoff_yq())
  })
  
  # Tab 1: Visualization plot
  output$viz_plot <- renderPlot({
    req(filtered_data())
    
    filtered_data() |>
      autoplot(Trips) +
      geom_vline(
        xintercept = as.Date(train_cutoff_yq()),
        linetype = "dashed",
        color = "red",
        linewidth = 0.6
      ) +
      labs(y = "Trips", title = "Tourism Data") +
      facet_wrap(vars(Purpose), ncol = 1, scales = "free_y") +
      theme_minimal()
  })
  
  # Tab 2: Fit models
  fitted_models <- eventReactive(input$fit_models, {
    req(training_data())
    
    withProgress(message = "Fitting models...", {
      training_data() |>
        model(
          arima = ARIMA(Trips),
          ets = ETS(Trips)
        )
    })
  })
  
  # Tab 2: Model specification output
  output$model_spec <- renderPrint({
    req(input$show_model_spec)
    req(fitted_models())
    fitted_models()
  })
  
  # Tab 2: Training accuracy table
  output$train_accuracy_table <- render_gt({
    req(input$show_train_acc)
    req(fitted_models())
    
    fitted_models() |>
      accuracy() |>
      select(Purpose, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2) |>
      tab_header(title = "Training Accuracy")
  })
  
  # Tab 2: Generate forecasts
  forecasts <- reactive({
    req(fitted_models())
    req(input$forecast_horizon)
    
    fitted_models() |>
      forecast(h = paste(input$forecast_horizon, "quarters"))
  })
  
  # Tab 2: Forecast accuracy table
  output$forecast_accuracy_table <- render_gt({
    req(forecasts())
    
    forecasts() |>
      accuracy(tourism_data) |>
      select(Purpose, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2) |>
      tab_header(title = "Forecast Accuracy")
  })
  
  # Tab 3: Forecast plot
  output$forecast_plot <- renderPlot({
    req(forecasts())
    req(input$selected_model)
    req(input$forecast_purposes)
    
    trn_start <- yearquarter(as.Date(train_cutoff_yq()) - lubridate::years(2))
    
    forecasts() |>
      filter(.model == input$selected_model, Purpose %in% input$forecast_purposes) |>
      autoplot(tourism_data |> filter(Quarter >= trn_start)) +
      geom_vline(
        xintercept = as.Date(train_cutoff_yq()),
        linetype = "dashed",
        color = "red",
        linewidth = 0.6
      ) +
      labs(y = "Trips", title = "Tourism Data Forecasts") +
      facet_grid(Purpose ~ .model, scales = "free_y") +
      theme_minimal()
  })
}

shinyApp(ui, server)