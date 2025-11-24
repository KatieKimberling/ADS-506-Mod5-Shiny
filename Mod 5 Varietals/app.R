library(shiny)
library(bslib)
library(fpp3)
library(fabletools)  
library(fable)        
library(gt)
library(readr)
library(urca)

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
        checkboxGroupInput(
          "models_to_run",
          "Models to run",
          choices = c("tslm", "ets", "arima"),
          selected = c("tslm", "ets", "arima")
        ),
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
      uiOutput("tbl_ets_spec"),
      uiOutput("tbl_arima_spec"),
      uiOutput("tbl_train_acc"),
      uiOutput("tbl_forecast_acc")
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
  
  # Tab 2: Fit models (dynamic based on input$models_to_run)
  fitted_models <- eventReactive(input$fit_models, {
    req(training_data())
    sel <- input$models_to_run
    if (is.null(sel) || length(sel) == 0) {
      showNotification("No models selected to fit.", type = "warning")
      return(NULL)
    }

    notif_id <- showNotification("Fitting models...", duration = NULL, type = "message")
    start_time <- Sys.time()
    on.exit({
      try(removeNotification(notif_id), silent = TRUE)
    }, add = TRUE)

    result <- tryCatch({
      withProgress(message = "Fitting models...", value = 0, {
        incProgress(0.25, detail = "Preparing specs...")
        # build named expressions for the selected models
        specs <- list()
        if ("tslm" %in% sel)  specs[["tslm"]]  <- rlang::expr(TSLM(Sales ~ trend() + season()))
        if ("ets" %in% sel)   specs[["ets"]]   <- rlang::expr(ETS(Sales))
        if ("arima" %in% sel) specs[["arima"]] <- rlang::expr(ARIMA(Sales))
        incProgress(0.3, detail = "Fitting models...")

        # Evaluate a programmatic call to model(data, !!!specs)
        data_to_fit <- training_data()
        model_call <- rlang::call2(quote(model), data_to_fit, !!!specs)
        eval(model_call, envir = environment())
      })
    }, error = function(e) {
      showNotification(paste("Model fitting failed:", conditionMessage(e)), type = "error", duration = 10)
      NULL
    })

    # --- diagnostic: report any NULL model list-elements per-model ---
    if (!is.null(result)) {
      failures <- list()
      for (m in intersect(c("tslm", "ets", "arima"), names(result))) {
        is_null <- vapply(result[[m]], function(x) is.null(x) || (length(x) == 0 && is.list(x)), logical(1))
        if (any(is_null)) failures[[m]] <- result$Varietal[is_null]
      }
      if (length(failures)) {
        msgs <- vapply(names(failures), function(nm) {
          paste0(nm, " failed for: ", paste(failures[[nm]], collapse = ", "))
        }, character(1))
        showNotification(paste(msgs, collapse = " | "), type = "error", duration = 20)
      }
    }

    result
  })

  # After fitting, update the Forecasts tab model selector to only show fitted models
  observeEvent(fitted_models(), {
    fm <- fitted_models()
    if (is.null(fm)) return()
    available <- intersect(names(fm), c("tslm", "ets", "arima"))
    if (length(available) == 0) return()
    updateSelectInput(session, "selected_model", choices = available, selected = available[1])
  })
  
  # Tab 2: Model specification output (full)
  output$out_model_spec <- renderPrint({
    if (input$fit_models == 0) return(NULL)
    req(fitted_models())
    if (!input$show_model_spec) return(NULL)
    fitted_models()
  })
  
  # Add forecasts() reactive (before any output that uses it)
  forecasts <- reactive({
    req(fitted_models())
    trn_end <- as.Date(train_cutoff_ym())
    h <- as.integer(input$forecast_horizon)
    if (is.na(h) || h < 1) h <- 12L

    # get first future month safely, then sequence by "month"
    first_future <- seq(from = trn_end, by = "month", length.out = 2)[2]
    future_dates <- seq(from = first_future, by = "month", length.out = h)

    new_data <- tidyr::crossing(
      Varietal = unique(wine_data$Varietal),
      Month = yearmonth(future_dates)
    ) |>
      as_tsibble(index = Month, key = Varietal)

    fitted_models() |>
      forecast(new_data = new_data)
  })

  # Render the gt tables as static HTML to avoid input bindings

  output$tbl_ets_spec <- renderUI({
    if (input$fit_models == 0) return(NULL)
    req(fitted_models())
    if (!input$show_model_spec) return(NULL)

    tryCatch({
      fm <- fitted_models()

      if (!"ets" %in% names(fm)) {
        return(htmltools::HTML('<div class="alert alert-warning">No ETS models found.</div>'))
      }

      tbl <- fm |>
        select(Varietal, ets) |>
        as_tibble()

      # format() each model object into a single string
      tbl$ets_spec <- vapply(tbl$ets, format, FUN.VALUE = character(1))
      tbl <- tbl |>
        select(Varietal, ets_spec)

      if (nrow(tbl) == 0L) {
        return(htmltools::HTML('<div class="alert alert-warning">ETS table is empty.</div>'))
      }

      htmltools::HTML(
        knitr::kable(tbl, format = "html", table.attr = 'class="table table-sm"')
      )
    }, error = function(e) {
      msg <- htmltools::htmlEscape(conditionMessage(e))
      htmltools::HTML(paste0('<div class="alert alert-danger">Error rendering ETS specs: ', msg, '</div>'))
    })
  })

  output$tbl_arima_spec <- renderUI({
    if (input$fit_models == 0) return(NULL)
    req(fitted_models())
    if (!input$show_model_spec) return(NULL)

    tryCatch({
      fm <- fitted_models()

      if (!"arima" %in% names(fm)) {
        return(htmltools::HTML('<div class="alert alert-warning">No ARIMA models found.</div>'))
      }

      tbl <- fm |>
        select(Varietal, arima) |>
        as_tibble()

      tbl$arima_spec <- vapply(tbl$arima, format, FUN.VALUE = character(1))
      tbl <- tbl |>
        select(Varietal, arima_spec)

      if (nrow(tbl) == 0L) {
        return(htmltools::HTML('<div class="alert alert-warning">ARIMA table is empty.</div>'))
      }

      htmltools::HTML(
        knitr::kable(tbl, format = "html", table.attr = 'class="table table-sm"')
      )
    }, error = function(e) {
      msg <- htmltools::htmlEscape(conditionMessage(e))
      htmltools::HTML(paste0('<div class="alert alert-danger">Error rendering ARIMA specs: ', msg, '</div>'))
    })
  })

  output$tbl_train_acc <- renderUI({
    if (input$fit_models == 0) return(NULL)
    req(fitted_models())
    if (!input$show_train_acc) return(NULL)

    tbl <- fitted_models() |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      mutate(across(c(RMSE, MAE, MAPE), ~ round(.x, 2))) |>
      as_tibble()

    htmltools::HTML(
      knitr::kable(tbl, format = "html", table.attr = 'class="table table-sm"')
    )
  })

  output$tbl_forecast_acc <- renderUI({
    if (input$fit_models == 0) return(NULL)
    req(forecasts())

    tbl <- forecasts() |>
      accuracy(wine_data) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      mutate(across(c(RMSE, MAE, MAPE), ~ round(.x, 2))) |>
      as_tibble()

    htmltools::HTML(
      knitr::kable(tbl, format = "html", table.attr = 'class="table table-sm"')
    )
  })

  # Tab 3: Forecast plot
  output$forecast_plot <- renderPlot({
    req(forecasts())
    req(input$selected_model)
    req(input$forecast_varietals)

    trn_start <- yearmonth(as.Date(train_cutoff_ym()) - lubridate::years(2))

    fc <- forecasts()
    # if .model present, filter by it; otherwise assume single-model forecasts
    if (".model" %in% names(fc)) {
      fc <- fc |> filter(.model == input$selected_model)
    }

    fc |>
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