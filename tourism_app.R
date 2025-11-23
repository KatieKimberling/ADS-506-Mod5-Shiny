library(shiny)
library(fpp3)
library(gt)
library(bslib)

# prepare data once
data("tourism")
tourism_ag <- tourism |>
  group_by(Purpose) |>
  summarise(Trips = sum(Trips, na.rm = TRUE), .groups = "drop")

max_q <- max(tourism_ag$Quarter)
max_date <- as.Date(max_q)
train_default <- max_date - lubridate::years(1)

ui <- navbarPage(
  title = "Australian Tourism Forecast",
  theme = bs_theme(
    version = 4,
    bg = "#ffffff",
    fg = "#1a1a1a",
    primary = "#4285F4",   # Google/GAFA-like blue
    secondary = "#FF9900"  # Amazon-like orange accent
  ),
  tabPanel(
    "Visualize",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("purposes", "Purpose", choices = unique(tourism_ag$Purpose),
                    selected = unique(tourism_ag$Purpose), multiple = TRUE),
        dateInput("train_date", "Training cutoff (first date)", value = train_default),
        dateInput("horizon_end", "Horizon end (optional)", value = max_date),
        helpText("Train cutoff defaults to 1 year before last date; facets use free y-scales.")
      ),
      mainPanel(
        width = 9,
        plotOutput("ts_plot", height = "700px")
      )
    )
  ),
  tabPanel(
    "Model Building",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        checkboxInput("fit_models", "Fit models (ARIMA + ETS)", value = FALSE),
        checkboxInput("show_train_acc", "Show training accuracy table", value = TRUE),
        numericInput("h_quarters", "Forecast horizon (quarters)", value = 4, min = 1, max = 40)
      ),
      mainPanel(
        width = 9,
        verbatimTextOutput("model_specs"),
        gt_output("train_acc_gt"),
        gt_output("fc_acc_gt")
      )
    )
  ),
  tabPanel(
    "Forecasts",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        uiOutput("model_choice_ui"),
        selectInput("purposes_fc", "Purpose (choose one or more)", choices = unique(tourism_ag$Purpose),
                    selected = unique(tourism_ag$Purpose)[1], multiple = TRUE),
        actionButton("refresh_fc", "Refresh forecasts")
      ),
      mainPanel(
        width = 9,
        plotOutput("fc_plot", height = "700px"),
        gt_output("fc_table")
      )
    )
  )
)

server <- function(input, output, session) {
  # helper: convert date -> yearquarter (use start of quarter)
  date_to_yq <- function(d) yearquarter(as.Date(d))

  filtered_data <- reactive({
    req(input$purposes)
    tourism_ag |>
      filter(Purpose %in% input$purposes)
  })

  # Plot time series with vline at cutoff and free y-scales
  output$ts_plot <- renderPlot({
    req(filtered_data())
    train_yq <- date_to_yq(input$train_date)
    filtered_data() |>
      autoplot(Trips) +
      geom_vline(xintercept = as.Date(train_yq), linetype = "dashed", color = "red", linewidth = 0.6) +
      labs(y = "Trips", title = "Tourism data") +
      facet_wrap(vars(Purpose), ncol = 1, scales = "free_y") +
      theme_minimal()
  })

  # Reactive training set used by modeling
  trn_data <- reactive({
    req(input$train_date)
    cutoff <- date_to_yq(input$train_date)
    tourism_ag |>
      filter(Quarter < cutoff)
  })

  # Fit models when requested
  fits <- eventReactive(input$fit_models, {
    req(trn_data())
    withProgress(message = "Fitting models", value = 0, {
      incProgress(0.3)
      m <- trn_data() |>
        model(
          arima = ARIMA(Trips),
          ets = ETS(Trips)
        )
      incProgress(0.7)
      m
    })
  }, ignoreNULL = FALSE)

  output$model_specs <- renderPrint({
    req(fits())
    fits()
  })

  output$train_acc_gt <- render_gt({
    req(fits())
    if (!input$show_train_acc) return(NULL)
    fits() |>
      accuracy() |>
      select(Purpose, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(columns = c(RMSE, MAE, MAPE), decimals = 2)
  })

  # Forecasts (using numeric quarters horizon)
  fc <- reactive({
    req(fits())
    h_q <- as.integer(input$h_quarters)
    fits() |>
      forecast(h = paste(h_q, "quarters"))
  })

  output$fc_acc_gt <- render_gt({
    req(fc())
    fc() |>
      accuracy(tourism_ag) |>
      select(Purpose, .model, RMSE, MAE, MAPE) |>
      arrange(.model, MAPE) |>
      gt() |>
      fmt_number(decimals = 2)
  })

  # model choice UI populated after fits
  output$model_choice_ui <- renderUI({
    req(fits())
    models <- unique(fits()$.model)
    selectInput("model_choice", "Model", choices = models, selected = models[1])
  })

  # Forecast plot
  output$fc_plot <- renderPlot({
    req(fc(), input$model_choice)
    # show recent training history + forecasts; choose trn_start 2 years prior to cutoff
    cutoff_yq <- date_to_yq(input$train_date)
    trn_start <- yearquarter(cutoff_yq |> as.Date() - years(2))
    # filter displayed data by selected purposes
    display_data <- tourism_ag |>
      filter(Purpose %in% input$purposes_fc)
    fc() |>
      filter(.model == input$model_choice, Purpose %in% input$purposes_fc) |>
      autoplot(display_data |> filter(Quarter >= trn_start)) +
      labs(y = "Trips", title = paste("Forecasts —", input$model_choice)) +
      facet_grid(Purpose ~ .model, scales = "free_y") +
      geom_vline(xintercept = as.Date(cutoff_yq), linetype = "dashed", color = "red", linewidth = 0.6) +
      theme_minimal()
  })

  # Forecast table (gt)
  output$fc_table <- render_gt({
    req(fc(), input$model_choice)
    fc() |>
      filter(.model == input$model_choice, Purpose %in% input$purposes_fc) |>
      as_tibble() |>
      select(Quarter, Purpose, .model, .mean) |>
      mutate(Quarter = as.character(Quarter)) |>
      gt() |>
      cols_label(.mean = "Forecast") |>
      fmt_number(columns = vars(.mean), decimals = 0)
  })
}

shinyApp(ui, server)