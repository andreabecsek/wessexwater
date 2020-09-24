# source global which contains helper functions, global variables and the data
source("./global.R")

# Define User Interface
ui <- dashboardPage(
  dashboardHeader(title = "Time series for given meter ID"),
  dashboardSidebar(
    checkboxGroupInput(
      "cp_types", "Type of change point:",
      c(
        "Mean" = "mean",
        "Trend" = "trend",
        "Tracking" = "tracking"
      )
    ),

    # Select meter id to be plotted
    selectInput(
      inputId = "meter_id",
      label = strong("Meter Id"),
      choices = unique(datFlow_ss_short$id),
      selected = "AW008"
    ),


    # Thresholds for the change point detection algorithm
    sliderInput(
      inputId = "thresh1",
      label = strong("Threshold 1"),
      min = 1, max = 30,
      value = 10
    ),

    sliderInput(
      inputId = "thresh2",
      label = strong("Threshold 2"),
      min = 1, max = 30,
      value = 10
    ),

    sliderInput(
      inputId = "thresh3",
      label = strong("Threshold 3"),
      min = 1000, max = 3000,
      value = 2000
    ),

    sliderInput(
      inputId = "thresh4",
      label = strong("Threshold 4"),
      min = 1000, max = 3000,
      value = 2000
    ),

    # Date range for the time series plot
    dateRangeInput(
      inputId = "meter_date",
      strong("Date range for meter"),
      start = "2017-01-01",
      end = "2020-01-26",
      min = "2017-01-01",
      max = "2020-01-26"
    ),

    # Date range for detecting change points
    dateRangeInput(
      inputId = "alerts_date",
      strong("Date range for alerts"),
      start = "2020-01-01",
      end = "2020-01-26",
      min = "2017-01-01",
      max = "2020-01-26"
    )
  ),

  # Main Body
  dashboardBody(
    fluidRow(

      # Box that contains the time series plot
      box(
        title = "ADF and MNF for chosen meter",
        width = 7,
        status = "primary",
        plotOutput(outputId = "main",
                   dblclick = "plotmain_dblclick",
                   brush = brushOpts(
                     id="plotmain_brush",
                     resetOnNew = TRUE
                   ))
      ),

      # Box with the number of detected alerts
      valueBoxOutput("progressBox"),

      # Box that contains the table of dates and meter ids for the detected
      # change points
      box(
        width = 4,
        title = "Alerts",
        DT::dataTableOutput(outputId = "table"),
        style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
      )
    )
  )
)

# Define Server function (this is where the data is processed and outputs
# are created)
server <- function(input, output) {
  selected_series <- reactive({

    # Validate input data
    req(input$meter_date)
    validate(need(
      !is.na(input$meter_date[1]) & !is.na(input$meter_date[2]),
      "Error: Please provide both a start and an end date."
    ))
    validate(need(
      input$meter_date[1] < input$meter_date[2],
      "Error: Start date should be earlier than end date."
    ))

    # Filter data by given meter id and date range
    datFlow_ss_short %>%
      filter(id == input$meter_id) %>%
      filter(date >= as.Date(input$meter_date[1]) & date <= as.Date(input$meter_date[2]))
  })

  # Based on input decide the types of change points to be detected
  detect_mean <- reactive({
    "mean" %in% input$cp_types
  })

  detect_trend <- reactive({
    "trend" %in% input$cp_types
  })

  detect_tracking <- reactive({
    "tracking" %in% input$cp_types
  })


  # Compute a list of job starts for given id and date range
  job_starts <- reactive({
    alljobs %>%
      filter(id == input$meter_id) %>%
      filter(str_detect(key, "Start")) %>%
      filter(
        as.Date(value) >= as.Date(input$meter_date[1]),
        as.Date(value) <= as.Date(input$meter_date[2])
      )
  })

  # Compute a list of job ends for given id and date range
  job_ends <- reactive({
    alljobs %>%
      filter(id == input$meter_id) %>%
      filter(str_detect(key, "End")) %>%
      filter(
        as.Date(value) >= as.Date(input$meter_date[1]),
        as.Date(value) <= as.Date(input$meter_date[2])
      )
  })

  # output$timeseries <- renderPlot({
  #   ggplot(selected_series()) +
  #     geom_line(aes(x = as.Date(date), y = adf)) +
  #     geom_line(aes(x = as.Date(date), y = mnf)) +
  #     theme_minimal() +
  #     scale_color_manual(
  #       labels = c("psavert", "uempmed"),
  #       values = c("psavert" = "#00ba38", "uempmed" = "#f8766d")
  #     )
  # })

  # Detect change points for selected meter id
  change_points <- reactive({
    detect_changepoints(selected_series(),
      myid = input$meter_id,
      inspect_thresh = c(
        input$thresh1,
        input$thresh2,
        input$thresh3,
        input$thresh4
      ),
      mean = detect_mean(),
      trend = detect_trend(),
      tracking = detect_tracking()
    )
  })

  # Add column of detected change points to make plotting easier
  dat_with_cp <- reactive({
    selected_series() %>%
      mutate(is_cp = ifelse(row_number() %in% change_points(), 1, 0))
  })
  
  # Playing around with linked plots
  ranges <- reactiveValues(x = NULL, y = NULL)
  

  # Create time series plot of adf, mnf, job dates and change points
  output$main <- renderPlot({
    plot <- ggplot() +
      geom_line(
        data = dat_with_cp(),
        aes(
          x = as.Date(date),
          y = adf_res,
          color = "ADF"
        )
      ) +
      geom_line(
        data = dat_with_cp(),
        aes(
          x = as.Date(date),
          y = mnf_res,
          color = "MNF"
        )
      ) +
      geom_vline(xintercept = dat_with_cp()$date[dat_with_cp()$is_cp == 1]) +
      labs(
        x = "Date",
        y = "Flow",
        color = ""
      ) +
      theme_minimal() +
      scale_color_manual(values = c(
        "ADF" = "#00ba38",
        "MNF" = "#f8766d"
      )) +
      #theme(aspect.ratio = 0.5)
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    
    
    y_min = plot$coordinates$limits$y[1]
    
    no_start_jobs = length(as.Date(job_starts()$value))

    # Add job start and end dates
    if (nrow(job_starts())) {
      plot <- plot +
        geom_vline(
          xintercept = as.Date(job_starts()$value),
          lty = 2, color = "red"
        )
        # geom_segment(aes(x=as.Date(job_starts()$value),
        #              xend=as.Date(job_starts()$value),
        #              y=rep(y_min,no_start_jobs),
        #              yend=rep(y_min,no_start_jobs)), 
        #              color='red')
    }
    if (nrow(job_ends())) {
      plot <- plot +
        geom_vline(
          xintercept = as.Date(job_ends()$value),
          lty = 2, color = "blue"
        )
    }
    plot
  })

  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plotmain_dblclick, {
    brush <- input$plotmain_brush
    if (!is.null(brush)) {
      ranges$x <- c(as.Date(brush$xmin, origin = "1970-01-01"), 
                    as.Date(brush$xmax, origin = "1970-01-01"))
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  # Find all change points within given time range
  all_cps <- reactive({
    detect_cps_within_dates(
      datFlow_ss_short,
      as.Date(input$alerts_date[1]),
      as.Date(input$alerts_date[2])
    )
  })


  # Add table of alerts with the detected change points
  output$table <- renderDataTable({
    datatable(all_cps()$alerts)
  })

  # Display the number of alerts
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(
        as.character(dim(all_cps()$alerts)[1]),
        " Alerts"
      ),
      paste0(
        "Between ",
        as.character(input$alerts_date[1]),
        " and ",
        as.character(input$alerts_date[2])
      ),
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
}

# Create shiny object
shinyApp(ui = ui, server = server)
