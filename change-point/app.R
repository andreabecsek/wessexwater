library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(shinythemes)
library(shinydashboard)
library(zoo)

# source global which contains helper functions, global variables and the data
source("./global.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Time series for given meter ID"),
  dashboardSidebar(
    # select meter id to be plotted
    selectInput(
      inputId = "meter_id",
      label = strong("Meter Id"),
      choices = unique(datFlow_ss_short$id),
      selected = "AW008"
    ),

    checkboxInput(
      inputId = "mean",
      label = "Mean",
      value = FALSE,
      width = NULL
    ),

    checkboxInput(
      inputId = "trend",
      label = "Trend",
      value = FALSE,
      width = NULL
    ),

    checkboxInput(
      inputId = "tracking",
      label = "Tracking",
      value = FALSE,
      width = NULL
    ),


    # thresholds
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

    dateRangeInput(
      inputId = "meter_date",
      strong("Date range"),
      start = "2017-01-01",
      end = "2020-01-26",
      min = "2017-01-01",
      max = "2020-01-26"
    ),
    
    dateRangeInput(
      inputId = "alerts_date",
      strong("Date range"),
      start = "2020-01-01",
      end = "2020-01-26",
      min = "2017-01-01",
      max = "2020-01-26"
    ),

    sliderInput("roll_mean_days",
      "Rolling Mean Window Size:",
      min = 1, max = 30,
      value = 7
    )
  ),

  # main body
  dashboardBody(
    fluidRow(
      column(
        width = 7,
        box(
          title = "ADF and MNF for chosen meter",
          width = NULL,
          status = "primary",
          plotOutput(outputId = "changepoints")
        ),
        box(
          title = "Alerts",
          tableOutput(outputId = "table")
        )
      )
    )
  )
)

# Define Server function
server <- function(input, output) {
  selected_series <- reactive({

    # validate input data
    req(input$meter_date)
    validate(need(
      !is.na(input$meter_date[1]) & !is.na(input$meter_date[2]),
      "Error: Please provide both a start and an end date."
    ))
    validate(need(
      input$meter_date[1] < input$meter_date[2],
      "Error: Start date should be earlier than end date."
    ))

    # filter data by given meter id and date range
    datFlow_ss_short %>%
      filter(id == input$meter_id) %>%
      filter(date >= as.Date(input$meter_date[1]) & date <= as.Date(input$meter_date[2]))
  })

  # # Compute MNF and ADF
  # mnf <- reactive({
  #     selected_series() %>%
  #         filter((night_start <= tod) & (tod <= night_end)) %>%
  #         group_by(id, date) %>%
  #         summarise(value = min(y)) %>%
  #         ungroup()
  # })
  #
  # adf <- reactive({
  #     selected_series() %>%
  #         filter((night_start > tod) | (tod > night_end)) %>%
  #         group_by(id, date) %>%
  #         summarise(value = mean(y)) %>%
  #         ungroup()
  # })

  # # get rolling means for mnf and adf
  # mnf_roll <- reactive({
  #     inter_roll(mnf(), input$roll_mean_days)
  # })
  #
  # adf_roll <- reactive({
  #     inter_roll(adf(), input$roll_mean_days)
  # })

  # # Put mnf and adf together
  # mnf_adf <- reactive({
  #     mnf_roll() %>%
  #         left_join(adf_roll(), by = c("id", "date"), suffix = c("_mnf", "_adf")) %>%
  #         transform(difference=value_roll_adf -  value_roll_mnf)
  # })
  #
  # # Put into long format
  # mnf_adf_1 <- reactive({
  #     mnf_adf() %>%
  #         gather("key", "y", -id, -date)
  # })

  # # start of jobs
  # job_starts <- reactive({
  #     alljobs %>%
  #         filter(id == input$meter_id) %>%
  #         filter(str_detect(key, "Start")) %>%
  #         filter(as.Date(value) >= as.Date(input$date[1]), as.Date(value) <= as.Date(input$date[2]))
  # })
  #
  # # end of jobs
  # job_ends <- reactive({
  #     alljobs %>%
  #         filter(id == input$meter_id) %>%
  #         filter(str_detect(key, "End")) %>%
  #         filter(value >= as.Date(input$date[1]), value <= as.Date(input$date[2]))
  # })


  # differences = reactive({
  #     data.frame(date = mnf_adf()$date,
  #                diff = c(NA, diff(mnf_adf()$difference, lag = 1)))
  # })
  #
  # output$differences = renderPlot({
  #     #plot(diff(mnf_adf()$difference, lag=1), type='l')
  #     ggplot(differences(), aes(x = as.Date(date), y= diff))+
  #         geom_line()
  # })

  # output$test = DT::renderDataTable({
  #     mnf_adf_1()
  # })

  # # create plot object that the plotOutput function is expecting
  # output$timeseries <- renderPlot({
  #     plot <- ggplot(mnf_adf_1(), aes(x = as.Date(date), y = y, color = key)) +
  #         geom_line() +
  #         theme_bw() +
  #         labs(x = "Date", y = "Flow")+
  #         theme(legend.position = "none")
  #
  #     if (nrow(job_starts())) {
  #         plot <- plot +
  #             geom_vline(xintercept = as.Date(job_starts()$value), lty = 2)
  #     }
  #     if (nrow(job_ends())) {
  #         plot <- plot + geom_vline(xintercept = as.Date(job_ends()$value), lty = 1)
  #     }
  #     plot
  # })

  output$timeseries <- renderPlot({
    ggplot(selected_series()) +
      geom_line(aes(x = as.Date(date), y = adf)) +
      geom_line(aes(x = as.Date(date), y = mnf))
  })

  # detect change points
  change_points <- reactive({
    detect_changepoints(selected_series(),
                        myid = input$meter_id,
      inspect_thresh = c(input$thresh1, input$thresh2, input$thresh3, input$thresh4),
      mean = input$mean,
      trend = input$trend,
      tracking = input$tracking
    )
  })

  # add column of detected change points
  dat_with_cp <- reactive({
    selected_series() %>%
      mutate(is_cp = ifelse(row_number() %in% change_points(), 1, 0))
  })

  # plot change points
  output$changepoints <- renderPlot({
    ggplot(dat_with_cp()) +
      geom_line(aes(x = as.Date(date), y = adf), color = "red") +
      geom_line(aes(x = as.Date(date), y = mnf), color = "blue") +
      geom_vline(xintercept = dat_with_cp()$date[dat_with_cp()$is_cp == 1]) +
      theme_minimal()
  })

  # # find all change points within given time range
  all_cps = reactive({
    detect_cps_within_dates(datFlow_ss_short,
                            as.Date(input$alerts_date[1]),
                            as.Date(input$alerts_date[2]))
  })

  
  # add table of alerts
  output$table = renderTable({
    data.frame(all_cps()$alerts)
  })
}

# create shiny object
shinyApp(ui = ui, server = server)
