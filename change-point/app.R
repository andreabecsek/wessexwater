library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(shinythemes)
library(shinydashboard)
library(zoo)
library(DT)
library(InspectChangepoint)

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
      strong("Date range for meter"),
      start = "2017-01-01",
      end = "2020-01-26",
      min = "2017-01-01",
      max = "2020-01-26"
    ),
    
    dateRangeInput(
      inputId = "alerts_date",
      strong("Date range for alerts"),
      start = "2020-01-01",
      end = "2020-01-26",
      min = "2017-01-01",
      max = "2020-01-26"
    )

  ),

  # main body
  dashboardBody(
    fluidRow(
        box(
          title = "ADF and MNF for chosen meter",
          width = 7,
          status = "primary",
          plotOutput(outputId = "changepoints")
        ),
        valueBoxOutput("progressBox"),
        
          box(
          width = 4,
          title = "Alerts",
          DT::dataTableOutput(outputId = "table"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
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
      geom_line(aes(x = as.Date(date), y = mnf))+
      theme_minimal()+
      scale_color_manual(labels = c("psavert", "uempmed"), 
                         values = c("psavert"="#00ba38", "uempmed"="#f8766d"))
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
      geom_line(aes(x = as.Date(date), y = adf_res, color="ADF")) +
      geom_line(aes(x = as.Date(date), y = mnf_res, color="MNF")) +
      geom_vline(xintercept = dat_with_cp()$date[dat_with_cp()$is_cp == 1]) +
      labs(x="Date",y="Flow",color="")+
      theme_minimal()+
      scale_color_manual(values = c("ADF"="#00ba38", "MNF"="#f8766d"))
      #theme(legend.position = "bottom")
  })

  # # find all change points within given time range
  all_cps = reactive({
    detect_cps_within_dates(datFlow_ss_short,
                            as.Date(input$alerts_date[1]),
                            as.Date(input$alerts_date[2]))
  })

  
  # add table of alerts
  output$table = renderDataTable({
    datatable(all_cps()$alerts)
  })
  
  # number of alerts
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(as.character(dim(all_cps()$alerts)[1]), " Alerts"), paste0("Between ",as.character(input$alerts_date[1])," and ", as.character(input$alerts_date[2])), icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
}

# create shiny object
shinyApp(ui = ui, server = server)
