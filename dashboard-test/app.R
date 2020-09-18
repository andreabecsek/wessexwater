library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(shinythemes)
library(shinydashboard)
library(zoo)

#Load Data

load("../../datFlow.Rda")
load("../../repFlow.Rda")

#Functions to get mnf and based on the night start and end time
get_mnf <- function(df, night_start, night_end) {
    df <- df %>%
        filter((night_start <= tod) & (tod <= night_end)) %>%
        group_by(id, date) %>%
        summarise(mnf = min(y)) %>%
        ungroup()
    return(df)
}

get_adf <- function(df, night_start, night_end) {
    df <- df %>%
        filter((night_start > tod) | (tod > night_end)) %>%
        group_by(id, date) %>%
        summarise(adf = mean(y)) %>%
        ungroup()
    return(df)
}

### Compute 7 days rolling average for MNF

inter_roll <- function(df, n_days = 7) {
    new <- df %>%
        mutate(mnf_roll = rollmean(mnf, n_days, na.pad = TRUE)) %>%
        select(-mnf)
    new$mnf_roll <- na.spline(new$mnf_roll, 1:nrow(new))
    return(new)
}


#Define UI

ui <- dashboardPage(
    dashboardHeader(title = "Time series for given meter ID"),
    dashboardSidebar(
            # select meter id to be plotted
            selectInput(
                inputId = "meter_id",
                label = strong("Meter Id"),
                choices = unique(datFlow$id),
                selected = "AW008"
            ),
            
            dateRangeInput(
                inputId = "date",
                strong("Date range"),
                start = "2017-01-01",
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
                box(plotOutput(outputId = "timeseries"))
            )
        )
    
)

# Define Server function

server <- function(input, output) {
    selected_series <- reactive({
        
        # validate input data
        req(input$date)
        validate(need(
            !is.na(input$date[1]) & !is.na(input$date[2]),
            "Error: Please provide both a start and an end date."
        ))
        validate(need(
            input$date[1] < input$date[2],
            "Error: Start date should be earlier than end date."
        ))
        
        # filter data by given meter id and date range
        datFlow %>%
            filter(id == input$meter_id) %>%
            filter(date >= as.Date(input$date[1]) & date <= as.Date(input$date[2]))
    })
    
    # Compute MNF and ADF
    mnf <- reactive({
        selected_series() %>%
            filter((night_start <= tod) & (tod <= night_end)) %>%
            group_by(id, date) %>%
            summarise(value = min(y)) %>%
            ungroup()
    })
    
    adf <- reactive({
        selected_series() %>%
            filter((night_start > tod) | (tod > night_end)) %>%
            group_by(id, date) %>%
            summarise(value = mean(y)) %>%
            ungroup()
    })
    
    # get rolling means for mnf and adf
    mnf_roll <- reactive({
        inter_roll(mnf(), input$roll_mean_days)
    })
    
    adf_roll <- reactive({
        inter_roll(adf(), input$roll_mean_days)
    })
    
    # Put mnf and adf together
    mnf_adf <- reactive({
        mnf_roll() %>%
            left_join(adf_roll(), by = c("id", "date"), suffix = c("_mnf", "_adf"))
    })
    
    # Put into long format
    mnf_adf_1 <- reactive({
        mnf_adf() %>%
            gather("key", "y", -id, -date)
    })
    
    # start of jobs
    job_starts <- reactive({
        alljobs %>%
            filter(id == input$meter_id) %>%
            filter(str_detect(key, "Start")) %>%
            filter(as.Date(value) >= as.Date(input$date[1]), as.Date(value) <= as.Date(input$date[2]))
    })
    
    # end of jobs
    job_ends <- reactive({
        alljobs %>%
            filter(id == input$meter_id) %>%
            filter(str_detect(key, "End")) %>%
            filter(value >= as.Date(input$date[1]), value <= as.Date(input$date[2]))
    })
    
    
    # create plot object that the plotOutput function is expecting
    output$timeseries <- renderPlot({
        plot <- ggplot(mnf_adf_1(), aes(x = date, y = y, color = key)) +
            geom_line() +
            theme_bw() +
            labs(x = "Date", y = "Flow")
        
        # if (nrow(job_starts())) {
        #     plot <- plot +
        #         geom_vline(xintercept = as.Date(job_starts()$value), lty = 2)
        # }
        # if (nrow(job_ends())) {
        #     plot <- plot + geom_vline(xintercept = as.Date(job_ends()$value), lty = 1)
        # }
        plot
    })
}

# create shiny object
shinyApp(ui = ui, server = server)
