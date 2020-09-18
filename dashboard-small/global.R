library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(shinythemes)
library(shinydashboard)
library(zoo)


#Load Data for a single id and rename for ease
load("../../smalldata.RData")
load("../../smalljobdata.RData")
datFlow = datFlow2
repFlow = repFlow2

# night start % night end
night_start <- 9 # 2 am
night_end <- 13 # 3 am

# function to compute the rolling mean and uses interpolation to impute NAs
inter_roll <- function(df, n_days = 7) {
  new <- df %>%
    mutate(value_roll = rollmean(value, n_days, na.pad = TRUE)) %>%
    select(-value)
  new$value_roll <- na.spline(new$value_roll, 1:nrow(new))
  return(new)
}

# All repair jobs
alljobs <- repFlow %>%
  select(-Region, -Area, -Area_reference) %>%
  gather("key", "value", -id) %>%
  na.omit()