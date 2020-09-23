library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(shinythemes)
library(shinydashboard)
library(zoo)


# Load Data for a single id and rename for ease
load("~/Documents/ww/datFlow_ss_short.RData")
# datFlow = datFlow2
# repFlow = repFlow2
# dat <- datFlow_ss %>%
#   filter(id == "AW070")

# # function to compute the rolling mean and uses interpolation to impute NAs
# inter_roll <- function(df, n_days = 7) {
#   new <- df %>%
#     mutate(value_roll = rollmean(value, n_days, na.pad = TRUE)) %>%
#     select(-value)
#   new$value_roll <- na.spline(new$value_roll, 1:nrow(new))
#   return(new)
# }
# 
# # All repair jobs
# alljobs <- repFlow %>%
#   select(-Region, -Area, -Area_reference) %>%
#   gather("key", "value", -id) %>%
#   na.omit()

# helper function for transformng series
compute_windowed_variance <- function(data, window_size){
  out <- numeric(length(data))
  
  for(i in 1:length(data)){
    window_ind <- seq(max(1,i-window_size/2), min(length(data), i+window_size/2))
    out[i] <- var(data[window_ind])
  }
  return(out)
}

# given a set of data (currently datFlow_ss, but Danny will create residuals ASAP), the ID of a time series,
# and a set of 4 thresholds, this function will output a set of times which contain changepoints
# you can detect 3 types of changepoint: changes in the mean of adf or mnf, changes in the trend, and
# you can also detect if adf and mnf are tracking eachother
detect_changepoints <- function(data, myid, var_window = 20, inspect_thresh, mean=TRUE, trend=TRUE, tracking=TRUE){
  data <- data %>% filter(id == myid)
  
  series <- list(
    adf_res = scale(data$adf_res),
    mnf_res = scale(data$mnf_res),
    transform1 = scale(compute_windowed_variance(data$adf_res-data$mnf_res, window_size = var_window)),
    transform2 = scale(diff(data$adf_res))
  )
  
  cp <- c()
  if(mean == TRUE){
    cp <- c(cp, inspect(series[[1]], thresh=inspect_thresh[1])$changepoints[,1])
    cp <- c(cp, inspect(series[[2]], thresh=inspect_thresh[2])$changepoints[,1])
  }
  if(tracking == TRUE){
    cp <- c(cp, inspect(series[[3]], thresh=inspect_thresh[3])$changepoints[,1])
  }
  if(trend==TRUE){
    cp <- c(cp, inspect(series[[4]], thresh=inspect_thresh[4])$changepoints[,1])
  }
  
  return(cp)
}



# function that takes a date range and computes all the change points within that range
detect_cps_within_dates = function(data, start_date, end_date){
  # keep only data from given date range
  reduced_df = data %>% filter((as.Date(date) >= as.Date(start_date) & as.Date(date) <= as.Date(end_date)))
  # create an empty matrix to store the detected change points for every meter
  dates = reduced_df$date %>% unique()
  ids = reduced_df$id %>% unique()
  cps = matrix(rep(0, length(ids)*length(dates)),nrow = length(dates))
  #compute change points
  i = 1
  for (id in ids) {
    cp = detect_changepoints(reduced_df, myid = id, inspect_thresh = c(10,10,2000,2000))
    cps[cp,i] = 1
    i = i+1
  }
  
  #obtain matrix indeces where there is a changepoint
  indices = which(cps==1, arr.ind = TRUE)
  alerts = data.frame(date=dates[indices[,1]],
                     id=ids[indices[,2]])
  return(list(cps=cps, alerts=alerts))
}
