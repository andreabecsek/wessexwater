
library(tidyr)
library(InspectChangepoint)

load("~/Documents/ww/datFlow_ss.RData")

# helper function for transforming series
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
    adf = scale(data$adf),
    mnf = scale(data$mnf),
    transform1 = scale(compute_windowed_variance(data$adf-data$mnf, window_size = var_window)),
    transform2 = scale(diff(data$adf))
  )
  
  cp <- c()
  if(mean == TRUE){
    cp <- c(cp, inspect(series[[1]], thresh=inspect_thresh[1])$changepoints[,1])
    cp <- c(cp, inspect(series[[2]], thresh=inspect_thresh[2])$changepoints[,1])
  }
  if(trend == TRUE){
    cp <- c(cp, inspect(series[[3]], thresh=inspect_thresh[3])$changepoints[,1])
  }
  if(tracking==TRUE){
    cp <- c(cp, inspect(series[[4]], thresh=inspect_thresh[4])$changepoints[,1])
  }
  
  return(cp)
}

# below is an example using the function detect_changepoints
dat <- datFlow_ss %>%
  filter(id == "AW070")

plot(dat$adf, type="l", col="red", ylim=c(0,0.5))
lines(dat$mnf, col="blue")
abline(v = detect_changepoints(dat, myid="AW070", inspect_thresh=c(10,10,2000,2005)))
