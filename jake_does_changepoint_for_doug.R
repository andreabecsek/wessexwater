
library(tidyverse)
library(InspectChangepoint)

# # data processing
# load("~/Desktop/phd/industry lab WW/data/datFlow_clean.Rda")
# load("~/Desktop/phd/industry lab WW/data/all_gam_residuals.RData")
# load("~/Desktop/phd/industry lab WW/data/okay_ids.RData")
# 
# ## format datFlow dates
# datFlow <- datFlow %>%
#   mutate(date = as.Date(date))
# 
# ## create empty tibble
# datFlow_ss <- tibble()
# 
# ## compute minimum night flow (0200-0300)
# datFlow_ss <- datFlow %>% 
#   group_by(id, date) %>% 
#   filter(tod >= 8, tod <= 12) %>% 
#   summarise(mnf = min(y, na.rm=TRUE))
# 
# ## compute average daily flow (0600-2000)
# datFlow_ss <- inner_join(datFlow %>% 
#     group_by(id, date) %>% 
#     summarise(adf = mean(y, na.rm=TRUE)), datFlow_ss)
# 
# datFlow_ss_short <- datFlow_ss %>%
#   filter(id %in% okay_ids)
# 
# gam_res_adf_long <- gam_res_adf_long %>%
#   mutate(date = datFlow_ss_short$date, adf_res = residuals, residuals=NULL)
# 
# gam_res_mnf_long <- gam_res_mnf_long %>%
#   mutate(date = datFlow_ss_short$date, mnf_res = residuals, residuals=NULL)
# 
# datFlow_ss_short <- inner_join(inner_join(gam_res_adf_long, datFlow_ss_short), gam_res_mnf_long)
# 
# save(datFlow_ss_short, file="datFlow_ss_short.RData")


# load required data
load("~/Desktop/phd/industry lab WW/data/datFlow_ss_short.RData")

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

# below is an example using the function detect_changepoints
myid <- "AW330"
dat <- data <- datFlow_ss_short %>%
  filter(id == myid)

plot(dat$adf, type="l", col="red", ylim=c(0,1))
lines(dat$mnf, col="blue")
abline(v = detect_changepoints(data, myid=myid, inspect_thresh=c(15,15,1500,1500)))
