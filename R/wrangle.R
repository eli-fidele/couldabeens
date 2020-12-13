# Insert single-use wrangling scripts/functions here

#==========================================================
#                     COUNT RETIREES
#==========================================================

# Yields the sum of retirees in two datasets (pitchers, position commonly)
total_retirees_by_yr <- function(pitchers, position){
  year_count_pitchers <- retirees_by_yr(pitchers)$retirees
  year_count_position <- retirees_by_yr(position)$retirees
  data.frame(Year = 1969:2018, retirees = year_count_position + year_count_pitchers)
}

# Counts the retirees in a single dataset
retirees_by_yr <- function(dataset){
  year_count <- dataset %>%
    group_by(Year) %>%
    summarize(retirees = n())
}

# Gets number of retirees from retirees dataset (if being used)
get_retirees <- function(dataset){
  colnames(dataset)[5] <- "retirees"
  dataset %>% select(Year, retirees)
}

#==========================================================
#                 ELEMENTARY WRANGLING
#==========================================================

# Filters year to be in the post-rule era
postrule <- function(dataset, rule_year = 2002){
  dataset %>% filter(Year > rule_year)
}

# Filters year to be in the pre-rule era
prerule <- function(dataset, rule_year = 2002){
  dataset %>% filter(Year <= rule_year)
}

# select appopriate columns from player datasets
wrangle_init <- function(dataset){
  colnames(dataset)[3] <- "WAR"
  dataset %>% select(WAR, Year)
}

#==========================================================
#                 THRESHOLD SMOOTHING
#==========================================================


smoothed_thresholds <- function(thresholds, w = 1, center_weight = 0.5){
  # Make copy of thresholds
  smoothed <- thresholds
  # Run loop over all years to smooth out their thresholds
  for(year in 1:nrow(thresholds)){
    # find current window
    yr_w <- find_year_w(thresholds, year, w)
    # get current year to weigh properly
    curr_year <- thresholds[year,1]
    #make weights vector around that year
    weights <- make_weights(yr_w, curr_year, center_weight)
    # get thresholds for the window
    thrsh_w <- threshold_w(thresholds, yr_w, weights)
    # set year threshold to the smoothed windowed threshold
    smoothed[year, 2] <- thrsh_w
  }
  # return final smoothed thresholds
  smoothed
}

threshold_w <- function(thresholds, yr_w, weights){
  # extract w
  w <- nrow(yr_w)
  # lower and upper years
  lwr_yr <- as.numeric(yr_w[1,])
  upr_yr <- as.numeric(yr_w[w,])
  # years in the window
  in_window <- which(thresholds$Year >= lwr_yr & thresholds$Year <= upr_yr)
  # obtain the relevant thresholds in the window
  selected <- thresholds[in_window,] %>% pull(threshold)
  # multiply thresholds in the year range by the weight vector
  selected %*% weights
}

make_weights <- function(yr_w, curr_year, center_weight){
  # find number of window elements
  w <- nrow(yr_w)
  # find non center weight
  noncenter_weight <- (1 - center_weight)/(w - 1)
  # make uniform uncentered weight 
  weights <- rep(noncenter_weight, w)
  # find the appopriate weighing center
  center <- 1
  while(yr_w[center, ] != curr_year){
    center <- center + 1
  }
  weights[center] <- center_weight
  weights
}

find_year_w <- function(thresholds, year, w){
  # length of windowed year
  ct_w <- 2*w 
  # set initial year window
  yr_w <- thresholds[year, 1]
  # set current leftmost and rightmost yrs to center
  cl <- yr_w
  cr <- yr_w
  # index for while loop
  i <- 1
  while(i <= ct_w){
    # try left
    if((cl - 1) %in% 1969:2018){
      cl <- cl - 1
      yr_w <- rbind(cl, yr_w)
      i <- i + 1
    }
    # try right
    if((cr + 1) %in% 1969:2018){
      cr <- cr + 1
      yr_w <- rbind(yr_w, cr)
      i <- i + 1
    }
  }
  yr_w
}
