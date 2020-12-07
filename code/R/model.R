# Insert single-use wrangling scripts/functions here

#==========================================================
#                     COUNT RETIREES
#==========================================================

total_retirees_by_yr <- function(pitchers, position){
  year_count_pitchers <- retirees_by_yr(pitchers)$retirees
  year_count_position <- retirees_by_yr(position)$retirees
  data.frame(Year = 1969:2018, retirees = year_count_position + year_count_pitchers)
}

retirees_by_yr <- function(dataset){
  year_count <- dataset %>%
    group_by(Year) %>%
    summarize(retirees = n())
}

#==========================================================
#                      COULDABEENS
#==========================================================

count_cbns <- function(dataset){
  dataset %>% 
    group_by(Year) %>%
    summarize(cbns = sum(above_threshold))
}

compare_thresholds <- function(dataset, summary_dataset){
  above_threshold <- rep(NA, nrow(dataset))
  for(i in 1:nrow(dataset)){
    year <- as.numeric(dataset[i,2])
    above_threshold[i] <- (dataset[i,1] > summary_dataset[year - 1968, 2])
  }
  dataset <- cbind(dataset,above_threshold)
  colnames(dataset)[3] <- "above_threshold"
  dataset
}

# obtain summary of WAR: median and variance
find_thresholds <- function(dataset, sds = -1){
  dataset %>% 
    group_by(Year) %>%
    summarize(mean_WAR = mean(WAR), sd_WAR = sqrt(var(WAR))) %>%
    mutate(threshold = mean_WAR + sds*sd_WAR) %>% 
    select(Year, threshold)
}