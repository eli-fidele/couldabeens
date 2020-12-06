# Insert single-use wrangling scripts/functions here

count_cbns <- function(dataset){
  dataset %>% 
    group_by(Year) %>%
    summarize(count_cbns_mean = sum(above_mean_threshold))
}

append_thresholds <- function(dataset, summary_dataset){
  above_threshold <- rep(NA, nrow(dataset))
  for(i in 1:nrow(dataset)){
    year <- as.numeric(dataset[i,2])
    above_threshold[i] <- (dataset[i,1] > summary_dataset[year - 1968, 2])
  }
  dataset <- cbind(dataset,above_threshold)
  dataset
}

# select appopriate columns
wrangle_ <- function(dataset){
  colnames(dataset)[3] <- "WAR"
  dataset %>% select(WAR, Year)
}
# obtain summary of WAR: median and variance
summary_ <- function(dataset){
  dataset %>% 
    group_by(Year) %>%
    summarize(mean_WAR = mean(WAR), sd_WAR = sqrt(var(WAR))) %>%
    mutate(threshold = mean_WAR - sd_WAR)
}