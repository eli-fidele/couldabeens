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
postrule <- function(dataset){
  dataset %>% filter(Year > 2002)
}

# Filters year to be in the pre-rule era
prerule <- function(dataset){
  dataset %>% filter(Year <= 2002)
}

# select appopriate columns from player datasets
wrangle_init <- function(dataset){
  colnames(dataset)[3] <- "WAR"
  dataset %>% select(WAR, Year)
}
