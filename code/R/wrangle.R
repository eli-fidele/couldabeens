# Insert single-use wrangling scripts/functions here

get_retirees <- function(dataset){
  colnames(dataset)[5] <- "retirees"
  dataset %>% select(Year, retirees)
}

# select appopriate columns from player datasets
wrangle_init <- function(dataset){
  colnames(dataset)[3] <- "WAR"
  dataset %>% select(WAR, Year)
}