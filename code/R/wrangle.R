# Insert single-use wrangling scripts/functions here

get_retirees <- function(dataset){
  colnames(dataset)[5] <- "retirees"
  dataset %>% select(Year, retirees)
}