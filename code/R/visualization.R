# Insert single-use visualization scripts/functions here

# base visualization functions
hist.var <- function(var, dataframe, title = ""){
  ggplot(data = dataframe) + 
    geom_histogram(aes_string(x = var)) + 
    labs(title = title)
}
dens.var <- function(var, dataframe, title = ""){
  ggplot(data = dataframe) + 
    geom_density(aes_string(x = var)) +
    labs(title = title)
}