# Insert single-use visualization scripts/functions here

#==========================================================
#                         SUMMARY
#==========================================================

plot_cbns <- function(dataset, title, color){
  ggplot(data = dataset, aes(x = Year, y = count_cbns_mean)) + 
    stat_smooth(method = "lm", se = F, color = paste(color)) +
    geom_point(color = paste(color)) +
    labs(title = paste(title))
}

#==========================================================
#                         BASIC
#==========================================================

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