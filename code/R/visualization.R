# Insert single-use visualization scripts/functions here

#==========================================================
#                     SUMMARY PLOTS
#==========================================================

# Plot the logisitic model for a given retirees dataset on the exceeds_threshold variable
plot_logmodel <- function(dataset, title = ""){
  dataset <- prep_booleans(dataset)
  ggplot(data = dataset, aes(x = Year, y = above_threshold)) + 
    geom_jitter() +
    geom_smooth(method = 'glm', method.args = list(family = "binomial"), se = F) +
    theme(legend.position = "none") +
    labs(title = title)
}

# Plot proportion of couldabeens by year and dataset
plot_props_linear <- function(couldabeens, coefs, color = "black", title = ""){
  ggplot(data = couldabeens, mapping = aes(x = Year, y = prop)) +
    geom_point(color = color) +
    geom_abline(mapping = aes(x = Year, y = prop), data = couldabeens, 
                slope = coefs[2], intercept = coefs[1], color = color) +
    labs(y = "Proportion", title = title)
}

# Plot number of couldabeens in a given year
plot_cbns <- function(dataset, title, color){
  ggplot(data = dataset, aes(x = Year, y = count_cbns_mean)) + 
    stat_smooth(method = "lm", se = F, color = paste(color)) +
    geom_point(color = paste(color)) +
    labs(title = paste(title))
}

#==========================================================
#                 ELEMENTARY VISUALIZATION
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