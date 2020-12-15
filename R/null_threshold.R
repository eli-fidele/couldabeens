

#==========================================================
#               COULDABEENS CLASSIFICATION
#==========================================================

# Aggregate function finds couldabeens for a given threshold in standard deviations from the mean rookie WAR
couldabeens_null_threshold <- function(ls_datasets, threshold = 0){
  # Obtain the sd value (esentially renaming variable)
  sd <- threshold
  # Unwind datasets from list
  df_pos_rkes <- as.data.frame(ls_datasets[1])
  df_pos_ret <- as.data.frame(ls_datasets[2])
  df_pit_rkes <- as.data.frame(ls_datasets[3])
  df_pit_ret <- as.data.frame(ls_datasets[4])
  num_retirees <- as.data.frame(ls_datasets[5])
  # Obtain wrangled datasets
  pit_rkes <- wrangle_init(df_pit_rkes)
  pit_ret <- wrangle_init(df_pit_ret)
  pos_rkes <- wrangle_init(df_pos_rkes)
  pos_ret <- wrangle_init(df_pos_ret)
  # Create null threshold
  null_threshold <- data.frame(Year = 1969:2018, threshold = rep(0,50))
  # Set thresholds to null threshold
  pit_thresholds <- null_threshold
  pos_thresholds <- null_threshold
  # See and record which players cross that year's adjusted threshold from rookie players
  pit_ret <- compare_thresholds(pit_ret, pit_thresholds)
  pos_ret <- compare_thresholds(pos_ret, pos_thresholds)
  # Get retired couldabeens
  retirees <- rbind(pit_ret,pos_ret)
  couldabeens <- count_cbns(retirees)
  # Append threshold for reference
  threshold_idx <- data.frame(threshold = rep(sd, nrow(couldabeens)))
  couldabeens <- cbind(couldabeens, threshold_idx)
  # Append number of retirees that year
  couldabeens <- cbind(couldabeens, num_retirees)
  # Find and append proportion of couldabeens : retirees
  couldabeens <- couldabeens %>% mutate(prop = cbns/retirees)
  # Return dataframe
  couldabeens
}
