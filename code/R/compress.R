# Insert single-use wrangling scripts/functions here

#==========================================================
#                   FIND COULDABEENS
#==========================================================

find_prop <- function(couldabeens, num_retirees){
  # Append number of retirees that year
  couldabeens <- cbind(couldabeens, num_retirees)
  # Find proportion of couldabeens : retirees
  couldabeens <- couldabeens %>% mutate(prop = cbns/retirees)
  couldabeens
}

find_couldbeens <- function(df_pos_rkes, df_pos_ret, df_pit_rkes, df_pit_ret, sd = 1){
  # Obtain wrangled datasets
  pit_rkes <- wrangle_(df_pit_rkes)
  pit_ret <- wrangle_(df_pit_ret)
  pos_rkes <- wrangle_(df_pos_rkes)
  pos_ret <- wrangle_(df_pos_ret)
  # Get thresholds in each year
  pit_rkes_thresholds <- find_thresholds(pit_rkes, sd)
  pos_rkes_thresholds <- find_thresholds(pos_rkes, sd)
  # See and record which players cross that year's threshold
  pit_ret <- compare_thresholds(pit_ret, pit_rkes_thresholds)
  pos_ret <- compare_thresholds(pos_ret, pos_rkes_thresholds)
  # Get retired couldabeens
  retirees <- rbind(pit_ret,pos_ret)
  couldabeens <- count_cbns(retirees)
  # Append threshold for reference
  threshold_idx <- data.frame(threshold = rep(sd, nrow(couldabeens)))
  couldabeens <- cbind(couldabeens, threshold_idx)
  # Return dataframe
  couldabeens
}