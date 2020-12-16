# Insert wrangling scripts/functions here

# Combine the couldabeens from the payroll data to the player data
combine_NA_data <- function(ls_datasets, couldabeens){
  # couldabeens is data with no NA's (1989 onwards)
  couldabeens_t <- couldabeens_by_threshold(ls_datasets, threshold = 0)
  couldabeens_t <- couldabeens_t[c(1,5)]
  couldabeens_t <- couldabeens_t[1:20,]
  dr <- 20
  mt <- rep(NA, dr)
  couldabeens_t <- data.frame(couldabeens_t, totRev = mt, totPayroll = mt, labShare = mt, postMoneyball = rep(0, dr))
  rbind(couldabeens_t, couldabeens)
}

#==========================================================
#                     WRANGLE PAYROLL
#==========================================================

wrangle_final <- function(couldabeens, payroll){
  # Append payroll data in appopriate year (accounting for lag)
  couldabeens <- append_payrolls(couldabeens, payroll)
  # Create moneyball variable
  couldabeens <- couldabeens %>% mutate(postMoneyball = 1 - (Year < 2004))
  # Remove unused columns
  couldabeens[-c(4)]
}

# incorporate the payrolls data to the couldabeens, accounting for lag
append_payrolls <- function(couldabeens, payroll){
  # Select years
  yrs_C <- couldabeens %>% pull(Year)
  yrs_P <- payroll %>% pull(Year)
  # Find common years, lag by 1 so we predict the couldabeens for next year
  yr_lwr <- max(min(yrs_C),min(yrs_P)) - 1
  yr_upr <- min(max(yrs_C),max(yrs_P)) + 1
  # Select data in appopriate year range
  sel_C <- couldabeens[which(couldabeens$Year >= yr_lwr & couldabeens$Year <= yr_upr),]
  sel_P <- payroll[which(payroll$Year >= yr_lwr & payroll$Year <= yr_upr),]
  # Remove year from the data
  colnames(sel_P)[1] <- "Year_P"
  # Combine the data
  couldabeens <- cbind(sel_C, sel_P)
  # Rename rows and columns
  colnames(couldabeens)[ncol(couldabeens)] <- "labShare"
  rownames(couldabeens) <- 1:nrow(couldabeens)
  # Return dataset
  couldabeens
}

# wrangle payroll revenue datasets
wrangle_payroll <- function(dataset){
  # Rename columns
  colnames(dataset) <- c("Year", "totRev", "totPayroll")
  # Normalize money units and create labor share variable
  dataset %>% 
    mutate(totPayroll = totPayroll/10e5, labShare = totPayroll/totRev)
}

#==========================================================
#                    COUNT COULDABEENS
#==========================================================

# Counts couldabeens in a given year
count_cbns <- function(dataset){
  dataset %>% 
    group_by(Year) %>%
    summarize(cbns = sum(above_threshold))
}

# Appends above_threshold column to retirees dataset (checks if a retiree exceeds a threshold)
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
find_thresholds <- function(dataset, sds = 0){
  dataset %>% 
    group_by(Year) %>%
    summarize(mean_WAR = mean(WAR), sd_WAR = sqrt(var(WAR))) %>%
    mutate(threshold = mean_WAR + sds*sd_WAR) %>% 
    select(Year, threshold)
}

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

remove_years <- function(dataset, yrs){
  dataset[-which(dataset$Year %in% yrs),]
}

# Filters year to be in the post-rule era
postrule <- function(dataset, rule_year = 2002){
  dataset %>% filter(Year > rule_year)
}

# Filters year to be in the pre-rule era
prerule <- function(dataset, rule_year = 2002){
  dataset %>% filter(Year <= rule_year)
}

# select appopriate columns from player datasets
wrangle_init <- function(dataset){
  colnames(dataset)[3] <- "WAR"
  dataset %>% select(WAR, Year)
}
