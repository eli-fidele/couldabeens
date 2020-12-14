# Insert single-use wrangling scripts/functions here

#==========================================================
#                     WRANGLE PAYROLL
#==========================================================

wrangle_final <- function(couldabeens, payroll){
  # Find labor share in payroll data
  payroll_rev <- find_labShare(payroll)
  # Append payroll data in appopriate year (accounting for lag)
  couldabeens <- append_payrolls(couldabeens, payroll_rev, lag = 1)
  # Create moneyball variable
  couldabeens <- couldabeens %>% mutate(postMoneyball = 1 - (Year < 2004))
  # Remove unused columns
  couldabeens[-c(3)]
}

# incorporate the payrolls data to the couldabeens, accounting for lag
append_payrolls <- function(couldabeens, payroll, lag = 0){
  # Select years
  yrs_C <- couldabeens %>% pull(Year)
  yrs_P <- payroll %>% pull(Year)
  # Find common years
  yr_lwr <- max(min(yrs_C),min(yrs_P)) - lag
  yr_upr <- min(max(yrs_C),max(yrs_P)) + lag
  # Select data
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
  couldabeens[-c(2,3,4)]
}

# wrangle payroll revenue datasets
find_labShare <- function(dataset){
  # Rename columns
  colnames(dataset) <- c("Year", "totRev", "totPayroll")
  # Normalize money units and create labor share variable
  dataset %>% 
    mutate(totPayroll = totPayroll/10e5, labShare = totPayroll/totRev)
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
