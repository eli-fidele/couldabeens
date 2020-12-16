

#==========================================================
#                      LINEAR MODEL
#==========================================================

linear_model <- function(dataset){
  linear_model <- lm(formula = prop ~ Year, data = dataset)
  linear_model
}

# Fit
laborShare_model <- function(dataset){
  linear_model <- lm(formula = prop ~ labShare, data = dataset)
  linear_model
}