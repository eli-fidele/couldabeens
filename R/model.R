

#==========================================================
#                      LINEAR MODEL
#==========================================================

linear_model <- function(dataset){
  linear_model <- lm(formula = prop ~ I(Year), data = dataset)
  linear_model
}