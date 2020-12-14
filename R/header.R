#============================#
#       Import Scripts       #
#============================#
source(file = "R/wrangle.R")
source(file = "R/model.R")
source(file = "R/visualization.R")
source(file = "R/threshold.R")

#=============================#
#       Import Datasets       #
#=============================#
# Load rookies datasets
pit_rkes <- read_csv("data/rookie-pitcher.csv")
pos_rkes <- read_csv("data/rookie-position.csv")
# Load retirees datasets
pit_ret <- read_csv("data/retirees-pitcher.csv")
pos_ret <- read_csv("data/retirees-position.csv")
# Find number of retirees by year
num_retirees <- total_retirees_by_yr(pit_ret, pos_ret)
num_retirees <- data.frame(retirees = num_retirees$retirees)
# Aggregate datasets to compute couldabeens
ls_datasets <- list(pos_rkes, pos_ret, pit_rkes, pit_ret, num_retirees)
# Wrangle the datasets
pit_rkes <- wrangle_init(pit_rkes)
pos_rkes <- wrangle_init(pos_rkes)
pit_ret <- wrangle_init(pit_ret)
pos_ret <- wrangle_init(pos_ret)

#================================#
#       Obtain Couldabeens       #
#================================#
# Get couldabeens
couldabeens <- read_csv("data-gen/couldabeens.csv")
couldabeens_t <- read_csv("data-gen/couldabeens_t.csv")
# Get and wrangle payroll revenue data
payroll <- wrangle_payroll(read_csv("data/revenue-payroll.csv"))
# Split data
couldabeens_pre <- couldabeens_t[which(couldabeens_t$postMoneyball == 0),]
couldabeens_post <- couldabeens_t %>% anti_join(couldabeens_pre)

#====================================#
#       Global Output Settings       #
#====================================#

bplot <- T
bloud <- T

