
#=============================#
#       Import Datasets       #
#=============================#
# Load rookies datasets
pit_rkes <- read_csv("../data/rookie-pitcher.csv")
pos_rkes <- read_csv("../data/rookie-position.csv")
# Load retirees datasets
pit_ret <- read_csv("../data/retirees-pitcher.csv")
pos_ret <- read_csv("../data/retirees-position.csv")
# Find number of retirees by year
num_retirees <- total_retirees_by_yr(pit_ret, pos_ret)
num_retirees <- data.frame(retirees = num_retirees$retirees)
# Aggregate datasets to compute couldabeens
ls_datasets <- list(pos_rkes, pos_ret, pit_rkes, pit_ret, num_retirees)

#==================================#
#       Generate Couldabeens       #
#==================================#
# Get couldabeens
couldabeens <- read_csv("../data-gen/couldabeens.csv")
# Get and wrangle payroll revenue data
payroll <- wrangle_payroll(read_csv("../data/revenue-payroll.csv"))
