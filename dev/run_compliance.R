devtools::load_all()
source("./dev/run_tests.R")
devtools::document()
devtools::build_vignettes()

devtools::check()
