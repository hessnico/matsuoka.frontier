devtools::load_all()
source("./dev/run_tests.R")
devtools::document()

devtools::clean_vignettes()
devtools::build_vignettes()

devtools::check(remote = TRUE)
devtools::check_win_devel()
rhub::check_for_cran()