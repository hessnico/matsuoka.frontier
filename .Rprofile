if (interactive()) {
  run_dev <- function(script) {
    path <- file.path("dev", script)
    if (file.exists(path)) source(path)
  }
  
  run_tests <- function() run_dev("run_tests.R")
  run_coverage <- function() run_dev("run_coverage.R")
  run_compliance <- function() run_dev("run_compliance.R")
  load_all <- function() run_dev("load_all.R")
}
