# dev/run_coverage.R

devtools::load_all()

cov <- covr::package_coverage()
print(cov)

covr::report(cov)