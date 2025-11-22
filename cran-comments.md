## Test environments

* Local Windows 10, R 4.5.2
* Posit Cloud standard environment
* Ubuntu (GitHub Actions), check the [github workflow](https://github.com/hessnico/matsuoka.frontier/blob/main/.github/workflows/r-cmd-check.yaml)
* R-hub (All platforms failed to start, even on Posit Cloud environment)

## R CMD check results

* There were **0 ERRORs**, **0 WARNINGs**, and **3 NOTEs**.

### Notes

1. **CRAN incoming feasibility NOTE**  
   This is a *new submission*.  
   Listing of maintainer: *Nicolas Hess <nicolashess1@gmail.com>*.

2. **Top-level README.md / NEWS.md NOTE**  
   Pandoc is not installed on my machine, so CRAN cannot fully check these files.  
   This appears to be harmless.

3. **HTML manual math rendering NOTE**  
   `V8` package is not installed in some environments used during checks.  
   The NOTE is known and not related to package functionality.

## R-hub results

* R-hub checks were attempted but currently return a “Failed to start check: Not Found” error.  

## Reverse dependencies

* This is a **new package**, so there are no reverse dependencies.

## Additional comments

* All package documentation, examples, tests, and vignettes build correctly.
* The PDF manual with package documentation builds successfully.
