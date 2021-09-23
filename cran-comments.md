## Test environments
* local R installation, R 4.1.1
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* Release status: revision to first submission (details follow)

# Changes since previous CRAN submission

This version, numbered 1.0.3, addresses an error with the mapApp() Shiny app.
We were resetting a par(mar) call within the plot function of mapApp(), and
that caused a problem in subsequent interpretation of a mouse "brush"
operation.



