## Test environments
* local R installation, R 4.1.1
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* Release status: revision to first submission (details follow)

# Changes since first CRAN submission

In response to a detailed, informative and helpful review by Julia Haider, the
following changes were made to the version (1.0.0) submitted to CRAN, with the
new version being labeled 1.0.1.

1. The DESCRIPTION no longer starts with "The argoFloats package", and its
   literature citation is now in the suggested format.

2. \value was added to the documentation of all functions. A list of affected
   files is as below. Please note that last two items were found using
       grep -L '\\value' $(git grep -l '\\usage' *Rd)|sort
   with the others having been listed in the review.
      * argoFloatsDebug.Rd
      * argoFloatsGetFromCache.Rd
      * argoFloatsIsCached.Rd
      * argoFloatsStoreInCache.Rd
      * mapApp.Rd
      * plot-argoFloats-method.Rd
      * summary-argoFloats-method.Rd
      * useAdjusted.Rd
      * argoFloats-methods.Rd
      * sub-sub-argoFloats-method.Rd

3. Describe the output class in more detail.

4. Reduce the use of dontrun in examples, relying on the more appropriate
   donttest, and narrow the focus of examples. (Thanks, particularly, for
   noting this.)

5. Plotting functions now use on.exit() to reset par() values, so that the
   user's setup is not altered by the plot() call.

