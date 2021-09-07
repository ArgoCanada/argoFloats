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

1. DESCRIPTION no longer starts with "The argoFloats package".

2. DESCRIPTION refers to a published paper using the suggested format.

3. \value added to the documentation of all functions. A list of affected files
   is as below. Please note that last two items were found using
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

4. Describe the output class.

5. Change \dontrun entries to \donttest.

6. Expose a few more plot() examples, but not the whole list, because doing so
   causes testing to exceed the 5-second limit.

7. Ensure that plotting functions use on.exit() to reset par() values, and
   adjust some documentation entries appropriately, to get margins that are
   closer to oceanographic norms. (For example, oceanographers draw the x axis
   at the top of a so-called "profile" plot, so space is reallocated from the
   bottom margin to the top one.)

