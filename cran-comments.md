## Test environments
* local R installation, R 4.1.1
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* Release status: revision to first submission (details follow)

# Changes since first two CRAN submissions

This is the third submission.  We thank Julia Haider and Gregor for their
helpful reviews of our first and second submissions, respectively. The changes
are outlined in the next two subsections, in reverse order of time.

## Changes in 1.0.2, in response to Gregor Seyer's review of version 1.0.1

1. Examples and vignettes reset par() values to their initial state. (Gregor
   Seyer also commented about resetting options(), but the package only sets
   options in tests/testthat/test_defaults.R, where setting and checking is
   part of the test.)

## Changes in 1.0.1, in response to Julia Haider's review of version 1.0.0

These changes were incorporated in version 1.0.1, as reviewed by Gregor Seyer.

1. The DESCRIPTION no longer starts with "The argoFloats package", and its
   literature citation is now in the suggested format.

2. \value was added to the documentation of all functions. A list of affected
   files is as below. Please note that last two items were found using
       grep -L '\\value' $(git grep -l '\\usage' *Rd)|sort
   with the others were listed in the review.
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

3. We now describe the output class in more detail.

4. We have reduced the use of dontrun in examples, relying on the more
   appropriate donttest.  We have also removed some examples that were
   slow-running or that were not required for illustration.  (We are
   particularly thankful for pointing this out.)

5. The plotting functions now use on.exit() to reset par() values, so that the
   user's values for par("mar"), etc., are not altered by the plot() call.



