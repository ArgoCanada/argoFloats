## Test environments
* local R installation, R 4.1.1
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* Release status: revision to first submission (details follow)

# Changes since first two CRAN submissions

This is the third submission.  Our first submission was numbered 1.0.0, our
second 1.0.1, and our third (the present one) 1.0.2.

We are indebted to Julia Haider and Gregor Seyer, whose helpful and informative
reviews led to changes as summarized below (items 1 through 5 being in response
to Haider, and item 6 being in response to Seyer).

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

6. Examples and vignettes reset par() values to their initial state. (Seyer
   also commented on options(), but the package does not set those, except in
   tests, where setting and checking is part of the test.)


