In response to an informative and helpful review by Julia Haider, the following
changes were made to the version (1.0.0) submitted to CRAN, with the new
version being labeled 1.0.1.

[x] 1. DESCRIPTION no longer starts with "The argoFloats package".

[x] 2. DESCRIPTION refers to a literature item paper using the suggested,
abbreviated, format.

[x] 3. \value added to all functions. All but the last two of the following
checklist were provided in the review, and

    grep -L '\\value' $(git grep -l '\\usage' *Rd)|sort

was used to find those last two, which relate to show() and [[.

  * [x] argoFloatsDebug.Rd
  * [x] argoFloatsGetFromCache.Rd
  * [x] argoFloatsIsCached.Rd
  * [x] argoFloatsStoreInCache.Rd
  * [x] mapApp.Rd
  * [x] plot-argoFloats-method.Rd
  * [x] summary-argoFloats-method.Rd
  * [x] useAdjusted.Rd
  * [x] argoFloats-methods.Rd
  * [x] sub-sub-argoFloats-method.Rd


[x] 4. Describe the output class.

[ ] 5. \dontrun changed to \donttest

[ ] 6. plot functions use on.exit() to reset par() values. *Need to think about
    this. We want to let them do e.g. points() after. Maybe this means we just
    shouldn't be using par() on margins, etc. I think mapApp() is different
    though, because they shouldn't be trying to add things (how could they?)*

[ ] 7. Please fix and resubmit.

