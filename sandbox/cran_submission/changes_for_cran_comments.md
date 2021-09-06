In response to an informative and helpful review by Julia Haider, the following
changes were made to a version submitted to CRAN.

[x] 1. DESCRIPTION updated to not start with "The argoFloats package".

[x] 2. DESCRIPTION refers to a peer-reviewed paper using the proper format.

[x] 3. \value added to all functions.  The following were missing that item:
   argoFloatsDebug.Rd
   argoFloatsGetFromCache.Rd
   argoFloatsIsCached.Rd
   argoFloatsStoreInCache.Rd
   mapApp.Rd
   plot-argoFloats-method.Rd
   summary-argoFloats-method.Rd
   useAdjusted.Rd

[x] 4. Describe the output class.

[x] 5. \dontrun changed to \donttest

[x] 6. plot functions use on.exit() to reset par() values. *Need to think about
    this. We want to let them do e.g. points() after. Maybe this means we just
    shouldn't be using par() on margins, etc. I think mapApp() is different
    though, because they shouldn't be trying to add things (how could they?)*
