# useBest ideas

#' @param a
#' @param fallback a character value indicating what to do if all the adjusted
#' values for a particular parameter-profile pair are `NA`. There are two
#' cases.  *Case 1*: If `fallback` is `"NA"`
#' (the default), then the adjusted values are returned, even if they
#' are all `NA`. This is a cautious approach that should yield only
#' data of high quality.  *Case 2*: if `fallback` is `"raw"`, then the data in
#' the adjusted columns are examined one by one. If the values in a given
#' column are all `NA`, then the parameter's raw values are returned.
#' However, if any of the values in a given adjusted column are non-`NA`,
#' then that whole adjusted column is returned, ignoring the raw values.
#' Thus, the `"raw"` method provides a rough estimate even for dubious
#' profiles.
#'
#' @details
useAdjusted(a, fallback="NA") # default
useAdjusted(a, fallback="raw")






Looking column by column:

# case 1: "R" column
```R
rval <- if (adj field exists) {
    if (all adj is NA) raw else adj
} else {
    raw
}
```

# case 2: "D" column (this agrees with rev3)
```R
rval <- if (adj field exists) adj else raw
```

# case 3: "A" column
```R
rval <- if (adj field exists) adj else raw # FIXME what to do?
```

