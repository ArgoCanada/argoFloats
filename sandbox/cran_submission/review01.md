Thanks,

Please do not start the description with "This package", package name,
title or similar.

Please write references in the description of the DESCRIPTION file in
the form
authors (year) <doi:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
    argoFloatsDebug.Rd: \value
    argoFloatsGetFromCache.Rd: \value
    argoFloatsIsCached.Rd: \value
    argoFloatsStoreInCache.Rd: \value
    mapApp.Rd: \value
    plot-argoFloats-method.Rd: \value
    summary-argoFloats-method.Rd: \value
    useAdjusted.Rd: \value

\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user.

Please unwrap the examples if they are executable in < 5 sec, or replace
\dontrun{} with \donttest{}.
Please put functions which download data in \donttest{}.


Please make sure that you do not change the user's options, par or
working directory. If you really have to do so within functions, please
ensure with an *immediate* call of on.exit() that the settings are reset
when the function is exited. e.g.:
...
oldpar <- par(no.readonly = TRUE)    # code line i
on.exit(par(oldpar))            # code line i + 1
...
par(mfrow=c(2,2))            # somewhere after
...
e.g.: plot.R
If you're not familiar with the function, please check ?on.exit. This
function makes it possible to restore options before exiting a function
even if the function breaks. Therefore it needs to be called immediately
after the option change within a function.

Please fix and resubmit.

Best,
Julia Haider

