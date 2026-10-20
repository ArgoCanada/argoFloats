## Local tests

* local R installation, R 4.6.1
* R-CMD-check test (no errors, warnings, or notes)
* Spelling check.
* URL check.
* Reverse-dependency checks.

## Remote tests

The following worked.

* devtools::check_win_release()
* devtools::check_win_devel()
* devtools::check_win_oldrelease()

# Changes since previous CRAN submission

This version, numbered 1.0.10, addresses two things:

1. A problem with mapApp(), an R-shiny app.  With a recent update to the Shiny
   system, it started to flash rapidly (and repeatedly, with no end in sight)
   from a blank map to one with points.  This was fixed by using isolate()
   calls on some reactive elements.
2. A CRAN-package-check warning on R-devel systems regarding "Found calls to
   structure() using deprecated special names".

The first was the primary concern, as the update to Shiny would cause mapApp()
to be useless.

