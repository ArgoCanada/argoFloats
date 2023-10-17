## Local tests

* local R installation, R 4.3.1
* R-CMD-check test (no errors, warnings, or notes)

## Remote tests

The following worked, except for one PREPERROR on a rhub test (which does not
seem to relate to argoFloats directly).

* devtools::check_win_release()
* devtools::check_win_devel()
* devtools::check_win_oldrelease()
* rhub::check_for_cran(show_status=FALSE)
* rhub::check(platform="debian-clang-devel", show_status=FALSE)

# Changes since previous CRAN submission

This version, numbered 1.0.7, addresses a problem reported to us on 2022-08-19,
regarding HTML pages, and also removes dependence on now-retired packages such
as rgdal.



