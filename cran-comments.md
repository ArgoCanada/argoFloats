## Local tests

* local R installation, R 4.2.0
* R-CMD-check test (no errors, warnings, or notes)

## Remote tests

* devtools::check_win_release()
* devtools::check_win_devel()
* devtools::check_win_oldrelease()
* rhub::check_for_cran(show_status=FALSE)
* rhub::check(platform="debian-clang-devel", show_status=FALSE)

# Changes since previous CRAN submission

This version, numbered 1.0.6, addresses a problem reported to us on 2022-08-19,
regarding HTML pages.



