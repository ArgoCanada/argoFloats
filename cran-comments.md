## Local tests

* local R installation, R 4.5.1
* R-CMD-check test (no errors, warnings, or notes)

## Remote tests

The following worked.

* devtools::check_win_release()
* devtools::check_win_devel()
* devtools::check_win_oldrelease()
* rhub::rhub_check()

# Changes since previous CRAN submission

This version, numbered 1.0.9, addresses a problem reported to us on 2025-10-28,
regarding tests failing if web downloads failed.  This was addressed by using
try().
