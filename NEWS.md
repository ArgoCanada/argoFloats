# argoFloats 1.0.9

* Change `getIndex()` so that the `"usgodae"` nickname refers to
  https://usgodae.org/pub/outgoing/argo, not the old FTP site.
* Change `getIndex()` so that using `debug=2` prints out times during
  processing steps (may help in debugging; see issue #602).
* Change importing indications so that package will build and check (issue
  #603).

# argoFloats 1.0.8

* Fix formatting of `NEWS.md` file.

# argoFloats 1.0.7

* Remove suggestion for now-retired `rgdal` package.
* Require R 4.1.0 or higher, to permit `|>` syntax.
* `plot(a, which="TS")` can color by cycle for `type="l"` now.
* `getIndex()` can work with previously-downloaded `.tgz` files.

# argoFloats 1.0.6

* Update docs for CRAN.

# argoFloats 1.0.5

* Change `getProfiles()` to permit age="latest".
* Change `mapApp()` to allow subset by polygon. 

# argoFloats 1.0.4 (on CRAN)

* Fix image-size declarations in 3 man pages (required by CRAN).
* Change `plot()` to not show bathymetry by default.
* Improve `index[["ID"]]` speed by 3X.

# argoFloats 1.0.3 (on CRAN)

* Fix a `mapApp()` problem with paths that cross the dateline.
* Fix a `mapApp()` problem in handling mouse brush events.

# argoFloats 1.0.2 (on CRAN)

* Examples reset `par()` to its initial state.

# argoFloats 1.0.1

* DESCRIPTION improved.
* Document return values of all functions.
* Describe base class.
* `plot()` resets `par()` to its initial state before returning.

# argoFloats 1.0.0

* Improve documentation for a CRAN release.
* Remove version number from `mapApp()`.

# argoFloats 0.2.0

* Add subset by section.
* Add "traj" and "bio-traj" arguments to `getIndex()`.
* Created subtype cycles and trajectories for index type.
* Created map plot for trajectories type.

# argoFloats 0.1.3

* Widen support of `age` argument, remove support for `force`.
* Add several new built-in datasets.
* Add several new functions.
* Improve vignette and other documentation.

# argoFloats 0.1.2

* Repository made public, transferred to ArgoCanada organization.

# argoFloats 0.1.1

* Initial version, on github but not CRAN.

