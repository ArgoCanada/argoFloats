# argoFloats_hex_dk_07.R

`argoFloats_hex_dk_07.R` is based on `../../jlh/40_hexsticker/background.R`
with the following differences:

1. The topo file is downloaded, instead of assuming it is present locally.  (It
   is not in the repo.  And it shouldn't be; the repo should not contain
   reconstructible files.)
2. The argo data are cached for speed.  This changes a run from about a minute
   to a few seconds. To restart, either change the first line of the R file or
   remove the `floats.rda` file.
3. I altered colours, symbol size, etc., to try to improve clarity at the
   target size (width 4cm).
4. I cleaned up parts of the code that were extraneous (some unused variables)
   or confusing (remnants from prior work).

To create the sticker (stored in argoFloats_hex_dk_07.png), type `make` in this
directory.

See https://github.com/ArgoCanada/argoFloats/issues/39#issuecomment-1336147752
for a discussion thread.
