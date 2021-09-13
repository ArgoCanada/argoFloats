CAUTION: The Sender of this email is not from within Dalhousie.

Thanks,

Please always make sure to reset to user's options(), working directory
or par() after you changed it in examples and vignettes and demos.
e.g.:
oldpar <- par(mfrow = c(1,2))
...
par(oldpar)

e.g.: inst/doc/qc_flowchart.R, vignettes/..., examples


Please fix and resubmit.

Best,
Gregor Seyer


Am 09.09.21 um 21:09 schrieb CRAN submission:
