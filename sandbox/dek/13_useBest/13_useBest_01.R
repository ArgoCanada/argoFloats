library(argoFloats)
set.seed(408)
if (!exists("a")) {
    i <- getIndex(age=10) # no need to get latest and greatest
    n <- 5 # 200
    s <- subset(i, sample(seq_len(i[["length"]]), n))
    a <- readProfiles(getProfiles(s))
}

debug <- 1

useBest <- function(a)
{
    if (!inherits(a, "argoFloats"))
        stop("'a' must be an object created with argoFloats::readProfiles()")
    rval <- a
    for (i in seq_along(a[["argos"]])) {
        ai <- a[[i]]
        fn <- ai[["filename"]]
        typeFromFilename <- switch(substring(gsub(".*/","",fn),1,1), "A"="adjusted", "D"="delayed", "R"="realtime")
        message(sprintf("\n%2d. %s (filename suggests %s data)", i, fn, typeFromFilename))
        varNames <- names(ai[["data"]])
        adjustedNames <- grepl("Adjusted$", varNames)
        varNamesRaw <- varNames[!adjustedNames]
        varNamesAdjusted <- varNames[adjustedNames]
        if (debug > 1) {
            message("      varNames: \"", paste(varNames, collapse="\" \""), "\"")
            message("      varNamesRaw: \"", paste(varNamesRaw, collapse="\" \""), "\"")
            message("      varNamesAdjusted: \"", paste(varNamesAdjusted, collapse="\" \""), "\"")
        }
        if ("dataMode" %in% names(ai@metadata)) { # core
            dm <- ai@metadata$dataMode[1]
            message("    core dataset with dataMode=\"", dm, "\"")
            if (dm == "D") {
                # FIXME: see note at "A" below.
                # FIXME: copy flags also.
                for (name in varNamesRaw) {
                    adjustedName <- paste0(name, "Adjusted")
                    if (adjustedName %in% varNamesAdjusted) {
                        nok <- sum(is.finite(ai@data[[adjustedName]]))
                        ai@data[[name]] <- ai@data[[adjustedName]]
                        message("      ", adjustedName, " -> ", name, " (", nok, " finite data)")
                    }
                }
            } else if (dm == "R") {
                weird <- FALSE
                nok <- 0
                for (name in varNamesRaw) {
                    adjustedName <- paste0(name, "Adjusted")
                    if (adjustedName %in% varNamesAdjusted) {
                        nok <- sum(is.finite(ai@data[[adjustedName]]))
                        ai@data[[name]] <- ai@data[[adjustedName]]
                        message("      ", adjustedName, " -> ", name, " (", nok, " finite data)")
                        weird <- TRUE
                    }
                }
                if (weird) {
                    if (nok) {
                        message("    ERROR: 'realtime' file contains non-NA 'adjusted' data.")
                    } else {
                        message("    WARNING: 'realtime' file contains 'adjusted' data, which seems wrong, but at least they all are NA.")
                    }
                }
            } else if (dm == "A") {
                # FIXME: if this will be identical to the "D" case, we'll copy the code there.
                # FIXME: copy flags also.
                weird <- TRUE
                for (name in varNamesRaw) {
                    adjustedName <- paste0(name, "Adjusted")
                    if (adjustedName %in% varNamesAdjusted) {
                        nok <- sum(is.finite(ai@data[[adjustedName]]))
                        ai@data[[name]] <- ai@data[[adjustedName]]
                        message("      ", adjustedName, " -> ", name, " (", nok, " finite data)")
                        weird <- FALSE
                    }
                }
                if (weird)
                    message("    ERROR: 'adjusted' file lacks 'adjusted' data.")
            } else {
                stop("dataMode must be \"A\", \"R\", or \"D\", not \"", dm, "\"")
            }
        } else if ("parameterDataMode" %in% names(ai@metadata)) { # BGC
            dm <- ai@metadata$dataMode
            message("   BGC dataset (contains \"parameterDataMode\"=\"", paste(dm, collapse="\" \""), "\")")
        }
    }
    rval
}

b <- useBest(a)

