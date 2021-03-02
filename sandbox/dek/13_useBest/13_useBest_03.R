# NOTE 1: As 13_useBest_02.Rmd shows, some files have matrix (multi-column)
# data, with *different* DATA_MODE values for each.  How should we handle that,
# in deciding whether to choose raw or adjusted data?

library(argoFloats)
set.seed(408)
if (!exists("a")) {
    i <- getIndex(age=10) # no need to get latest and greatest
    n <- 200
    s <- subset(i, sample(seq_len(i[["length"]]), n))
    a <- readProfiles(getProfiles(s))
}

debug <- 1

useBestSingle <- function(a, debug=0)
{
    if (!(inherits(a, "oce") && inherits(a, "argo")))
        stop("First argument must be an oce::argo object")
    res <- a
    fn <- a[["filename"]]
    typeFromFilename <- switch(substring(gsub(".*/","",fn),1,1), "A"="adjusted", "D"="delayed", "R"="realtime")
    cat(sprintf("%s (%s?)", gsub(".*/", "", fn), typeFromFilename))
    if ("dataMode" %in% names(a@metadata))
        cat(" DATA_MODE=", paste(a@metadata$dataMode, collapse=" "), "\n", sep="")
    varNames <- names(a[["data"]])
    adjustedNames <- grepl("Adjusted$", varNames)
    varNamesRaw <- varNames[!adjustedNames]
    varNamesAdjusted <- varNames[adjustedNames]
    if (debug > 1) {
        cat("      varNames: \"", paste(varNames, collapse="\" \""), "\"\n", sep="")
        cat("      varNamesRaw: \"", paste(varNamesRaw, collapse="\" \""), "\"\n", sep="")
        cat("      varNamesAdjusted: \"", paste(varNamesAdjusted, collapse="\" \""), "\"\n", sep="")
    }
    if ("dataMode" %in% names(a@metadata)) { # core
        dm <- a@metadata$dataMode[1]
        cat("      non-BGC dataset since dataMode exists\n", sep="")
        if (dm == "D") { # SEE NOTE 1 above
            # FIXME: see note at "A" below.
            # FIXME: copy flags also.
            for (name in varNamesRaw) {
                adjustedName <- paste0(name, "Adjusted")
                if (adjustedName %in% varNamesAdjusted) {
                    nok <- sum(is.finite(a@data[[adjustedName]]))
                    res@data[[name]] <- a@data[[adjustedName]]
                    cat("      ", adjustedName, " -> ", name, " (", nok, " finite data)\n", sep="")
                }
            }
        } else if (dm == "R") {
            weird <- FALSE
            nok <- 0
            for (name in varNamesRaw) {
                adjustedName <- paste0(name, "Adjusted")
                if (adjustedName %in% varNamesAdjusted) {
                    nok <- sum(is.finite(a@data[[adjustedName]]))
                    res@data[[name]] <- a@data[[adjustedName]]
                    cat("      ", adjustedName, " -> ", name, " (", nok, " finite data)\n", sep="")
                    weird <- TRUE
                }
            }
            if (weird) {
                if (nok) {
                    cat("      ERROR: 'realtime' file contains non-NA 'adjusted' data.\n")
                } else {
                    cat("      WARNING: 'realtime' file contains 'adjusted' data, which seems wrong, but at least they all are NA.\n")
                }
            }
        } else if (dm == "A") {
            # FIXME: if this will be identical to the "D" case, we'll copy the code there.
            # FIXME: copy flags also.
            weird <- TRUE
            for (name in varNamesRaw) {
                adjustedName <- paste0(name, "Adjusted")
                if (adjustedName %in% varNamesAdjusted) {
                    nok <- sum(is.finite(a@data[[adjustedName]]))
                    res@data[[name]] <- a@data[[adjustedName]]
                    cat("      ", adjustedName, " -> ", name, " (", nok, " finite data)\n", sep="")
                    weird <- FALSE
                }
            }
            if (weird)
                cat("    ERROR: 'adjusted' file lacks 'adjusted' data.\n")
        } else {
            stop("dataMode must be \"A\", \"R\", or \"D\", not \"", dm, "\"")
        }
    } else if ("parameterDataMode" %in% names(a@metadata)) { # BGC
        dm <- a@metadata$dataMode
        cat("   BGC dataset (contains \"parameterDataMode\"=\"", paste(dm, collapse="\" \""), "\")\n", sep="")
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


useBest <- function(a, debug=0)
{
    if (!inherits(a, "argoFloats"))
        stop("'a' must be an argoFloats object")
    if ("argos" != a@metadata$type)
        stop("'a' must be an argoFloats object created with argoFloats::readProfiles()")
    res <- a
    for (i in seq_along(a[["argos"]])) {
        cat(sprintf("\n%2d. ", i))
        res@data$argos[[i]] <- useBestSingle(a[[i]], debug=debug)
    }
    res@processingLog <- oce::processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

b <- useBest(a)

