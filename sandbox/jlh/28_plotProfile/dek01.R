library(argoFloats)
library(oce)

argoFloatsPlotProfile <- function(x, parameter, ...)
{
    N <- length(x[['argos']])
    varNames <- unique(unlist(lapply(1:N, function(i) names(x[[i]][['data']]))))
    print(varNames)
    if (!parameter %in% varNames)
        stop('FIXME(jlh)')
    pressure <- lapply(1:N, function(i) x[[i]][['pressure']])
    variable <- lapply(1:N, function(i) x[[i]][[parameter]])
    nn <- unlist(lapply(1:N, function(i) prod(dim(x[[i]][[parameter]]))))
    pp <- NULL
    vv <- NULL
    punit <- NULL
    vunit <- NULL
    for (i in seq_len(N)) {
        if (nn[i] > 0) {
            if (is.null(vunit))
                vunit <- x[[1]][[paste0(parameter, "Unit")]]
            if (is.null(punit))
                punit <- x[[1]][[paste0("pressureUnit")]]
            pp <- c(pp, NA, pressure[[i]])
            vv <- c(vv, NA, variable[[i]])
        }
        ## cat(vectorShow(i))
        ## cat(vectorShow(length(pp)))
        ## cat(vectorShow(length(vv)))
    }
    o <- new("ctd")
    o <- oceSetData(o, "pressure", pp, unit=punit)
    o <- oceSetData(o, parameter, vv, unit=vunit)
                                        #summary(o)
    if ("keepNA" %in% names(list(...))) {
        oce::plotProfile(o, xtype=parameter, ...)
    } else {
        oce::plotProfile(o, xtype=parameter, keepNA=TRUE, ...)
    }
}


if (exists("argos")) { # cache for speed
    message("using cached 'argos'")
} else {
    message("caching 'argos'")
    data("indexSynthetic")
    subset <- subset(indexSynthetic, 1:10)
    argos <- readProfiles(getProfiles(subset))
}
argoFloatsPlotProfile(argos,"temperature",  col='blue', ytype="depth")


