# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Plot a TS diagram for an argoFloats object
#'
#' Plot a TS diagram for an [argoFloats-class] object that was created with
#' [readProfiles()].  This function is called by [plot,argoFloats-method()],
#' but may also be called directly.
#'
#' @param x an argoFloats object that was created with [readProfiles()].
#'
#' @param xlim,ylim optional limits of salinity and temperature axes; if not provided,
#' the plot region will be large enough to show all the data.
#'
#' @param type character value indicating the of plot.  This has the same meaning
#' as for general R plots: `"p"` for points, `"l"` for lines, etc.  Note that
#' lines are joined up between cycles, unless the `TSControl` parameter indicates
#' otherwise.
#'
#' @param cex,col,pch,bg values that have the same meaning as for general R plots
#' if `TSControl$groupByCycle` is FALSE, except that if `col` is not
#' provided by the user and if `type` is `"l"`, then the points
#' are colour-coded to indicate the value of data-quality flags.
#' Black symbols indicate good data, i.e. data for which the
#' flags for pressure, salinity and temperature are all in the set
#' (1, 2, 5, 8).  Red is used for bad data, with any of these three
#' variables being flagged in the set (3, 4, 6, 7). And, finally
#' gray is used if data that have not been assessed for quality,
#' with the flags for all three of these variables being 0.
#' (For more on flags, see Reference Table 2 in Section 3.2 of
#' reference 1.)
#'
#' @param eos character value, either `"gsw"` (the default) for the Gibbs-Seawater
#' (TEOS-10) equation of state or `"unesco"` for the 1980s-era UNESCO equation of
#' state.
#'
#' @param TSControl an optional list that may have a logical element named `groupByCycle`,
#' meaning to group the data by cycle. If `TSControl` is not provided, it is set
#' to `list(groupByDefault=FALSE)`.  In grouped cases, the values of `cex`, `col`,
#' and `pch` are passed to [rep()] to achieve the same length as the number of
#' cycles in `x`.  This can be useful in distinguishing between cycles.
#'
#' @param debug an integer controlling how much information is to be printed
#' during operation. Use 0 for silent work, 1 for some information and 2 for
#' more information.  Note that [plot,argoFloats-method()] reduces its `debug`
#' value by 1 before passing to [plotArgoTS()].
#'
#' @param \dots extra arguments provided to [oce::plotTS()].
#'
#' @references
#'
#' 1. Argo Data Management. “Argo User’s Manual.” Ifremer, July 5, 2022.
#' https://doi.org/10.13155/29825.
#'
#' @export
#'
#' @author Dan Kelley and Jaimie Harbin
plotArgoTS <- function(x, xlim=NULL, ylim=NULL,
    type="p", cex=1, col=NULL, pch=1, bg="white", eos="gsw", TSControl=NULL,
    debug=0, ...)
{
    #message('debug=',debug)
    if (!inherits(x, "argoFloats"))
        stop("x must be an argo object")
    if ((x[["type"]] != "argos"))
        stop("x must be the result of a call to readProfiles()")
    if (!(eos %in% c("gsw", "unesco")))
        stop("eos must be \"gsw\" or \"unesco\", not \"", eos, "\"")
    argoFloatsDebug(debug, "plotArgoTS(x, ..., eos=\"", eos, "\", ...) {\n", sep="", unindent=1, style="bold")
    if (!(eos %in% c("gsw", "unesco")))
        stop("In plot,argoFloats-method(): eos must be \"gsw\" or \"unesco\", not \"", eos, "\"", call.=FALSE)
    colGiven <- !is.null(col)
    # compute cycle, in case we need that
    ncycles <- x[["length"]]
    cycleIndex <- unlist(lapply(seq_len(ncycles),
            function(icycle)
                rep(icycle, length(x@data$argos[[icycle]]@data$pressure))))
    salinity <- unlist(x[["salinity", debug=debug-1L]])
    temperature <- unlist(x[["temperature", debug=debug-1L]])
    pressure <- unlist(x[["pressure", debug=debug-1L]])
    ## Use byLevel to repeat the latitude and longitude values across
    ## the depths in each profile, so that the resultant vector
    ## will match the salinity, temperature and pressure vectors.
    latitude <- unlist(x[["latitude", "byLevel", debug=debug-1L]])
    longitude <- unlist(x[["longitude", "byLevel", debug=debug-1L]])
    # Interpolate across NA longitudes (required for traj data, to get TS plot)
    n <- length(longitude)
    if (any(is.na(longitude)))
        longitude <- approx(1:n, longitude, 1:n)$y
    if (any(is.na(latitude)))
        latitude <- approx(1:n, latitude, 1:n)$y
    ctd <- oce::as.ctd(salinity=salinity,
        temperature=temperature,
        pressure=pressure,
        latitude=latitude,
        longitude=longitude)
    ctd@data$cycleIndex <- cycleIndex
    if (is.null(TSControl)) {
        argoFloatsDebug(debug, "defaulting TSControl\n")
        TSControl <- list(groupByCycle=NULL)
    }
    #message("next is col:");print(col)
    #cat("next is cex inside call (spot 1):\n");cat(vectorShow(cex))
    if (is.null(TSControl$groupByCycle)) {
        argoFloatsDebug(debug, "TSControl does not contain groupByCycle\n")
        if (is.null(col)) {
            col <- "flags"
        }
    } else {
        #? argoFloatsDebug(debug, "TSControl contains groupByCycle\n")
        #? cycle <- unlist(x[["cycle", debug=debug-1L]])
        #? lengths <- sapply(x[["argos"]], function(cc) length(cc[["pressure"]]))
        #? # Increase the col length, so e.g. TSControl=list(groupByCycle=1:2) will alternate colours
        #? groupByCycle <- rep(TSControl$groupByCycle, length.out=length(cycle))
        #? col <- rep(col, length.out=ncycles)
        #? cex <- rep(cex, length.out=ncycles)
        #? #print(cex)
        #? pch <- rep(pch, length.out=ncycles)
        #? type <- rep(type, length.out=ncycles)
        #? #col <- unlist(lapply(seq_len(ncycles),
        #? #        function(i)
        #? #            rep(col[i], lengths[i])))
        #? #type <- rep(TSControl$type, length.out=ncycles)
    }
    #cat("next is cex inside call (spot 2):\n");cat(vectorShow(cex))
    # Calculate whether data are good
    pressureFlag <- unlist(x[["pressureFlag"]])
    salinityFlag <- unlist(x[["salinityFlag"]])
    temperatureFlag <- unlist(x[["temperatureFlag"]])
    # Consider these to be good: 1=good, 2=probably good, 5=changed, 8=estimated
    # Consider these to be bad: 3=bad data that are potentially correctable,
    # 4=bad data, 6=not used, 7=not used, 9=missing value.
    goodp <- pressureFlag %in% c(1, 2, 5, 8)
    goodT <- temperatureFlag %in% c(1, 2, 5, 8)
    goodS <- salinityFlag %in% c(1, 2, 5, 8)
    goodData <- goodp & goodS & goodT
    unassessedp <- pressureFlag == 0
    unassessedT <- temperatureFlag == 0
    unassessedS <- salinityFlag == 0
    unassessedData <- unassessedp & unassessedS & unassessedT
    # Colour-code by flag status (may be overruled by the user setting colors)
    if (identical(col[1], "flags")) {
        argoFloatsDebug(debug, "col is \"flags\"\n")
        col <- ifelse(goodData, 1, ifelse(unassessedData, "gray", 2))
        # FIXME: pch=21 does not work.  I think the new name might be pt.bg or something;
        # using bg colours field. Or is it 'fill'? Revisit this later.
        if (pch == 21) {
            bg <- ifelse(goodData, 1, ifelse(unassessedData, "gray", 2))
        }
    }
    argoFloatsDebug(debug, "about to set xlim and ylim\n")
    if (is.null(xlim)) {
        Slim <- if (eos == "gsw") range(ctd[["SA"]], na.rm=TRUE)
            else range(ctd[["salinity"]], na.rm=TRUE)
        argoFloatsDebug(debug, "inferred xlim=", paste(round(Slim, 5), collapse=" "), " from data\n", sep="")
    } else {
        Slim <- xlim
        argoFloatsDebug(debug, "using provided ylim=", paste(round(Slim, 5), collapse=" "), "\n", sep="")
    }
    if (is.null(ylim)) {
        Tlim <- if (eos == "gsw") range(ctd[["CT"]], na.rm=TRUE)
            else range(ctd[["theta"]], na.rm=TRUE)
        argoFloatsDebug(debug, "inferred ylim=", paste(round(Tlim, 5), collapse=" "), " from data\n", sep="")
    } else {
        Tlim <- ylim
        argoFloatsDebug(debug, "using provided ylim=", paste(round(Tlim, 5), collapse=" "), "\n", sep="")
    }
    if (isTRUE(TSControl$groupByCycle)) {
        argoFloatsDebug(debug, "colorizing by index\n")
        cycles <- unique(cycleIndex)
        ncycles <- length(cycles)
        argoFloatsDebug(debug, "Slim=c(", paste(Slim, collapse=","), ")\n", sep="")
        argoFloatsDebug(debug, "Tlim=c(", paste(Tlim, collapse=","), ")\n", sep="")
        if (length(longitude) != length(salinity))
            longitude <- rep(longitude[1], length(salinity))
        if (length(latitude) != length(salinity))
            latitude <- rep(latitude[1], length(salinity))
        if (length(cex) != ncycles)
            cex <- rep(cex, length.out=ncycles)
        if (length(col) != ncycles)
            col <- rep(col, length.out=ncycles)
        if (length(type) != ncycles)
            type <- rep(type, length.out=ncycles)
        if (length(pch) != ncycles)
            pch <- rep(pch, length.out=ncycles)
        argoFloatsDebug(debug, "cycle-by-cycle overlay, since groupByCycle is TRUE\n")
        message('debug=',debug)
        for (i in seq_len(ncycles)) {
            argoFloatsDebug(debug>1L, "  handling cycle ", i, " of ", ncycles, ", which has ",
                length(salinity[i==cycleIndex]), " points\n", sep="")
            look <- i == cycleIndex
            ctd <- oce::as.ctd(
                salinity=salinity[look],
                temperature=temperature[look],
                pressure=pressure[look],
                latitude=latitude[look],
                longitude=longitude[look])
            if (i == 1L) {
                plotTS(ctd, Slim=Slim, Tlim=Tlim,
                    cex=cex[i], col=col[i], pch=pch[i], type=type[i],
                    mar=par("mar"), mgp=par("mgp"), eos=eos, ...)
            } else {
                plotTS(ctd, add=TRUE,
                    cex=cex[i], col=col[i], pch=pch[i], type=type[i],
                    mar=par("mar"), mgp=par("mgp"), eos=eos, ...)
            }
        }
    } else {
        argoFloatsDebug(debug, "making single plotTS() call, since groupByCycle is FALSE\n")
        if (colGiven) {
            oce::plotTS(ctd, Slim=Slim, Tlim=Tlim, col=col, pch=pch, cex=cex, ...)
        } else {
            #print(table(col))
            ctdBad <- subset(ctd, !goodData)
            #DAN<<-list(ctd=ctd, ctdBad=ctdBad, goodData=goodData)
            oce::plotTS(ctd, Slim=Slim, Tlim=Tlim, col=col, pch=pch, cex=cex, ...)
            #?oce::plotTS(ctdBad, col=2, pch=pch, cex=cex, add=TRUE)
            #?oce::plotTS(ctd, cex=cex, bg=bg, col=col, pch=pch,
            #?    mar=par("mar"), mgp=par("mgp"), eos=eos,
            #?    type=if (is.null(type)) "p" else type[1], debug=debug-1L)
        }
    }
}

