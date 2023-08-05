# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

plotArgoTS <- function(x, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL,
    type="p", cex, col, pch, bg, eos="gsw", TSControl=NULL, debug=0)
{
    if (!inherits(x, "argoFloats"))
        stop("x must be an argo object")
    if ((x[["type"]] != "argos"))
        stop("x must be the result of a call to readProfiles()")
    if (!(eos %in% c("gsw", "unesco")))
        stop("eos must be \"gsw\" or \"unesco\", not \"", eos, "\"")
    argoFloatsDebug(debug, "plotArgoTS(x, ..., eos=\"", eos, "\", ...) {\n", sep="", unindent=1, style="bold")
    if (!(eos %in% c("gsw", "unesco")))
        stop("In plot,argoFloats-method(): eos must be \"gsw\" or \"unesco\", not \"", eos, "\"", call.=FALSE)
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
    if (identical(col[1], "flags")) {
        argoFloatsDebug(debug, "col is \"flags\"\n")
        salinityFlag <- unlist(x[["salinityFlag"]])
        temperatureFlag <- unlist(x[["temperatureFlag"]])
        goodT <- temperatureFlag %in% c(1, 2, 5, 8)
        goodS <- salinityFlag %in% c(1, 2, 5, 8)
        good <- goodS & goodT
        okT <- temperatureFlag %in% c(0)
        okS <- salinityFlag %in% c(0)
        ok <- okS & okT
        col <- ifelse(good, "black", ifelse(ok, "gray", "red"))
        if (pch == 21)
            bg <- ifelse(good, "black", ifelse(ok, "gray", "red"))
    }
    argoFloatsDebug(debug, "about to plotTS()\n")
    if (isTRUE(TSControl$groupByCycle)) {
        argoFloatsDebug(debug, "colorizing by index\n")
        cycles <- unique(cycleIndex)
        ncycles <- length(cycles)
        if (eos == "gsw") {
            Slim <- range(ctd[["SA"]], na.rm=TRUE)
            Tlim <- range(ctd[["CT"]], na.rm=TRUE)
        } else {
            Slim <- range(ctd[["salinity"]], na.rm=TRUE)
            Tlim <- range(ctd[["temperature"]], na.rm=TRUE)
        }
        argoFloatsDebug(debug, "set Slim=", Slim, "\n")
        argoFloatsDebug(debug, "set Tlim=", Tlim, "\n")
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
        argoFloatsDebug(debug, "cycle-by-cycle overlay, since groupByCycle is FALSE\n")
        for (i in seq_len(ncycles)) {
            argoFloatsDebug(debug-1L, "    handling cycle ", i, " of ", ncycles, ", which has ",
                length(salinity[i==cycleIndex]), " points\n", sep="")
            ctd <- oce::as.ctd(
                salinity=salinity[i == cycleIndex],
                temperature=temperature[i == cycleIndex],
                pressure=pressure[i == cycleIndex],
                latitude=latitude[i == cycleIndex],
                longitude=longitude[i == cycleIndex])
            if (i == 1L) {
                plotTS(ctd, Slim=Slim, Tlim=Tlim,
                    cex=cex[i], col=col[i], pch=pch[i], type=type[i])
            } else {
                plotTS(ctd, add=TRUE,
                    cex=cex[i], col=col[i], pch=pch[i], type=type[i])
            }
        }
    } else {
        argoFloatsDebug(debug, "making single plotTS() call, since groupByCycle is FALSE\n")
        oce::plotTS(ctd,
            cex=cex,
            bg=bg,
            col=col,
            pch=pch,
            mar=par("mar"), mgp=par("mgp"), eos=eos,
            type=if (is.null(type)) "p" else type)
    }
}

