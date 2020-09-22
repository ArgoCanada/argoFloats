library(argoFloats)
library(oce)
if (exists("argos")) { # cache for speed
    message("using cached 'argos'")
} else {
    message("caching 'argos'")
    data("indexSynthetic")
    subset <- subset(indexSynthetic, 1:10)
    argos <- readProfiles(getProfiles(subset))
}
x <- argos
parameter <- "oxygen"
N <- length(x[['argos']])
varNames <- unique(unlist(lapply(1:N, function(i) names(x[[i]][['data']]))))
if (!parameter %in% varNames)
	stop('jaimie')
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
            pp <- c(pp, pressure[[i]])
            vv <- c(vv, variable[[i]])
	}
	cat(vectorShow(i))
	cat(vectorShow(length(pp)))
	cat(vectorShow(length(vv)))
}
o <- new("ctd")
o <- oceSetData(o, "pressure", pp, unit=punit)
o <- oceSetData(o, parameter, vv, unit=vunit)
summary(o)
plotProfile(o, xtype="oxygen", type="p")

