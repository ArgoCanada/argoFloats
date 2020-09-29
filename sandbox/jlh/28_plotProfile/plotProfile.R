library(argoFloats)
library(oce)
data("indexSynthetic")
subset <- subset(indexSynthetic, 1:10)
argos <- readProfiles(getProfiles(subset))
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
for (i in seq_len(N)) {
	message("i = " ,i)
	if (nn[i] > 0) {
		pp <- c(pp, pressure[[i]])
		vv <- c(vv, variable[[i]])
	}
	message("length(p) = " ,length(pp))
	message("length(v) = " ,length(vv))
}
#plot(vv,pp, ylim=rev(range(pp)))

