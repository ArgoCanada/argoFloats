## Get the context for some cases with NA dates
library(argoFloats)
if (!exists("ai")) {                   # cache to speed up development
    ai <- getIndex()
    lat <- ai[["latitude"]]
    lon <- ai[["longitude"]]
    date <- ai[["date"]]
}
NATime <- !is.finite(date)
cat(sprintf("* %d profiles (%.2g%%) have bad date\n", sum(NATime), 100*sum(NATime)/length(date)))
ws <- tail(which(NATime), 3)
for (w in ws) {
    print(ai[[seq(-3, 3) + w]], width=200)
}

