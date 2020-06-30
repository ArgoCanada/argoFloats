## Download several files, to find a short one
## ftp://ftp.ifremer.fr/ifremer/argo/dac/aoml/5903586/profiles
library(argoFloats)
i <- getIndex('synthetic')
head(subset(i, ID='5903586')[["file"]])
s <- subset(subset(i, ID='5903586'), 1:5)
s[["file"]]
p <- getProfiles(s, debug=3)
p[["file"]]
a <- readProfiles(p, debug=1)
for (i in seq_len(a[["length"]])) {
    aa <- a[[i]]
    summary(aa)
    system(paste("cp", aa[["filename"]], "."))
}

