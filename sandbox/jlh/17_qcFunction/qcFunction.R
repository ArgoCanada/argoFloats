options(warn=1)
library(argoFloats)
data('index')
s <- subset(index,100)
gp <- getProfiles(s)
rp <- readProfiles(gp)
rp1 <- rp[[1]]
rp1[['HISTORY_QCTEST']]
rp1[['HISTORY_ACTION']]

showQCTests <- function(a) {
    if (!inherits(a, 'argo'))
        stop("Can only provide Quality Control Summary for argo object")
    result <-a[['HISTORY_QCTEST']]
    action <- a[['HISTORY_ACTION']]
    performed <- which(action == 'QCP$')
    cat(vectorShow(result))
    failed <- which(action == 'QCF$')
    cat(vectorShow(result[,2]))
    cat(vectorShow(failed))
    # x is a hex string
    B <- function(x) {
        cat(vectorShow(x))
        tmp2 <- paste0('0x0',x)
        cat(vectorShow(tmp2))
        res <- ifelse(tail(rev(rawToBits(as.raw(paste0('0x0',x)))),4) == '01', 1, 0)
        cat(vectorShow(res))
        res
    }
    # 
    BB <- function(x) {
        cat("Next is x to BB \n")
        print(x)
        unlist(lapply(strsplit(x, '')[[1]], function(l) B(l)))
    }
    cat("Tests performed:", paste(which(1 == tail(rev(BB(performed)),-1)), collapse=' '), "\n")
    #cat("Tests failed:   ", paste(which(1 == tail(rev(BB(failed)),-1)), collapse=' '), "\n")
}

showQCTests(rp1)
