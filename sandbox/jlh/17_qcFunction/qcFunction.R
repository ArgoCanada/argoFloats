qf <- function(a) {
    if(!inherits(a, 'argos'))
        ("Can only provide Quality Control Summary for a single argo profile")
    result <-a[['HISTORY_QCTEST']]
    action <- a[['HISTORY_ACTION']]
    performed <- which(action == 'QCP$')
    failed <- which(action == 'QCF$')
    B <- function(x) ifelse(tail(rev(rawToBits(as.raw(paste0('0x0',x)))),4) == '01', 1, 0)
    BB <- function(x) unlist(lapply(strsplit(x, '')[[1]], function(l) B(l)))
    #cat("Tests performed:", paste(which(1 == tail(rev(BB('QCP$')),-1)), collapse=' ')) # make these general
    #cat("Tests failed:", paste(which(1 == tail(rev(BB('QCF$')),-1)), collapse=' ')) # make these general
    
    cat("Tests performed:", paste(which(1 == tail(rev(BB('5f03e')),-1)), collapse=' ')) # make these general
    cat("Tests failed:", paste(which(1 == tail(rev(BB('4000')),-1)), collapse=' ')) # make these general
}
