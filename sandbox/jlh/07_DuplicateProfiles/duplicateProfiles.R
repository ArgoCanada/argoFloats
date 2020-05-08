library(argoFloats)
ai <- getIndex()

# Doing first sample
set.seed(1) # First sample
n <- 10
s1 <- subset(ai, sample(seq_along(ai[['longitude']]), n))
p1 <- getProfiles(s1)
a1 <- readProfiles(p1)
#salinity1 <- a1[['argos']][[1]][['salinity']] # Only looking at the first one (there are 30)
#dim(salinity1)

for (trial in 1:5){
    cat("Trial=", trial, "\n")
for (i in 1:n) {
    S <- a1[['argos']][[i]][['salinity']]
    if (2== dim(S)[2]) {
        cat(i)
        nok <- sum(!is.na(S[,2]))
        nna <- 100*nok/length(S[,2])
        cat(" ", length(S[,2]), " ", nok," ", nna, "% \n")
                   }
                 }
                       }

# ifelse(dim(salinity1)[2] == 2, 'TRUE', 'FALSE') # If there is a second column, write 'TRUE'

stop()
# Doing second sample
set.seed(2) # First sample
n <- length(ai[['latitude']])
s2 <- subset(ai, sample(seq_along(ai[['longitude']]), 10))
p2 <- getProfiles(s2)
a2 <- readProfiles(p2)
salinity2 <- a2[['argos']][[2]][['salinity']] # only for the first one, there are 30
dim(salinity2)

f2 <- for (i in 1:30) {
    eq2 <- dim(a2[['argos']][[i]][['salinity']])
    print(eq2)
    
}

if2 <- ifelse(f2[2] == 2, 'TRUE', 'FALSE') # If there is a second column, write 'TRUE'