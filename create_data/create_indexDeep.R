## Note that this code is not run, because we want the dataset to be *static*.
## In fact, we check that it is unchanged, in the test suite.

## The download was done on 2020 September 10, and yielded 174 profiles.

if (!FALSE) {
    ## An indexDeep of Argo profile data within 800km
    ## of Antarctica (67S,105E).
    library(argoFloats)
    subset <- subset(getIndex(age=0), deep=TRUE)
    indexDeep <- subset(subset, circle=list(longitude=105, latitude=-67, radius=800))
    save(indexDeep, file="indexDeep.rda")
    tools::resaveRdaFiles('indexDeep.rda')
}
