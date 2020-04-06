## This was not what we ended up using, but it is good to know how to grab different numbers
library(argoFloats)
ai <- getIndex(file='merged', destdir = '~/data/argo')
data('index')
index[['index']]
names(index[['index']])
index[['index']][['profiler_type']][1:5]
digit1 <- floor(index[['index']][['profiler_type']]/100) # ?floor
head(digit1)
digit2 <- floor(floor(index[['index']][['profiler_type']]-100*digit1)/10)
head(digit2)
digit3 <- floor(index[['index']][['profiler_type']]-(100*digit1)-(10*digit2))
head(digit3)

#pDeep1 <- grep("digit1=8&digit2=4&digit3=9", index[['index']][['profiler_type']])
#pDeep2 <- grep("digit1=8&digit2=6&digit3=2", index[['index']][['profiler_type']])
#pDeep3 <- grep("digit1=8&digit2=6&digit3=4", index[['index']][['profiler_type']])

#Using grep function for deep profiles
deep1 <- grepl("849", ai@data$index$profiler_type)
sum(deep1)
deep2 <- grepl("862", ai@data$index$profiler_type)
sum(deep2)
deep3 <- grepl("864", ai@data$index$profiler_type)
sum(deep3)
keepdeep <- grepl("849|862|864", ai@data$index$profiler_type)
keepdeep