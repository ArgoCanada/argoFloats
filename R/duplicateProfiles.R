library(argoFloats)
ai <- getIndex(file='merged')
names (ai[['index']]) # To find out header names of file
profiles <- getProfiles(ai)