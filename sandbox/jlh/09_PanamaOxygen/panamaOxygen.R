library(oce)
library(argoFloats)

# Acquire merged argo index, subset by circle, and then subset by oxygen
if(!exists("bai"))
bai <- getIndex(filename='synthetic')
s <- subset(bai, circle=list(longitude=-83, latitude=9, radius=800))
#plot(s, which='map') # To get a visual
pax <- c(-92.56199, -87.32731, -83.66646, -79.35555, -88.28529, -92.76727, -92.76727, -92.73306)
pay <- c(15.204447, 12.422940,  9.071727,  5.653489,  5.720514,  5.653489, 10.546261, 15.439032)
a <- subset(s, polygon=list(longitude=pax, latitude=pay))
subo <- subset(a, parameter='DOXY')
#plot(subo)
# Download argo files, read them, and thn extract O2
profiles <- getProfiles(subo)
argos <- readProfiles(profiles)
adjusted <- useAdjusted(argos)
plot(argos, which='profile', parameter='oxygen', col='black')
par(new=TRUE)
plot(adjusted, which='profile', parameter='oxygen', col='blue')
