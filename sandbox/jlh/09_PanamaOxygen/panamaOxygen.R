library(oce)
library(argoFloats)

# Acquire merged argo index, subset by circle, and then subset by oxygen
bai <- getIndex(filename='bgc')
s <- subset(bai, circle=list(longitude=-83, latitude=9, radius=800))
plot(s, which='map') # To get a visual
pax <- c(-92.56199, -87.32731, -83.66646, -79.35555, -88.28529, -92.76727, -92.76727, -92.73306)
pay <- c(15.204447, 12.422940,  9.071727,  5.653489,  5.720514,  5.653489, 10.546261, 15.439032)
a <- subset(s, polygon=list(longitude=pax, latitude=pay))
subo <- subset(a, parameter='DOXY')

# Download argo files, read them, and thn extract O2
profiles <- getProfiles(subo)
argos <- readProfiles(profiles)
pressure <- unlist(argos[["pressure"]])
oxygen <- unlist(argos[["oxygen"]])
plot(oxygen, pressure, ylim=rev(range(pressure, na.rm=TRUE)))

## The oxygen is fill with NA values. Need to get familiar with coplot. 