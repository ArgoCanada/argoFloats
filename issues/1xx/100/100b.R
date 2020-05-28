library(argoFloats)
bai <- getIndex(filename='merge')
s <- subset(bai, circle=list(longitude=-83, latitude=9, radius=800))
pax <- c(-92.56199, -87.32731, -83.66646, -79.35555, -88.28529, -92.76727, -92.76727, -92.73306)
pay <- c(15.204447, 12.422940,  9.071727,  5.653489,  5.720514,  5.653489, 10.546261, 15.439032)
a <- subset(s, polygon=list(longitude=pax, latitude=pay))
subo <- subset(a, parameter='DOXY')

profiles <- getProfiles(subo)
argos <- readProfiles(profiles)
argosRaw <- readProfiles(profiles, handleFlags=FALSE)
pressure <- unlist(argos[["pressure"]])
oxygen <- unlist(argos[["oxygen"]])
oxygenRaw <- unlist(argosRaw[["oxygen"]])
summary(oxygen)
percentNA <- 100 * sum(is.na(oxygen)) / length(oxygen)

message("Float ID(s): ", paste(sort(unique(subo[["ID"]])), collapse=", "))
message(paste("With default readProfiles(), have ", length(oxygen), "oxygen values, of which", sum(is.na(oxygen)), " are NA"))
message(paste("With readProfiles(...,handleFlags=FALSE), have ", length(oxygenRaw), "oxygen values, of which", sum(is.na(oxygenRaw)), " are NA"))

message("profile 1, temperature flags:")
table(argos[[1]][["flags"]]$temperature)

message("profile 1, oxygen flags:")
table(argos[[1]][["flags"]]$oxygen)

summary(argos[[1]])
