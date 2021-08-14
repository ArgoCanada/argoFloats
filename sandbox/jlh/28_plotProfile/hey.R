library(oce)
data(ctd)
ctd@data$temperature[10:20] <- NA
plotProfile(ctd, xtype="temperature", type="o", keepNA=TRUE)
