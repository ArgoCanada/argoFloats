ai <- getIndex(file='argo', destdir='~/data/argo')
names(ai@data$index)
ai@data$index$file[1]
keep<- grepl("([ ]DOXY)|(^DOXY)", argoFloatsIndex$index$parameters)


a <- ai@data$index$file[1]
gg <- grepl("^[a-z]*/[0-9]*/profiles/[A-Z]*([0-9]*_[0-9]{3}).nc", "\\1", ai@data$file)

parameters <- unlist(parameter)
nparameters <- length(parameters)
parametersList <- lapply(x[["parameters"]], function(p) strsplit(p, " ")[[1]])
keep <- unlist(lapply(parametersList, function(pl) nparameters == sum(parameters %in% pl)))