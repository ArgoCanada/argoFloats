library(oce)
library(stringr)
library(ocedata)
data <- read.csv("sandbox/jlh/42_noaa/ODV4903622QC.csv", header=TRUE)
names(data) # this tells us names to use in accessing the data

# 4903622
# Now to get the temperature, salinity, and pressure:
data2 <- data[data$Cruise == "4903622",]

# Make the dates in each data frame to be consistent
# data2 is in mm/dd/yyyy. df2 is in dd/MON/yyyy
df2 <- read.csv("sandbox/jlh/42_noaa/4903622_MLD_SSH_16Feb2023.csv")
dates <- gsub("(.+?)(\\ .*)", "\\1", df2$DATE) # Only grabs until the year
dates <- str_replace(dates, "-", "/") # Replace "-" with "/" to be consistent with data2 format
dates <- str_replace(dates, "-", "/") # Replaces the second "-" with "/"

# Replace the name of the month with the number for the month
time <- NULL
for (i in seq_along(dates)) {
    if (gsub("^[^/]+/|/.*", "", dates[i]) == "Jan") { # The gsub finds what's between the two //
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "1")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "Feb") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "2")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "Mar") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "3")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "Apr") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "4")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "May") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "5")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "Jun") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "6")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "Jul") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "7")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "Aug") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "8")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "Sep") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "9")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "Oct") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "10")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "Nov") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "11")
    } else if (gsub("^[^/]+/|/.*", "", dates[i]) == "Dec") {
        time[[i]] <- str_replace(dates[i], gsub("^[^/]+/|/.*", "", dates[i]), "12")
    }
}

# In df2, we now need to switch from dd/mm/yyyy to mm/dd/yyyy (similar to data2)
time <- unlist(time)
fixedDates <- NULL
for (i in seq_along(time)) {
mm <- gsub("^[^/]+/|/.*", "", time[i]) # Extracts between two / /
dd <- str_extract(time[i], "[^/]+") # Obtains until the first / occurrence
dd <- str_remove(dd, "^0+") # Removes leading zeros (ie. turns 01 to 1)
yyyy <- gsub(".*/","",time[i]) # Removes everything until final /
fixedDates[[i]] <- paste0(mm,"/",dd,"/", yyyy, collapse="")
}

fixedDates <- unlist(fixedDates)

df2$DATE <- fixedDates # now df2 is in mm/dd/yyyy

# Only keeping temperature/salinity etc. for dates with Eddy data
df2 <- df2[which(df2$DATE %in% unique(data2$date)),]
data2 <- data2[which(data2$date %in% df2$DATE),]

X <- split(data2, data2$date) # Break up data frame to be different cycles (see data2)

# Add eddy phase to temperature/salinity, etc. data
for (i in seq_along(X)) {
    keep <- which(df2$DATE == unique(X[[i]]$date))[1]
    X[[i]]$eddy <- rep(df2$EDDY[keep], length(X[[i]]$Lon))
}

ctds <- NULL
# Note: You could add Nitrate, etc. using oceSetData
for (i in seq_along(X)) {
ctd <- as.ctd(salinity=X[[i]]$Salinity, temperature=X[[i]]$Temperature, pressure=X[[i]]$Pressure, longitude=X[[i]]$Lon, latitude=X[[i]]$Lat)
ctd <- oceSetMetadata(ctd, name="eddy", value=unique(X[[i]]$eddy))
ctd <- oceSetData(ctd, name="oxygen", value=X[[i]]$Oxygen) 
ctds[[i]] <- oceSetMetadata(ctd, name="time", value=rep(as.POSIXct(X[[i]]$date, format="%m/%d/%Y", tz="UTC"), length(X[[i]]$Oxygen))) # Look at strptime documentation for % explanation
}

# Plotting map of the data
# Group and plot parameters (MLD, SSH, eddy). In this case we've done eddy
# Seasonal figures, for example an anticyclonic eddy profile plot showing an oxygen profile for winter and summer.

data(coastlineWorldFine)
mapPlot(coastlineWorldFine, latitudelim = c(10,40), longitudelim = c(-100,-70), col="tan")
for (i in seq_along(ctds)) {
    mapPoints(latitude=ctds[[i]][['latitude']], longitude=ctds[[i]][['longitude']], col=i, pch=20)
}

# Plotting oxygen profiles
# anticyclonic eddy profile plot showing an oxygen profile for winter and summer.

eddys <- unlist(lapply(ctds, function(x) x[["eddy"]]))

anti <- ctds[which(eddys == 3)] # anticyclonic

# Looking at seasons
par(mar=c(3,3,1,1))
t <- do.call(c, lapply(anti, function(x) unique(x[["time"]])))
cm <- colormap(t, col=oceColorsJet)
drawPalette(colormap=cm, tformat="%Y-%m", pos=3)
plotProfile(anti[[1]], xtype="oxygen", xlim=c(0,500), type="p", pch=20)
for (i in seq_along(anti)) {
    points(anti[[i]][['oxygen']], anti[[i]][['pressure']], col=cm$zcol, pch=20)
}



#4903624
df4 <- read.csv("sandbox/jlh/42_noaa/4903624_MLD_SSH_16Feb2023.csv")

#df4903625
df5 <- read.csv("sandbox/jlh/42_noaa/4903625_MLD_SSH_16Feb2023.csv")


