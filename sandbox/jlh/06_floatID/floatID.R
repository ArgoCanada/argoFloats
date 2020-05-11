#Subsetting for float ID
ai <- getIndex(filename='merge', destdir='~/data/argo')
names(ai@data$index)
a <- ai@data$index$file[1] # To determine format of file name
gg <- gsub("^[a-z]*/[0-9]*/profiles/[A-Z]*([0-9]*_[0-9]{3}).nc", "\\1", a)
#floatID <- gsub("^[a-z]*/([0-9]*)/profiles/[A-Z]*[0-9]*_[0-9]{3}.nc$", "\\1", a)


# [1] "aoml/1900722/profiles/MD1900722_003.nc"

# ^ Matching from the start of the string
# [a-z] means there is 0 or more lower case letters (this will grab amol)
# Then there is /
# [0-9] means we want 0 or more numbers, meaning it'll grab 1900722
# Then we see /
# Then we see profiles
# When we see [A-Z]; which grabs for uppercase MD
# Then we see (, which tells the command to not match for this, but rather we want to remember it
# 

floatID <- function(s) gsub("^[a-z]*/([0-9]*)/profiles/[A-Z]*[0-9]*_[0-9]{3}.nc$", "\\1", s)
floatID(a)
