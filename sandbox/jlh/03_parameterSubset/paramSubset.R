#Setup

library(argoFloats)
library(oce)

#Download Index
getIndex(
    server = "ftp://usgodae.org/pub/outgoing/argo",
    filename = "argo_merged-profile_index.txt.gz",
    destdir = "~/data/argo",
    age = 6,
    quiet = FALSE,
    debug = 3
)

if (!exists("ai"))
    ai <- getIndex(filename='argo_merged-profile_index.txt.gz', destdir="~/data/argo",debug=3, age=0)


load("~/data/argo/argo_bio-profile_index.txt") 
str(argoFloatsIndex,1) #Clarify argoFloatsIndex
str(argoFloatsIndex$index) # Want to be working with the index (it's the data frame)
head(argoFloatsIndex$index$parameters,3) # To focus on parameters
look<- grepl("([ ]DOXY)|(^DOXY)", argoFloatsIndex$index$parameters) # This is using logical grep, saying if something is true or not.It also uses regexp to make sure it is only getting DOXY, and not, for example, BPAHSE_DOXY
#look[1:3]
lines[look] # to solely write the lines that have isolates what we want.

# To find out how many:

sum(look)/ length(look) # This indicates there is a lot of oxygen (94%). 

# Now it's time to add this to subset method. 

# Creating an example for the paramter subset. 
ai <- getIndex(filename='merge', destdir='~/data/argo')
head(ai@data$index$parameters,3) # To focus on parameters
aiDoxy<- grepl("([ ]DOXY)|(^DOXY)", ai@data$index$parameters)
sum(aiDoxy)/length(aiDoxy)



