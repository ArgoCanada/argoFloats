library(argoFloats)
ai <- getIndex()
s <- subset(ai, circle=list(longitude=-83, latitude=9, radius=500))
plot(s, which='map') # To get a visual
atlantic <- subset(s, ocean='A') # Subsetting for Atlantic Ocean
pacific <- subset(s, ocean='P')
points(atlantic[['longitude']], atlantic[['latitude']], pch=20, col=2)
points(pacific[['longitude']], pacific[['latitude']], pch=20, col=3)