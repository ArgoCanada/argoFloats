library(argoFloats)
source("style.R")
ID <- "5903586"                # saved to rda so text can use it
cycle <- "001"                 # saved to rda so text can use it
index <- subset(subset(getIndex("synthetic"), ID=ID), cycle=cycle)
raw <- readProfiles(getProfiles(index))
#adjusted <- useAdjusted(raw)
adjusted <- useAdjustedNEW(raw, fallback="raw")

if (!interactive()) png("09_useAdjusted.png",
                        unit="in",
                        width=style$width,
                        height=4/7*style$width,
                        pointsize=style$pointsize,
                        res=style$res)
par(mar=c(1,3.5,3.5,1))
plot(adjusted, which="profile", profileControl=list(parameter="oxygen"), type="o")
grid()
points(raw[[1]][["oxygen"]], raw[[1]][["pressure"]], pch=3, type="o")
legend("bottomright", pch=c(3,1), c("Raw", "Adjusted"), bg="white")

R <- raw[[1]][["oxygen"]]
A <- adjusted[[1]][["oxygen"]]
model <- lm(A ~ R)

if (!interactive()) dev.off()
adjusted <- list(ID=ID, cycle=cycle, model=model)
save(adjusted, file="09_useAdjusted.rda")

