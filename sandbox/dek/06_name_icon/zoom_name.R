## Two colours picked with "gimp" application from the 2500 year-old
## argonaut vase shown in https://en.wikipedia.org/wiki/Argonauts
bright <- "#d98853"
dark <- "#4a4326"
name <- "Dan Kelley"
fg <- bright
bg <- dark
filename <- paste0("zoom_name_", gsub(" ", "_", name), ".png")
text <- gsub(" ", "\n", name)


if (!interactive()) png(filename, width=200, height=200, pointsize=40)
par(fg=fg, bg=bg)
par(mar=rep(0,4))
plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
text(0.5, 0.5, text, font=2)
if (!interactive()) dev.off()

