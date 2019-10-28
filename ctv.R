## Load the package
library("ctv")

## Read the Task View
track <- read.ctv("Tracking.ctv")

## Convert to HTML
ctv2html(track, file = "Tracking.html")
