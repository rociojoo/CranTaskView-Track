## Load the package
library("ctv")

## Read the Task View
track <- read.ctv("Tracking.ctv")

## Check if everything's OK
check_ctv_packages("Tracking.ctv")

## Convert to HTML
ctv2html(track, file = "Tracking.html")
