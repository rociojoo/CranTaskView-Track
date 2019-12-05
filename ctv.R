## Load the package
library("ctv")

## Read the Task View
track <- read.ctv("Tracking.ctv")

## Convert to HTML
ctv2html(track, file = "Tracking.html")

## Check if everything's OK
ctv::check_ctv_packages("Tracking.ctv")
