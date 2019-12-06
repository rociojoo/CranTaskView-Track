## Load the package
library("ctv")

## Read the Task View
track <- read.ctv("Tracking.ctv")

## Convert to HTML
ctv2html(track, file = "Tracking.html")

## Check if everything's OK
ctv::check_ctv_packages("Tracking.ctv")

## Getting table with release dates
library(kableExtra)
info_pkgs <- read.csv("checks/RmovementPackagesInformation_checked.csv")
info_pkgs_cran <- info_pkgs[!is.na(info_pkgs$recent_publish_data), c("Package","recent_publish_data")]
names(info_pkgs_cran) <- c("Package", "Published date")
rownames(info_pkgs_cran) <- NULL
x <-  kable(info_pkgs_cran, format = "html", caption = "Published date of the examined CRAN packages")
save_kable(kable_styling(x), file = "Pkg-Date.html", bs_theme = "simplex")
