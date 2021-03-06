## Load the package
library("ctv")

## Read the Task View
track <- read.ctv("Tracking.ctv")

## Convert to HTML
ctv2html(track, file = "Tracking.html")

## Check if everything's OK
ctv::check_ctv_packages("Tracking.ctv")

##########################################

## Checking imports and suggest networks
info_pkgs <- read.csv("checks/RmovementPackagesInformation_checked.csv")
info_pkgs_cran <- info_pkgs[!is.na(info_pkgs$recent_publish_data), c("Package","recent_publish_data")]
names(info_pkgs_cran) <- c("Package", "Published date")
rownames(info_pkgs_cran) <- NULL

## We only care about the sum of imports and suggest; not making a difference for 
## this analysis
info_import_suggest <- info_pkgs[ !is.na(info_pkgs$recent_publish_data) | (!is.na(info_pkgs$cran_check) & info_pkgs$cran_check == TRUE) , c("Package", "imports", "suggests")]
# separate imported packages by commas
df_import <- strsplit(as.character(info_import_suggest$imports), split = ",")
df_import_space <- lapply(X = df_import, FUN = strsplit, " ")
df_import_noR <- lapply(df_import_space, FUN =  function(x){
  sub_list <- unlist(x)
  return(sub_list[! (sub_list %in% c("R",""))])
})
# get the number of elements in each element (dependencies for each package)
size_list_import <- unlist(lapply(X = df_import_noR, FUN = length))
# create empty data.frame with that number of elements
df_import_gather <- data.frame(package = rep.int(info_import_suggest$Package, times = size_list_import), 
                               imports = unlist(df_import_noR), role = rep("import", sum(size_list_import)))
# now same thing for suggest
df_suggest <- strsplit(as.character(info_import_suggest$suggests), split = ",")
df_suggest_space <- lapply(X = df_suggest, FUN = strsplit, " ")
df_suggest_nospace <- lapply(df_suggest_space, FUN =  function(x){
  sub_list <- unlist(x)
  return(sub_list[! (sub_list %in% c("R",""))])
})
size_list_suggest <- unlist(lapply(X = df_suggest_nospace, FUN = length))
df_suggest_gather <- data.frame(package = rep.int(info_import_suggest$Package, times = size_list_suggest), 
                                imports = unlist(df_suggest_nospace), role = rep("suggest", sum(size_list_suggest)))
# one data frame to rule them all
df_imp_sugg <- rbind.data.frame(df_import_gather, df_suggest_gather)
# filtering out non movement packages
df_imp_sugg_mov <- df_imp_sugg[which(df_imp_sugg$imports %in% info_import_suggest$Package == TRUE), ]
# counting how much is package is needed or suggested
table_imp_sugg <- table(as.character(df_imp_sugg_mov$imports))
# formatting the table
df_freq <- data.frame(package = names(table_imp_sugg), mentions = as.numeric(table_imp_sugg))
df_freq <- df_freq[order(df_freq$mentions,decreasing = TRUE),]
df_freq$t <- 1:dim(df_freq)[1]
library(coin)
maxstat_test(mentions ~ t, dist = "approx", data = df_freq)
# Approximative Generalized Maximally Selected Statistics
# 
# data:  mentions by t
# maxT = 3.0859, p-value = 0.0326
# alternative hypothesis: two.sided
# sample estimates:
#   “best” cutpoint: <= 2
#   




## Getting table with release dates
library(kableExtra)

df_mentions <- df_freq[,c("package","mentions")]
df_date_mentions <- merge(info_pkgs_cran, df_mentions, by.x = "Package", by.y = "package", all.x = TRUE)
df_date_mentions$mentions[is.na(df_date_mentions$mentions)] <- 0
names(df_date_mentions) <- c("Package", "Published date", "Mentions")

x <-  kable(df_date_mentions, format = "markdown", caption = "Published date of the examined CRAN packages and number of mentions as dependency, import or suggestion within our list of packages")
kable_styling(x)
# kable_as_image(kable_styling(x), filename = "Pkg-Date", file_format = "png", density = 150)
# save_kable(kable_styling(x), file = "Pkg-Date.md", bs_theme = "simplex")

