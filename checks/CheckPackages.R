## Set up private library (delete it first) and install necessary utility
## packages (devtools, remotes and desc)
##
## /!\ Start with an up-to-date system /!\
unlink("packrat", recursive = TRUE)
packrat::init(infer.dependencies = FALSE)
install.packages(c("devtools", "remotes", "desc"))


## Working directory to download and run checks in.
download_local <- "downloads"
test <- FALSE
pkg_db <- tools::CRAN_package_db()


## Import packages list, and create columns if not exist
track <- read.csv("Tracking_tbl.csv")
track$source <- tolower(track$source)
track$recent_commit <- NA
track$cran_check <- NA
track$warnings <- NA
track$errors <- NA
track$vignette_error <- NA
track$imports <- NA_character_
track$suggests <- NA_character_
track$recent_publish_track <- NA
## set up an error list to check later if installs failed because of
## dependencies the computer doesn't have aaaaaaa whhhhataaaaaaa whhhhataaaaaaa
## whhhhataaaaaaa whhhhat
error_list <- list()
sub <- track # unecessary now


## sub$package_name
i <- 2
for (i in 1:nrow(track)) {
    if(isTRUE(sub$skip[i])) { next }

    ## Check if package even has a repository
    if (!sub$source[i] %in% c("cran", "github", "bioc", "rforge") &
        nchar(as.character(sub$download_link[i])) == 0) {
        message(paste0(
            sub$package_name[i],
            " does not have a selected repository, skipping..."
        ))
        next
    }

    if (sub$source[i] == "cran") {
        sub_pkg <- pkg_db[pkg_db$Package == sub$package_name[i], ]
        if (nrow(sub_pkg) == 0) {
            warning(paste0(sub$package_name[i], " is not on CRAN"))
            track$errors[i] <- TRUE
            track$cran_check[i] <- FALSE
            next
        }
        string <-
            paste(
                ifelse(is.na(sub_pkg$Depends), "", sub_pkg$Depends),
                ifelse(is.na(sub_pkg$Imports), "", sub_pkg$Imports),
                ifelse(is.na(sub_pkg$Suggests), "", sub_pkg$Suggests)
            )
        string <- trimws(gsub("\n" , " ", string), "both")
        string <- gsub("\\(.*\\)|,$", "" , string)
        track$imports[track$package_name == sub$package_name[i]] <- as.character(string)
        string <- ifelse(is.na(sub_pkg$Suggests), "", sub_pkg$Suggests)
        string <- trimws(gsub("\n" , " ", string), "both")
        string <- gsub("\\(.*\\)|,$", "" , string)
        track$suggests[track$package_name == sub$package_name[i]] <-
            ifelse(length(string) == 0, NA, as.character(string))
        ## recent track
        track$recent_publish_track[track$package_name == sub$package_name[i]] <-
            sub_pkg$Published
        track$cran_check[i] <- TRUE
        next
    }

    if (sub$source[i] == "github") {
        branch <- sub$sub[i]
        owner <- sub$owner[i]
        repo <- sub$repository[i]
        download_file <-
            paste0("https://github.com/",
                owner,
                "/",
                repo,
                "/archive/master.zip")
        if (branch == "") {
            working_folder <- paste0(download_local, "/", repo, "-master")
        } else{
            working_folder <-
                paste0(download_local, "/", repo, "-master/", branch)
        }
        download_folder <-
            paste0(download_local, "/", sub$repository[i], ".zip")
    }

    ## For Bioconductor sites, we trawl the website for the correct link, as
    ## versions may change rapidly
    if (sub$source[i] == "bioc" &
        nchar(as.character(sub$download_link[i])) > 2){
        ## update the root Bioconductor URL as versions are released
        bioconductor_url <- "https://bioconductor.org/packages/3.11/bioc/"
        url <-  sub$download_link[i]
        url <- paste0(bioconductor_url, "html/", sub$package_name[i], ".html")
        page <- readLines(as.character(url))
        page[grepl("<h3 id=\"archives\">Package Archives</h3>", page)]
        sub_page <- page[]
        phrase <- sub_page[grep("Source Package",sub_page) + 1]
        phrase
        ## '\" href=\"../src/contrib/SwimR_1.26.0.tar.gz\"'
        match <- regexec("(?:href=\\"\\.\\.)(.*)(?:\\">)", phrase)
        download_file <- paste0(bioconductor_url,regmatches(phrase,match)[[1]][2])
        if(!is.na(sub$subfolder[i])) {
            working_folder <-
                paste0(download_local, "/", sub$package_name[i], "/", sub$subfolder[i])
        } else{
            working_folder <-
                paste0(download_local, "/", sub$package_name[i])
        }
        download_folder <-
            paste0(download_local, "/", sub$package_name[i], ".tar.gz")
    }

    ## R-Forge
    if(sub$source[i] == "rforge") {
        out <- download.packages(sub$package_name[i],
            destdir = download_local, repos = "http://R-Forge.R-project.org")
        download_folder <- out[2]
        if(!is.na(sub$subfolder[i])) {
            working_folder <-
                paste0(download_local, "/", sub$package_name[i], "/", sub$subfolder[i])
        } else{
            working_folder <-
                paste0(download_local, "/", sub$package_name[i])
        }
        untar(tarfile = download_folder, exdir = download_local)
    }

    # if package is not from GitHub we assume that we provide the download URL
    # to the .tar.gz file.
    if (sub$source[i] == "other" &
        nchar(as.character(sub$download_link[i])) > 2) {
        download_file <- as.character(sub$download_link[i])
        if(!is.na(sub$subfolder[i])) {
            working_folder <-
                paste0(download_local, "/", sub$package_name[i], "/", sub$subfolder[i])
        } else{
            working_folder <-
                paste0(download_local, "/", sub$package_name[i])
        }
        download_folder <-
            paste0(download_local, "/", sub$package_name[i], ".tar.gz")
    }

    if(sub$source[i] != "rforge") {
        if (!exists("download_file")) {
            message(paste0(
                sub$package_name[i],
                " does not have a downloadable link, skipping..."
            ))
            next
        }
        ## download and unzip folder so we can install dependencies
        download.file(url = download_file, destfile = download_folder,
            method = "libcurl")
        if (grepl("\\.zip", download_folder)) {
            unzip(zipfile = download_folder, exdir = download_local)
        }
        if (grepl("\\.tar\\.gz", download_folder)) {
            untar(tarfile = download_folder, exdir = working_folder)
        }
    }

    ## delete some folders that may exist that may interfere with building
    ## process. For now these are not strictly 'wrong' but bad form. In some
    ## cases the packages pass CRAN after these are installed.
    if (dir.exists(paste0(working_folder, "/inst/doc"))) {
        unlink(paste0(working_folder, "/inst/doc"), recursive = TRUE)
    }
    if (file.exists(paste0(working_folder, "/build/vignette.rds"))) {
        unlink(paste0(working_folder, "/build/vignette.rds"),
            recursive = TRUE)
    }

    ## install all dependencies. We have to install suggestions as well, as one
    ## of the CRAN checks is that suggested packages are installable. Also
    ## sometimes these packages are used in the vignette
    here <- tryCatch(
        remotes::install_deps(
            pkgdir = working_folder,
            dependencies = TRUE,
            upgrade = "never"
        ),
        error = function(e) as.character(e)
    )
    ## These are errors that keep the remainder of steps from completing so we
    ## have to exit
    if (!is.null(here)) {
        error_list[[paste0(i)]] <- here
        next
    }
    ## We now build the file from scratch so we can guarantee all the correct
    ## folders are in the correct spot.
    there <- tryCatch(
        build_file <- devtools::build(working_folder),
        error = function(e) "vignette_error"
    )
    ## In the build stage vignette errors can halt building, and this implies a
    ## failure of cran check.
    if (there == "vignette_error") {
        checks <- FALSE
        warnings <- FALSE
        errors <- TRUE
        vignette <- TRUE
    }
    if (there != "vignette_error") {
        ll <- tryCatch(
            devtools::check_built(build_file),
            error = function(e) track.frame(errors = as.character(e))
        )

        ## For now we'll query the GitHub API for last commit, though this isn't
        ## being used currently
        if (sub$source[i] == "github") {
            commit_list <-
                gh::gh(
                    "/repos/:owner/:repo/commits",
                    owner = owner,
                    repo = repo,
                    state = "all",
                    .limit = 1
                )
            commit <- (Sys.time() - as.POSIXct(strptime(commit_list[[1]]$commit$author$date,
                format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))) < 365
            track$recent_commit[track$package_name == sub$package_name[i]] <-
                commit
        }

        ## log errors
        checks <- (length(ll$errors)  + length(ll$warnings)) == 0
        warnings <- length(ll$warnings) > 0
        errors <- length(ll$errors) > 0
        vignette <- FALSE
    }

    track$cran_check[track$package_name == sub$package_name[i]] <- checks
    track$warnings[track$package_name == sub$package_name[i]] <- warnings
    track$errors[track$package_name == sub$package_name[i]] <- errors
    track$vignette_error[track$package_name == sub$package_name[i]] <- vignette

    ## Create dependency tree
    if (exists("build_file")) {
        test_folder <- paste0(download_local, "/test")
        untar(tarfile = build_file, exdir = test_folder)
        textz <- desc::desc(file =  paste0(test_folder, "/", sub$package_name[i],
            "/DESCRIPTION"))$get_deps()
        string <- paste(textz$package[textz$type %in%
            c("Depends", "Imports", "LinkingTo")], collapse = ", ")
        string <- trimws(gsub("\n" , " ", string), "both")
        string <- gsub("\\(.*\\)|,$", "" , string)
        track$imports[track$package_name == sub$package_name[i]] <- as.character(string)
        string <- paste(textz$package[textz$type == "Suggests"], collapse = ", ")
        string <- trimws(gsub("\n" , " ", string), "both")
        string <- gsub("\\(.*\\)|,$", "" , string)
        track$suggests[track$package_name == sub$package_name[i]] <-
            ifelse(nchar(string) == 0, NA, as.character(string))
    }

    ## Look for check log
    temp_files <- list.files(paste0(tempdir(), "/", sub$package_name[i], ".Rcheck"),
        "check.log", full.names = TRUE)
    file.copy(temp_files, paste0("check_logs/", sub$package_name[i], "_check.log",
        overwrite = TRUE))

    ## clean house
    suppressWarnings(rm(download_file, build_file, checks, warnings,
        errors, vignette, temp_files))
}

track <- track[order(track$source,track$cran_check, decreasing = TRUE), ]
track <- track[, c("package_name", "source", "recent_commit", "cran_check",
    "warnings", "errors", "vignette_error", "imports", "suggests",
    "recent_publish_track")]
write.csv(track, 'checks/Checked_packages.csv', row.names = FALSE)
