########################################################
## Run Cran Checks and see when the last commit was made
library(lubridate)
data <- read.csv('/home/matt/r_programs/CranTaskView-Track/checks/RmovementPackagesInformation_checked.csv')
# working directory to download and run checks in.
download_local <- '/home/matt/Downloads'
#######################################################
# create columns if not exist
sub <- data[data$CRAN=='No' & data$Bioconductor=='No',]
if(is.null(data$recent_commit)){data$recent_commit <- NA}
if(is.null(data$cran_check)){data$cran_check <- NA}
if(is.null(data$warnings)){data$warnings <- NA}
if(is.null(data$errors)){data$errors <- NA}

# set up an error list to check later if installs failed because of dependencies the computer doesnt have
error_list <- list()

for(i in seq_len(nrow(sub))){
#  for(i in c(7,17,19)){
  # If packages has a github page, uses that to get download information
if(sub$github[i] == 'Yes') { 
  branch <- sub$sub[i]
  owner <- sub$owner[i]
  repo <- sub$repository[i]
  download_file <- paste0("https://github.com/",owner,"/",repo,"/archive/master.zip") 
  if(branch==''){  working_folder <- paste0(download_local,'/',repo,'-master')} else{
    working_folder <- paste0(download_local,'/',repo,'-master/',branch)
  }
  download_folder <- paste0(download_local,'/',sub$repository[i],'.zip')
} 

  # if package is not from github we assume that we provide the download URL to the .tar.gz file.
if(sub$github[i]=='No' & nchar(as.character(sub$download_link[i]))>2) {
  download_file <- as.character(sub$download_link[i])
  working_folder <- paste0(download_local,'/',sub$download_folder[i])
  download_folder <- paste0(download_local,"/",sub$download_folder_sub[i],".tar.gz")
  
} 
  # download and unzip folder so we can install dependencies
  download.file(url = download_file, destfile = download_folder)
  if(grepl('\\.zip',download_folder)){unzip(zipfile = download_folder, exdir=download_local)}
  if(grepl('\\.tar\\.gz',download_folder)){untar(tarfile = download_folder, exdir=working_folder)}
  # delete some folders that may exist that may interfere with building process. For now these are not
  # strictly 'wrong' but bad form. In some cases the packages pass cran after these are installed.
  if(dir.exists(paste0(working_folder,'/inst/doc'))){unlink(paste0(working_folder,'/inst/doc'), recursive=T)}
  if(file.exists(paste0(working_folder,'/build/vignette.rds'))){unlink(paste0(working_folder,'/build/vignette.rds'),recursive=T)}
  # install all dependencies. We have to install suggestions as well, as one of the cran checks is that 
  # suggested packages are installable. ALso sometimes these packages are used in the vignette
  here <- NA
  here <- tryCatch( devtools::install_deps(working_folder, dependencies = TRUE, upgrade='never'), 
    error = function(e) as.character(e))
  # These are errors that keep the remainder of steps from completing so we have to exit
  if(!is.null(here)){error_list[[paste0(i)]] <- here; next}
  # We now build the file from scratch so we can gaurantte all the correct folders are in the correct spot. 
  there <- tryCatch(build_file <- devtools::build(working_folder),
    error = function(e)'vignette_error')
  # In the build stage vignette errors can halt building, and this implies a failure of cran check.
  if(there == 'vignette_error'){
    checks <- FALSE
    warnings <- FALSE
    errors <- FALSE
  }
  
  if(there != 'vignette_error'){
  ll <- tryCatch(devtools::check_built(build_file), 
    error=function(e)data.frame(errors=as.character(e)))
  
  # For now we'll query the github API for last commit, though this isnt being used currently
  if(sub$github[i]=='Yes'){
  commit_list <-
    gh::gh("/repos/:owner/:repo/commits", owner = owner, repo = repo,
      state = "all", .limit = 1)
  commit <- (now() - ymd_hms(commit_list[[1]]$commit$author$date))<365
  data$recent_commit[data$Package==sub$Package[i]] <- commit
  }
  # log errors
  checks <- (length(ll$errors)  + length(ll$warnings))==0
  warnings <- length(ll$warnings)>0
  errors <- length(ll$errors)>0
  }
  data$cran_check[data$Package==sub$Package[i]] <- checks
  data$warnings[data$Package==sub$Package[i]] <- warnings
  data$errors[data$Package==sub$Package[i]] <- errors
}
write.csv(data, '/home/matt/r_programs/CranTaskView-Track/checks/RmovementPackagesInformation_checked.csv',row.names=F)
