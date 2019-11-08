## Run Cran Checks and see when the last commit was made

data <- read.csv('/home/matt/r_programs/cranTaskView/RmovementPackagesInformation.csv')
download_local <- '/home/matt/Downloads'
#######################################################
sub <- data[data$CRAN=='No' & data$github=='Yes',]
if(is.null(data$recent_commit)){data$recent_commit <- NA}
if(is.null(data$cran_check)){data$cran_check <- NA}
if(is.null(data$warnings)){data$warnings <- NA}
if(is.null(data$errors)){data$errors <- NA}
error_list <- list()
library(lubridate)
for(i in seq_len(nrow(sub))){
  #if(i==1){next}
  #i <- 8
branch <- sub$sub[i]
owner <- sub$owner[i]
repo <- sub$repository[i]

  download.file(url = paste0("https://github.com/",owner,"/",repo,"/archive/master.zip")
    , destfile = paste0(download_local,"/temp.zip"))
  unzip(zipfile = paste0(download_local,"/temp.zip"), exdir=download_local) 
  
  if(branch==''){  working_folder <- paste0(download_local,'/',repo,'-master')} else{
    working_folder <- paste0(download_local,'/',repo,'-master/',branch)
  }
  if(dir.exists(paste0(working_folder,'/inst/doc'))){unlink(paste0(working_folder,'/inst/doc'), recursive=T)}
here <- NA
 here <- tryCatch( devtools::install(working_folder,upgrade ='never',force=T, dependencies = NA), 
   error = function(e) as.character(e))
 if(here!=T){error_list[[paste0(i)]] <- here; next}
  ll <- tryCatch(devtools::check(working_folder), 
    error=function(e)data.frame(errors=e))
  
  commit_list <-
    gh::gh("/repos/:owner/:repo/commits", owner = owner, repo = repo,
      state = "all", .limit = 1)
  commit <- (now() - ymd_hms(commit_list[[1]]$commit$author$date))<365
  checks <- (length(ll$errors)  + length(ll$warnings))==0
  warnings <- length(ll$warnings)==0
  errors <- length(ll$errors)==0
  data$recent_commit[data$Package==sub$Package[i]] <- commit 
  data$cran_check[data$Package==sub$Package[i]] <- checks
  data$warnings[data$Package==sub$Package[i]] <- warnings
  data$errors[data$Package==sub$Package[i]] <- errors
}
write.csv(data, '/home/matt/r_programs/cranTaskView/RmovementPackagesInformation_checked.csv',row.names=F)
