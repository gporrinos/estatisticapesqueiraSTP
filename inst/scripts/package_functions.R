




# FUNCTION TO LOAD PACKAGES AND INSTALL THEM IF NOT INSTALLED
load_package <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, repos = "https://cloud.r-project.org")
    library(package, character.only = TRUE)
  }
}






### FUNCTION TO CHECK WHETHER GIT HUB PACKAGE NEEDS UPDATE
needs_update <- function(package, repository) {



  ### STEP 1. GET LOCAL VERSION
  local_version <- tryCatch(packageVersion(package),
                            error = function(e) NULL
  )
  if(is.null(local_version)) return(TRUE)      # Package not installed, needs installation




  ### STEP 2. QUERY GITHUB API FOR 'DESCRIPTION' FILE
  res = tryCatch(readLines(paste0("https://raw.githubusercontent.com/",
                                  repository,
                                  "/HEAD/DESCRIPTION"),
                           warn = FALSE),
                 error = function(e) NULL
  )
  if(is.null(res)){
    message("Could not reach GitHub, skipping update check for ", package)
    return(FALSE)      # Skip update
  }



  ### STEP 3. PARSE VERSION LINE FROM 'DESCRIPTION'
  version_line = grep("^Version:", res, value = TRUE)
  if(length(version_line) == 0) {
    message("Could not parse version of ", repository, ", skipping update check for ", package)
    return(FALSE)      # Skip update
  }



  ### STEP 4. PARSE VERSION FIELD FROM 'version_line'
  remote_version =
    tryCatch(
      package_version(trimws(
        sub("^Version:", "", version_line)
      )),
      error = function(e) NULL
    )
  if(is.null(remote_version)){
    message("Could not parse version of ", repository, ", skipping update check for ", package)
    return(FALSE)      # Skip update
  }



  ### STEP 5. COMPARE LOCAL AND REMOTE VERSION
  return(local_version < remote_version)
}


