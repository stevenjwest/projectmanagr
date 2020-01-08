#' Get Project Path
#'
#' Assumes fileSystemPath is a PROGRAMME DIRECTORY, containing a 'PROJECTS/' Directory.
#'
getProjectPath <- function( fileSystemPath, projectName ) {

  # identify the Organisation Dir:
  orgPath <- findOrgDir(fileSystemPath)

  # specify the Projects Dir:
  projsPath <- paste(fileSystemPath, .Platform$file.sep, "PROJECTS" , sep="")

  # extract the PROGRAMME NAME from the fileSystemPath:
  programmeName <-basename(fileSystemPath)


  # extract the programme prefix from its config file:
  statusFile = paste( orgPath, .Platform$file.sep, "config", .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )
  programmePrefix <- status[["PROGRAMMES"]][[programmeName]][["programmePrefix"]]


  # read all DIRs in projsPath that start with prefix:
  directories <- dir(projsPath, recursive = FALSE, full.names = FALSE, pattern= paste(programmePrefix,"[0-9]{1,}[~]{1}[_]{1}", sep="")  )

  projectIndexes <- sapply( directories, function(x)
      substr(x, gregexpr(programmePrefix, x)[[1]][1]+nchar(programmePrefix), gregexpr("~", x)[[1]][1]-1 )  )

  projectIndex <- sort( as.numeric(projectIndexes) )[length(projectIndexes)]

  projectIndex <- projectIndex+1 # add one to max projectIndex

  if(length(projectIndex) == 0 ) {

    projectIndex <- 1 #this ensures if there are NO directories that match the glob above, that the index is set to 1!

  }

    # if projectIndex is only one digit, append "0" to front:
  if(projectIndex < 10 ) {

    projectIndex <- paste("0", projectIndex, sep="")

  } else {

    projectIndex <- paste("", projectIndex, sep="")

  }

  paste(projsPath, .Platform$file.sep, programmePrefix, projectIndex, "~_", projectName, ".Rmd", sep="")

}
