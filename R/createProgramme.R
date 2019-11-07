#' Create a New Programme
#'
#' Generates a new Programme at the top level of the Organisation.  If the
#' fileSystemPath is not at the top of the Organisation, will traverse until
#' it is found.  Automatically numbers the Programme.  User must supply
#' the path, programmeName, programmeTitle and programmePrefix, project name and project title.
#'
#' @export
createProgramme <- function(fileSystemPath, programmeName, programmeTitle,
                             programmePrefix, programmeIndex=0) {


  # Check fileSystemPath is at the root of an ORGANISATION:

  # look for the .config/ and templates/ dirs:
  confPath = paste(fileSystemPath, .Platform$file.sep, ".config" , sep="")
  tempPath = paste(fileSystemPath, .Platform$file.sep, "templates" , sep="")

  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    fileSystemPath <- dirname(fileSystemPath)
    if(nchar(fileSystemPath) <=1) {
      stop( cat("Could not identify ORGANISATION in fileSystemPath: ",fileSystemPath, "\n") )
    }
    confPath = paste(fileSystemPath, .Platform$file.sep, ".config" , sep="")
    tempPath = paste(fileSystemPath, .Platform$file.sep, "templates", sep="")
  }

  # now fileSystemPath should contain the path to the ORG ROOT DIR


  # read all DIRs in fileSystemPath:

  if(programmeIndex < 1 ) { # if programmeIndex is below 1 (default is 0),
                            # then try to identify what programmeIndex should be by looking at DIR numbers:

    directories <- dir(fileSystemPath, recursive = FALSE, full.names = FALSE, pattern="*[0-9]{1-6}[-]{1}")

    programmeIndexes <- sapply( directories, function(x) substr(x, 0, gregexpr("-", x)[[1]][1]-1) )

    programmeIndex <- sort( as.numeric(programmeIndexes) )[length(programmeIndexes)]

    programmeIndex <- programmeIndex+1

    if(length(programmeIndex) == 0 ) {

      programmeIndex <- 1 #this ensures if there are NO directories that match the glob above, that the index is set to 1!

    }

    # if programmeIndex is only one digit, append "0" to front:
    if(programmeIndex < 10 ) {

      programmeIndex <- paste("0", programmeIndex, sep="")

    } else {

      programmeIndex <- paste("", programmeIndex, sep="")

    }

  } else { # else, if programmeIndex was set to be above 0, then use this number!

    if(programmeIndex < 10 ) {

      programmeIndex <- paste("0", programmeIndex, sep="")

    } else {

      programmeIndex <- paste("", programmeIndex, sep="")

    }

  }


  ### CREATING A PROGRAMME: ###

  # create Dir:
  progPath = paste(fileSystemPath, .Platform$file.sep, programmeIndex, "-", programmeName, sep="")
  done <- dir.create(progPath)

  if(!done) {
    stop( cat("Programme directory could not be created: ", progPath, "\n") )
  }

  cat( "Made Programme dir: ",progPath, "\n" )


  # create PROJECTS dir:
  projsPath = paste(progPath, .Platform$file.sep, "PROJECTS", sep="")
  done <- dir.create(projsPath)

  if(!done) {
    stop( cat("PROJECTS directory could not be created: ", projsPath, "\n") )
  }

  cat( "  Made PROJECTS dir: ",projsPath, "\n" )


  # create SOP dir:
  sopPath = paste(progPath, .Platform$file.sep, "SOP", sep="")
  dir.create(sopPath)

  if(!done) {
    stop( cat("SOP directory could not be created: ", sopPath, "\n") )
  }

  cat( "  Made SOP dir: ",sopPath, "\n" )


  # create Rmd file:
  progFile = paste(progPath, .Platform$file.sep, "index.Rmd", sep="")
  done <- file.create(progFile)

  if(!done) {
    stop( cat("Programme index.md file could not be created: ", progFile, "\n") )
  }

  cat( "  Made Programme index.md file: ",progFile, "\n" )


  # Create a config file for this Programme:
  programme <- list(programmeName, programmePrefix)
  names(programme) <- c("programmeName", "programmePrefix")
  yaml::write_yaml( yaml::as.yaml(programme), paste(confPath, .Platform$file.sep, programmeName, ".yaml", sep="") )

  cat( "  made Programme index.md file: ",progFile, "\n" )

}
