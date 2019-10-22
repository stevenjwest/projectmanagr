#' Create a New Programme
#'
#' Generates a new Programme at the top level of the Organisation.  If the
#' fileSystemPath is not at the top of the Organisation, will traverse until
#' it is found.  Automatically numbers the Programme.  User must supply
#' the path, programmeName, programmeTitle and programmePrefix, project name and project title.
#'
#' @export
createProgramme <- function(fileSystemPath, programmeName, programmeTitle,
                             programmePrefix, projectName, projectTitle,
                             programmeIndex=0) {


  # Check fileSystemPath is at the root of an ORGANISATION:

  # look for the .config/ and templates/ dirs:
  confPath = paste(fileSystemPath, .Platform$file.sep, ".config" , sep="")
  tempPath = paste(fileSystemPath, .Platform$file.sep, "templates" , sep="")

  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    fileSystemPath <- dirname(fileSystemPath)
    if(nchar(fileSystemPath) <=1) {
      stop( paste("Could not identify ORGANISATION in fileSystemPath: ",fileSystemPath, sep="") )
    }
    confPath = paste(fileSystemPath, .Platform$file.sep, ".config" , sep="")
    tempPath = paste(fileSystemPath, .Platform$file.sep, "templates" , sep="")
  }

  # now fileSystemPath should contain the path to the ORG ROOT DIR

  # read all DIRs in fileSystemPath:

  if(programmeIndex < 1 ) { # if programmeIndex is below 1 (default is 0), then try to identify what programmeIndex should be by looking at DIR numbers:

    directories <- dir(fileSystemPath, recursive = FALSE, full.names = FALSE, pattern="*[0-9]{1-6}[-]{1}")
    programmeIndexes <- sapply( directories, function(x) substr(x, 0, gregexpr("-", x)[[1]][1]-1) )
    programmeIndex <- sort( as.numeric(programmeIndexes) )[length(programmeIndexes)]
    programmeIndex <- programmeIndex+1

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
  dir.create(progPath)

  # create PROJECTS dir:
  projsPath = paste(progPath, .Platform$file.sep, "PROJECTS", sep="")
  dir.create(projsPath)

  # create SOP dir:
  sopPath = paste(progPath, .Platform$file.sep, "SOP", sep="")
  dir.create(sopPath)

  # create Rmd file:
  progFile = paste(progPath, .Platform$file.sep, "index.Rmd", sep="")
  done <- file.create(progFile)

  if(!done) {
    stop( paste("Programme file could not be created: ", progFile, sep="") )
  }


  # Create a config file for this Programme:
  programme <- list(programmeName, programmePrefix)
  names(programme) <- c("programmeName", "programmePrefix")
  yaml::write_yaml( yaml::as.yaml(programme), paste(confPath, .Platform$file.sep, programmeName, ".yaml", sep="") )


  ### CREATING A PROJECT: ###

  # create Dir:
  projPath = paste(projsPath, .Platform$file.sep, programmePrefix, "01", sep="")
  dir.create(projPath)

  # create Rmd file:
  projFile = paste(progPath, .Platform$file.sep, "PROJECTS", .Platform$file.sep, programmePrefix, "01~_", projectName, ".Rmd", sep="")
  done <- file.create(projFile)

  if(!done) {
    stop( paste("Project file could not be created: ", projFile, sep="") )
  }

  # read project doc template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, "Project-Doc-Template.Rmd", sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)

  # modify templateContents to include PREFIX and projectTitle
  templateContents <- gsub("{{PREFIX}}", paste(programmePrefix, "01", sep=""), templateContents, fixed=TRUE)
  templateContents <- gsub("{{TITLE}}", projectTitle, templateContents, fixed=TRUE)

  # write to projFile
  fileConn <- file(projFile)
  writeLines(templateContents, fileConn)
  close(fileConn)

}
