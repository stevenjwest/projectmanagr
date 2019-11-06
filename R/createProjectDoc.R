#' Create a New Project
#'
#' Generates a new Project inside a Programme in an Organisation.  The fileSystemPath
#' must be either in the Programme directory, which is the sub-Dir to the root ORGANISATION
#' dir.
#'
#' The project is automatically numbered, by reading the current projects and determining
#' the next number in the sequence, as well as deriving the Programme Prefix.
#'
#' User must supply the path, project name and project title.
#'
#' @export
createProjectDoc <- function( fileSystemPath, projectName, projectTitle, projectIndex=0 ) {


  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname(fileSystemPath)

  # look for the .config/ and templates/ dirs:
  confPath = paste(orgPath, .Platform$file.sep, ".config" , sep="")
  tempPath = paste(orgPath, .Platform$file.sep, "templates" , sep="")

  if(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    stop( paste("fileSystemPath is not in a PROGRAMME Directory: ",fileSystemPath, sep="") )
  }

  # fileSystemPath is therefore in a PROGRAMME DIR

  # also check if the PROJECTS/ dir is in the current DIR, and if not, exit:
  projsPath = paste(fileSystemPath, .Platform$file.sep, "PROJECTS" , sep="")

  if(  !( file.exists(projsPath) )  ) {
    stop( paste("fileSystemPath PROGRAMME Directory does not contain a PROJECTS/ dir: ",fileSystemPath, sep="") )
  }

  # extract the PROGRAMME NAME from the fileSystemPath:
  programmeName <-substr(basename(fileSystemPath), gregexpr("-", basename(fileSystemPath) )[[1]][1]+1, nchar( basename(fileSystemPath) ) )

  # extract the programme prefix from its config file:
  programme <- yaml::yaml.load( yaml::read_yaml( paste(confPath, .Platform$file.sep, programmeName, ".yaml" , sep="") ) )
  programmePrefix <- programme$programmePrefix

  if(projectIndex < 1) { # if projectIndex is below 1 (default is 0), then try to identify what projectIndex should be by looking at DIR numbers:

    # read all DIRs in projsPath that start with prefix:
    directories <- dir(projsPath, recursive = FALSE, full.names = FALSE, pattern= paste(programmePrefix,"[0-9]{1,}[~]{1}[_]{1}", sep="")  )
    projectIndexes <- sapply( directories, function(x)
                  substr(x, gregexpr(programmePrefix, x)[[1]][1]+nchar(programmePrefix), gregexpr("~", x)[[1]][1]-1 )  )
    projectIndex <- sort( as.numeric(projectIndexes) )[length(projectIndexes)]
    projectIndex <- projectIndex+1 # add one to max projectIndex
    # if projectIndex is only one digit, append "0" to front:
    if(projectIndex < 10 ) {
      projectIndex <- paste("0", projectIndex, sep="")
    } else {
      projectIndex <- paste("", projectIndex, sep="")
    }
  } else { # else, if projectIndex was set to be above 0, then use this number!
    if(projectIndex < 10 ) {
      projectIndex <- paste("0", projectIndex, sep="")
    } else {
      projectIndex <- paste("", projectIndex, sep="")
    }
  }


  ### CREATING THE PROJECT: ###

  # create Dir:
  projPath = paste(projsPath, .Platform$file.sep, programmePrefix, projectIndex, sep="")
  dir.create(projPath)

  # create Rmd file:
  projFile = paste(projsPath, .Platform$file.sep, programmePrefix, projectIndex, "~_", projectName, ".Rmd", sep="")
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
