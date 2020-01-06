#' Create a New Project
#'
#' Generates a new Project inside a Programme in an Organisation.  The fileSystemPath
#' must be in the Programme directory, which is the sub-Dir to the root ORGANISATION
#' dir.
#'
#' The project is automatically numbered, by reading the current projects and determining
#' the next number in the sequence, as well as deriving the Programme Prefix.
#'
#' User must supply the project name, which must contain NO SPACES, and optionally a project
#' title.  If no title is provided, the Title is derived from the project name, replacing any
#' "_" & "-" with " ". The default fileSystemPath is the working directory.
#'
#' @export
createProjectDoc <- function(projectName, projectTitle="", fileSystemPath=getwd(),
                             projDocTemplate="Project-Doc-Template.Rmd", projectIndex=0 ) {

  # check projectName contains NO SPACES:
  if( grepl("\\s+", projectName) ) {
    stop( cat("projectName contains a SPACE: ", projectName, "\n") )
  }

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  orgPath <- checkProgDir(fileSystemPath)

  if(  orgPath == ""  ) {
    stop( cat("fileSystemPath is not in a PROGRAMME Directory: ", fileSystemPath, "\n") )
  }

  # fileSystemPath is therefore in a PROGRAMME DIR


  # extract the PROGRAMME NAME from the fileSystemPath:
  programmeName <-basename(fileSystemPath)


  # extract the programme prefix from its config file:
  statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )
  programmePrefix <- status[["PROGRAMMES"]][[programmeName]][["programmePrefix"]]


  if(projectIndex < 1) { # if projectIndex is below 1 (default is 0), then try to identify what projectIndex should be
                         # by looking at DIR numbers:

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
  done <- dir.create(projPath)

  if(!done) {
    stop( cat("Project directory could not be created: ", projPath, "\n") )
  }

  cat( "Made Project dir: ",projPath, "\n" )


  # create Rmd file:
  projFile = paste(projsPath, .Platform$file.sep, programmePrefix, projectIndex, "~_", projectName, ".Rmd", sep="")
  done <- file.create(projFile)

  if(!done) {
    stop( cat("Project file could not be created: ", projFile, "\n") )
  }

  cat( "Made Project file: ", projFile, "\n" )


  # read project doc template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, projDocTemplate, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(projectTitle)==0 ) {
    projectTitle = gsub("-", " ", gsub("_", " ", projectName) )
  }


  # extract the Author value from the settings.yml file:
  settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  authorValue <- settings[["Author"]]

  # modify templateContents to include PREFIX and projectTitle
  templateContents <- gsub("{{PREFIX}}", paste(programmePrefix, projectIndex, sep=""), templateContents, fixed=TRUE)
  templateContents <- gsub("{{TITLE}}", projectTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)

  # write to projFile
  fileConn <- file(projFile)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written template to Project file: ", projFile, "\n" )


  # Write PROJECT to the status.yml file:

  # Read the status.yml file first into a LIST:
  statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  # add programmeName, programmePrefix, projectIndex under the FULL projectName (including prefix, index and name)
    # in the "PROJECTS" section of the status.yml List:
  attrs <- list(programmeName, programmePrefix, projectIndex, as.character(file.info(projFile)[,5]) )
  names(attrs) <- c("programmeName", "programmePrefix", "projectIndex", "creationTime")
  status[["PROJECTS"]][[ paste(programmePrefix, projectIndex, "~_", projectName, sep="") ]] <- attrs
  # can retrieve the programmePrefix with call to:
    # status[["PROJECTS"]][[projectName]][["programmeName"]]
    # status[["PROJECTS"]][[projectName]][["projectPrefix"]]

  # Write status list to the statusFile:
  yaml::write_yaml( yaml::as.yaml(status), statusFile )

  cat( "  Written PROJECT to Status.yml file: ", statusFile, "\n" )


}
