#' Create a New Project Organisation
#'
#' Generates the layout for a new Project Organisation in the File System,
#' and adds an initial Programme with an initial Project.
#'
#' This will also initialise the config and templates for this Organisation.
#'
#'
#' @export
createProjectOrg <- function(fileSystemPath, programmeName, programmeTitle,
                             programmePrefix, projectName, projectTitle,
                             orgName = "ORGANISATION", orgTitle = "ORGANISATION") {

  # create the fileSystem layout:
  orgPath = paste(fileSystemPath, .Platform$file.sep, orgName , sep="")
  dir.create( orgPath )

  tempPath = paste(orgPath, .Platform$file.sep, "templates" , sep="")
  dir.create( tempPath )

  confPath = paste(orgPath, .Platform$file.sep, ".config" , sep="")
  dir.create( confPath )

  progPath = paste(orgPath, .Platform$file.sep, "01-", programmeName, sep="")
  dir.create(progPath)

  projsPath = paste(progPath, .Platform$file.sep, "PROJECTS", sep="")
  dir.create(projsPath)

  projPath = paste(projsPath, .Platform$file.sep, programmePrefix, "01", sep="")
  dir.create(projPath)

  # copy template files:
  # need to copy from the PACKAGE!
  file.copy()

  # create files:
  orgFile = paste(orgPath, .Platform$file.sep, "index.Rmd", sep="")
  done <- file.create(orgFile)

  if(!done) {
    stop( paste("Org file could not be created: ", orgFile, sep="") )
  }

  progFile = paste(progPath, .Platform$file.sep, "index.Rmd", sep="")
  done <- file.create(progFile)

  if(!done) {
    stop( paste("Programme file could not be created: ", progFile, sep="") )
  }

  projFile = paste(progPath, .Platform$file.sep, "PROJECTS", .Platform$file.sep, programmePrefix, "01~_", projectName, ".Rmd", sep="")
  done <- file.create(projFile)

  if(!done) {
    stop( paste("Project file could not be created: ", projFile, sep="") )
  }

  # write to the files:

  fileConn<-file(projFile)
  readLines(c("Hello","World"), fileConn)
  close(fileConn)

  fileConn<-file(projFile)
  writeLines(c("Hello","World"), fileConn)
  close(fileConn)

}
