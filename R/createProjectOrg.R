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

  ### CREATING THE ORGANISATION: ###

    # create the fileSystem layout:
  orgPath = paste(fileSystemPath, .Platform$file.sep, orgName , sep="")
  dir.create( orgPath )

  # templates Dir:
  tempPath = paste(orgPath, .Platform$file.sep, "templates" , sep="")
  dir.create( tempPath )

  # hidden .config dir:
  confPath = paste(orgPath, .Platform$file.sep, ".config" , sep="")
  dir.create( confPath )

  # copy template files:
  # need to copy from the PACKAGE!
  templateRmd <- paste( find.package("projectmanagr"), .Platform$file.sep, "templates", .Platform$file.sep, "Project-Doc-Template.Rmd", sep="")
  file.copy(templateRmd, tempPath)

  # create Rmd file:
  orgFile = paste(orgPath, .Platform$file.sep, "index.Rmd", sep="")
  done <- file.create(orgFile)

  if(!done) {
    stop( paste("Org file could not be created: ", orgFile, sep="") )
  }


  ### CREATING A PROGRAMME: ###

  # create Dir:
  progPath = paste(orgPath, .Platform$file.sep, "01-", programmeName, sep="")
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
