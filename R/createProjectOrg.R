#' Create a New Project Organisation
#'
#' Generates the layout for a new Project Organisation in the File System,
#' including the content, in '00_ORG' directory by default, and the hugo
#' site, in '00_SITE' by default.  The default location is the working directory.
#'
#' This will also initialise the config and templates directories for this Organisation,
#' as well as copy several default templates for Project Docs and Notes.
#'
#' @export
createProjectOrg <- function(fileSystemPath=getwd(), orgName = "00_ORG",
                             orgTitle = "ORGANISATION", siteName = "00_SITE") {


  ### CREATING THE ORGANISATION: ###

    # create the fileSystem layout:
  orgPath = paste(fileSystemPath, .Platform$file.sep, orgName , sep="")
  done <- dir.create( orgPath )

  if(!done) {
    stop( cat("Organisation directory could not be created: ", orgFile, "\n") )
  }

  cat( "Made ORG dir: ",orgPath, "\n" )


  # templates Dir:
  tempPath = paste(orgPath, .Platform$file.sep, "templates" , sep="")
  done <- dir.create( tempPath )

  if(!done) {
    stop( cat("Templates directory could not be created: ", tempPath, "\n") )
  }

  cat( "Made templates dir: ",tempPath, "\n" )


  # hidden .config dir:
  confPath = paste(orgPath, .Platform$file.sep, ".config" , sep="")
  done <- dir.create( confPath )

  if(!done) {
    stop( cat("Config directory could not be created: ", confPath, "\n") )
  }

  cat( "Made config dir: ", confPath, "\n" )


  # copy template files:
    # need to copy from the PACKAGE!
  templateDir <- paste( find.package("projectmanagr"), .Platform$file.sep, "templates", .Platform$file.sep, "Project-Doc-Template.Rmd", sep="")
  templateRmds <- list.files(templateDir)
  for(f in templateRmds) {
    done <- file.copy(paste(templateDir, f, sep=""), tempPath)
    if(!done) {
      stop( cat("  Copied Template: ", f, "\n") )
    }
    cat( "Copied template: ",f, "\n" )
  }


  # create Rmd file:
  orgFile = paste(orgPath, .Platform$file.sep, "index.Rmd", sep="")
  done <- file.create(orgFile)

  if(!done) {
    stop( cat("Org file could not be created: ", orgFile, "\n") )
  }

  cat( "Made config dir: ",confPath, "\n" )



  ### CREATING THE HUGO SITE: ###

  # create Dir:
  sitePath = paste(fileSystemPath, .Platform$file.sep, siteName , sep="")
  dir.create( sitePath )

  if(!done) {
    stop( cat("Site directory could not be created: ", sitePath, "\n") )
  }

  cat( "Made SITE dir: ",sitePath )


}
