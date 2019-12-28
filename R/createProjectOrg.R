#' Create a New Project Organisation
#'
#' Generates the layout for a new Project Organisation in the File System,
#' including the content, in '00_ORG' directory by default.  The default location is the working directory.
#'
#' This will also initialise the config/ and templates/ directories for this Organisation.  The config/
#' directory will contain the status.yml and settings.yml files.  status.yml will contain data on the
#' Organisation's status (programmes, projects and project work in progress, update times, etc).  settings.yml
#' will contain the settings needed for various projectmanagr functions.
#'
#' The templates/ directory contains several default templates for Project Docs and Notes.
#'
#' A directory is also created to store the compiled html site from the Organisation - site/
#'
#' @export
createProjectOrg <- function(fileSystemPath=getwd(), orgName = "00_ORG", orgTitle = "ORGANISATION") {


  ### CREATING THE ORGANISATION: ###

    # create the fileSystem layout:
  orgPath = paste(fileSystemPath, .Platform$file.sep, orgName , sep="")
  done <- dir.create( orgPath )

  if(!done) {
    stop( cat("Organisation directory could not be created: ", orgFile, "\n") )
  }

  cat( "Made ORG dir: ",orgPath, "\n" )



  ### CREATE THE SITE DIRECTORY ###

  # This will contain the compiled HTML - which will be made using the R Markdown in the first instance
  # In future, want to move to using HUGO and Blogdown potentially?

  # Do NOT make this a Hidden directory - as the user will want to access it!

  # site dir:
  sitePath = paste(orgPath, .Platform$file.sep, "site" , sep="")
  done <- dir.create( sitePath )

  if(!done) {
    stop( cat("Site directory could not be created: ", sitePath, "\n") )
  }

  cat( "Made site dir: ", sitePath, "\n" )


  ### CREATE CONFIG DIRECTORY ###

  # Contain configuration information, mainly for each Programme information on incomplete notes for To Do lists

  # DO NOT make this a Hidden Driectory - as User may want to look in here?

  # config dir:
  confPath = paste(orgPath, .Platform$file.sep, "config" , sep="")
  done <- dir.create( confPath )

  if(!done) {
    stop( cat("Config directory could not be created: ", confPath, "\n") )
  }

  cat( "Made config dir: ", confPath, "\n" )

  # Create config files:
  # a settings.yml file and a status.yml file
  # settings.yml contains user-defined defaults for operations
  # status.yml contains a list of incomplete projects and notes, for each programme

  # COPY default settings.yml file from the package:
  settingsFile = paste(confPath, .Platform$file.sep, "settings.yml", sep="") # location to copy file to
  settingsPackageFile <- paste( find.package("projectmanagr"), .Platform$file.sep, "config", .Platform$file.sep, "settings.yml", sep="")

  done <- file.copy(settingsPackageFile, settingsFile)
    if(!done) {
      stop( cat("Settings file could not be copied: ", settingsPackageFile, " ", settingsFile, "\n") )
    }
  cat( "Copied settings file: ", settingsFile, "\n" )


  # create status.yml file - need to create it here to get the mtime for this file
  statusFile = paste(confPath, .Platform$file.sep, "status.yml", sep="")
  done <- file.create(statusFile)

  if(!done) {
    stop( cat("Status file could not be created: ", statusFile, "\n") )
  }

  cat( "Made status file: ", statusFile, "\n" )


  # Create initial content for status.yml file - data on the Org, plus UPDATE datetime:
  updateTime <- file.info(statusFile)[,5] # retrieve mtime for file, save this time to yaml by converting to a character:
  org <- list(orgPath, orgName, orgTitle, as.character(updateTime) )
  names(org) <- c("orgPath", "orgName", "orgTitle", "updateTime")
  yaml::write_yaml( yaml::as.yaml(org), statusFile )



  # templates Dir:
  tempPath = paste(orgPath, .Platform$file.sep, "templates" , sep="")
  done <- dir.create( tempPath )

  if(!done) {
    stop( cat("Templates directory could not be created: ", tempPath, "\n") )
  }

  cat( "Made templates dir: ",tempPath, "\n" )

  # copy template files:
    # need to copy from the PACKAGE!
  templateDir <- paste( find.package("projectmanagr"), .Platform$file.sep, "templates", .Platform$file.sep, sep="")
  templateRmds <- list.files(templateDir)
  for(f in templateRmds) {
    done <- file.copy( paste(templateDir, f, sep=""), tempPath)
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

  cat( "Made Org file: ",orgFile, "\n" )

}
