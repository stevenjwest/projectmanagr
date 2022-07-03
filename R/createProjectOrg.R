#' Create a New Project Organisation
#'
#' Generates the layout for a new Project Organisation in the File System.
#'
#' The function generates the organisation directory and its initial contents:
#'
#' * `index_<orgName>.Rmd` the landing page for the organisation.
#'
#' * `config/`` directory with the config files and templates/ directory containing
#' component templates.
#'
#' * `docs/` directory, that will hold documentation for a
#' projectmanagr usage.
#'
#' * `volumes/` directory, that will hold all volumes for
#' managing data storage.
#'
#' A `<orgName>_site/` directory is created next to org directory, which will
#' contain the generated html site.
#'
#' @param orgName defines the directory name of the Organisation. Default is "00_ORG",
#'
#' @param orgTitle defines the title of the Organisation, in its document.  Default is "ORGANISATION",
#'
#' @param fileSystemPath defines where the organisation directory is written, Default is the working directory
#'
#' @param orgTemplate defines the template to use from the projectmanagr package.  Default is "Org-Template.Rmd".
#'
#' @export
createProjectOrg <- function( orgName, orgTitle, fileSystemPath=getwd(), orgTemplate="Org-Template.Rmd" ) {


  cat( "\nprojectmanagr::createProjectOrg():\n" )

  ### CREATING THE ORGANISATION: ###

    # create the fileSystem layout:
  orgPath <- paste(fileSystemPath, .Platform$file.sep, orgName , sep="")
  done <- dir.create( orgPath )

  if(!done) {
    stop( paste0("  Organisation directory could not be created: ", orgFile) )
  }

  cat( "  Made ORG dir: ",orgPath, "\n" )


  ### CREATE THE SITE DIRECTORY ###

  # This will contain the compiled HTML - which will be made using the R Markdown in the first instance
    # In future, want to move to using HUGO and Blogdown potentially?

  # site dir - placed NEXT TO the orgDir:
  sitePath <- paste(fileSystemPath, .Platform$file.sep, orgName, "_site" , sep="")

  if( file.exists(sitePath) == TRUE) {
    # do nothing - dir already exists
    cat( "  site dir exists: ", sitePath, "\n" )
  } else {
    done <- dir.create( sitePath )

    if(!done) {
      stop( paste0("  Site directory could not be created: ", sitePath) )
    }

    cat( "  Made site dir: ", sitePath, "\n" )

  }


  ### CREATE THE VOLUMES DIRECTORY ###

  # This will contain SYMLINKS to Volume Mounts, which then act as data stores for data as needed:
    # From each Project Note can run volumes_mkdir() to create a new DIR on a Volume Mount
    # this will form a DIR on the Volume Mount and be SYMLINKED TO a sub-dir in the Project Note DIR (both same name!)
    # NO LONGER using a local data storage idea - can just offset data dynamically as needed WITHIN the Project Note DIR!

  # volumes dir:
  volumesPath = paste(orgPath, .Platform$file.sep, "volumes" , sep="")
  done <- dir.create( volumesPath )

  if(!done) {
    stop( paste0("  Volumes directory could not be created: ", volumesPath) )
  }

  cat( "  Made volumes dir: ", volumesPath, "\n" )

  # Add the volumes.Rmd file to the volumes/ DIR
    # this contains the workflow for MOUNTING an External Volume, then
    # SYMLINKING location(s)  in the External Volume to the volumes/ directory, then
    # how to use the projectmanagr::volumes_mkdir() command to generate a DIR on this External Volume for external storage

  # COPY default settings.yml file from the package:
  volumesFile = paste(orgPath, .Platform$file.sep, "volumes", .Platform$file.sep, "volumes.Rmd", sep="") # location to copy file to
  volumesPackageFile <- paste( find.package("projectmanagr"), .Platform$file.sep,
                               "volumes", .Platform$file.sep, "volumes.Rmd", sep="")

  done <- file.copy(volumesPackageFile, volumesFile)

  if(!done) {
    stop( paste0("  Volumes file could not be copied: ", volumesPackageFile, " ", volumesFile) )
  }
  cat( "  Copied volumes file: ", volumesFile, "\n" )


  ### CREATE DOCS DIRECTORY ###

  # dir:
  docsPath = paste(orgPath, .Platform$file.sep, "docs" , sep="")
  done <- dir.create( docsPath )

  if(!done) {
    stop( paste0("  docs directory could not be created: ", todoPath) )
  }

  cat( "  Made docs dir: ", docsPath, "\n" )

  # copy projectmanagr docs file:
    # need to copy from the PACKAGE!
  docsDir <- paste( find.package("projectmanagr"), .Platform$file.sep, "docs", .Platform$file.sep, sep="")
  docsRmds <- list.files(docsDir)
  for(f in docsRmds) {
    done <- file.copy( paste(docsDir, f, sep=""), docsPath)
    if(!done) {
      stop( paste0("  Could Not copy Template: ", f) )
    }
    cat( "  Copied template: ", f, "\n" )
  }


  ### CREATE CONFIG DIRECTORY ###
    # Contains configuration information for Organisation
  # config dir:
  confPath = paste(orgPath, .Platform$file.sep, "config" , sep="")
  done <- dir.create( confPath )

  if(!done) {
    stop( paste0("  Config directory could not be created: ", confPath) )
  }

  cat( "  Made config dir: ", confPath, "\n" )


  # Create config files:

  # a settings.yml file and a status.yml file
    # settings.yml contains user-defined defaults for operations
    # status.yml contains a list of incomplete projects and notes, for each programme

  # COPY default settings.yml file from the package:
  settingsFile = paste(confPath, .Platform$file.sep, "settings.yml", sep="") # location to copy file to
  settingsPackageFile <- paste( find.package("projectmanagr"), .Platform$file.sep, "config",
                                .Platform$file.sep, "settings.yml", sep="")

  done <- file.copy(settingsPackageFile, settingsFile)

    if(!done) {
      stop( paste0("  Settings file could not be copied: ", settingsPackageFile, " ", settingsFile) )
    }
  cat( "  Copied settings file: ", settingsFile, "\n" )


  # Adjust the AUTHOR Setting to authorValue:
  #settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  #settings[["Author"]] <- authorValue
  #yaml::write_yaml( yaml::as.yaml(settings), settingsFile )
  # NOT USING NOW - moved to using the Sys.info()["user"] username as author for docs as they are created

  # create status.yml file - need to create it here to get the mtime for this file
  statusFile = paste(confPath, .Platform$file.sep, "status.yml", sep="")
  done <- file.create(statusFile)

  if(!done) {
    stop( paste0("  Status file could not be created: ", statusFile) )
  }

  cat( "  Made status file: ", statusFile, "\n" )


  # Create initial content for status.yml file - data on the Org, plus UPDATE datetime:
  updateTime <- file.info(statusFile)[,5] # retrieve mtime for file, save this time to yaml by converting to a character:
  org <- list(orgPath, orgName, orgTitle, as.character(updateTime) )
  names(org) <- c("orgPath", "orgName", "orgTitle", "updateTime")
  yaml::write_yaml( yaml::as.yaml(org), statusFile )


  # templates Dir - INSIDE the config DIR (these templates are part of the projectmanagr config!):
  tempPath = paste(confPath, .Platform$file.sep, "templates" , sep="")
  done <- dir.create( tempPath )

  if(!done) {
    stop( paste0("  Templates directory could not be created: ", tempPath) )
  }

  cat( "  Made templates dir: ",tempPath, "\n" )

  # copy template files:
    # need to copy from the PACKAGE!
  templateDir <- paste( find.package("projectmanagr"), .Platform$file.sep, "templates", .Platform$file.sep, sep="")
  templateRmds <- list.files(templateDir)
  for(f in templateRmds) {
    done <- file.copy( paste(templateDir, f, sep=""), tempPath)
    if(!done) {
      stop( paste0("  Copied Template: ", f) )
    }
    cat( "  Copied template: ",f, "\n" )
  }


  # create ORG Rmd file:
  orgFile = paste(orgPath, .Platform$file.sep, "index_", orgName, ".Rmd", sep="")
  done <- file.create(orgFile)

  if(!done) {
    stop( paste0("  Org file could not be created: ", orgFile) )
  }

  cat( "  Made Org file: ",orgFile, "\n" )


  # read org template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, orgTemplate, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(orgTitle)==0 ) {
    orgTitle = gsub("-", " ", gsub("_", " ", orgName) )
  }


  # extract the Author value from the settings.yml file:
  #settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  #settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  #authorValue <- settings[["Author"]]
  authorValue <- Sys.info()["user"] # use username as author instead

  # modify templateContents to include orgTitle and authorValue
  templateContents <- gsub("{{TITLE}}", orgTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)

  # write to projFile
  fileConn <- file(orgFile)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written template to Org file: ", orgFile, "\n" )

}
