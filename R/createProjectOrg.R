#' Create a New Project Organisation
#'
#' Generates the layout for a new Project Organisation in the File System.
#'
#' The function generates the organisation directory and its initial contents:  config/ direcotry with the
#' config files and templates/ directory containing component templates.  The site/ directory, which will
#' contain the generated html site.  The volumes/ directory, with an initial local/ directory, that will
#' hold all volumes for managing data storage.
#'
#' @param authorValue name of author of this organisation - written into the yaml header of each Rmd file.
#' @param orgName defines the directory name of the Organisation. Default is "00_ORG",
#' @param orgTitle defines the title of the Organisation, in its document.  Default is "ORGANISATION",
#' @param fileSystemPath defines where the organisation directory is written, Defaultis the working directory
#' @param orgTemplate defines the template to use from the projectmanagr package.  Default is "Org-Template.Rmd".
#'
#' @export
createProjectOrg <- function(authorValue, orgName = "00_ORG", orgTitle = "00 ORGANISATION", fileSystemPath=getwd(),
                             orgTemplate="Org-Template.Rmd" ) {


  cat( "\nprojectmanagr::createProjectOrg():\n" )

  ### CREATING THE ORGANISATION: ###

    # create the fileSystem layout:
  orgPath = paste(fileSystemPath, .Platform$file.sep, orgName , sep="")
  done <- dir.create( orgPath )

  if(!done) {
    stop( paste0("  Organisation directory could not be created: ", orgFile) )
  }

  cat( "  Made ORG dir: ",orgPath, "\n" )



  ### CREATE THE SITE DIRECTORY ###


  # This will contain the compiled HTML - which will be made using the R Markdown in the first instance
    # In future, want to move to using HUGO and Blogdown potentially?

  # site dir:
  sitePath = paste(orgPath, .Platform$file.sep, "site" , sep="")
  done <- dir.create( sitePath )

  if(!done) {
    stop( paste0("  Site directory could not be created: ", sitePath) )
  }

  cat( "  Made site dir: ", sitePath, "\n" )



  ### CREATE THE VOLUMES DIRECTORY ###


  # This will contain SYMLINKS to Volume Mounts, which can then be used as DESTINATIONS for Project Note Directories.
    # The Project Note DIR next to the Project Note itself will then be a SYMLINK to a Project Note dir on a MOUNT.

  # volumes dir:
  volumesPath = paste(orgPath, .Platform$file.sep, "volumes" , sep="")
  done <- dir.create( volumesPath )

  if(!done) {
    stop( paste0("  Volumes directory could not be created: ", volumesPath) )
  }

  cat( "  Made volumes dir: ", volumesPath, "\n" )



  ### CREATE INITIAL DATA STORAGE DIR IN VOLUMES DIRECTORY ###


  # The most basic data storage Mount will be a local/ directory in volumes/ - i.e. storage on local machine.
    # Users can add further volumes in the volumes/ directory with createVolume() Function

  # volumes/local sub-dir:
  volumesDataPath = paste(orgPath, .Platform$file.sep, "volumes", .Platform$file.sep, "local" , sep="")
  done <- dir.create( volumesDataPath )

  if(!done) {
    stop( paste0("  Volumes Local sub-directory could not be created: ", volumesDataPath) )
  }

  cat( "  Made volumes local sub-dir: ", volumesDataPath, "\n" )



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
  settingsPackageFile <- paste( find.package("projectmanagr"), .Platform$file.sep, "config", .Platform$file.sep, "settings.yml", sep="")

  done <- file.copy(settingsPackageFile, settingsFile)

    if(!done) {
      stop( paste0("  Settings file could not be copied: ", settingsPackageFile, " ", settingsFile) )
    }
  cat( "  Copied settings file: ", settingsFile, "\n" )


  # Adjust the AUTHOR Setting to authorValue:
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  settings[["Author"]] <- authorValue
  yaml::write_yaml( yaml::as.yaml(settings), settingsFile )


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
  orgFile = paste(orgPath, .Platform$file.sep, orgName, "_index.Rmd", sep="")
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
  settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  authorValue <- settings[["Author"]]

  # modify templateContents to include orgTitle and authorValue
  templateContents <- gsub("{{TITLE}}", orgTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)

  # write to projFile
  fileConn <- file(orgFile)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written template to Org file: ", orgFile, "\n" )

}
