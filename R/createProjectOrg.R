#' Create a New Project Organisation
#'
#' Generates the layout for a new Project Organisation in the File System.
#'
#' The function generates the organisation directory and its initial contents:
#'
#' * `index_<orgName>.Rmd` the landing page for the organisation.
#'
#' * `config/` directory with the config files and templates/ directory containing
#' component templates.
#'
#' * `docs/` directory, that will hold documentation for a projectmanagr usage.
#'
#' * `volumes/` directory, that will hold all volumes for managing data storage.
#'
#' A HTML site directory is created which will contain the generated html site.
#' By default this directory will exist next to org directory, suffixed with
#' `_site`.  This can be modified with the `organsiationSiteDirectory` parameter.
#'
#' @param orgName defines the directory name of the Organisation.
#'
#' @param orgTitle defines the title of the Organisation, in its document.
#'
#' @param organisationSiteDirectory an absolute path that defines where the compiled
#' HTML from the organisation is written. If left blank, the default site directory
#' will be the organisation directory path, with directory name suffixed with `_site`.
#'
#' @param organisationParentDirectory defines where the organisation directory is
#' written, Default is the working directory.
#'
#' @param settingsYamlPath A VALID YAML file for establishing the projectmanagr
#' organisation layout.  For this function the SitePath VolumesDir DocsDir ConfigDir
#' & OrgIndexFileName params MUST be set.  Optionally all other params for the
#' projectmanagr settings yaml file can be set, or they are set to default
#' values.
#'
#' @param orgTemplate defines the template to use from the projectmanagr package.
#' Default is "Org-Template.Rmd".
#'
#' @export
createProjectOrg <- function( orgName, orgTitle, organisationParentDirectory=getwd(),
                              settingsYamlPath = "", orgTemplate="Org-Template.Rmd" ) {


  cat( "\nprojectmanagr::createProjectOrg():\n" )


  #### Create Organisation Dir ####

  orgPath <- paste(organisationParentDirectory, .Platform$file.sep, orgName , sep="")

  # get all org paths (including this one) from the parent dir of current orgPath
  orgPaths <- getOrgPaths(orgPath) # this also updates the existing org configs to be aware of this new org!
   # this ensures all organisations (at least those written to parent DIR of this new ORG) are aware of each other
   # thus can update links across organisations

  # create the fileSystem layout:
  done <- dir.create( orgPath )

  if(!done) {
    stop( paste0("  Organisation directory could not be created: ", orgFile) )
  }

  cat( "  Made ORG dir: ",orgPath, "\n" )


  #### Load Settings YAML file ####
  external_settings <- FALSE

  # The default is loaded from the package OR settingsYamlPath yaml file is used
  if( settingsYamlPath == "" ) {

    # no settingsYamlFile supplied - so load the DEFAULT settingsYamlFile from projectmanagr
    settingsYamlFile <- paste( find.package("projectmanagr"), .Platform$file.sep, "config",
                                  .Platform$file.sep, "settings.yml", sep="")

    settings <- yaml::yaml.load( yaml::read_yaml( settingsYamlFile ) )

  } else {

    # settingsYamlFile supplied - load this and CHECK its valid
    settingsYamlFile <- settingsYamlPath

    settings <- yaml::yaml.load( yaml::read_yaml( settingsYamlFile ) )

    # CHECK YAML is valid - must contain all named elements
    #VolumesDir DocsDir ConfigDir OrgIndexFileName
    valid <- { "SitePath" %in% names(settings) &&
               "VolumesDir" %in% names(settings) &&
               "VolumesFile" %in% names(settings) &&
               "DocsDir" %in% names(settings) &&
               "ConfigDir" %in% names(settings) &&
               "ConfigSettingsYamlFile" %in% names(settings) &&
               "ConfigStatusYamlFile" %in% names(settings) &&
               "TemplatesDir" %in% names(settings) &&
               "OrgIndexFileName" %in% names(settings) }

    if( valid == FALSE ) {
      stop( paste0("  settingsYamlPath is not a valid projectmanagr YAML file : ", settingsYamlPath))
    }
    external_settings <- TRUE # flag to know when to use settings for values

  }


  #### Create HTML Site Dir ####

  # This will contain the compiled HTML - which will be made using the R Markdown in the first instance
    # In future, want to move to using HUGO and Blogdown potentially?

  # site dir - placed NEXT TO the orgDir and using the orgName as prefix:
  if( external_settings == FALSE ) {

    # using default sitePath - in parent of organisation Dir, and same Dir name suffixed with _site
    sitePath <- paste(organisationParentDirectory, .Platform$file.sep, orgName, "_site" , sep="")

    if( file.exists(sitePath) == TRUE) {
      # do nothing - dir already exists
      cat( "  site dir exists: ", sitePath, "\n" )
    } else {
      done <- dir.create( sitePath )

      if(!done) {
        stop( paste0("  Site directory could not be created: ", sitePath) )
      }

      cat( "  Made html site dir: ", sitePath, "\n" )

    }
  } else { # external_settings : settings['SitePath'] should be used

    sitePath <- settings[["SitePath"]]

    if( file.exists(sitePath) == TRUE) {
      # do nothing - dir already exists
      cat( "  site dir exists: ", sitePath, "\n" )
    } else {
      done <- dir.create( sitePath, recursive = TRUE ) # recursive to make any parent dirs as needed

      if(!done) {
        stop( paste0("  Site directory could not be created: ", sitePath) )
      }

      cat( "  Made html site dir: ", sitePath, "\n" )

    }
  }

  # ensure sitePath is absolute
  sitePath <- path.expand(sitePath)

  # set SitePath YAML setting to sitePath
  settings[["SitePath"]] <- sitePath


  #### Create org volumes/ Dir ####

  # This will contain SYMLINKS to Volume Mounts, which then act as data stores for data as needed:
    # From each Project Note can run volumes_mkdir() to create a new DIR on a Volume Mount
    # this will form a DIR on the Volume Mount and be SYMLINKED TO a sub-dir in the Project Note DIR (both same name!)
    # NO LONGER using a local data storage idea - can just offset data dynamically as needed WITHIN the Project Note DIR!

  # volumes dir:
  volumesPath = paste(orgPath, .Platform$file.sep, settings[["VolumesDir"]], sep="")
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
  volumesFile = paste(volumesPath, .Platform$file.sep, settings[["VolumesFile"]], sep="") # location to copy file to
  volumesPackageFile <- paste( find.package("projectmanagr"), .Platform$file.sep,
                               "volumes", .Platform$file.sep, "volumes.Rmd", sep="")

  done <- file.copy(volumesPackageFile, volumesFile)

  if(!done) {
    stop( paste0("  Volumes file could not be copied: ", volumesPackageFile, " ", volumesFile) )
  }
  cat( "  Copied volumes file: ", volumesFile, "\n" )


  #### Create org docs/ Dir ####

  # dir:
  docsPath <- paste(orgPath, .Platform$file.sep, settings[["DocsDir"]], sep="")
  done <- dir.create( docsPath )

  if(!done) {
    stop( paste0("  docs directory could not be created: ", todoPath) )
  }

  cat( "  Made docs dir: ", docsPath, "\n" )

  # copy projectmanagr docs file(s):
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


  #### Create org config/ dir & contents ####

  # Contains configuration information for Organisation

  # config dir:
  confPath <- paste(orgPath, .Platform$file.sep, settings[["ConfigDir"]], sep="")
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
  if( settingsYamlPath == "" ) {
    settingsFileName <- settings[["ConfigSettingsYamlFile"]] # the default settings file has the default name in this variable!
  } else {
    settingsFileName <- basename(settingsYamlPath)
  }

  # define the settingsYamlFile in the org being created
  settingsYamlFile <- paste0(confPath, .Platform$file.sep, settingsFileName)

  # write YAML to settingsYamlFile location
  yaml::write_yaml( yaml::as.yaml(settings), settingsYamlFile )

  # record name in YAML settings
  settings[["ConfigSettingsYamlFile"]] <- settingsFileName

  cat( "  Written settings file: ", settingsYamlFile, "\n" )


  # create status.yml file - need to create it here to get the mtime for this file
  statusFile = paste(confPath, .Platform$file.sep, settings[["ConfigStatusYamlFile"]], sep="")
  done <- file.create(statusFile)

  if(!done) {
    stop( paste0("  Status file could not be created: ", statusFile) )
  }

  cat( "  Made status file: ", statusFile, "\n" )


  # Create initial content for status.yml file - data on the Org, plus UPDATE datetime:
  updateTime <- file.info(statusFile)[,5] # retrieve mtime for file, save this time to yaml by converting to a character:
  org <- list(orgPaths, orgPath, orgName, orgTitle, as.character(updateTime) )
   # orgPaths contains all EXISTING ORGs and THIS ORG path at end
  names(org) <- c("orgPaths", "orgPath", "orgName", "orgTitle", "updateTime")
  yaml::write_yaml( yaml::as.yaml(org), statusFile )


  # templates Dir - INSIDE the config DIR (these templates are part of the projectmanagr config!):
  tempPath = paste(confPath, .Platform$file.sep, settings[["TemplatesDir"]] , sep="")
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


  #### Create org Rmd file ####

  if( external_settings == FALSE ) {
    # use the DEFAULT orgPath - prefix with index_ then ORG NAME then .Rmd
    orgFile <- paste(orgPath, .Platform$file.sep, "index_", orgName, ".Rmd", sep="")
  } else {
    # use orgFile defined by yaml settings
    orgFile <- settings[["OrgIndexFileName"]]
  }
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

  # use username as author value
  authorValue <- Sys.info()["user"] # use username as author instead

  # modify templateContents to include orgTitle and authorValue
  templateContents <- gsub("{{TITLE}}", orgTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)

  # modify templateContents with rmarkdown-html-header content
  templateContents <- replaceMarkdownHeader(templateContents, orgPath)

  # modify templateContents with SEP values
  templateContents <- replaceSepValues(templateContents, orgPath)

  # write to orgFile
  fileConn <- file(orgFile)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written template to Org file: ", orgFile, "\n" )

}



#' Get existing Org paths & update the existing org configs to be aware of newOrgPath
#'
#' This function ensures all new Orgs are aware of existing ones, and existing Orgs are aware
#' of new ones.  Organisations can only be aware of eachother if they are in the same parent
#' directory.
#'
#' @param newOrgPath defines the new orgs directory.
#'
#' @export
getOrgPaths <- function(newOrgPath) {

  # get all org paths/names from the parent dir of current orgPath & return as list
  # this also updates the existing org configs to be aware of this new org!

  parent <- dirname(newOrgPath)

  # get list of directories from parent
  dirs <- list.dirs(parent, full.names = TRUE, recursive = FALSE)

  # identify each dir that is an organisation
  for (i in 1:length(dirs) ) {
    dirs[i] <- findOrgDir(dirs[i])
  }

  # remove all blank strings - all dirs that ARE NOT ORGS
  dirs <- dirs[ nzchar(dirs) ]

  # define the orgPaths as all org paths (including new one)
  orgPaths <- c(dirs, newOrgPath)

  # add the orgPaths into each ORGs config/status.yml file
  for (i in 1:length(dirs) ) {

    confPath <- paste(dirs[i], .Platform$file.sep, "config" , sep="")

    # Read the status.yml file first into a LIST:
    statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
    status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

    # add/update the orgPaths
    status$orgPaths <- orgPaths

    # write to statusFile
    yaml::write_yaml( yaml::as.yaml(status), statusFile )

  }

  # return orgPaths
  orgPaths

}
