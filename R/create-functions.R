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
#' @param organisationSiteDirectory an absolute path that defines where the compiled
#' HTML from the organisation is written. If left blank, the default site directory
#' will be the organisation directory path, with directory name suffixed with `_site`.
#'
#' @param orgTitle defines the title of the Organisation, in its document.
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
create_project_org <- function( orgName, orgTitle, organisationParentDirectory,
                              settingsYamlPath = "", orgTemplate="Org-Template.Rmd" ) {


  cat( "\nprojectmanagr::create_project_org():\n" )


  #### Create Organisation Dir ####

  orgPath <- paste0(organisationParentDirectory, .Platform$file.sep, orgName)

  # get all org paths (including this one) from the parent dir of current orgPath
  orgPaths <- get_org_paths(orgPath) # this also updates the existing org configs to be aware of this new org!
   # this ensures all organisations (at least those written to parent DIR of this new ORG) are aware of each other
   # thus can update links across organisations

  # create the fileSystem layout:
  done <- dir.create( orgPath )

  orgPath <- normalizePath(orgPath)

  if(!done) {
    stop( paste0("  Organisation directory could not be created: ", orgFile) )
  }

  cat( "  Made ORG dir: ",orgPath, "\n" )


  #### Load Settings YAML file ####

  # The default is loaded from the package OR function points to settingsYamlPath yaml file which is used
  if( settingsYamlPath == "" ) {

    # no settingsYamlFile supplied - so load the DEFAULT settingsYamlFile from projectmanagr
    settingsYamlFile <- paste0( find.package("projectmanagr"), .Platform$file.sep,
                                "config", .Platform$file.sep, "settings.yml")

    settings <- yaml::yaml.load( yaml::read_yaml( settingsYamlFile ) )

  } else {

    # settingsYamlFile supplied - load this and CHECK its valid
    settingsYamlFile <- settingsYamlPath

    settings <- yaml::yaml.load( yaml::read_yaml( settingsYamlFile ) )

    # CHECK YAML is valid - must contain all named elements
    #VolumesDir DocsDir ConfigDir OrgIndexFileName
    valid <- { "SiteDir" %in% names(settings) &&
               "VolumesDir" %in% names(settings) &&
               "VolumesFile" %in% names(settings) &&
               "DocsDir" %in% names(settings) &&
               "ConfigStatusYamlFile" %in% names(settings) &&
               "OrgIndexFileNamePrefix" %in% names(settings) }

    if( valid == FALSE ) {
      stop( paste0("  settingsYamlPath is not a valid projectmanagr YAML file : ", settingsYamlPath))
    }

  }


  #### Create HTML site Dir ####

  # This will contain the compiled HTML - which will be made using R Markdown package in the first instance
    # In future, want to move to using pandoc directly??

  # site dir - placed in the orgPath:
  sitePath <- paste0(orgPath, .Platform$file.sep, settings[["SiteDir"]])
  done <- dir.create( sitePath )

  if(!done) {
    stop( paste0("  Site directory could not be created: ", sitePath) )
  }

  cat( "  Made html site dir: ", sitePath, "\n" )


  #### Create org volumes Dir ####

  # This will contain SYMLINKS to Volume Mounts, which then act as data stores for data as needed:
    # From each Project Note can run volumes_mkdir() to create a new DIR on a Volume Mount
    # this will form a DIR on the Volume Mount and be SYMLINKED TO a sub-dir in the Project Note DIR (both same name!)
    # NO LONGER using a local data storage idea - can just offset data dynamically as needed WITHIN the Project Note DIR!

  # volumes dir:
  volumesPath <- paste0(orgPath, .Platform$file.sep, settings[["VolumesDir"]])
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
  docsPath <- paste0(orgPath, .Platform$file.sep, settings[["DocsDir"]])
  done <- dir.create( docsPath )

  if(!done) {
    stop( paste0("  docs directory could not be created: ", todoPath) )
  }

  cat( "  Made docs dir: ", docsPath, "\n" )

  # copy projectmanagr docs file(s):
    # need to copy from the PACKAGE!
  docsDir <- paste0( find.package("projectmanagr"), .Platform$file.sep, "docs", .Platform$file.sep)
  docsRmds <- list.files(docsDir)
  for(f in docsRmds) {
    done <- file.copy( paste(docsDir, f, sep=""), docsPath)
    if(!done) {
      stop( paste0("  Could Not copy Template: ", f) )
    }
    cat( "  Copied template: ", f, "\n" )
  }


  #### Create org config/ dir ####

  # Contains configuration information for Organisation

  # config dir: FIXED NAME
  confPath <- paste0(orgPath, .Platform$file.sep, "config")
  done <- dir.create( confPath )

  if(!done) {
    stop( paste0("  Config directory could not be created: ", confPath) )
  }

  cat( "  Made config dir: ", confPath, "\n" )


  #### Create settings.yml file ####

  # a settings.yml file and a status.yml file
    # settings.yml contains user-defined defaults for operations
    # status.yml contains a list of incomplete projects and notes, for each programme

  # define the settingsYamlFile in the org being created
  settingsYamlFile <- paste0(confPath, .Platform$file.sep, "settings.yml")

  # write YAML to settingsYamlFile location
  yaml::write_yaml( yaml::as.yaml(settings), settingsYamlFile )

  cat( "  Written settings file: ", settingsYamlFile, "\n" )


  #### Create status.yml file ####

  # create status.yml file - need to create it here to get the mtime for this file
  statusFile <- paste0(confPath, .Platform$file.sep, settings[["ConfigStatusYamlFile"]])
  done <- file.create(statusFile)

  if(!done) {
    stop( paste0("  Status file could not be created: ", statusFile) )
  }

  cat( "  Made status file: ", statusFile, "\n" )


  # Create initial content for status.yml file - data on the Org, plus UPDATE datetime:
  updateTime <- file.info(statusFile)[,5] # retrieve mtime for file, save this time to yaml by converting to a character:
  org <- list(orgPaths, orgPath, orgName, orgTitle, as.character(updateTime), sitePath )
   # orgPaths contains all EXISTING ORGs and THIS ORG path at end
  names(org) <- c("orgPaths", "orgPath", "orgName", "orgTitle", "updateTime", "sitePath")
  yaml::write_yaml( yaml::as.yaml(org), statusFile )


  #### Copy addins.json to config/ dir ####

  # no settingsYamlFile supplied - so load the DEFAULT settingsYamlFile from projectmanagr
  addinsJsonPackageFile <- paste0( find.package("projectmanagr"), .Platform$file.sep,
                              "config", .Platform$file.sep, "addins.json")

  addinsJsonProjectManagrFile <- paste0(confPath, .Platform$file.sep, "addins.json")

  done <- file.copy(addinsJsonPackageFile, addinsJsonProjectManagrFile)
  if(!done) {
    stop( paste0("  Failed to copy addins.json: ", addinsJsonPackageFile) )
  }
  cat( "  Copied addins.json \n" )

  #### Create templates dir: copy templates from package ####

  # templates Dir - INSIDE the config DIR (these templates are part of the projectmanagr config!):
  # FIXED NAME
  tempPath <- paste0(confPath, .Platform$file.sep, "templates")
  done <- dir.create( tempPath )

  if(!done) {
    stop( paste0("  Templates directory could not be created: ", tempPath) )
  }

  cat( "  Made templates dir: ",tempPath, "\n" )

  # copy template files:
    # need to copy from the PACKAGE!
  templateDir <- paste0( find.package("projectmanagr"), .Platform$file.sep, "templates", .Platform$file.sep)
  templateRmds <- list.files(templateDir)
  for(f in templateRmds) {
    done <- file.copy( paste0(templateDir, f), tempPath)
    if(!done) {
      stop( paste0("  Failed with Template: ", f) )
    }
    cat( "  Copied template: ",f, "\n" )
  }


  #### Create ORG Rmd file ####

  # use the orgPath - prefix with OrgIndexFileNamePrefix then ORG NAME then .Rmd
  orgFile <- paste0(orgPath, .Platform$file.sep, settings[["OrgIndexFileNamePrefix"]], orgName, ".Rmd")
  done <- file.create(orgFile)

  if(!done) {
    stop( paste0("  Org file could not be created: ", orgFile) )
  }

  cat( "  Made Org file: ",orgFile, "\n" )


  # read org template:
  templateContents <- read_file( paste0( tempPath, .Platform$file.sep, orgTemplate) )

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(orgTitle)==0 ) {
    orgTitle = gsub("-", " ", gsub("_", " ", orgName) )
  }

  # use username as author value
  authorValue <- Sys.info()["user"]

  # modify templateContents to include orgTitle and authorValue
  templateContents <- gsub("{{TITLE}}", orgTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)

  templateContents <- sub_template_param(templateContents, "{{PROGRAMME_HEADER}}",
                                         settings[["OrgProgrammeHeader"]], orgPath)
  templateContents <- sub_template_param(templateContents, "{{PROGRAMME_FOOTER}}",
                                         settings[["OrgProgrammeFooter"]], orgPath)

  # modify templateContents with rmarkdown-html-header content
  templateContents <- replace_markdown_header(templateContents, orgPath)

  # modify templateContents with SEP values
  templateContents <- replace_sep_values(templateContents, orgPath)

  # write to orgFile
  write_file(templateContents, orgFile)

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
get_org_paths <- function(newOrgPath) {

  # get all org paths/names from the parent dir of current orgPath & return as list
  # this also updates the existing org configs to be aware of this new org!

  parent <- dirname(newOrgPath)

  # get list of directories from parent
  dirs <- list.dirs(parent, full.names = TRUE, recursive = FALSE)

  # return early if no dirs identified
  if( length(dirs)==0 ) {
    return(dirs) # return the 0-length char vector
  }

  # identify each dir that is an organisation
  for (i in 1:length(dirs) ) {
    dirs[i] <- find_org_directory(dirs[i])
  }

  # remove all blank strings - all dirs that ARE NOT ORGS
  dirs <- dirs[ nzchar(dirs) ]

  # return early if no ORG dirs identified
  if( length(dirs)==0 ) {
    return(dirs) # return the 0-length char vector
  }

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


#' Create a New Programme
#'
#' Generates a new Programme at the top level of the Organisation.  If the
#' organisationPath is not at the top of the Organisation, will traverse until
#' it is found.  This function will halt if no Organisation is found - the
#' organisation root is defined by the presence of config/ and config/templates
#' dirs at root.
#'
#' User must supply the programmeName and programmePrefix.  The
#' programmeName must NOT contain a space, and programmePrefix should be two to three
#' CAPITAL LETTERS.  An optional programmeTitle (which if not supplied will default
#' to the programmeName, replacing "_" & "-" with " ").  The default organisationPath
#' is the working directory.
#'
#' @param programmeName Name of Progamme - must NOT contain a space.
#'
#' @param programmePrefix PREFIX used for all new PROJECTS in this PROGRAMME -
#' should use ALL CAPS, and NO NUMBERS.  Must not contain a space. Ideally two
#' or three letters long.
#'
#' @param programmeTitle Title of programme - typically the programmeName with
#' "_" & "-" replaced with spaces.
#'
#' @param organisationPath Path to insert the PROGRAMME into.  If this is not an
#' Organisation Directory, will search up the directory tree to attempt to find
#' one.  If none found, the method will end without making a PROGRAMME.
#'
#' @param progTemplate The Rmd file to use as a template to create the Programme.  This is set to "Programme-Template.Rmd" in
#' projectmanagr.
#'
#' @export
create_programme <- function(programmeName, programmePrefix,
                            organisationPath, programmeTitle="",
                            progTemplate="Programme-Template.Rmd",
                            progSummaryTemplate = "Programme-Summary-Template.Rmd" ) {

  cat( "\nprojectmanagr::create_programme():\n" )

  # check programmeName contains NO SPACES:
  if( grepl("\\s+", programmeName) ) {
    stop( paste0("  programmeName contains a SPACE: ", programmeName) )
  }

  # check programmePrefix contains NO SPACES:
  if( grepl("\\s+", programmePrefix) ) {
    stop( paste0("  programmePrefix contains a SPACE: ", programmePrefix) )
  }

  # Search for root of an ORGANISATION:

  # get the orgPath from organisationPath
  orgPath <- find_org_directory(organisationPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  organisationPath is not in an Organisation: ", organisationPath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )


  #### Load Settings YAML file ####

  # load from orgPath
  settingsYamlFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="")
  settings <- yaml::yaml.load( yaml::read_yaml( settingsYamlFile ) )


  #### Create Programme Dir ####

  # create PROG Dir:
  progPath <- paste(orgPath, .Platform$file.sep, programmeName, sep="")
  done <- dir.create(progPath)

  if(!done) {
    stop( paste0("  Programme directory could not be created: ", progPath) )
  }

  cat( "  Made Programme dir: ", progPath, "\n" )


  #### create PROJECTS dir ####

  projsPath <- paste(progPath, .Platform$file.sep, settings[["ProgrammeProjectsDir"]], sep="")
  done <- dir.create(projsPath)

  if(!done) {
    stop( paste0("  PROJECTS directory could not be created: ", projsPath) )
  }

  cat( "  Made PROJECTS dir: ",projsPath, "\n" )


  #### create protocols dir ####

  templatesPath <- paste(progPath, .Platform$file.sep, settings[["ProgrammeProtocolsDir"]], sep="")
  done <- dir.create(templatesPath)

  if(!done) {
    stop( paste0("  SOP directory could not be created: ", templatesPath) )
  }

  cat( "  Made SOP dir: ",templatesPath, "\n" )


  #### create Rmd file ####

  progFilePath <- paste0(progPath, .Platform$file.sep, settings[["ProgIndexFileNamePrefix"]],
                         programmeName, ".Rmd")
  done <- file.create(progFilePath)

  if(!done) {
    stop( paste0("  Programme index .Rmd file could not be created: ", progFilePath) )
  }

  cat( "  Made Programme index .Rmd file: ",progFilePath, "\n" )


  # read progTemplate:
  progContents <- read_file( paste0(tempPath, .Platform$file.sep, progTemplate) )

  # Check programmeTitle, and if blank, fill with programmeName, replacing all "_" and "-" with spaces
  if( nchar(programmeTitle) == 0 ) {
    programmeTitle <- gsub("-", " ", gsub("_", " ", programmeName) )
  }

  # Read the status.yml file first into a LIST:
  statusFile <- paste( confPath, .Platform$file.sep, settings[["ConfigStatusYamlFile"]], sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  # use username as author value
  authorValue <- Sys.info()["user"]

  # modify progContents to include programmeTitle and authorValue
  progContents <- gsub("{{TITLE}}", programmeTitle, progContents, fixed=TRUE)
  progContents <- gsub("{{AUTHOR}}", authorValue, progContents, fixed=TRUE)

  # modify programme summary header/footer
  progContents <- sub_template_param(progContents, "{{SUMMARY_HEADER}}",
                                     settings[["ProgSummaryHeader"]], orgPath)
  progContents <- sub_template_param(progContents, "{{SUMMARY_FOOTER}}",
                                     settings[["ProgSummaryFooter"]], orgPath)

  # modify progContents to include relative link to org index Rmd
  orgFilePath <- paste0(orgPath, .Platform$file.sep,
                        settings[["OrgIndexFileNamePrefix"]], status[["orgName"]], ".Rmd")
  orgTitle <- substring(basename(orgFilePath), first=1, last=nchar(basename(orgFilePath))-4)
  NoteLink <- R.utils::getRelativePath(orgFilePath, relativeTo=progFilePath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  orgLink <- paste("[", orgTitle, "](", NoteLink, ")",  sep="")

  progContents <- gsub("{{ORGLINK}}", orgLink, progContents, fixed=TRUE)

  # modify programme projects header/footer
  progContents <- sub_template_param(progContents, "{{PROJECTS_HEADER}}",
                                     settings[["ProgProjectsHeader"]], orgPath)
  progContents <- sub_template_param(progContents, "{{PROJECTS_FOOTER}}",
                                     settings[["ProgProjectsFooter"]], orgPath)

  # modify progContents with rmarkdown-html-header content
  progContents <- replace_markdown_header(progContents, orgPath)

  # modify progContents with SEP values
  progContents <- replace_sep_values(progContents, orgPath)

  # write to progFile
  write_file(progContents, progFilePath)

  cat( "  Written template to Programme index.Rmd file: ", progFilePath, "\n" )


  #### Write Programme to Status file ####

  # add the programmePrefix under the programmeName in the "PROGRAMMES" section of the status.yml List:
  attrs <- list(programmePrefix, programmeTitle, as.character(file.info(progFilePath)[,5]) )
  names(attrs) <- c("programmePrefix", "programmeTitle", "creationTime")
  status[["PROGRAMMES"]][[programmeName]] <- attrs
  # can retrieve the programmePrefix with call to:  status[["PROGRAMMES"]][[programmeName]][["programmePrefix"]]

  # Write status list to the statusFile:
  yaml::write_yaml( yaml::as.yaml(status), statusFile )

  cat( "  Written PROGRAMME to Status.yml file: ", statusFile, "\n" )


  #### Insert PROG Summary in ORG index file ####

  # first generate the progSummary filling in the progSummaryTemplate:

  # read programme summary header template
  progSummaryTemplateContents <- read_file( paste0(tempPath, .Platform$file.sep, progSummaryTemplate) )

  # create the progIndexLink:
  NoteLink <- R.utils::getRelativePath(progFilePath, relativeTo=orgFilePath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  progIndexLink <- paste("[", programmeName, "](", NoteLink, ")",  sep="")

  # collect string from programme summary section to paste into org index
  # first identify the indices between which the programme summary exists in prog Rmd
  progSummaryHeadIndex <- match_line_index( load_param_vector(settings[["ProgSummaryHeader"]], orgPath),
                                          progContents)
  progSummaryFootIndex <- grep_line_index_from( load_param_vector(settings[["ProgSummaryFooter"]], orgPath),
                                             progContents, progSummaryHeadIndex)-1
  # now get programme summary content to paste into org Rmd
  summaryContents <- progContents[progSummaryHeadIndex:progSummaryFootIndex]
  progSummary <- summaryContents[ (match_line_index(orgLink, summaryContents) +1 ) : length(summaryContents) ]


  # fill progSummaryTemplateContents with correct content
  progSummaryTemplateContents <- sub_template_param(progSummaryTemplateContents,
                                                    "{{PROG_SUMMARY_SEP}}",
                                                    settings[["OrgProgrammeSummarySep"]], orgPath)

  progSummaryTitle <- paste0(settings[["ProgSummaryTitle"]], programmeName)
  progSummaryTemplateContents <- gsub("{{PROG_SUMMARY_TITLE}}", progSummaryTitle,
                                      progSummaryTemplateContents, fixed=TRUE)

  progSummaryTemplateContents <- gsub("{{PROG_LINK}}", progIndexLink,
                                      progSummaryTemplateContents, fixed=TRUE)

  progSummaryTemplateContents <- sub_template_param(progSummaryTemplateContents,
                                                    "{{PROG_SUMMARY}}",
                                                    progSummary, orgPath)

  # Next, insert progSummary into orgContents

  # read Organisation File from ORG
  orgContents <- read_file(orgFilePath)

  # find programme header -> footer indices
  orgProgrammeHeaderIndex <- match_line_index( load_param_vector(settings[["OrgProgrammeHeader"]], orgPath),
                                             orgContents) # finds FIRST MATCH
  orgProgrammeFooterIndex <- grep_line_index_from( load_param_vector(settings[["OrgProgrammeFooter"]], orgPath),
                                                orgContents, orgProgrammeHeaderIndex)

  # simply insert progSummaryVector at END of orgProgramme summary in orgContents - at orgProgrammeFooterIndex
  orgContents <- insert_at_indices(orgContents, orgProgrammeFooterIndex, progSummaryTemplateContents)

  # write to orgFilePath
  write_file(orgContents, orgFilePath)

  cat( "  Written Programme Link to Org File: ", basename(orgFilePath), "\n" )


}


#' Create a New Project Document
#'
#' Generates a new Project inside a Programme in an Organisation.  The programmePath
#' must be in the Programme directory, which is the sub-Dir to the root ORGANISATION
#' dir.
#'
#' The project is automatically numbered, by reading the current projects and determining
#' the next number in the sequence, as well as deriving the Programme Prefix.
#'
#' User must supply the project name, which must contain NO SPACES, and optionally a project
#' title.  If no title is provided, the Title is derived from the project name, replacing any
#' "_" & "-" with " ". The default programmePath is the working directory.
#'
#' @param projectName Name of Project - must NOT contain a space.
#'
#' @param projectTitle Title of project - typically the projectName with "_" & "-" replaced with spaces.
#'
#' @param programmePath Path to insert the Project into.  This must be a Programme dir, one level below the
#' organisation, and containing a PROJECTS/ directory.  The Project will be placed into the PROJECTS/ directory.
#' If none found, the method will end without making a Project
#'
#' @param projDocTemplate Rmd template used to create the Project Doc - default is "Project-Doc-Template.Rmd"
#'
#' @param projectIndex If 0, the function will determine the projectIndex by searching the PROJECTS/ directory.
#' Otherwise, the projectIndex is used to number the Project in its Prefix.
#'
#' @export
create_project_doc <- function(projectName, programmePath, projectTitle="",
                             projDocTemplate="Project-Doc-Template.Rmd",
                             projDocSummaryTemplate="Project-Doc-Summary-Template.Rmd",
                             projectIndex=0 ) {

  cat( "\nprojectmanagr::create_project_doc():\n" )


  #### instance variables ####

  # check projectName contains NO SPACES:
  if( grepl("\\s+", projectName) ) {
    stop( paste0("  projectName contains a SPACE: ", projectName) )
  }


  # get the orgPath from programmePath - confirmed above to sit in an organisation!
  orgPath <- find_org_directory(programmePath)

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  settingsYamlFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="")
  settings <- yaml::yaml.load( yaml::read_yaml( settingsYamlFile ) )

  # Check programmePath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  progPath <- check_prog_dir(programmePath, settings)

  if(  progPath == ""  ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, programmePath is not inside a PROGRAMME sub-dir!
    stop( paste0("  programmePath is not in a PROGRAMME Directory: ", programmePath) )
  }

  # programmePath is therefore in a PROGRAMME DIR


  # extract the PROGRAMME NAME from the programmePath:
  programmeName <-basename(progPath)

  # define the projects path - settings[["ProgrammeProjectsDir"]] in progPath
  projsPath <- paste0( progPath, .Platform$file.sep, settings[["ProgrammeProjectsDir"]])

  # extract the programme prefix from status file
  statusFile <- paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )
  programmePrefix <- status[["PROGRAMMES"]][[programmeName]][["programmePrefix"]]


  if(projectIndex < 1) { # if projectIndex is below 1 (default is 0), then try to identify what projectIndex should be
    # by looking at DIR numbers:

    projectIndex <- compute_project_index(projsPath, programmePrefix)

  } else { # else, if projectIndex was set to be above 0, then use this number!

    if(projectIndex < 10 ) {
      projectIndex <- paste("0", projectIndex, sep="")
    } else {
      projectIndex <- paste("", projectIndex, sep="")
    }

  }


  #### Create Project Dir ####

  projPath <- paste0(projsPath, .Platform$file.sep, programmePrefix, projectIndex)
  done <- dir.create(projPath)

  if(!done) {
    stop( paste0("  Project directory could not be created: ", projPath) )
  }

  cat( "  Made Project dir: ",projPath, "\n" )


  #### create Rmd file ####

  projDocFilePath <- paste0(projsPath, .Platform$file.sep, programmePrefix, projectIndex,
                            settings[["ProjectPrefixSep"]], projectName, ".Rmd" )
  done <- file.create(projDocFilePath)

  if(!done) {
    stop( paste0("  Project file could not be created: ", projDocFilePath) )
  }

  cat( "  Made Project file: ", projDocFilePath, "\n" )

  # read project doc template:
  projDocContents <- read_file(paste0( tempPath, .Platform$file.sep, projDocTemplate))

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(projectTitle)==0 ) {
    projectTitle <- gsub("-", " ", gsub("_", " ", projectName) )
  }

  # use username as author value
  authorValue <- Sys.info()["user"]

  # modify projDocContents to include PREFIX projectTitle author
  projDocContents <- gsub("{{PREFIX}}", paste(programmePrefix, projectIndex, sep=""), projDocContents, fixed=TRUE)
  projDocContents <- gsub("{{TITLE}}", projectTitle, projDocContents, fixed=TRUE)
  projDocContents <- gsub("{{AUTHOR}}", authorValue, projDocContents, fixed=TRUE)

  # modify programme summary header/footer
  projDocContents <- sub_template_param(projDocContents, "{{SUMMARY_HEADER}}",
                                        settings[["ProjectSummaryHeader"]], orgPath)
  projDocContents <- sub_template_param(projDocContents, "{{SUMMARY_FOOTER}}",
                                        settings[["ProjectSummaryFooter"]], orgPath)

  # write correct programme link
  progFilePath <- paste(progPath, .Platform$file.sep, "index_", programmeName, ".Rmd", sep="")
  programmeTitle <- programmeName
  progLink <- create_hyperlink( programmeTitle,
                               progFilePath, projDocFilePath)
  projDocContents <- gsub("{{PROGLINK}}", progLink, projDocContents, fixed=TRUE)

  # modify goal/del/task sep, header, footer vals
  projDocContents <- sub_template_param(projDocContents, "{{GOAL_SEP}}",
                                        settings[["ProjectGoalSep"]], orgPath)
  projDocContents <- sub_template_param(projDocContents, "{{GOAL_HEADER}}",
                                        settings[["ProjectGoalHeader"]], orgPath)
  projDocContents <- sub_template_param(projDocContents, "{{DELIVERABLE_SEP}}",
                                        settings[["ProjectDeliverableSep"]], orgPath)
  projDocContents <- sub_template_param(projDocContents, "{{DELIVERABLE_HEADER}}",
                                        settings[["ProjectDeliverableHeader"]], orgPath)
  projDocContents <- sub_template_param(projDocContents, "{{TASK_SEP}}",
                                        settings[["ProjectTaskSep"]], orgPath)
  projDocContents <- sub_template_param(projDocContents, "{{TASK_HEADER}}",
                                        settings[["ProjectTaskHeader"]], orgPath)
  projDocContents <- sub_template_param(projDocContents, "{{TASK_FOOTER}}",
                                        settings[["ProjectTaskFooter"]], orgPath)

  # write Task Overview & Task Log values
  projDocContents <- sub_template_param(projDocContents, "{{PROJECT_TASK_OVERVIEW}}",
                                        settings[["ProjectTaskOverviewHeader"]], orgPath)
  projDocContents <- sub_template_param(projDocContents, "{{PROJECT_TASK_LOG}}",
                                        settings[["ProjectTaskLogHeader"]], orgPath)

  # modify projDocContents with rmarkdown-html-header content
  projDocContents <- replace_markdown_header(projDocContents, orgPath)

  # modify projDocContents with SEP values
  projDocContents <- replace_sep_values(projDocContents, orgPath)

  # write to projDocFilePath
  write_file(projDocContents, projDocFilePath)

  cat( "  Written template to Project file: ", projDocFilePath, "\n" )


  #### Insert Project Doc Summary in Programme ####

  # read Programme File from ORG
  progContents <- read_file( progFilePath )

  # read programme summary header template
  projDocSummaryTemplateContents <- read_file( paste0(tempPath, .Platform$file.sep, projDocSummaryTemplate) )

  # create hyperlink from prog to projDoc - to insert in prog summary of projDoc
  projDocName <- substring( basename(projDocFilePath),
                            first=1, last=nchar(basename(projDocFilePath))-4)
  projDocLink <- create_hyperlink( projDocName,
                                  projDocFilePath, progFilePath )

  # collect string from projDoc summary section to paste into prog index
  # first identify the indices between which the projDoc summary exists in projDoc Rmd
  projSummaryHeadIndex <- match_line_index( load_param_vector(settings[["ProjectSummaryHeader"]], orgPath),
                                          projDocContents)
  projSummaryFootIndex <- grep_line_index_from( load_param_vector(settings[["ProjectSummaryFooter"]], orgPath),
                                             projDocContents, projSummaryHeadIndex)-1
  # now get projDoc summary content to paste into prog Rmd
  summaryContents <- projDocContents[projSummaryHeadIndex:projSummaryFootIndex]
  projSummary <- summaryContents[ (match_line_index(progLink, summaryContents) +1 ) : length(summaryContents) ]


  # fill projDocSummaryTemplateContents with correct content
  projSummaryTitle <- paste0(settings[["ProjectSummaryTitle"]], projDocName)
  projDocSummaryTemplateContents <- gsub("{{PROJECT_DOC_SUMMARY_TITLE}}", projSummaryTitle,
                                         projDocSummaryTemplateContents, fixed=TRUE)
  projDocSummaryTemplateContents <- gsub("{{PROJECT_DOC_LINK}}", projDocLink,
                                         projDocSummaryTemplateContents, fixed=TRUE)

  projDocSummaryTemplateContents <- sub_template_param(projDocSummaryTemplateContents,
                                                       "{{PROJECT_DOC_SUMMARY_SEP}}",
                                                       settings[["ProgProjectSummarySep"]], orgPath)
  projDocSummaryTemplateContents <- sub_template_param(projDocSummaryTemplateContents,
                                                       "{{PROJECT_DOC_SUMMARY}}",
                                                       projSummary, orgPath)


  # compute location in org to insert the programme summary
  # FIRST check if projDocLink exists IN THE PROJECTS SECTION of progContents
  progProjDocHeaderIndex <- match_line_index( load_param_vector(settings[["ProgProjectsHeader"]], orgPath),
                                            progContents) # finds FIRST MATCH
  progProjDocFooterIndex <- grep_line_index_from( load_param_vector(settings[["ProgProjectsFooter"]], orgPath),
                                               progContents, progProjDocHeaderIndex)

  # insert projDocSummaryTemplateContents at END of project summary in progContents - progProjDocFooterIndex
  progContents <- insert_at_indices(progContents, progProjDocFooterIndex, projDocSummaryTemplateContents)

  # write to progFilePath
  write_file(progContents, progFilePath)

  cat( "  Written Project Doc to Programme File: ", basename(progFilePath), "\n" )

}


#' Add a New Project Note
#'
#' This Addin adds a single Project Note to a Poject Doc under a Goal / Del /
#' Task - the Project Note consists of one Rmd Note and its corresponding
#' Directory, named with a computed `projectNotePrefix` including identifier
#' and Major Numbering, separated by `ProjectIndexSep`, as specified in
#' `settings.yml`, and `projectNoteName`.
#'
#' @param projectNoteName The name of the Project Note, a String that should
#' not contain any SPACES.
#'
#' @param projectNotePath The directory where the Project Note will be stored.
#' This may be a Project Directory, or another Directory specified by the User.
#' MUST be a sub-directory or lower inside a PROGRAMME Directory. The
#' `projectNotePrefix` is computed from this directory - where the string identifier
#' is the first string in the directory name up to `ProjectIdentifierSep` (specified
#' in `settings.yml`, default is '_').
#'
#' @param selection List containing the Goal, Del, Task selected from the Project
#' Doc, as well as other useful information - lines of Task/Del/Goal, projectDoc
#' path content of selection line.  See `cursor_selection()` or `user_selection()`.
#'
#' @param projectNoteTitle OPTIONAL title for the Project Note.  Default is to use
#' projectNoteName and replace all `_` and `-` with SPACES.
#'
#' @param projNoteTemplate Template to use, as found in the `config/templates/`
#' directory.  Default is "Project-Note-Template.Rmd"
#'
#' @param projNoteLinkTemplate Template with structure to add the Project Doc
#' Goal/Del/Task link in the Project Note.  Includes Project Doc title, link,
#' and then links to Goal / Del / Task, plus a Summary Info section (filled
#' with the content from `taskSectionHeaderTemplate`).
#'
#' @param projNoteLinkSummaryTemplate Template with structure to add underneath the
#' Project Doc Goal/Del/Task link in the Project Note.  Includes a 'summary' section
#' and a 'todo' section by default, linked to `NoteObjectivesSummarySectionHeader`
#' & `NoteObjectivesTodoSectionHeader` in `config/settings.yml`
#'
#' @param projNoteSummaryTemplate Template with structure to add Project Note
#' summary to Project Doc under Goal/Del/Task.
#'
#' @export
create_project_note <- function( projectNoteName, projectNotePath,
                                 selection, projectNoteTitle="",
                                 projNoteTemplate="Project-Note-Template.Rmd",
                                 projNoteLinkTemplate="Project-Note-Link-Template.Rmd",
                                 projNoteLinkSummaryTemplate="Project-Note-Link-Summary-Template.Rmd",
                                 todoTemplate="Todo-Template.Rmd",
                                 projNoteSummaryTemplate="Project-Note-Summary-Template.Rmd" ) {


  cat( "\nprojectmanagr::create_project_note():\n" )


  #### CHECK FOR ERRORS IN INPUT ####

  # Check projectNoteName contains NO SPACES:
  if( grepl("\\s+", projectNoteName) ) {
    stop( paste0("  projectNoteName contains a SPACE: ", projectNoteName) )
  }

  # check selection is a project DOC
  if( selection[["rmdType"]] != "DOC" ) {
    stop( paste0("  selection is not a Project DOC: ", selection[["filePath"]]) )
  }


  #### Set Instance Variables ####

  # Check projectNotePath is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname( dirname(projectNotePath) ) # this should be the orgPath!
  orgPath <- find_org_directory(orgPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNotePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  # If projectTitle blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(projectNoteTitle)==0 ) {
    projectNoteTitle = gsub("-", " ", gsub("_", " ", projectNoteName) )
  }

  projectDocPath <- selection[["filePath"]] # selection is a project DOC

  projectNotePath <- normalizePath(projectNotePath)

  projectNotePrefix <- get_next_simple_prefix(projectNotePath, settings)



  #### Read Rmds ####

  projNoteRmdContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteTemplate) )

  projNoteLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkTemplate) )
  projNoteLinkSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkSummaryTemplate) )
  todoContents <- read_file( paste0( tempPath, .Platform$file.sep, todoTemplate) )

  projNoteSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteSummaryTemplate) )

  projDocContents <- read_file(projectDocPath)


  #### Create Note Dir ####

  projDirPath <- paste0( projectNotePath, .Platform$file.sep, projectNotePrefix)
  done <- dir.create( projDirPath )

  if(!done) {
    stop( paste0("  DIR for Project Note could not be created: ", projDirPath) )
  }

  cat( "  Made Project Note DIR: ", projDirPath, "\n" )


  #### create Rmd file ####

  # Create blank RMD DOCUMENT:
  projNoteRmdPath <- paste0( projectNotePath, .Platform$file.sep, projectNotePrefix,
                             settings[["ProjectPrefixSep"]], projectNoteName, ".Rmd")
  done <- file.create( projNoteRmdPath )

  if(!done) {
    file.remove(projDirPath) # remove the project note dir
    stop( paste0("  Project Note could not be created: ", projNoteRmdPath) )
  }


  #### Replace markup in Note with values ####

  # use username as author value
  authorValue <- Sys.info()["user"]

  # modify projNoteRmdContents to include PREFIX and projectTitle
  projNoteRmdContents <- sub_template_param(projNoteRmdContents, "{{PREFIX}}",
                                         projectNotePrefix, orgPath)
  projNoteRmdContents <- sub_template_param(projNoteRmdContents, "{{TITLE}}",
                                         projectNoteTitle, orgPath)
  projNoteRmdContents <- sub_template_param(projNoteRmdContents, "{{AUTHOR}}",
                                         authorValue, orgPath)

  projNoteRmdContents <- sub_template_param(projNoteRmdContents, "{{OBJECTIVES_HEADER}}",
                                         settings[["NoteObjectivesHeader"]], orgPath)
  projNoteRmdContents <- sub_template_param(projNoteRmdContents, "{{OBJECTIVES_FOOTER}}",
                                         settings[["NoteObjectivesFooter"]], orgPath)

  projNoteRmdContents <- sub_template_param(projNoteRmdContents, "{{DATA_STORAGE_HEADER}}",
                                         settings[["NoteStorageHeader"]], orgPath)
  projNoteRmdContents <- sub_template_param(projNoteRmdContents, "{{DATA_STORAGE_FOOTER}}",
                                         settings[["NoteStorageFooter"]], orgPath)


  # modify projNoteRmdContents with rmarkdown-html-header content
  projNoteRmdContents <- replace_markdown_header(projNoteRmdContents, orgPath)

  # modify projNoteRmdContents with SEP values
  projNoteRmdContents <- replace_sep_values(projNoteRmdContents, orgPath)


  #### write Project Note ####

  write_file(projNoteRmdContents, projNoteRmdPath)

  cat( "  Made Project Note: ", projNoteRmdPath, "\n" )


  #### Link Project Note and Project Doc ####

  linkFormed <- link_project_note_doc(selection, settings, projNoteRmdPath, projNoteRmdContents, projNoteLinkContents,
                                      projNoteLinkSummaryContents, todoContents, projNoteSummaryContents,
                                      projDocContents, orgPath)

  if( linkFormed == FALSE ) {
    # remove the project note and directory
    file.remove(projDirPath) # remove the project note dir
    file.remove(projNoteRmdPath) # remove the project note Rmd
    stop( paste0("  Creating Project Note Failed - link already exists."))
  }
}


#' Add a New Group of Project Notes
#'
#' This Function adds a Project Note Group - consisting of one HEADER
#' Note, and one SUBNOTE inside the Header Note Dir, named with a computed
#' `groupNotePrefix`  including identifier and Major Numbering, separated by
#' `GroupNotePrefixSep` & `HeaderNotePrefix` for header note or first subNote
#' index for subNote, as specified in `settings.yml`.
#'
#' This Note Group can be expanded by adding further SubNotes - useful for
#' Optimisation and Experimental Repeats - see `create_sub_note()`.
#'
#' @param groupNoteName The name of the Header Project Note, a String that should
#' not contain any SPACES.
#'
#' @param groupNotePath The directory where the Header Note will be stored.
#' This may be a Project Directory, or another Directory specified by the User.
#' MUST be a sub-directory or lower inside a PROGRAMME Directory.
#'
#' @param selection List containing the Goal, Del, Task selected from the Project
#' Doc, as well as other useful information - lines of Task/Del/Goal, projectDoc
#' path content of selection line.  See `cursor_selection()` or `user_selection()`.
#'
#' @param subNoteName The First SubNote name, added to the Project Note Group
#' in the Header Note DIR.
#'
#' @param addObjToHeader Boolean to indicate whether the Objective from the
#' Project Doc is set in the Header Note. True by default.  If False, no Project
#' Doc Goal/Del/Task is inserted into the Header Note, and the first subnote
#' is linked in the ProjectDoc G/D/T as a simple note.
#'
#' @param projectNoteTitle OPTIONAL title for the Project HEADER Note.  Default
#' is to use groupNoteName and replaceall _ and - with SPACES.
#'
#' @param subNoteTitle OPTIONAL title for the Project Sub Note.  Default is to
#' use subNoteName and replace all _ and - with SPACES.
#'
#' @param projNoteTemplate Template to use, as found in the `config/templates/`
#' directory.  Default is "Project-Header-Note-Template.Rmd"
#'
#' @param subNoteTemplate Template to use, as found in the `config/templates/`
#' directory.  Default is "Project-Sub-Note-Template.Rmd"
#'
#' @param projNoteLinkTemplate Template with structure to add the Project Doc
#' Goal/Del/Task link in the Project Note.  Includes Project Doc title, link,
#' and then links to Goal / Del / Task, plus a Summary Info section (filled
#' with the content from `taskSectionHeaderTemplate`).
#'
#' @param projNoteLinkSummaryTemplate Template with structure to add underneath the
#' Project Doc Goal/Del/Task link in the Project Note.  Includes a 'summary' section
#' and a 'todo' section by default, linked to `NoteObjectivesSummarySectionHeader`
#' & `NoteObjectivesTodoSectionHeader` in `config/settings.yml`
#'
#' @param projNoteSummaryTemplate Template with structure to add Project Note
#' summary to Project Doc under Goal/Del/Task.
#'
#' @param subNoteSummaryTemplate Template with structure to add Project Sub Note
#' summary to Project Doc under Goal/Del/Task.
#'
#' @export
create_group_note  <- function( groupNoteName, groupNotePath,
                                selection, subNoteName, addObjToHeader=TRUE,
                                groupNoteTitle="", subNoteTitle="",
                                projNoteTemplate="Project-Header-Note-Template.Rmd",
                                subNoteTemplate="Project-Sub-Note-Template.Rmd",
                                headerNoteContentLinkTemplate="Project-Header-Note-Content-Link-Template.Rmd",
                                subNoteContentLinkTemplate="Project-Sub-Note-Content-Link-Template.Rmd",
                                projNoteLinkTemplate="Project-Note-Link-Template.Rmd",
                                projNoteLinkSummaryTemplate="Project-Note-Link-Summary-Template.Rmd",
                                todoTemplate="Todo-Template.Rmd",
                                projNoteSummaryTemplate="Project-Note-Summary-Template.Rmd",
                                subNoteSummaryTemplate="Project-Sub-Note-Summary-Template.Rmd" ) {


  cat( "\nprojectmanagr::create_group_note():\n" )


  #### CHECK FOR ERRORS IN INPUT ####

  # Check groupNoteName contains NO SPACES:
  if( grepl("\\s+", groupNoteName) ) {
    stop( paste0("  groupNoteName contains a SPACE: ", groupNoteName) )
  }

  # check selection is a project DOC
  if( selection[["rmdType"]] != "DOC" ) {
    stop( paste0("  selection is not a Project DOC: ", selection[["filePath"]]) )
  }


  #### Set Instance Variables ####

  # Check groupNotePath is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname( dirname(groupNotePath) ) # this should be the orgPath!
  orgPath <- find_org_directory(orgPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  groupNotePath is not in a sub-dir of a PROGRAMME Directory: ", groupNotePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(groupNoteTitle)==0 ) {
    groupNoteTitle <- gsub("-", " ", gsub("_", " ", groupNoteName) )
  }

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(subNoteTitle)==0 ) {
    subNoteTitle <- gsub("-", " ", gsub("_", " ", subNoteName) )
  }

  projectDocPath <- selection[["filePath"]]
  # groupNotePath is the parent directory the headerNoteRmdPath (Rmd file) sits in
  groupNotePath <- normalizePath(groupNotePath)


  #### Read Rmds ####

  headerNoteRmdContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteTemplate) )
  subNoteContents <- read_file( paste0( tempPath, .Platform$file.sep, subNoteTemplate) )

  headerNoteContentLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, headerNoteContentLinkTemplate) )
  subNoteContentLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, subNoteContentLinkTemplate) )

  headerNoteLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkTemplate) )

  projNoteLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkTemplate) )
  projNoteLinkSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkSummaryTemplate) )
  todoContents <- read_file( paste0( tempPath, .Platform$file.sep, todoTemplate) )

  projNoteSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteSummaryTemplate) )
  subNoteSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, subNoteSummaryTemplate) )

  projDocContents <- read_file(projectDocPath)


  #### Create Header Note Dir ####

  headerNotePrefix <- get_next_header_prefix(groupNotePath, settings)

  headerNoteDir <- paste0( groupNotePath, .Platform$file.sep, headerNotePrefix)
  done <- dir.create( headerNoteDir )

  if(!done) {
    stop( paste0("  Header Note directory could not be created: ", headerNoteDir) )
  }

  cat( "  Made Project Header dir: ", headerNoteDir, "\n" )


  #### Create Sub Note Dir ####

  subNotePrefix <- get_next_subnote_prefix(headerNoteDir, settings)
  subNotePath <- headerNoteDir
  subNoteDir <- paste0( subNotePath, .Platform$file.sep, subNotePrefix)
  done <- dir.create( subNoteDir )

  if(!done) {
    file.remove(headerNoteDir) # remove the header note dir
    stop( paste0("  Sub Note directory could not be created: ", subNoteDir) )
  }

  cat( "  Made Project Sub Note dir: ", subNoteDir, "\n" )


  #### create Header Note Rmd file ####

  headerNoteRmdPath <- paste0( groupNotePath, .Platform$file.sep, headerNotePrefix,
                               settings[["ProjectPrefixSep"]], groupNoteName, ".Rmd")
  headerNoteFileName <- basename(headerNoteRmdPath)
  done <- file.create( headerNoteRmdPath )

  if(!done) {
    file.remove(headerNoteDir) # remove the header note dir
    file.remove(subNoteDir) # remove the sub note dir
    stop( paste0("  Project Header Note could not be created: ", headerNoteRmdPath) )
  }


  #### create Sub Note Rmd file ####

  subNoteRmdPath <- paste0( subNotePath, .Platform$file.sep, subNotePrefix,
                            settings[["ProjectPrefixSep"]], subNoteName, ".Rmd")
  subNoteFileName <- basename(subNoteRmdPath)
  done <- file.create( subNoteRmdPath )

  if(!done) {
    file.remove(headerNoteDir) # remove the header note dir
    file.remove(subNoteDir) # remove the sub note dir
    file.remove(headerNoteRmdPath) # remove the header note file
    stop( paste0("  Project Sub Note could not be created: ", subNoteRmdPath) )
  }


  #### HEADER : Replace markup  with values ####

  # use username as author value
  authorValue <- Sys.info()["user"]

  # modify headerNoteRmdContents
  headerNoteRmdContents <- sub_template_param(headerNoteRmdContents, "{{PREFIX}}",
                                           headerNotePrefix, orgPath)
  headerNoteRmdContents <- sub_template_param(headerNoteRmdContents, "{{TITLE}}",
                                           groupNoteTitle, orgPath)
  headerNoteRmdContents <- sub_template_param(headerNoteRmdContents, "{{AUTHOR}}",
                                           authorValue, orgPath)

  headerNoteRmdContents <- sub_template_param(headerNoteRmdContents, "{{OBJECTIVES_HEADER}}",
                                           settings[["NoteObjectivesHeader"]], orgPath)
  headerNoteRmdContents <- sub_template_param(headerNoteRmdContents, "{{OBJECTIVES_FOOTER}}",
                                           settings[["NoteObjectivesFooter"]], orgPath)

  headerNoteRmdContents <- sub_template_param(headerNoteRmdContents, "{{DATA_STORAGE_HEADER}}",
                                           settings[["NoteStorageHeader"]], orgPath)
  headerNoteRmdContents <- sub_template_param(headerNoteRmdContents, "{{DATA_STORAGE_FOOTER}}",
                                           settings[["NoteStorageFooter"]], orgPath)

  headerNoteRmdContents <- sub_template_param(headerNoteRmdContents, "{{HEADER_NOTE_CONTENTS_HEADER}}",
                                           settings[["HeaderNoteContentsHeader"]], orgPath)
  headerNoteRmdContents <- sub_template_param(headerNoteRmdContents, "{{HEADER_NOTE_CONTENTS_FOOTER}}",
                                           settings[["HeaderNoteContentsFooter"]], orgPath)

  # modify headerNoteRmdContents with rmarkdown-html-header content
  headerNoteRmdContents <- replace_markdown_header(headerNoteRmdContents, orgPath)

  # modify headerNoteRmdContents with SEP values
  headerNoteRmdContents <- replace_sep_values(headerNoteRmdContents, orgPath)


  #### insert sub note content link into header note ####

  subNoteContentLink <- create_hyperlink( subNoteFileName, subNoteRmdPath, headerNoteRmdPath)
  subNoteContentLinkContents <- sub_template_param(subNoteContentLinkContents,
                                                   "{{HEADER_NOTE_CONTENT_LINK}}",
                                                   subNoteContentLink, orgPath)

  noteContentsHeadIndex <- match_line_index( load_param_vector(settings[["HeaderNoteContentsHeader"]], orgPath),
                                           headerNoteRmdContents)
  noteContentsFootIndex <- grep_line_index_from( load_param_vector(settings[["HeaderNoteContentsFooter"]], orgPath),
                                                 headerNoteRmdContents, noteContentsHeadIndex)

  headerNoteRmdContents <- insert_at_indices(headerNoteRmdContents, noteContentsFootIndex, subNoteContentLinkContents)


  #### write Header Note ####

  write_file(headerNoteRmdContents, headerNoteRmdPath)

  cat( "  Made Project Header Note: ", headerNoteRmdPath, "\n" )


  #### SUBNOTE : Replace markup with values ####

  # use username as author value
  authorValue <- Sys.info()["user"]

  # sub subNoteContents with params
  subNoteContents <-sub_subnote_params(subNoteContents, subNotePrefix,
                                       subNoteTitle, authorValue,
                                       settings, orgPath)


  #### insert header note content link into sub note ####

  subNoteContents <- insert_subnote_header_link(subNoteContents, headerNoteFileName,
                                                headerNoteRmdPath, subNoteRmdPath,
                                                headerNoteContentLinkContents,
                                                settings, orgPath)


  #### write Sub Note ####

  write_file(subNoteContents, subNoteRmdPath)

  cat( "  Made Project Sub Note: ", subNoteRmdPath, "\n" )


  #### Link Header/Sub Note and Project Doc GDT ####

  if( addObjToHeader == TRUE ) { # add Doc link to HeaderNote if requested

    subNoteContents <- NULL # recoup memory - will open each subnote in link_group_note_doc()

    linkFormed <- link_group_note_doc(selection, settings, headerNoteRmdPath, headerNoteRmdContents,
                                      headerNoteLinkContents, projNoteLinkContents, projNoteLinkSummaryContents, todoContents,
                                      projNoteSummaryContents, subNoteSummaryContents, projDocContents, orgPath)
    # adds Doc Link to header and all its SubNotes

    if( linkFormed == FALSE ) {
      # return an error
      stop( paste0("  Linking Project Note Failed - link already exists."))
    }

  } else { # add Doc Link to the SubNote ONLY

    linkFormed <- link_project_note_doc(selection, settings, subNoteRmdPath, subNoteContents,
                                        projNoteLinkContents, projNoteLinkSummaryContents, todoContents,
                                        projNoteSummaryContents, projDocContents, orgPath)

    if( linkFormed == FALSE ) {
      # remove the project note and directory
      file.remove(projDirPath) # remove the project note dir
      file.remove(projNoteRmdPath) # remove the project note Rmd
      stop( paste0("  Creating Project Note Failed - link already exists."))
    }
  }
}


#' Add a New Sub Note to a Project Group
#'
#' This Function adds a Sub Note to a Project Group: The Sub Note is placed into
#' the Project Group Directory, links to the Sub Note are added to all Project
#' Doc(s) Goal-Del-Tasks linked to the Group Header Note, and a link to the Sub
#' Note is added to the project Group Header Note contents section.
#'
#' If a Project Doc is in the selection, a Project Note Group must be in the
#' selection  (`addingSubNote` must be TRUE), and all Project Doc links from the
#' Header Note  are added to the new SubNote.  If a Header Note is in the
#' selection, all Project Doc links from the Header Note are added to the new
#' SubNote.  If a SubNote is in the selection, all Project Doc links from the
#' SubNote are added to the new SubNote.
#'
#' @param subNoteName The name of the Project Sub Note, a Title with all SPACES
#' replaced with - or _ by default.
#'
#' @param subNotePath The directory where the Sub Note will be stored.  This will
#' be the Project Group Note Directory.  Must be an ABSOLUTE path!
#'
#' @param selection List containing `rmdType` that defines the current selection.
#' From this the `headerNoteRmdPath` is defined.  The function will deal with
#' SubNote generation with a valid selection of a DOC HEAD or SUB file.
#'
#' @param subNoteTitle OPTIONAL title for the Project SubNote.  Default is to use
#' subNoteName and replace all _ and - with SPACES.
#'
#' @param subNoteTemplate Template to use, as found in the `config/templates/`
#' directory.  Default is "Project-Sub-Note-Template.Rmd"
#'
#' @param projNoteLinkTemplate Template with structure to add the Project Doc
#' Goal/Del/Task link in the Project Note.  Includes Project Doc title, link,
#' and then links to Goal / Del / Task, plus a Summary Info section (filled
#' with the content from `taskSectionHeaderTemplate`).
#'
#' @param projNoteLinkSummaryTemplate Template with structure to add underneath the
#' Project Doc Goal/Del/Task link in the Project Note.  Includes a 'summary' section
#' and a 'todo' section by default, linked to `NoteObjectivesSummarySectionHeader`
#' & `NoteObjectivesTodoSectionHeader` in `config/settings.yml`
#'
#' @param projNoteSummaryTemplate Template with structure to add Project Note
#' summary to Project Doc under Goal/Del/Task.
#'
#' @param subNoteSummaryTemplate Template with structure to add Project Sub Note
#' summary to Project Doc under Goal/Del/Task.
#'
#' @export
create_sub_note <- function( subNoteName, subNotePath,
                             selection, subNoteTitle="",
                             subNoteTemplate="Project-Sub-Note-Template.Rmd",
                             headerNoteContentLinkTemplate="Project-Header-Note-Content-Link-Template.Rmd",
                             subNoteContentLinkTemplate="Project-Sub-Note-Content-Link-Template.Rmd",
                             projNoteLinkTemplate="Project-Note-Link-Template.Rmd",
                             projNoteLinkSummaryTemplate="Project-Note-Link-Summary-Template.Rmd",
                             todoTemplate="Todo-Template.Rmd",
                             projNoteSummaryTemplate="Project-Note-Summary-Template.Rmd",
                             subNoteSummaryTemplate="Project-Sub-Note-Summary-Template.Rmd" ) {


  cat( "\nprojectmanagr::create_sub_note():\n" )


  #### CHECK FOR ERRORS IN INPUT ####

  # Check subNoteName contains NO SPACES:
  if( grepl("\\s+", subNoteName) ) {
    stop( paste0("  subNoteName contains a SPACE: ", subNoteName) )
  }

  # Check valid selection - can handle DOC HEAD or SUB Rmd selections
  if( selection[["rmdType"]] != "DOC" && selection[["rmdType"]] != "HEAD" &&
      selection[["rmdType"]] != "SUB" ) {
    stop( paste0("  unsupported Rmd file type selected: ", selection[["rmdType"]],
                 " - ", selection[["filePath"]]) )
  }

  # Check valid DOC selection - addingSubNote must be TRUE
  if( selection[["rmdType"]] == "DOC" ) {
    if( selection[["addingSubNote"]] != TRUE ) {
      stop( paste0("  unsupported Project DOC selection - not selecting a project note group.") )
    }
  }


  #### Set Instance Variables ####

  # Check projectNotePath is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname( dirname(subNotePath) ) # this should be the orgPath!
  orgPath <- find_org_directory(orgPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  subNotePath is not in a sub-dir of a PROGRAMME Directory: ", subNotePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )

  # Check subNoteTitle, and if blank, fill with subNoteName, replacing all "_" and "-" with spaces
  if( nchar(subNoteTitle) == 0 ) {
    subNoteTitle <- gsub("-", " ", gsub("_", " ", subNoteName) )
  }

  # define headerNoteRmdPath & linkNoteRmdPath - depends on selection
  if( selection[["rmdType"]] == "DOC" ) {

    # extract header note relative path
    hnrp <- substr(selection[["headerNoteLink"]],
                   regexpr("](", selection[["headerNoteLink"]], fixed=TRUE)+2,
                   nchar(selection[["headerNoteLink"]]) )
    hnrp <- substr(hnrp, 1, regexpr(")", hnrp, fixed=TRUE)-1)
    # combine with filePath to get the full path
    headerNoteRmdPath <- R.utils::getAbsolutePath(
      paste0(dirname(selection[["filePath"]]), .Platform$file.sep, hnrp) )
    linkNoteRmdPath <- headerNoteRmdPath # add all links from header note

  } else if( selection[["rmdType"]] == "HEAD" ) {

    headerNoteRmdPath <- selection[["filePath"]] # filePath points to header note
    linkNoteRmdPath <- headerNoteRmdPath # add all links from header note

  } else if( selection[["rmdType"]] == "SUB" ) {

    headerNoteRmdPath <- find_header_Rmd_path(selection[["filePath"]], settings) # get header path from subnote path
    linkNoteRmdPath <- selection[["filePath"]] # add all links from selected subnote
  }

  headerNoteFileName <- basename(headerNoteRmdPath)
  subNotePath <- normalizePath(subNotePath)
  subNotePrefix <- get_next_subnote_prefix(subNotePath, settings)


  #### Read Rmds ####

  subNoteContents <- read_file( paste0( tempPath, .Platform$file.sep, subNoteTemplate) )

  headerNoteContentLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, headerNoteContentLinkTemplate) )
  subNoteContentLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, subNoteContentLinkTemplate) )

  projNoteLinkContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkTemplate) )
  projNoteLinkSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteLinkSummaryTemplate) )
  todoContents <- read_file( paste0( tempPath, .Platform$file.sep, todoTemplate) )

  projNoteSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, projNoteSummaryTemplate) )
  subNoteSummaryContents <- read_file( paste0( tempPath, .Platform$file.sep, subNoteSummaryTemplate) )

  headerNoteRmdContents <- read_file(headerNoteRmdPath)
  linkNoteRmdContents <- read_file(linkNoteRmdPath)


  #### Create Sub Note Dir ####

  subNoteDir <- paste0( subNotePath, .Platform$file.sep, subNotePrefix)
  done <- dir.create( subNoteDir )

  if(!done) {
    stop( paste0("  Sub Note directory could not be created: ", subNoteDir) )
  }

  cat( "  Made Project Sub Note dir: ", subNoteDir, "\n" )


  #### create SubNote Rmd file ####

  # Create blank RMD DOCUMENT:
  subNoteRmdPath <- paste0( subNotePath, .Platform$file.sep, subNotePrefix,
                            settings[["ProjectPrefixSep"]], subNoteName, ".Rmd")
  subNoteFileName <- basename(subNoteRmdPath)
  done <- file.create( subNoteRmdPath )

  if(!done) {
    file.remove(subNoteDir) # remove the sub note dir
    stop( paste0("  Project Sub Note could not be created: ", subNoteRmdPath) )
  }


  #### Replace markup in Sub Note with values ####

  # use username as author value
  authorValue <- Sys.info()["user"]

  # modify subNoteContents
  subNoteContents <-sub_subnote_params(subNoteContents, subNotePrefix,
                                       subNoteTitle, authorValue,
                                       settings, orgPath)


  #### insert header note content link into sub note ####

  subNoteContents <- insert_subnote_header_link(subNoteContents, headerNoteFileName,
                                                headerNoteRmdPath, subNoteRmdPath,
                                                headerNoteContentLinkContents,
                                                settings, orgPath)


  #### write Sub Note ####

  write_file(subNoteContents, subNoteRmdPath)

  cat( "  Made Sub Note: ", subNoteRmdPath, "\n" )


  #### insert sub note content link into header note ####

  subNoteContentLink <- create_hyperlink( subNoteFileName, subNoteRmdPath, headerNoteRmdPath)
  subNoteContentLinkContents <- sub_template_param(subNoteContentLinkContents,
                                                   "{{HEADER_NOTE_CONTENT_LINK}}",
                                                   subNoteContentLink, orgPath)

  noteContentsHeadIndex <- match_line_index( load_param_vector(settings[["HeaderNoteContentsHeader"]], orgPath),
                                           headerNoteRmdContents)
  noteContentsFootIndex <- grep_line_index_from( load_param_vector(settings[["HeaderNoteContentsFooter"]], orgPath),
                                                 headerNoteRmdContents, noteContentsHeadIndex)

  headerNoteRmdContents <- insert_at_indices(headerNoteRmdContents, noteContentsFootIndex, subNoteContentLinkContents)


  #### write Header Note ####

  write_file(headerNoteRmdContents, headerNoteRmdPath)

  cat( "  Edited Project Header Note: ", headerNoteRmdPath, "\n" )


  #### Link SubNote with all links in linkRmd ####

  link_sub_note_doc(selection, settings, subNoteRmdPath, subNoteContents,
                    headerNoteRmdPath, headerNoteRmdContents,
                    projNoteLinkContents, projNoteLinkSummaryContents, todoContents,
                    projNoteSummaryContents, subNoteSummaryContents,
                    linkNoteRmdPath, linkNoteRmdContents, orgPath)
}



#' Add a Protocol to a Project Note
#'
#' This Function adds a new Protocol to a Project Note - saved in the
#' PROGRAMMES' protocols Dir. The Protocol is formed using an Rmd template:
#' `Protocol-Template.Rmd` from the projectmanagr package. Protocols compile to
#' PDF as standard, and can therefore be used as independent files.
#'
#' Protocols are stored in the protocols directory inside the PROGRAMME
#' Directory (defined in projectmanagr settings.yml).  Each Protocol
#' exists in its own directory, to keep its compiled files together.
#'
#' The Protocol source Rmd will link to its creating Project Note at the originalLine
#' in the `selection` object, and the   Project Note will link to the compiled
#' PDF of the Protocol.
#'
#' Protocols can then be inserted into new Project Notes, using the
#' `insert_procotol()` function, which  allows insertion of preformed template
#' documentation into new Notes.
#'
#' @param projectNotePath The ABSOLUTE path of the Project Note.
#'
#' @param protocolName The name of the Protocol, a Title with all SPACES replaced
#' with - or _.
#'
#' @param selection List containing `rmdType` `filePath` & `originalLineNumber`,
#' indicating what Project Note is to document the added protocol, and what line
#' the protocol is to be linked to in the project note.  The function will deal
#' with protocol insertion into simple project notes and subnotes only.
#'
#' @param protocolTitle The title of the Protocol, by default the name with
#' all - and _ replaced with SPACES.
#'
#' @param protocolTemplate Template to use, as found in the `config/templates/`
#' directory.  Default is "Protocol-Template.Rmd"
#'
#' @export
create_protocol <- function(protocolName, selection, protocolTitle="",
                            protocolTemplate="Protocol-Template.Rmd") {

  cat( "\nprojectmanagr::create_protocol():\n" )


  #### Set Instance Variables ####

  projNoteRmdPath <- selection[["filePath"]]

  # get orgPath
  orgPath <- find_org_directory(projNoteRmdPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Cannot identify organisation directory: ", projNoteRmdPath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath - these names are FIXED:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # load settings file for user defined settings
  settingsFile <- paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )


  # If projectTitle blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(protocolTitle)==0 ) {
    protocolTitle <- gsub("-", " ", gsub("_", " ", protocolName) )
  }

  # define protocolDescription - name without suffix attached
  protocolDescription <- protocolName
  if( endsWith(  tolower(protocolDescription), tolower(settings[["ProtocolNameSuffix"]]) ) ) {
    protocolDescription <- substr(protocolDescription, 1, nchar(protocolDescription)-nchar(settings[["ProtocolNameSuffix"]]))
    protocolDescription = gsub("-", " ", gsub("_", " ", protocolDescription) )
  }

  # get the progPath:
  progPath <- find_prog_dir(projNoteRmdPath, settings)

  # get protocol directory path:
  protocolsPath <- paste0(progPath, .Platform$file.sep, settings[["ProgrammeProtocolsDir"]])

  # define protocol Dir path + Rmd path
  protocolDirPath <- paste0( protocolsPath, .Platform$file.sep, protocolName)
  protocolRmdPath <- paste0( protocolDirPath, .Platform$file.sep, protocolName, ".Rmd")


  #### CHECK FOR ERRORS IN INPUT ####

  # Check protocolName contains NO SPACES:
  if( grepl("\\s+", protocolName) ) {
    stop( paste0("  protocolName contains a SPACE: ", protocolName) )
  }

  # Check protocolName end with ProtocolNameSuffix
  if( endsWith(  protocolName, settings[["ProtocolNameSuffix"]] ) == FALSE ) {
    stop( paste0("  protocolName does not end with designated suffix: ", protocolName,
                 " settings:ProtocolNameSuffix: ", settings[["ProtocolNameSuffix"]]) )
  }

  # check selection is a Project Note - simple or sub
  if( selection[["rmdType"]] != "NOTE" && selection[["rmdType"]] != "SUB" ) {
    stop( paste0("  selection is not a suitable Project Note: ", selection[["filePath"]]) )
  }

  # Check protocolRmdPath doesnt exist
  if( file.exists(protocolRmdPath) == TRUE ) {
    stop( paste0("  protocol of this name already exists: ", protocolRmdPath) )
  }


  #### Read Rmds ####

  protocolContents <- read_file( paste0( tempPath, .Platform$file.sep, protocolTemplate) )
  projNoteRmdContents <- read_file(projNoteRmdPath)


  #### Create Protocol Dir ####

  done <- dir.create( protocolDirPath )

  if(!done) {
    stop( paste0("  DIR for Protocol could not be created: ", protocolDirPath) )
  }

  cat( "  Made Protocol DIR: ", protocolDirPath, "\n" )


  #### create Rmd file ####

  # Create blank RMD DOCUMENT:
  done <- file.create( protocolRmdPath )

  if(!done) {
    file.remove(protocolDirPath) # remove the Protocol dir
    stop( paste0("  Protocol could not be created: ", protocolRmdPath) )
  }


  #### Replace markup in Note with values ####

  # use username as author value
  authorValue <- Sys.info()["user"]

  # modify protocolContents to include PREFIX and projectTitle
  protocolContents <- sub_template_param(protocolContents, "{{TITLE}}",
                                         protocolTitle, orgPath)
  protocolContents <- sub_template_param(protocolContents, "{{AUTHOR}}",
                                         authorValue, orgPath)

  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_SUMMARY_HEADER}}",
                                         settings[["ProtocolSummaryHeader"]], orgPath)
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_SUMMARY_FOOTER}}",
                                         settings[["ProtocolSummaryFooter"]], orgPath)

  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_GRAPHICAL_ABSTRACT_HEADER}}",
                                         settings[["ProtocolGraphicalAbstractHeader"]], orgPath)
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_GRAPHICAL_ABSTRACT_FOOTER}}",
                                         settings[["ProtocolGraphicalAbstractFooter"]], orgPath)

  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_BACKGROUND_HEADER}}",
                                         settings[["ProtocolBackgroundHeader"]], orgPath)
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_BACKGROUND_FOOTER}}",
                                         settings[["ProtocolBackgroundFooter"]], orgPath)

  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_MATERIALS_HEADER}}",
                                         settings[["ProtocolMaterialsHeader"]], orgPath)
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_MATERIALS_FOOTER}}",
                                         settings[["ProtocolMaterialsFooter"]], orgPath)

  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_HEADER}}",
                                         settings[["ProtocolHeader"]], orgPath)

  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_SOP_HEADER}}",
                                         settings[["ProtocolSopHeader"]], orgPath)

  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_LOG_HEADER}}",
                                         settings[["ProtocolLogHeader"]], orgPath)
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_LOG_SEP}}",
                                         settings[["ProtocolLogSep"]], orgPath)

  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_FOOTER}}",
                                         settings[["ProtocolFooter"]], orgPath)

  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_RESULTS_LOG_HEADER}}",
                                         settings[["ProtocolResultsLogHeader"]], orgPath)
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_RESULTS_LOG_FOOTER}}",
                                         settings[["ProtocolResultsLogFooter"]], orgPath)

  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_TROUBLESHOOTING_HEADER}}",
                                         settings[["ProtocolTroubleshootingHeader"]], orgPath)
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_TROUBLESHOOTING_FOOTER}}",
                                         settings[["ProtocolTroubleshootingFooter"]], orgPath)


  # modify protocolContents with rmarkdown-html-header content
  protocolContents <- replace_markdown_header(protocolContents, orgPath,
                                              htmlMarkdown="{{HTML_HEADER_PROTOCOL}}",
                                              htmlHeaderFilename="rmarkdown-html-header-protocol.txt")

  # modify protocolContents with SEP values
  protocolContents <- replace_sep_values(protocolContents, orgPath)


  # modify _PROTOCOL_ with protocolDescription
  protocolContents <- sub_template_param(protocolContents, "_PROTOCOL_", protocolDescription, orgPath)

  # modify svg files with names
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_GRAPHICAL_ABSTRACT_SVG}}",
                                         settings[["ProtocolGraphicalAbstractSvg"]], orgPath)
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_EQUIPMENT_SVG}}",
                                         settings[["ProtocolEquipmentSvg"]], orgPath)
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_PROCEDURE_TEMPLATE_SVG}}",
                                         settings[["ProtocolProcedureTemplateSvg"]], orgPath)


  #### Link Protocol and Project Note ####

  # create link from protocol to project note
  projNoteName <- substr(basename(projNoteRmdPath), 1, regexpr(".Rmd", basename(projNoteRmdPath))-1)
  projNoteLink <- paste0(settings[["ProtocolLinkFormat"]],
                         create_hyperlink( projNoteName, projNoteRmdPath, protocolRmdPath),
                         settings[["ProtocolLinkFormat"]])

  # insert link into protocol
  protocolContents <- sub_template_param(protocolContents, "{{PROTOCOL_PROJECT_NOTE_LINK}}",
                                         projNoteLink, orgPath)

  # create link from project note to protocol
  protocolLink <- paste0(settings[["ProtocolLinkFormat"]],
                         create_hyperlink( protocolName, protocolRmdPath, projNoteRmdPath),
                         settings[["ProtocolLinkFormat"]])

  # insert link into project note - replacing the original line with link
  projNoteRmdContents <- replace_at_indices(projNoteRmdContents, selection[["originalLineNumber"]], protocolLink)


  #### write Protocol ####

  write_file(protocolContents, protocolRmdPath)

  cat( "  Made Protocol: ", protocolRmdPath, "\n" )


  #### add resources ####

  # graphical abstract svg
  graphicalAbstractSvg <- settings[['ProtocolGraphicalAbstractSvg']]
  graphicalAbstractSvgTemplate <- paste0( tempPath, .Platform$file.sep, graphicalAbstractSvg )
  graphicalAbstractSvgProtocol <- paste0( protocolDirPath, .Platform$file.sep, graphicalAbstractSvg )

  done <- file.copy(graphicalAbstractSvgTemplate, graphicalAbstractSvgProtocol)
  if(!done) {
    stop( paste0("  Failed to copy ",graphicalAbstractSvg,": ", graphicalAbstractSvgTemplate) )
  }
  cat( "  Copied ",graphicalAbstractSvg," \n" )

  # equipment svg
  equipmentSvg <- settings[['ProtocolEquipmentSvg']]
  equipmentSvgTemplate <- paste0( tempPath, .Platform$file.sep, equipmentSvg )
  equipmentSvgProtocol <- paste0( protocolDirPath, .Platform$file.sep, equipmentSvg )

  done <- file.copy(equipmentSvgTemplate, equipmentSvgProtocol)
  if(!done) {
    stop( paste0("  Failed to copy ",equipmentSvg,": ", equipmentSvgTemplate) )
  }
  cat( "  Copied ",equipmentSvg," \n" )

  # procedure svg
  procedureSvg <- settings[['ProtocolProcedureTemplateSvg']]
  procedureSvgTemplate <- paste0( tempPath, .Platform$file.sep, procedureSvg )
  procedureSvgProtocol <- paste0( protocolDirPath, .Platform$file.sep, procedureSvg )

  done <- file.copy(procedureSvgTemplate, procedureSvgProtocol)
  if(!done) {
    stop( paste0("  Failed to copy ",procedureSvg,": ", procedureSvgTemplate) )
  }
  cat( "  Copied ",procedureSvg," \n" )


  #### write Project Note ####

  write_file(projNoteRmdContents, projNoteRmdPath)

  cat( "  written Protocol link to Project Note: ", projNoteRmdPath, "\n" )

}




