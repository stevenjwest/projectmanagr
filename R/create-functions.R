#' Create a New Project Organisation
#'
#' Generates the layout for a new Project Organisation in the File System.
#'
#' The function generates the organisation directory and its initial contents:
#'
#' * `index_<orgName>.Rmd` the landing page for the organisation.
#'
#' * `.config/` directory with the config files and templates/ directory containing
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
#' @param orgParentPath defines where the organisation directory is
#' written, Default is the working directory.
#'
#' @param orgName defines the directory name of the Organisation.
#'
#' @param orgTitle defines the title of the Organisation, in its document.
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
#' @param utime Add update time to write to status.yml - for automated testing.
#'
#' @return orgIndexPath The path to the organisation index Rmd file.
#'
#' @export
create_project_org <- function( orgParentPath, orgName, orgTitle="",
                              settingsYamlPath="", orgTemplate="Org-Template.Rmd",
                              utime="") {


  cat( "\nprojectmanagr::create_project_org():\n" )


  #### check ARGS ####

  # Check orgTitle, and if blank, fill with orgName, replacing all "_" and "-" with spaces & trim whitespace
  if( nchar(orgTitle)==0 ) {
    orgTitle <- trimws( gsub("-", " ", gsub("_", " ", orgName) ) )
  }

  # check location of installed projectmanagr package - to retrieve files for org install
  # lib.loc ensures correct lib path is returned during testing
  projectmanagrPath <- find.package("projectmanagr", lib.loc = .libPaths())

  #### Create Organisation Dir ####

  orgPath <- paste0(orgParentPath, .Platform$file.sep, orgName)

  # get all org paths (including this one) from the parent dir of current orgPath
  orgPaths <- get_org_paths(orgPath) # this also updates the existing org configs to be aware of this new org!
   # this ensures all organisations (at least those written to parent DIR of this new ORG) are aware of each other
   # thus can update links across organisations

  # create the fileSystem layout:
  done <- dir.create( orgPath )

  orgPath <- normalizePath(orgPath)

  if(!done) {
    stop( paste0("  Organisation directory could not be created: ", orgPath) )
  }

  cat( "  Made ORG dir: ",orgPath, "\n" )


  #### Load Settings YAML file ####

  # The default is loaded from the package OR function points to settingsYamlPath yaml file which is used
  if( settingsYamlPath == "" ) {

    # no settingsYamlFile supplied - so load the DEFAULT settingsYamlFile from projectmanagr
    settingsYamlFile <- paste0( projectmanagrPath,
                                .Platform$file.sep, "config",
                                .Platform$file.sep, "settings.yml")

    settings <- yaml::read_yaml( settingsYamlFile ) # assumes encoding is UTF-8
    # to load other files - run yaml::yaml.load( yaml::read_yaml(path))

  } else {

    # settingsYamlFile supplied - load this and CHECK its valid
    settingsYamlFile <- settingsYamlPath

    settings <- yaml::read_yaml( settingsYamlFile )# assumes encoding is UTF-8

    # CHECK YAML is valid - must contain all named elements
    #VolumesDir DocsDir ConfigDir OrgIndexFileName
    # form list with: cat(paste0('"', names(settings), '" %in% names(settings) &&'), sep='\n')
    valid <- { "SiteDir" %in% names(settings) &&
        "FileType" %in% names(settings) &&
        "VolumesDir" %in% names(settings) &&
        "VolumesFile" %in% names(settings) &&
        "ConfigStatusYamlFile" %in% names(settings) &&
        "OrgIndexFileNamePrefix" %in% names(settings) &&
        "OrgProgrammeHeader" %in% names(settings) &&
        "OrgProgrammeFooter" %in% names(settings) &&
        "OrgProgrammeSummarySep" %in% names(settings) &&
        "ProgrammeProjectsDir" %in% names(settings) &&
        "ProgIndexFileNamePrefix" %in% names(settings) &&
        "ProgSummaryHeader" %in% names(settings) &&
        "ProgSummaryTitle" %in% names(settings) &&
        "ProgSummaryFooter" %in% names(settings) &&
        "ProgProjectsHeader" %in% names(settings) &&
        "ProgProjectsFooter" %in% names(settings) &&
        "ProgProjectSummarySep" %in% names(settings) &&
        "ProjectPrefixSep" %in% names(settings) &&
        "ProjectIdentifierSep" %in% names(settings) &&
        "ProjectIndexSep" %in% names(settings) &&
        "ProjectSummaryHeader" %in% names(settings) &&
        "ProjectSummaryTitle" %in% names(settings) &&
        "ProjectSummaryFooter" %in% names(settings) &&
        "ProjectGoalHeader" %in% names(settings) &&
        "ProjectGoalTitle" %in% names(settings) &&
        "ProjectGoalDivider" %in% names(settings) &&
        "ProjectGoalSep" %in% names(settings) &&
        "ProjectDeliverableHeader" %in% names(settings) &&
        "ProjectDeliverableTitle" %in% names(settings) &&
        "ProjectDeliverableDivider" %in% names(settings) &&
        "ProjectDeliverableSep" %in% names(settings) &&
        "ProjectTaskHeader" %in% names(settings) &&
        "ProjectTaskTitle" %in% names(settings) &&
        "ProjectTaskDivider" %in% names(settings) &&
        "ProjectTaskSep" %in% names(settings) &&
        "ProjectTaskFooter" %in% names(settings) &&
        "ProjectLinkFormat" %in% names(settings) &&
        "ProjectTaskLogHeader" %in% names(settings) &&
        "ProjectTaskLogSep" %in% names(settings) &&
        "NoteObjectivesSummarySectionHeader" %in% names(settings) &&
        "NoteObjectivesTodoSectionHeader" %in% names(settings) &&
        "NoteObjectivesHeader" %in% names(settings) &&
        "NoteObjectivesSep" %in% names(settings) &&
        "NoteObjectivesFooter" %in% names(settings) &&
        "NoteStorageHeader" %in% names(settings) &&
        "NoteStorageFooter" %in% names(settings) &&
        "NoteLinkFormat" %in% names(settings) &&
        "GroupNotePrefixSep" %in% names(settings) &&
        "HeaderNotePrefix" %in% names(settings) &&
        "HeaderNoteContentsHeader" %in% names(settings) &&
        "HeaderNoteContentsFooter" %in% names(settings) &&
        "HeaderLinkFormat" %in% names(settings) &&
        "SubNotePrefixSep" %in% names(settings) &&
        "SubNoteContentsHeader" %in% names(settings) &&
        "SubNoteContentsFooter" %in% names(settings) &&
        "SubNoteLinkFormat" %in% names(settings) &&
        "NoteSummaryTitle" %in% names(settings) &&
        "NoteGoalLinkLine" %in% names(settings) &&
        "NoteDeliverableLinkLine" %in% names(settings) &&
        "NoteTaskLinkLine" %in% names(settings) &&
        "ContentTitleField" %in% names(settings) &&
        "ContentDescriptionField" %in% names(settings) &&
        "ContentSourceField" %in% names(settings) &&
        "ContentSep" %in% names(settings) &&
        "ContentInsertionTitle" %in% names(settings) &&
        "ContentInsertionSep" %in% names(settings) &&
        "ContentLinkFormat" %in% names(settings) &&
        "ContentSummaryHeader" %in% names(settings) &&
        "ContentSummaryFooter" %in% names(settings) &&
        "ContentGraphicalAbstractHeader" %in% names(settings) &&
        "ContentGraphicalAbstractSvg" %in% names(settings) &&
        "ContentGraphicalAbstractFooter" %in% names(settings) &&
        "ContentBackgroundHeader" %in% names(settings) &&
        "ContentBackgroundFooter" %in% names(settings) &&
        "ContentMaterialsHeader" %in% names(settings) &&
        "ContentMaterialsFooter" %in% names(settings) &&
        "ContentEquipmentSvg" %in% names(settings) &&
        "ContentProcedureTemplateSvg" %in% names(settings) &&
        "ContentResultsLogHeader" %in% names(settings) &&
        "ContentResultsLogFooter" %in% names(settings) &&
        "ContentTroubleshootingHeader" %in% names(settings) &&
        "ContentTroubleshootingFooter" %in% names(settings) &&
        "ContentSopHeader" %in% names(settings) &&
        "ContentLogHeader" %in% names(settings) &&
        "ContentLogSep" %in% names(settings) &&
        "ContentFooter" %in% names(settings) &&
        "TodoItemHeaderTemplate" %in% names(settings) &&
        "TodoItemHeaderComplete" %in% names(settings) &&
        "TodoItemHeader" %in% names(settings) &&
        "TodoCollectionDir" %in% names(settings) &&
        "TodoProgrammeSep" %in% names(settings) &&
        "DateTimeZone" %in% names(settings) &&
        "DateSplit" %in% names(settings) &&
        "DateTimeSplit" %in% names(settings) &&
        "RunUpdateOnStartup" %in% names(settings) &&
        "RunCompileWithUpdate" %in% names(settings) &&
        "FileTypeSuffix" %in% names(settings) &&
        "WeeklyJournalDir" %in% names(settings) &&
        "GadgetWidth" %in% names(settings) &&
        "GadgetHeight" %in% names(settings) &&
        "rstudioInternalStateDir" %in% names(settings) }

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
  volumesPackageFile <- paste( projectmanagrPath, .Platform$file.sep,
                               "volumes", .Platform$file.sep, "volumes.Rmd", sep="")

  done <- file.copy(volumesPackageFile, volumesFile)

  if(!done) {
    stop( paste0("  Volumes file could not be copied: ", volumesPackageFile, " ", volumesFile) )
  }
  cat( "  Copied volumes file: ", volumesFile, "\n" )



  #### Create org .config/ dir ####

  # Contains configuration information for Organisation

  # config dir: FIXED NAME
  confPath <- get_config_dir(orgPath)
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
  statusFile <- get_status_yml_file(orgPath, settings)
  done <- file.create(statusFile)

  if(!done) {
    stop( paste0("  Status file could not be created: ", statusFile) )
  }

  cat( "  Made status file: ", statusFile, "\n" )


  # Create initial content for status.yml file - data on the Org, plus UPDATE datetime:
  if( utime == "") {
    updateTime <- file.info(statusFile)[,5]
  } else if( is.na(lubridate::ymd_hm(utime)) == FALSE ) {
    updateTime <- lubridate::ymd_hm(utime)
  } else {
    updateTime <- file.info(statusFile)[,5]
  }
  org <- list(orgPaths, orgPath, orgName, orgTitle, as.character(updateTime), sitePath )
   # orgPaths contains all EXISTING ORGs and THIS ORG path at end
  names(org) <- c("orgPaths", "orgPath", "orgName", "orgTitle", "updateTime", "sitePath")
  yaml::write_yaml( yaml::as.yaml(org), statusFile )


  #### Copy addins.json to .config/ dir ####

  # no settingsYamlFile supplied - so load the DEFAULT settingsYamlFile from projectmanagr
  addinsJsonPackageFile <- paste0( projectmanagrPath, .Platform$file.sep,
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
  # get config templates settings yml
  tempPath <- get_template_dir(orgPath)
  done <- dir.create( tempPath )

  if(!done) {
    stop( paste0("  Templates directory could not be created: ", tempPath) )
  }

  cat( "  Made templates dir: ",tempPath, "\n" )

  # copy template files:
    # need to copy from the PACKAGE!
  templateDir <- paste0( projectmanagrPath, .Platform$file.sep, "templates", .Platform$file.sep)
  templateRmds <- list.files(templateDir)
  for(f in templateRmds) {
    done <- file.copy( paste0(templateDir, f), tempPath)
    if(!done) {
      stop( paste0("  Failed with Template: ", f) )
    }
    cat( "  Copied template: ",f, "\n" )
  }


  #### Create ORG Rmd file ####

  # first remove any preceding '-' or '_' from orgName
  orgIndexName <- orgName
  #if( startsWith(orgIndexName, "_") | startsWith(orgIndexName, "-") ){
  #  orgIndexName <- sub('.', '', orgIndexName)
  #}
  # and remove any following '-' or '_' from orgName
  #if( endsWith(orgIndexName, "_") | endsWith(orgIndexName, "-") ){
  #  orgIndexName <- gsub('.{1}$', '', orgIndexName)
  #}

  orgIndexName <- paste0(settings[["OrgIndexFileNamePrefix"]], orgIndexName, ".Rmd")
  # use the orgPath - prefix with OrgIndexFileNamePrefix then ORG NAME then .Rmd
  orgFile <- paste0(orgPath, .Platform$file.sep, orgIndexName)
  done <- file.create(orgFile)

  if(!done) {
    stop( paste0("  Org file could not be created: ", orgFile) )
  }

  cat( "  Made Org file: ",orgFile, "\n" )


  # read org template:
  templateContents <- read_file( paste0( tempPath, .Platform$file.sep, orgTemplate) )

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

  # return orgIndexPath - orgFile
  orgFile

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

  # add the orgPaths into each ORGs .config/status.yml file
  for (i in 1:length(dirs) ) {

    # get config templates settings yml
    confPath <- get_config_dir(dirs[i])
    tempPath <- get_template_dir(dirs[i])
    settings <- get_settings_yml(dirs[i])

    # get status yml
    status <- get_status_yml(dirs[i], settings)
    statusFile <- get_status_yml_file(dirs[i], settings)

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
#' Generates a new Programme in an Organisation.  Generates a directory for the
#' programme, and an initial `_index_` .Rmd file that defines the programme
#' homepage.
#'
#' If `organisationPath` is not at the top of the Organisation, will traverse until
#' it is found.  This function will halt if no Organisation is found - the
#' organisation root is defined by the presence of .config/ and .config/templates
#' dirs at root. An optional programmeTitle (which if not supplied will default
#' to the programmeName, replacing "_" & "-" with " ").
#'
#' @param programmeName Name of Progamme - must NOT contain a space.
#'
#' @param organisationPath Path to insert the PROGRAMME into.  If this is not an
#' Organisation Directory, will search up the directory tree to attempt to find
#' one.  If none found, the method will end without making a PROGRAMME.
#'
#' @param programmeTitle Title of programme - typically the programmeName with
#' "_" & "-" replaced with spaces.
#'
#' @param progTemplate The Rmd file to use as a template to create the Programme.
#' Default is "Programme-Template.Rmd" from projectmanagr templates.
#'
#' @param progSummaryTemplate The Rmd file to use as a template to create the
#' Programme Summary. Default is "Programme-Summary-Template.Rmd" from
#' projectmanagr templates.
#'
#' @param ctime Add creation time to write to status.yml - for automated testing.
#'
#' @return progIndexPath The path to the programme index Rmd file.

#'
#' @export
create_programme <- function(programmeName, organisationPath,
                             programmeTitle="",
                             progTemplate="Programme-Template.Rmd",
                             progSummaryTemplate = "Programme-Summary-Template.Rmd",
                             ctime = "") {

  cat( "\nprojectmanagr::create_programme():\n" )

  # check programmeName contains NO SPACES:
  if( grepl("\\s+", programmeName) ) {
    stop( paste0("  programmeName contains a SPACE: ", programmeName) )
  }

  #### Identify root of ORGANISATION ####

  # get the orgPath from organisationPath
  orgPath <- find_org_directory(organisationPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  organisationPath is not in an Organisation: ", organisationPath) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get status yml
  statusFile <- get_status_yml_file(orgPath, settings)
  status <- get_status_yml(orgPath, settings)


  #### Create Programme Dir ####

  # create PROG Dir:
  progPath <- paste(orgPath, .Platform$file.sep, programmeName, sep="")
  done <- dir.create(progPath)

  if(!done) {
    stop( paste0("  Programme directory could not be created: ", progPath) )
  }

  cat( "  Made Programme dir: ", progPath, "\n" )


  #### create Rmd file ####

  # first remove any preceding '-' or '_' from orgName
  programmeNameIndex <- programmeName
  #if( startsWith(programmeNameIndex, "_") | startsWith(programmeNameIndex, "-") ){
  #  programmeNameIndex <- sub('.', '', programmeNameIndex)
  #}
  # and remove any following '-' or '_' from orgName
  #if( endsWith(programmeNameIndex, "_") | endsWith(programmeNameIndex, "-") ){
  #  programmeNameIndex <- gsub('.{1}$', '', programmeNameIndex)
  #}

  progFilePath <- paste0(progPath, .Platform$file.sep, settings[["ProgIndexFileNamePrefix"]],
                         programmeNameIndex, ".Rmd")
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

  # add title and creation time under the programmeName in the "PROGRAMMES" section of the status.yml List:
  if( ctime == "") {
    cTimeVal <- as.character(lubridate::ymd_hm(substr(as.character(file.info(progFilePath)[['ctime']]),1,16) ))
  } else if( is.na(lubridate::ymd_hm(ctime)) == FALSE ) {
    cTimeVal <- as.character(lubridate::ymd_hm(ctime))
  } else {
    cTimeVal <- as.character(lubridate::ymd_hm(substr(as.character(file.info(progFilePath)[['ctime']]),1,16) ))
  }
  attrs <- list(programmeTitle, cTimeVal )
  names(attrs) <- c("programmeTitle", "creationTime")
  status[["PROGRAMMES"]][[programmeName]] <- attrs
  # can retrieve the creationTime with call to:  status[["PROGRAMMES"]][[programmeName]][["creationTime"]]

  # Write status list to the statusFile:
  yaml::write_yaml(yaml::as.yaml(status), statusFile)

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

  # return progIndexPath - progFilePath
  progFilePath

}


#' Create a New Project Document
#'
#' Generates a new Project inside a Programme in an Organisation.  The projectParentPath
#' must be in the Programme directory, which is the sub-Dir to the root ORGANISATION
#' dir.
#'
#' The project is automatically numbered, by reading the current projects and determining
#' the next number in the sequence, as well as deriving the Programme Prefix.
#'
#' User must supply the project name, which must contain NO SPACES, and optionally a project
#' title.  If no title is provided, the Title is derived from the project name, replacing any
#' "_" & "-" with " ". The default projectParentPath is the working directory.
#'
#' @param projectPrefix Prefix of Project - must contain only alphanumeric chars & be unique to Programme.
#'
#' @param projectName Name of Project - must NOT contain a space.
#'
#' @param projectTitle Title of project - by default set to `projectName` with "_" & "-" replaced with spaces.
#'
#' @param projectParentPath Path to insert the Project into.  This must be a Programme dir, one level below the
#' organisation, and containing a PROJECTS/ directory.  The Project will be placed into the PROJECTS/ directory.
#' If none found, the method will end without making a Project
#'
#' @param projDocTemplate Rmd template used to create the Project Doc - default is "Project-Doc-Template.Rmd"
#'
#' @param projectIndex If 0, the function will determine the projectIndex by searching the PROJECTS/ directory.
#' Otherwise, the projectIndex is used to number the Project in its Prefix.
#'
#' @return projectDocRmd The path to the project docRmd file.
#'
#' @export
create_project_doc <- function(projectPrefix, projectName, projectParentPath, projectTitle="",
                             projDocTemplate="Project-Doc-Template.Rmd",
                             projDocSummaryTemplate="Project-Doc-Summary-Template.Rmd",
                             projectIndex=0 ) {

  cat( "\nprojectmanagr::create_project_doc():\n" )


  #### instance variables ####

  # check projectName contains NO SPACES:
  if( grepl("\\s+", projectName) ) {
    stop( paste0("  projectName contains a SPACE: ", projectName) )
  }


  # get the orgPath from projectParentPath - confirmed above to sit in an organisation!
  orgPath <- find_org_directory(projectParentPath)

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get programme path
  progPath <- check_prog_subdir(projectParentPath, settings)

  # extract the PROGRAMME NAME from the progPaths:
  programmeName <-basename(progPath)

  # define project dir - use prefix as dir name
  projDir <- paste0(projectParentPath, .Platform$file.sep, projectPrefix)

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(projectTitle)==0 ) {
    projectTitle <- gsub("-", " ", gsub("_", " ", projectName) )
  }


  #### ERROR CHECKING ####

  # Check projectParentPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  if(  progPath == ""  ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectParentPath is not inside a PROGRAMME sub-dir!
    stop( paste0("  projectParentPath is not withinin a PROGRAMME Directory: ", projectParentPath) )
  }

  # projectParentPath is therefore in a PROGRAMME DIR

  # Check projectPrefix is alphanumeric (no punctuation chars) and unique to programme
  if( grepl('[[:punct:]]', projectPrefix) == TRUE ) {
    stop( paste0("  projectPrefix contains non-alphanumeric characters: ", projectPrefix) )
  }

  if( file.exists(projDir) == TRUE ) {
    stop( paste0("  projectPrefix already used in directory: ", projDir) )
  }


  #### Create Project Dir ####

  done <- dir.create(projDir)

  if(!done) {
    stop( paste0("  Project directory could not be created: ", projDir) )
  }

  cat( "  Made Project dir: ",projDir, "\n" )


  #### create Rmd file ####

  projDocFilePath <- paste0(projectParentPath, .Platform$file.sep, projectPrefix,
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
  projDocContents <- gsub("{{PREFIX}}", projectPrefix, projDocContents, fixed=TRUE)
  projDocContents <- gsub("{{TITLE}}", projectTitle, projDocContents, fixed=TRUE)
  projDocContents <- gsub("{{AUTHOR}}", authorValue, projDocContents, fixed=TRUE)

  # modify programme summary header/footer
  projDocContents <- sub_template_param(projDocContents, "{{SUMMARY_HEADER}}",
                                        settings[["ProjectSummaryHeader"]], orgPath)
  projDocContents <- sub_template_param(projDocContents, "{{SUMMARY_FOOTER}}",
                                        settings[["ProjectSummaryFooter"]], orgPath)

  # write correct programme link
  progFilePath <- fs::path( progPath,
                            paste0(settings[["ProgIndexFileNamePrefix"]], programmeName, ".Rmd") )
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

  # return projDocFilePath
  projDocFilePath

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
#' @param projNoteTemplate Template to use, as found in the `.config/templates/`
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
#' & `NoteObjectivesTodoSectionHeader` in `.config/settings.yml`
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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

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
#' @param projNoteTemplate Template to use, as found in the `.config/templates/`
#' directory.  Default is "Project-Header-Note-Template.Rmd"
#'
#' @param subNoteTemplate Template to use, as found in the `.config/templates/`
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
#' & `NoteObjectivesTodoSectionHeader` in `.config/settings.yml`
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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

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

  subNoteContents <- insert_header_link_subnote(subNoteContents, headerNoteFileName,
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
#' @param subNoteTemplate Template to use, as found in the `.config/templates/`
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
#' & `NoteObjectivesTodoSectionHeader` in `.config/settings.yml`
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

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

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


  #### insert header link into sub note ####

  subNoteContents <- insert_header_link_subnote(
                        subNoteContents, headerNoteFileName,
                        headerNoteRmdPath, subNoteRmdPath,
                        headerNoteContentLinkContents, settings, orgPath)


  #### write Sub Note Contents ####

  write_file(subNoteContents, subNoteRmdPath)

  cat( "  Made Sub Note: ", subNoteRmdPath, "\n" )


  #### insert sub note content link into header note ####

  headerNoteRmdContents <- insert_subnote_link_header(
                              headerNoteRmdContents, subNoteFileName,
                              subNoteRmdPath, headerNoteRmdPath,
                              subNoteContentLinkContents, settings, orgPath)


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



#' Create Insertable Content in a Project Note
#'
#' This Function declares new Insertable Content, based on the
#' `contentDeclarationTemplate`, which is inserted into selected Project Note.
#' This contains key parameters to declare Insertable Content:
#'
#' * `CONTENT_NAME` specifies a name for the content, which can be used to select
#'  content to be inserted by the `insert_content()` function.
#'
#' * `CONTENT_DESCRIPTION` gives a succinct description of the insertable content
#'  what it contains & how it can be used.
#'
#' * `CONTENT_SOURCE_LINK` relative path to insertable content Rmd from the content
#' source Project Note Rmd file.
#'
#' The actual insertable content is located in a separate Rmd, pointed to by the
#' CONTENT_SOURCE_LINK parameter.  This is initially a blank Rmd document by
#' default, that should be filled with the content to be inserted, including all
#' whitespace.  The `contentSourceTemplate` can be modified to any existing Rmd
#' file, to provide a base parameterised template for consistent generation of
#' specific types of documentation (eg. suggested layouts, specific header
#' ordering, etc).
#'
#' Content is any templated insertable text.  Typically in ProjectManager it is
#' used to define a set of Standard Operating Procedures (SOPs) and/or LOG
#' Sections that record Protocol execution.
#'
#' The Content Declaration in a source Project Note is defined
#' between specific delimiters; these delimiters are defined in `CONTENT_SEP.txt`.
#'
#' Content defined in a source Project Note can be inserted into new Project
#' Notes, using the `insert_content()` function.
#'
#' @param selection projectmanagr selection object indicating the type of file
#' currently selected.  The current file must be a Project Note.
#'
#' @param contentName The name of the Content; a String that should
#' not contain any SPACES. For Protocols RECOMMEND using a VERB-DRIVEN Naming
#' Convention && use the common verb words first. eg. Fix Perfuse Mouse,
#' Fix Fog Drosophila.
#'
#' @param contentDescription Description of the protocol - shown to users when
#' choosing a protocol to insert.
#'
#' @param contentSourcePath Path to where the content Dir & Rmd should be stored.
#' This can be any location on the filesystem, but RECOMMEND placing it within
#' the project note's directory where the content declaration link will be written.
#'
#' @param projectNoteTitle OPTIONAL title for the Content.  Default is a blank
#' string (`""`), in which case the function will use contentName and replace
#' all `_` and `-` with SPACES.
#'
#' @param contentDeclarationTemplate Template used for content declaration,
#' found in the `.config/templates/`directory.  Default is
#' "Protocol-Declaration-Template.Rmd"
#'
#' @param contentSourceTemplate Template used for source of insertable text,
#' found in the `.config/templates/` directory.  Default is
#' "Protocol-Source-Template.Rmd", a blank Rmd file.
#'
#' @export
create_content <- function(selection, contentName, contentDescription,
                           contentSourcePath, contentTitle="",
                           contentDeclarationTemplate="Content-Declaration-Template.Rmd",
                           contentSourceTemplate="Content-Source-Template.Rmd") {

  cat( "\nprojectmanagr::create_content():\n" )


  #### Set Instance Variables & Check for Errors ####

  projNoteRmdPath <- selection[["filePath"]] # should be project note Rmd
  noteInsertionIndex <- selection[["originalLineNumber"]]

  # content location: contentDir basename is contentName
  contentDir <- fs::path(contentSourcePath, contentName)
  # contentRmd is in contentDir and basename is also contentName
  contentRmd <- fs::path(contentDir, paste0(contentName, ".Rmd"))

  # relative link FROM ProjNoteRmdPath TO contentRmd
  contentLink <- create_hyperlink(basename(contentRmd), contentRmd, projNoteRmdPath)

  # get orgPath
  orgPath <- find_org_directory(projNoteRmdPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  Cannot identify organisation directory: ", projNoteRmdPath) )
  }
  # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # check selection is a Project Note - simple or sub
  if( selection[["rmdType"]] != "NOTE" && selection[["rmdType"]] != "SUB" ) {
    stop( paste0("  selection is not a suitable Project Note: ", selection[["filePath"]]) )
  }

  # Check contentName contains NO SPACES:
  if( grepl("\\s+", contentName) ) {
    stop( paste0("  contentName contains a SPACE: ", contentName) )
  }

  # Check contentTitle, and if blank, fill with contentName, replacing all "_" and "-" with spaces
  if( nchar(contentTitle)==0 ) {
    contentTitle <- gsub("-", " ", gsub("_", " ", contentName) )
  }



  #### Read Rmds ####

  contentDeclarationContents <- read_file( paste0( tempPath, .Platform$file.sep, contentDeclarationTemplate) )
  contentSourceContents <- read_file( paste0( tempPath, .Platform$file.sep, contentSourceTemplate) )
  projNoteRmdContents <- read_file(projNoteRmdPath)


  #### Replace markup in Content with values ####

  # add Content section name field and value
  contentDeclarationContents <- sub_template_param(contentDeclarationContents, "{{CONTENT_TITLE_FIELD}}",
                                         settings[["ContentTitleField"]], orgPath)
  contentDeclarationContents <- sub_template_param(contentDeclarationContents, "{{CONTENT_TITLE}}",
                                         contentTitle, orgPath)

  # Add contentDescription field and value
  contentDeclarationContents <- sub_template_param(contentDeclarationContents, "{{CONTENT_DESCRIPTION_FIELD}}",
                                         settings[["ContentDescriptionField"]], orgPath)
  contentDeclarationContents <- sub_template_param(contentDeclarationContents, "{{CONTENT_DESCRIPTION}}",
                                         contentDescription, orgPath)

  # Add contentSource field and link (from ProjNote to Content)
  contentDeclarationContents <- sub_template_param(contentDeclarationContents, "{{CONTENT_SOURCE_FIELD}}",
                                                   settings[["ContentSourceField"]], orgPath)
  contentDeclarationContents <- sub_template_param(contentDeclarationContents, "{{CONTENT_SOURCE_LINK}}",
                                                   contentLink, orgPath)

  # Add Content Separators in base template
  contentDeclarationContents <- sub_template_param(contentDeclarationContents, "{{CONTENT_SEP}}",
                                         settings[["ContentSep"]], orgPath)

  # modify contentDeclarationContents with SEP values
  contentDeclarationContents <- replace_sep_values(contentDeclarationContents, orgPath)


  #### Add Content Declaration to Project Note ####

  projNoteRmdContents <- insert_at_indices(projNoteRmdContents, noteInsertionIndex,
                                           contentDeclarationContents)


  #### Create Content Dir ####

  done <- dir.create( contentDir )

  if(!done) {
    stop( paste0("  Content directory could not be created: ", contentDir) )
  }

  cat( "  Made Content dir: ", contentDir, "\n" )


  #### create Content Rmd file ####

  done <- file.create( contentRmd )

  if(!done) {
    file.remove(contentDir) # remove the sub note dir
    stop( paste0("  Content Rmd could not be created: ", contentRmd) )
  }

  cat( "  Made Content Rmd: ", contentRmd, "\n" )



  #### write Content Contents ####

  write_file(contentSourceContents, contentRmd)

  cat( "  Written content template to contents: ", contentRmd, "\n" )


  #### write Project Note ####

  write_file(projNoteRmdContents, projNoteRmdPath)

  cat( "  Inserted Content into Project Note: ", projNoteRmdPath, "\n" )

  # return contentRmd path
  contentRmd

}


#' Create Weekly Journal
#'
#' Saved in directory indicated in settings under "WeeklyJournalDir".
#'
#' Can optionally extract TODOs to this file with the `extract_todos()` function.
#'
#' @param date The start date of the Weekly Journal - typically a Monday.  This
#' should be in 'YYYY-MM-DD' format, and can be a String. Can create a Date
#' object with code `as.Date(paste(year, month, "01", sep = "-"))`.
#'
#' @param organisationPath The path to the Organisation where the weekly
#'  journal is created & saved.
#'
#' @param journalFileNameTemplate A string that defines the journal File Name.
#' YYYY & MM & DD are replaced with the year, month, day in the date arg.
#'
#' @param journalTemplate File in template/s dir that indicates the layout
#' for the weekly journal.
#'
#' @export
create_weekly_journal <- function(date=lubridate::today(),
                                  organisationPath=getwd(),
                                  journalFileNameTemplate="{{YYYY}}-{{MM}}-{{DD}}_{{ORGNAME}}",
                                  journalTemplate="Weekly-Work-Journal-Template.Rmd",
                                  openJournal = TRUE) {


  #### Instance Variables ####

  if( lubridate::is.Date(date) == FALSE ) {
    date <- lubridate::ymd(date) # parse the date and convert to ymd format
  }

  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")


  #### Identify root of ORGANISATION ####

  # get the orgPath from organisationPath
  orgPath <- find_org_directory(organisationPath)

  # get the organisation name
  orgName <- basename(orgPath)

  if(orgPath == "" ) { # only blank if orgPath not identified
    stop( paste0("  organisationPath is not in an Organisation: ", organisationPath) )
  } # now, orgPath should be the root dir of the organisation

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)


  #### Create Journal Dir ####

  # create PROG Dir:
  journalPath <- paste0(orgPath, .Platform$file.sep, settings[["WeeklyJournalDir"]])
  if(dir.exists(journalPath) ) {
    cat( "  Journal dir exists - creating weekly journal here: ", journalPath, "\n" )
  } else { # create the journalPath
    done <- dir.create(journalPath)
    if(!done) {
      stop( paste0("  Programme directory could not be created: ", journalPath) )
    } else {
      cat( "  Made Journal dir: ", journalPath, "\n" )
    }
  }


  #### create Journal Rmd file ####

  # modify journalFileNameTemplate - with any variable syntax
  journalFileNameTemplate <- gsub('{{YYYY}}', year, journalFileNameTemplate, fixed = TRUE)
  journalFileNameTemplate <- gsub('{{MM}}', month, journalFileNameTemplate, fixed = TRUE)
  journalFileNameTemplate <- gsub('{{DD}}', day, journalFileNameTemplate, fixed = TRUE)

  journalFileNameTemplate <- gsub('{{ORGNAME}}', orgName, journalFileNameTemplate, fixed = TRUE)

  journalRmdPath <- paste0(journalPath, .Platform$file.sep, journalFileNameTemplate, ".Rmd")

  if( fs::file_exists(journalRmdPath) ) {

    cat( "  Journal .Rmd file Exists: ",journalRmdPath, "\n" )

    # open the newly created journal
    if( openJournal == TRUE ) {

      # navigate to journalk file:
      rstudioapi::navigateToFile(journalRmdPath)

      journalDirPath <- dirname(journalRmdPath)
      # navigate to containing dir
      rstudioapi::filesPaneNavigate(journalDirPath)
      # and set working directory
      setwd(journalDirPath)

    }

    return()

  }

  done <- file.create(journalRmdPath)

  if(!done) {
    stop( paste0("  Journal .Rmd file could not be created: ", journalRmdPath) )
  }

  cat( "  Made Journal .Rmd file: ",journalRmdPath, "\n" )


  # read journalTemplate:
  journalContents <- read_file( paste0(tempPath, .Platform$file.sep, journalTemplate) )

  # use username as author value
  authorValue <- Sys.info()["user"]

  # modify journalContents to include date YYYY MM DD
  journalContents <- gsub("{{YYYY}}", year, journalContents, fixed=TRUE)
  journalContents <- gsub("{{MM}}", month, journalContents, fixed=TRUE)
  journalContents <- gsub("{{DD}}", day, journalContents, fixed=TRUE)

  # modify journalContents to include authorValue
  journalContents <- gsub("{{AUTHOR}}", authorValue, journalContents, fixed=TRUE)

  # add plaintext calendar
  journalContents <- sub_template_param(journalContents,
                                        "{{WEEKLY_JOURNAL_PLAINTEXT_CALENDAR}}",
                                        generate_plaintext_calendar(year, month, day),
                                        orgPath)

  # add weekly rmarkdown daily journal
  journalContents <- sub_template_param(journalContents,
                                        "{{WEEKLY_JOURNAL_DAILY_JOURNAL}}",
                                        generate_weekly_rmarkdown(year, month, day),
                                        orgPath)

  # add todo collection from org
  #journalContents <- sub_template_param(journalContents,
  #                                      "{{WEEKLY_JOURNAL_TODO_COLLECTION}}",
  #                                      extract_todos(orgPath),
  #                                      orgPath)
  # DEPRECATED - user should extract TODOs as needed into the weekly journal
  # allows flexibility in TODO extraction!
  # added separate __EXTRACT_TODOS_HERE__ marker in the Weekly Journal Template now

  # modify journalContents with rmarkdown-html-header content
  journalContents <- replace_markdown_header(journalContents, orgPath)

  # modify journalContents with SEP values
  journalContents <- replace_sep_values(journalContents, orgPath)

  # write to journalFile
  write_file(journalContents, journalRmdPath)

  cat( "  Written template to Journal .Rmd file: ", journalRmdPath, "\n" )

  # open the newly created journal
  if( openJournal == TRUE ) {

    # navigate to journalk file:
    rstudioapi::navigateToFile(journalRmdPath)

    journalDirPath <- dirname(journalRmdPath)
    # navigate to containing dir
    rstudioapi::filesPaneNavigate(journalDirPath)
    # and set working directory
    setwd(journalDirPath)

  }

}


#' Generate Plaintext Calendar
#'
#' Internal function to generate a plaintext calendar for insertion into
#' the weekly journal.
#'
#' The calendar is laid out as a matrix with columns for each day of the week,
#' starting with SUNDAY, and dates filled into this matrix - from the first date
#' of the month to the end of the week (ie. the Saturday) BEYOND the last date
#' of the month.
#'
generate_plaintext_calendar <- function(year, month, day, calendar_header="# DAILY LOG :") {
  # Create a date object for the first day of the specified month and year
  first_date <- as.Date(paste(year, month, "01", sep = "-"))

  # Determine the last day of the specified month
  last_date <- as.Date(seq.Date(from = first_date, by = "month", length.out = 2)[2]) - 1

  # Initialize an empty string to store the calendar
  calendar_text <- ""

  # Add the month and year as the title
  calendar_text <- paste(calendar_header, calendar_text, paste(day, month.name[as.integer(month)], year), "\n\n")

  # Create a matrix to represent the calendar grid
  calendar_matrix <- matrix("", nrow = 7, ncol = 7)

  # Fill in the day numbers in the matrix
  current_date <- first_date

  # define day names vector
  day_names <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

  # get the first of month as day
  day <- format(current_date, "%a")
  day_index <- which(day_names == day)

  # boolean to check when first date added
  add_first_date <- FALSE

  # add extra dates for NEXT month - to complete each week
  # calculate number of extra days over last day of month to add to complete calendar
  # how many extra days over a week (modulus 7)
  extra_days_week <- as.integer(format(last_date, "%d"))%%7
  # how many extra days over a month
  extra_days_month <- 7 - ( (day_index + extra_days_week - 1)%%7 )
  # need to fill (7 + 1 - day_index) but minus the extra days
  #extra_days_month <- (7 + 1 - day_index) - extra_days_week

  # add these extra days to last_date
  last_date <- last_date + extra_days_month

  num_dates <- as.integer(format(last_date, "%d")) + (7 + 1 - day_index)

  for (row in 1:7) {
    for (col in 1:7) {
      if(add_first_date == FALSE) {
        if(col == day_index) {
          # then add the first date and proceed
          day_number <- format(current_date, "%d")
          calendar_matrix[row, col] <- day_number
          current_date <- current_date + 1
          add_first_date <- TRUE
        } else {
          calendar_matrix[row, col] <- "  " # add two blank lines for spacing
        }
      } else {
        if (current_date >= first_date && current_date <= last_date) {
          day_number <- format(current_date, "%d")
          calendar_matrix[row, col] <- day_number
          current_date <- current_date + 1
        } else {
          calendar_matrix[row, col] <- "  " # add two blank lines for spacing
        }
      }
    }
  }

  # Add the column headers (day names)
  calendar_text <- paste(calendar_text, paste(day_names, collapse = "  "), "\n")

  # Add the calendar grid to the text
  for (row in 1:6) {
    calendar_text <- paste(calendar_text, paste(calendar_matrix[row, ], collapse = "   "), "\n")
  }

  # add blank line at end of calendar
  calendar_text <- paste(calendar_text, "\n")

  return(calendar_text)
}


#' Generate Weekly RMarkdown Daily Log
#'
#' Creates RMarkdown content for 7 days, from the year-month-day date passed
#' to this function.
#'
generate_weekly_rmarkdown <- function(year, month, day, separator_lines="{{SEP02}}") {

  # Create a Date object for the date provided to this function
  first_date <- as.Date(paste(year, month, day, sep = "-"))

  # get the last date: + 6 from first_date
  last_date <- first_date + 6

  # Determine the last day of the month
  #last_day <- as.Date(paste(year, month + 1, "01", sep = "-")) - 1

  # Initialize an empty string to store the R Markdown lines
  markdown_lines <- character(0)

  # Loop through each day of the month
  current_date <- first_date
  while (current_date <= last_date) {
    # Generate the R Markdown header for the current day
    day_header <- toupper( format(current_date, "# %A %d %B %Y") )

    # Add separator lines
    markdown_lines <- c(markdown_lines, separator_lines, "","","","")

    # then add day header
    markdown_lines <- c(markdown_lines, day_header, "","","","")


    # Move to the next day
    current_date <- current_date + 1
  }

  # Combine the R Markdown lines into a single string
  #markdown_text <- paste(markdown_lines, collapse = "")

  return(markdown_lines)
}


