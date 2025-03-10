
#' Create a New Project Organisation
#'
#' Generates the directory layout for a new Project Organisation in the file
#' system. It creates the organisation directory and its initial contents,
#' including an index Rmd file, a .config/ directory (with config files and
#' templates), a docs/ directory for documentation, and a volumes/ directory
#' for data storage. An HTML site directory is also created adjacent to the
#' organisation directory.
#'
#' @param orgParentPath A character string defining the parent directory
#' where the organisation will be created. Default is the working directory.
#' @param orgName A string defining the directory name of the Organisation.
#' @param authorValue A string representing the author name for the Programme
#' index file. Defaults to the current system user.
#' @param orgTitle A string defining the title of the Organisation for its
#' document.
#' @param settingsYamlPath A valid YAML file for establishing the projectmanagr
#' organisation layout. Must include keys for SitePath, VolumesDir, DocsDir,
#' ConfigDir, and OrgIndexFileName. Other parameters are optional.
#' @param orgTemplate A string defining the template to use from the
#' projectmanagr package. Default is "Org-Template.Rmd".
#'
#' @details
#' This function performs the following operations:
#' 1. Defines and validates the organisation title using the orgName.
#' 2. Determines the organisation directory based on orgParentPath and orgName.
#' 3. Retrieves the projectmanagr package path to access required files.
#' 4. Gathers existing organisation paths and updates their configurations.
#' 5. Loads the YAML settings from a provided file or uses the default.
#' 6. Creates the organisation directory and subdirectories for the HTML site,
#'    volumes, and configuration.
#' 7. Copies necessary files such as volumes.Rmd, settings.yaml, and status.yaml.
#' 8. Creates the organisation index file from the specified template.
#'
#' @return A character string representing the path to the organisation index
#' Rmd file.
#'
#' @examples
#' \dontrun{
#'   create_project_org(
#'     orgParentPath = "/projects",
#'     orgName = "MyOrganisation",
#'     authorValue = "sjwest",
#'     orgTitle = "My Organisation",
#'     settingsYamlPath = "path/to/settings.yml",
#'     orgTemplate = "Org-Template.Rmd"
#'   )
#' }
#'
#' @seealso
#' \code{\link{define_default_org_title}}, \code{\link{define_org_path}},
#' \code{\link{get_org_paths}}, \code{\link{load_yml_settings_default}},
#' \code{\link{create_org_dir}}, \code{\link{create_html_site_path}},
#' \code{\link{create_volumes_path}}, \code{\link{create_config_path}},
#' \code{\link{create_volumes_file}}, \code{\link{create_settings_yaml_file}},
#' \code{\link{create_status_yaml_file}}, \code{\link{create_addins_json}},
#' \code{\link{create_templates_path}}, \code{\link{create_org_index}}
#'
#' @note
#' This function directly creates and modifies directories and files on disk.
#' Ensure you have adequate backups before running it. The provided YAML
#' settings must include all required keys for the organisation layout.
#'
#' @export
create_project_org <- function( orgParentPath, orgName, authorValue=Sys.info()["user"],
                                orgTitle="", settingsYamlPath="",
                              orgTemplate="Org-Template.Rmd") {

  cat( "\nprojectmanagr::create_project_org():\n" )


  #### define and check args ####

  orgTitle <- define_default_org_title(orgTitle, orgName)
  orgPath <- define_org_path(orgParentPath, orgName)

  # check location of installed projectmanagr package - to retrieve files for org install
  projectmanagrPath <- find.package("projectmanagr", lib.loc = .libPaths()) # lib.loc ensures correct lib path is returned during testing

  # get all org paths (including this one) from the parent dir of current orgPath
  orgPaths <- get_org_paths(orgPath)
   # this also updates the existing org configs to be aware of this new org!
   # this ensures all organisations (at least those written to parent DIR of
    # this new ORG) are aware of each other
   # thus can update links across organisations
  settings <- load_yml_settings_default(settingsYamlPath, projectmanagrPath)


  #### create projectmanagr org ####

  tryCatch({

    create_org_dir(orgPath)

    sitePath <- create_html_site_path(orgPath, settings)
    volumesPath <- create_volumes_path(orgPath, settings)
    confPath <- create_config_path(orgPath, settings)

    volumesFile <- create_volumes_file(volumesPath, projectmanagrPath, orgPath,
                                       settings)

    settingsYamlFile <- create_settings_yaml_file(orgPath, settings)
    statusFile <- create_status_yaml_file(orgPath, settings, orgPaths, orgName,
                                          orgTitle, sitePath)

    addinsJsonProjectManagrFile <- create_addins_json(projectmanagrPath, orgPath,
                                                      settings)

    tempPath <- create_templates_path(projectmanagrPath, orgPath, settings)

    orgIndex <- create_org_index(orgName, tempPath, orgTemplate, orgPath, settings,
                                orgTitle, authorValue)

  }, error = function(e) {
    cat("  ====================  \n")
    cat("  Error encountered:", e$message, "\n")
    cleanup_created(orgPath)
    stop(e)  # Rethrow the error after cleanup
  })

  return(orgIndex)

}


#' Define Default Organisation Title
#'
#' Ensures that an organisation title is set. If the provided title is blank,
#' it replaces underscores and hyphens in the organisation name with spaces
#' and trims whitespace.
#'
#' @param orgTitle Character. The proposed title for the organisation.
#' @param orgName Character. The organisation name to use if no title is provided.
#'
#' @return Character. The final organisation title.
define_default_org_title <- function(orgTitle, orgName) {

  # Check orgTitle, and if blank, fill with orgName, replacing all "_" and "-"
  # with spaces & trim whitespace
  if( nchar(orgTitle)==0 ) {
    orgTitle <- trimws( gsub("-", " ", gsub("_", " ", orgName) ) )
  }
  orgTitle # return
}

#' Define Organisation Path
#'
#' Constructs the full path for a new organisation and ensures it does not already exist.
#'
#' @param orgParentPath Character. The parent directory for the organisation.
#' @param orgName Character. The name of the organisation.
#'
#' @return Character. The full path for the organisation.
define_org_path <- function(orgParentPath, orgName) {
  orgPath <- fs::path(orgParentPath, orgName)
  if(fs::dir_exists(orgPath) ) {
    stop( paste0("  Organisation directory already exists: ", orgPath) )
  }
  orgPath # return
}

#' Get Existing Organisation Paths
#'
#' Ensures all new organisations are aware of existing ones, and vice versa,
#' if they reside in the same parent directory.
#'
#' @param newOrgPath Character. The directory path of the new organisation.
#'
#' @return Character vector. List of paths to all identified organisations.
#' @export
get_org_paths <- function(newOrgPath) {

  # get all org paths/names from the parent dir of current orgPath & return as list
  # this also updates the existing org configs to be aware of this new org!

  parent <- dirname(newOrgPath)

  # get list of directories from parent
  dirs <- list.dirs(parent, full.names = TRUE, recursive = FALSE)

  # return early if no dirs identified
  #if( length(dirs)==0 ) {
  #  return(dirs) # return the 0-length char vector
  #}

  dirs <- check_dirs_org(dirs)

  # return early if no ORG dirs identified
  if( length(dirs)==0 ) {
    return(dirs) # return the 0-length char vector
  }

  cat("  identified other Orgs\n")

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
    write_yaml_status(status, statusFile)

    cat("    Written to Org: ", dirs[i], "\n")

  }
  cat("")

  # return orgPaths
  orgPaths

}

#' Write Status YAML to File
#'
#' Saves the status of an organisation as a YAML file.
#'
#' @param status List. The status data to be written.
#' @param statusFile Character. The path to the status YAML file.
write_yaml_status <- function(status, statusFile) {
  yaml::write_yaml( yaml::as.yaml(status), statusFile )
}

#' Load YAML Settings with Defaults
#'
#' Loads a YAML settings file, either from a provided path or from the default
#' settings in the projectmanagr package.
#'
#' @param settingsYamlPath Character. Path to a custom YAML settings file. If empty,
#'   the default projectmanagr settings are used.
#' @param projectmanagrPath Character. The file path of the projectmanagr package.
#'
#' @return List. The parsed settings from the YAML file.
load_yml_settings_default <- function(settingsYamlPath, projectmanagrPath) {

  # The default is loaded from the package OR function points to settingsYamlPath
  # yaml file which is used
  if( settingsYamlPath == "" ) {

    # no settingsYamlFile supplied - so load the DEFAULT settingsYamlFile from
     # projectmanagr
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
    extraSettings <- setdiff(names(settings), yaml_keys())
    missingSettings <- setdiff(yaml_keys(), names(settings))

    if( length(extraSettings) > 0 ) {
      cat("  settingsYamlPath contains some extra settings: ",
          paste(extraSettings), "\n")
    }

    if( length(missingSettings) > 0 ) {
      stop( paste0("  settingsYamlPath is not a valid projectmanagr YAML file : ",
                   settingsYamlPath, "\n   missing settings: ", paste(missingSettings)))
    }
  }

  # lst <- list(settings, settingsYamlFile)
  # names(list) <- c("settings", "settingsYamlFile")
  # lst # return named list

  settings # return settings list
}


#' Retrieve Expected YAML Keys
#'
#' Returns a character vector of all expected keys that should be present in a
#' YAML configuration file.
#'
#' @return Character vector. List of expected YAML keys.
yaml_keys <- function() {

  c("SiteDir",
    "FileType",
    "VolumesDir",
    "VolumesFile",
    "ConfigStatusYamlFile",
    "OrgIndexFileNamePrefix",
    "OrgProgrammeHeader",
    "OrgProgrammeFooter",
    "OrgProgrammeSummarySep",
    "ProgIndexFileNamePrefix",
    "ProgSummaryHeader",
    "ProgSummaryTitle",
    "ProgSummaryFooter",
    "ProgProjectsHeader",
    "ProgProjectsFooter",
    "ProgProjectSummarySep",
    "ProjectPrefixSep",
    "ProjectIdentifierSep",
    "ProjectIndexSep",
    "ProjectSummaryHeader",
    "ProjectSummaryTitle",
    "ProjectSummaryFooter",
    "ProjectGoalHeader",
    "ProjectGoalTitle",
    "ProjectGoalDivider",
    "ProjectGoalSep",
    "ProjectDeliverableHeader",
    "ProjectDeliverableTitle",
    "ProjectDeliverableDivider",
    "ProjectDeliverableSep",
    "ProjectTaskHeader",
    "ProjectTaskTitle",
    "ProjectTaskDivider",
    "ProjectTaskSep",
    "ProjectTaskFooter",
    "ProjectLinkFormat",
    "ProjectTaskLogHeader",
    "ProjectTaskLogSep",
    "NoteObjectivesTodoSectionHeader",
    "NoteObjectivesHeader",
    "NoteObjectivesSep",
    "NoteObjectivesFooter",
    "NoteStorageHeader",
    "NoteStorageFooter",
    "NoteLinkFormat",
    "GroupNotePrefixSep",
    "HeaderNotePrefix",
    "HeaderNoteContentsHeader",
    "HeaderNoteContentsFooter",
    "HeaderLinkFormat",
    "SubNotePrefixSep",
    "SubNoteContentsHeader",
    "SubNoteContentsFooter",
    "SubNoteLinkFormat",
    "NoteSummaryTitle",
    "NoteGoalLinkLine",
    "NoteDeliverableLinkLine",
    "NoteTaskLinkLine",
    "ContentTitleField",
    "ContentDescriptionField",
    "ContentSourceField",
    "ContentSep",
    "ContentInsertionTitle",
    "ContentInsertionSep",
    "ContentLinkFormat",
    "ContentSummaryHeader",
    "ContentSummaryFooter",
    "ContentGraphicalAbstractHeader",
    "ContentGraphicalAbstractSvg",
    "ContentGraphicalAbstractFooter",
    "ContentBackgroundHeader",
    "ContentBackgroundFooter",
    "ContentMaterialsHeader",
    "ContentMaterialsFooter",
    "ContentEquipmentSvg",
    "ContentProcedureTemplateSvg",
    "ContentResultsLogHeader",
    "ContentResultsLogFooter",
    "ContentTroubleshootingHeader",
    "ContentTroubleshootingFooter",
    "ContentSopHeader",
    "ContentLogHeader",
    "ContentLogSep",
    "ContentFooter",
    "TodoHeader",
    "TodoHeaderTemplate",
    "TodoItem",
    "TodoItemTemplate",
    "TodoItemComplete",
    "TodoProgrammeSep",
    "TodoProjectNoteSep",
    "TodoTaskHeader",
    "TodoGDTSep",
    "DateTimeZone",
    "DateSplit",
    "DateTimeSplit",
    "DatatableExportHeader",
    "RunUpdateOnStartup",
    "RunCompileWithUpdate",
    "FileTypeSuffix",
    "WeeklyJournalDir",
    "GadgetWidth",
    "GadgetHeight",
    "rstudioInternalStateDir")
}


#' Create Organisation Directory
#'
#' Creates the root directory for the organisation if it does not exist.
#'
#' @param orgPath Character. The full path to the organisation directory.
create_org_dir <- function(orgPath) {
  create_directory(orgPath,
                   "  Made ORG dir: ",
                   "  Organisation directory could not be created: ")
}



#' Create HTML Site Directory
#'
#' Creates a directory for compiled HTML output.
#'
#' @param orgPath Character. Path to the organisation directory.
#' @param settings List. Settings containing the site directory structure.
#'
#' @return Character. The path to the created site directory.
create_html_site_path <- function(orgPath, settings) {
  # This will contain the compiled HTML
  # which will be made using R Markdown package in the first instance
  # In future, want to move to using pandoc directly??
  sitePath <- get_site_dir(orgPath,settings)
  fs::dir_create(sitePath)
  confirm_dir(sitePath, "  Site directory could not be created: ")
  cat( "  Made html site dir: ", sitePath, "\n" )
  sitePath # return
}


#' Create Volumes Directory
#'
#' Creates a directory for storing data volumes.
#'
#' @param orgPath Character. Path to the organisation directory.
#' @param settings List. Settings containing the volumes directory structure.
#'
#' @return Character. The path to the created volumes directory.
create_volumes_path <- function(orgPath, settings) {

  volumesPath <- get_volumes_dir(orgPath, settings)
  fs::dir_create(volumesPath)
  confirm_dir(volumesPath, "  Volumes directory could not be created: ")
  cat( "  Made volumes dir: ", volumesPath, "\n" )
  volumesPath # return
}

#' Create Configuration Directory
#'
#' Creates a directory for storing configuration files.
#'
#' @param orgPath Character. Path to the organisation directory.
#' @param settings List. Settings containing the configuration directory structure.
#'
#' @return Character. The path to the created configuration directory.
create_config_path <- function(orgPath, settings) {
  # Contains configuration information for Organisation
  # config dir: FIXED NAME
  confPath <- get_config_dir(orgPath)
  fs::dir_create(confPath)
  confirm_dir(confPath, "  Config directory could not be created: ")
  cat( "  Made config dir: ", confPath, "\n" )
  confPath # return
}



#' Create Volumes File
#'
#' Copies the default volumes file template into the organisation's volumes
#' directory.
#'
#' @param volumesPath Character. Path to the volumes directory.
#' @param projectmanagrPath Character. Path to the projectmanagr package.
#' @param orgPath Character. Path to the organisation directory.
#' @param settings List. Project settings containing file structure configurations.
#'
#' @return Character. The path to the created volumes file.
create_volumes_file <- function(volumesPath, projectmanagrPath, orgPath, settings) {

  # Add the volumes.Rmd file to the volumes/ DIR
  # this contains the workflow for MOUNTING an External Volume, then
  # SYMLINKING location(s)  in the External Volume to the volumes/ directory, then
  # how to use the projectmanagr::volumes_mkdir() command to generate a DIR on
   # this External Volume for external storage

  # COPY template from the package:
  volumesFile <- fs::path(volumesPath, settings[["VolumesFile"]])
  volumesPackageFile <- fs::path(projectmanagrPath, "volumes", "volumes.Rmd")

  fs::file_copy(volumesPackageFile, volumesFile)

  confirm_file(volumesFile, "  Volumes file could not be copied: ")

  cat( "  Copied volumes file: ", volumesFile, "\n" )

  volumesFile # return
}

#' Create Settings YAML File
#'
#' Generates a settings YAML file for the organisation.
#'
#' @param orgPath Character. Path to the organisation directory.
#' @param settings List. Project settings to be saved in the YAML file.
#' @param utime Unused.
#' @param orgPaths Character vector. List of existing organisation paths.
#' @param orgName Character. Name of the organisation.
#' @param orgTitle Character. Title of the organisation.
#' @param sitePath Character. Path to the site directory.
#'
#' @return Character. Path to the created settings YAML file.
create_settings_yaml_file <- function(orgPath, settings, utime,
                                      orgPaths, orgName, orgTitle, sitePath) {

  # a settings.yml file and a status.yml file
  # settings.yml contains user-defined defaults for operations
  # status.yml contains a list of incomplete projects and notes, for each programme

  # define the settingsYamlFile in the org being created
  settingsYamlFile <- get_settings_yml_file(orgPath)

  # write YAML to settingsYamlFile location
  yaml::write_yaml( yaml::as.yaml(settings), settingsYamlFile )

  confirm_file(settingsYamlFile, "  Settings file could not be created: ")

  cat( "  Written settings file: ", settingsYamlFile, "\n" )
  settingsYamlFile # return
}

#' Create Status YAML File
#'
#' Generates a status YAML file for the organisation, storing metadata and
#' update timestamps.
#'
#' @param orgPath Character. Path to the organisation directory.
#' @param settings List. Project settings used to generate the status file.
#' @param orgPaths Character vector. List of existing organisation paths.
#' @param orgName Character. Name of the organisation.
#' @param orgTitle Character. Title of the organisation.
#' @param sitePath Character. Path to the site directory.
#'
#' @return Character. Path to the created status YAML file.
create_status_yaml_file <- function(orgPath, settings, orgPaths, orgName,
                                    orgTitle, sitePath) {

  # create status.yml file - need to create it here to get the mtime for this file
  statusFile <- get_status_yml_file(orgPath, settings)
  fs::file_create(statusFile)

  updateTime <- get_datetime() # use the current time: y-m-d:h:m string!

  org <- create_status_yaml_content(orgPaths, orgPath, orgName, orgTitle,
                                    updateTime, sitePath )
  yaml::write_yaml( yaml::as.yaml(org), statusFile )

  confirm_file(statusFile, "  Status file could not be created: ")
  cat( "  Made status file: ", statusFile, "\n" )

  statusFile # return
}

#' Generate Status YAML Content
#'
#' Creates a structured list to be written into a status YAML file.
#'
#' @param orgPaths Character vector. List of existing organisation paths.
#' @param orgPath Character. Path to the new organisation.
#' @param orgName Character. Name of the organisation.
#' @param orgTitle Character. Title of the organisation.
#' @param updateTime Character. Timestamp of the last update.
#' @param sitePath Character. Path to the site directory.
#'
#' @return List. A named list containing metadata for the organisation.
create_status_yaml_content <- function(orgPaths, orgPath, orgName, orgTitle,
                                      updateTime, sitePath) {

  org <- list(orgPaths, orgPath, orgName, orgTitle, updateTime, sitePath )
  # orgPaths contains all EXISTING ORGs and THIS ORG path at end
  names(org) <- c("orgPaths", "orgPath", "orgName", "orgTitle", "updateTime", "sitePath")
  return(org)
}


#' Create Addins JSON File
#'
#' Copies the default addins.json file from the projectmanagr package into the organisation's config directory.
#'
#' @param projectmanagrPath Character. Path to the projectmanagr package.
#' @param orgPath Character. Path to the organisation directory.
#' @param settings List. Project settings containing file structure configurations.
#'
#' @return Character. The path to the created addins.json file.
create_addins_json <- function(projectmanagrPath, orgPath, settings) {

  # no settingsYamlFile supplied - so load the DEFAULT settingsYamlFile from projectmanagr
  addinsJsonPackageFile <- fs::path(projectmanagrPath, "config", "addins.json")
  addinsJsonProjectManagrFile <- fs::path(get_config_dir(orgPath), "addins.json")

  fs::file_copy(addinsJsonPackageFile, addinsJsonProjectManagrFile)

  confirm_file(addinsJsonProjectManagrFile,
               "  addins.json file could not be created: ")
  cat( "  Copied addins.json file: ", addinsJsonProjectManagrFile, "\n" )
  addinsJsonProjectManagrFile # return
}

#' Create Templates Directory
#'
#' Creates a directory for storing templates and copies template files from the
#' package.
#'
#' @param projectmanagrPath Character. Path to the projectmanagr package.
#' @param orgPath Character. Path to the organisation directory.
#' @param settings List. Project settings containing file structure configurations.
#'
#' @return Character. The path to the created templates directory.
create_templates_path <- function(projectmanagrPath, orgPath, settings) {

  # templates Dir - INSIDE the config DIR (these templates are part of the projectmanagr config!):
  tempPath <- get_template_dir(orgPath)
  fs::dir_create(tempPath)
  confirm_dir(tempPath, "  Templates directory could not be created: ")
  cat( "  Made templates dir: ",tempPath, "\n" )

  # copy template files:
  # need to copy from the PACKAGE!
  templateDir <- fs::path(projectmanagrPath, "templates")
  templateRmds <- list.files(templateDir)
  for(f in templateRmds) {
    fs::file_copy( fs::path(templateDir, f), tempPath)
    cat( "  Copied template: ",f, "\n" )
  }
  # confirm all templates exist
  tps <- fs::path(tempPath, templateRmds)
  if( any(fs::file_exists(tps) == FALSE) ) {
    stop( paste0("  Failed to create Template: ", tps[any(fs::file_exists(tps) == FALSE)]) )
  }
  tempPath # return
}


#' Create Organisation Index File
#'
#' Generates the index Rmd file for the organisation based on a template.
#'
#' @param orgName Character. Name of the organisation.
#' @param tempPath Character. Path to the templates directory.
#' @param orgTemplate Character. Name of the template file.
#' @param orgPath Character. Path to the organisation directory.
#' @param settings List. Project settings used for configuring the index file.
#' @param orgTitle Character. Title of the organisation.
#' @param authorValue Character. Author of the organisation. Defaults to the system username.
#'
#' @return Character. The path to the created organisation index file.
create_org_index <- function(orgName, tempPath, orgTemplate, orgPath, settings,
                             orgTitle, authorValue=Sys.info()["user"]) {

  orgIndex <- get_index_org(orgPath, settings)

  # create Rmd file
  create_file(orgIndex, "  Made Organisation index file: ",
              "  Organisation index file could not be created: ")

  # read org template:
  templateContents <- read_file( fs::path(tempPath, orgTemplate))

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

  # write to orgIndex
  write_file(templateContents, orgIndex)
  cat( "  Written template to Org Index: ", orgIndex, "\n" )

  orgIndex # return
}


get_index_org <- function(orgPath, settings) {
  fs::path(orgPath,
           paste0(settings[["OrgIndexFileNamePrefix"]],
                  fs::path_file(orgPath), ".Rmd"))
}

# Helper function to remove a vector of directories and files for cleanup
cleanup_created <- function(createdFilesVector) {
  # delete all files in vector that exist
  fvex <- fs::file_exists(createdFilesVector)
  fs::file_delete( createdFilesVector[fvex] )
  cat("    cleanup - removed path(s): ",
        paste(createdFilesVector[fvex], sep=' '))

}#### ________________________________ ####


#' Create a New Programme within an Organisation
#'
#' Generates a new programme within an existing organisation directory, creating
#' the necessary directory structure, R Markdown files, and linking to the
#' organisation index file.
#'
#' @param programmeName Character. The name of the new programme. Must not contain
#'   spaces.
#' @param organisationPath Character. The file path to an existing organisation
#'   directory.
#' @param authorValue Character. The author of the programme. Defaults to system
#'   username.
#' @param programmeTitle Character. The title of the programme. If empty, it will
#'   be derived from `programmeName`.
#' @param progTemplate Character. The filename of the programme template. Default
#'   is `"Programme-Template.Rmd"`.
#' @param progSummaryTemplate Character. The filename of the programme summary
#'   template. Default is `"Programme-Summary-Template.Rmd"`.
#'
#' @details
#' This function performs the following steps:
#' 1. Ensures `programmeName` does not contain spaces.
#' 2. Identifies the root organisation directory from `organisationPath`.
#' 3. Creates a new programme directory within the organisation.
#' 4. Generates a new programme index Rmd file from a template.
#' 5. Updates the organisation's status YAML file with programme details.
#' 6. Links & inserts the programme summary into the organisation's index file.
#'
#' The function ensures that all directories and files are properly created and
#' linked within the project structure.
#'
#' @return Character. The file path of the created programme index Rmd file.
#'
#' @examples
#' \dontrun{
#' # Create a new programme in an organisation
#' create_programme(
#'   programmeName = "MyProgramme",
#'   organisationPath = "/path/to/organisation",
#'   authorValue = "sjwest",
#'   programmeTitle = "My Programme",
#'   progTemplate = "Programme-Template.Rmd",
#'   progSummaryTemplate = "Programme-Summary-Template.Rmd"
#' )
#' }
#'
#' @seealso
#' - \code{\link{find_org_directory}} to locate the organisation directory.
#' - \code{\link{get_settings_yml}} for retrieving configuration settings.
#' - \code{\link{write_file}} for writing content to files.
#'
#' @note
#' - The `programmeName` should not contain spaces.
#' - The function modifies files directly on disk. Ensure backups are created
#'   before running.
#' - The function relies on a structured organisation directory; incorrect
#'   structures may cause failures.
#'
#' @export
create_programme <- function(programmeName, organisationPath,
                             authorValue=Sys.info()["user"], programmeTitle="",
                             progTemplate="Programme-Template.Rmd",
                             progSummaryTemplate = "Programme-Summary-Template.Rmd") {

  cat( "\nprojectmanagr::create_programme():\n" )


  #### define and check args ####

  # get the orgPath from organisationPath
  orgPath <- confirm_find_org(organisationPath)

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get status yml
  statusFile <- get_status_yml_file(orgPath, settings)
  status <- get_status_yml(orgPath, settings)

  # check args validity
  check_prog_name(programmeName)
  programmeTitle <- define_prog_title(programmeTitle, programmeName)


  #### Create Programme ####

  tryCatch({

    progPath <- create_prog_dir(orgPath, programmeName)

    progContents <- create_prog_index(progPath, programmeTitle,
                                      tempPath, progTemplate, authorValue,
                                      orgPath, settings)

    #write_prog_to_status(programmeTitle, programmeName, status, statusFile)
     # no longer writing programme to status as not needed

    progIndex <- link_prog_to_org_index(progPath, programmeName, progContents,
                                        tempPath, progSummaryTemplate,
                                        orgPath, settings)

  }, error = function(e) {
    cat("  ====================  \n")
    cat("  Error encountered:", e$message, "\n")
    cleanup_created(progPath)
    stop(e)  # Rethrow the error after cleanup
  })

  return(progIndex)

}


#' check programmeName contains NO SPACES:
check_prog_name <- function(programmeName) {
  if( grepl("\\s+", programmeName) ) {
    stop( paste0("  programmeName contains a SPACE: ", programmeName) )
  }
}

#' Check programmeTitle, and if blank, fill with programmeName
#' replacing all "_" and "-" with spaces
define_prog_title <- function(programmeTitle, programmeName) {
  if( nchar(programmeTitle) == 0 ) {
    programmeTitle <- gsub("-", " ", gsub("_", " ", programmeName) )
  }
  return(programmeTitle)
}

#' Create Programme Directory
#'
create_prog_dir <- function(orgPath, programmeName) {

  progPath <- fs::path(orgPath, programmeName)
  create_directory(progPath, "  Made Programme dir: ",
                   "  Programme directory could not be created: ")
  return(progPath)
}

#' Create Programme Index Rmd
#'
#' Fill with contents, return contents
#'
create_prog_index <- function(progPath, programmeTitle,
                              tempPath, progTemplate, authorValue,
                              orgPath, settings) {

  # define index paths
  orgIndex <- get_index_org(orgPath, settings)
  progIndex <- get_index_prog(progPath, settings)

  # create Rmd file
  create_file(progIndex, "  Made Programme index file: ",
              "  Programme index file could not be created: ")

  progContents <- read_file( fs::path(tempPath, progTemplate) )

  progContents <- init_template_prog(progContents, programmeTitle, authorValue,
                                     orgIndex, progIndex, orgPath, settings)

  write_file(progContents, progIndex)
  cat( "  Written template to Programme index.Rmd file: ", progIndex, "\n" )

  return(progContents)

}


get_index_prog <- function(progPath, settings) {
  fs::path(progPath,
           paste0(settings[["ProgIndexFileNamePrefix"]],
                  fs::path_file(progPath), ".Rmd"))
}


init_template_prog <- function(progContents, programmeTitle, authorValue,
                               orgIndex, progIndex, orgPath, settings) {

  # modify progContents to include programmeTitle and authorValue
  progContents <- gsub("{{TITLE}}", programmeTitle, progContents, fixed=TRUE)
  progContents <- gsub("{{AUTHOR}}", authorValue, progContents, fixed=TRUE)

  # modify programme summary header/footer
  progContents <- sub_template_param(progContents, "{{SUMMARY_HEADER}}",
                                     settings[["ProgSummaryHeader"]], orgPath)
  progContents <- sub_template_param(progContents, "{{SUMMARY_FOOTER}}",
                                     settings[["ProgSummaryFooter"]], orgPath)

  # modify progContents to include relative link to org index Rmd
  orgLink <- create_hyperlink_no_ext(orgIndex, progIndex)
  progContents <- gsub("{{ORGLINK}}", orgLink, progContents, fixed=TRUE)

  # modify programme projects header/footer
  progContents <- sub_template_param(progContents, "{{PROJECTS_HEADER}}",
                                     settings[["ProgProjectsHeader"]], orgPath)
  progContents <- sub_template_param(progContents, "{{PROJECTS_FOOTER}}",
                                     settings[["ProgProjectsFooter"]], orgPath)

  progContents <- replace_markdown_header(progContents, orgPath)
  progContents <- replace_sep_values(progContents, orgPath)

  return(progContents)
}

#' Write Programme to Status yml file
#'
#' Fill with datetime of programme creation && programme title
#'
write_prog_to_status <- function(programmeTitle, programmeName, status, statusFile) {

  progCreationTime <- get_datetime()
  attrs <- list(programmeTitle, progCreationTime )
  names(attrs) <- c("programmeTitle", "creationTime")
  status[["PROGRAMMES"]][[programmeName]] <- attrs
  # can retrieve the creationTime with call to:
   #status[["PROGRAMMES"]][[programmeName]][["creationTime"]]

  # Write status list to the statusFile:
  yaml::write_yaml(yaml::as.yaml(status), statusFile)

  cat( "  Written PROGRAMME to Status.yml file: ", statusFile, "\n" )
}

#' Link Programme Index to Project Org Index
#'
#' Fill with default Programme Summary - updated with projectmanagr::update()
#'
link_prog_to_org_index <- function(progPath, programmeName, progContents,
                                   tempPath, progSummaryTemplate,
                                   orgPath, settings) {

  # define index paths
  progIndex <- get_index_prog(progPath, settings)
  orgIndex <- get_index_org(orgPath, settings)

  progSummContents <- read_file( fs::path(tempPath, progSummaryTemplate) )

  progSummary <- extract_prog_summ(progContents, orgIndex, progIndex,
                                   orgPath, settings)

  progSummContents <- init_template_prog_summ(progSummContents, programmeName,
                                              orgIndex, progIndex, progSummary,
                                              orgPath, settings)

  insert_prog_summ_org(progSummContents, orgIndex, orgPath, settings)

  return(progIndex)

}


#' extract string vector that contains the programme summary - between indices
#' of settings: `ProgSummaryHeader` & `ProgSummaryFooter` & after `orgLink`
extract_prog_summ <- function(progContents, orgIndex, progIndex,
                              orgPath, settings) {

  # collect string from programme summary section to paste into org index

  orgLink <- create_hyperlink_no_ext(orgIndex, progIndex)

  # first identify the indices between which the programme summary exists in prog Rmd
  progSummaryHeadIndex <- match_line_index(
    load_param_vector(settings[["ProgSummaryHeader"]], orgPath),
    progContents)
  progSummaryFootIndex <- grep_line_index_from(
    load_param_vector(settings[["ProgSummaryFooter"]], orgPath),
    progContents, progSummaryHeadIndex, orgPath)-1

  # now get programme summary content to paste into org Rmd
  summaryContents <- progContents[progSummaryHeadIndex:progSummaryFootIndex]
  # extract the summary vector AFTER the hyperlink from programme to org!
  progSummary <- summaryContents[
    (match_line_index(orgLink, summaryContents) +1 ) :
      length(summaryContents) ]

  return(progSummary)

}

init_template_prog_summ <- function(progSummContents, programmeName,
                                    orgIndex, progIndex, progSummary,
                                    orgPath, settings) {

  # fill progSummContents with correct content
  progSummContents <- sub_template_param(
    progSummContents, "{{PROG_SUMMARY_SEP}}",
    settings[["OrgProgrammeSummarySep"]], orgPath)

  progSummaryTitle <- paste0(settings[["ProgSummaryTitle"]], programmeName)
  progSummContents <- gsub("{{PROG_SUMMARY_TITLE}}", progSummaryTitle,
                           progSummContents, fixed=TRUE)

  progLink <- create_hyperlink_no_ext(progIndex, orgIndex)
  progSummContents <- gsub("{{PROG_LINK}}", progLink,
                           progSummContents, fixed=TRUE)

  progSummContents <- sub_template_param(progSummContents,
                                         "{{PROG_SUMMARY}}",
                                         progSummary, orgPath)

  return(progSummContents)
}

insert_prog_summ_org <- function(progSummContents, orgIndex, orgPath, settings) {
  # Next, insert progSummary into orgContents

  # read Organisation File from ORG
  orgContents <- read_file(orgIndex)

  # find programme header -> footer indices
  orgProgrammeHeaderIndex <- match_line_index(
    load_param_vector(settings[["OrgProgrammeHeader"]], orgPath),
    orgContents) # finds FIRST MATCH
  orgProgrammeFooterIndex <- grep_line_index_from(
    load_param_vector(settings[["OrgProgrammeFooter"]], orgPath),
    orgContents, orgProgrammeHeaderIndex, orgPath)

  # simply insert progSummaryVector at END of orgProgramme summary in orgContents
  #at orgProgrammeFooterIndex
  orgContents <- insert_at_indices(orgContents, orgProgrammeFooterIndex,
                                   progSummContents)

  # write to orgIndex
  write_file(orgContents, orgIndex)

  cat( "  Written Programme Link to Org Index: ", basename(orgIndex), "\n" )

} #### ________________________________ ####


#' Create Programme Section
#'
#' Generates a new programme section within an existing programme directory,
#' creating the necessary directory structure, R Markdown index file, and
#' linking to its programme or parent section index file.
#'
#' @param sectionName Character. The name of the new programme section. Must not
#'   contain spaces.
#' @param sectionParentPath Character. The file path to an existing organisation
#'   directory, within a programme.
#' @param authorValue Character. The author of the section. Defaults to system
#'   username.
#' @param sectionTitle Character. The title of the programme. If empty, it will
#'   be derived from `programmeName`.
#' @param sectTemplate Character. The filename of the programme template. Default
#'   is `"Programme-Section-Template.Rmd"`.
#' @param sectSummaryTemplate Character. The filename of the programme summary
#'   template. Default is `"Programme-Section-Summary-Template.Rmd"`.
#'
#' @details
#' This function performs the following steps:
#' 1. Ensures `sectionName` does not contain spaces.
#' 2. Identifies the root organisation directory from `sectionParentPath`.
#' 3. Creates a new programme sdction directory within the organisation.
#' 4. Generates a new programme section index Rmd file from a template.
#' 5. Links and inserts the programme section into its parent's index file.
#'
#' The function ensures that all directories and files are properly created and
#' linked within the project structure.
#'
#' @return Character. The file path of the created programme index Rmd file.
#'
#' @examples
#' \dontrun{
#' # Create a new programme in an organisation
#' create_programme_section(
#'   programmeName = "programme-section",
#'   organisationPath = "/path/to/organisation/programme",
#'   authorValue = "sjwest",
#'   programmeTitle = "Programme Section",
#'   progTemplate = "Programme-Section-Template.Rmd",
#'   progSummaryTemplate = "Programme-Section-Summary-Template.Rmd"
#' )
#' }
#'
#' @seealso
#' - \code{\link{find_org_directory}} to locate the organisation directory.
#' - \code{\link{get_settings_yml}} for retrieving configuration settings.
#' - \code{\link{write_file}} for writing content to files.
#'
#' @note
#' - The `sectionName` should not contain spaces.
#' - The function modifies files directly on disk. Ensure backups are created
#'   before running.
#' - The function relies on a structured organisation directory; incorrect
#'   structures may cause failures.
#'
#' @export
create_programme_section <- function(sectionName, sectionParentPath,
                             authorValue=Sys.info()["user"], sectionTitle="",
                             sectTemplate="Programme-Section-Template.Rmd",
                             sectSummaryTemplate = "Programme-Section-Summary-Template.Rmd") {

  cat( "\nprojectmanagr::create_programme_section():\n" )

  #### define and check args ####

  # get the orgPath from sectionParentPath
  orgPath <- confirm_find_org(sectionParentPath)

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  sectionParentPath <- check_section_name_pir(sectionParentPath, sectionName)
  sectionTitle <- define_section_title(sectionTitle, sectionName)

  # parentIndex can be to a parent programme section or programme index Rmd
  parentIndex <- confirm_parent_index(sectionParentPath, authorValue, sectTemplate,
                                      sectSummaryTemplate, settings)


  #### Create Programme Section ####

  tryCatch({

    sectPath <- create_sect_dir(sectionParentPath, sectionName)

    sectContents <- create_sect_index(sectPath, sectionTitle,
                                      parentIndex, tempPath, sectTemplate,
                                      authorValue, orgPath, settings)

    sectIndex <- link_sect_to_parent_index(sectPath, sectionName, sectContents,
                                           parentIndex, tempPath, sectSummaryTemplate,
                                           orgPath, settings)

  }, error = function(e) {
    cat("  ====================  \n")
    cat("  Error encountered:", e$message, "\n")
    cleanup_created(sectPath)
    stop(e)  # Rethrow the error after cleanup
  })

  return(sectIndex)
}


#' check programme section name & parent path
check_section_name_pir <- function(sectionParentPath, sectionName) {
  progPath <- check_prog_subdir(sectionParentPath, settings)
  if( progPath == "" ) { #
    stop( paste0("  sectionParentPath not within a PROGRAMME: ", sectionParentPath) )
  }
  if( grepl("\\s+", sectionName) ) {
    stop( paste0("  sectionName contains a SPACE: ", sectionName) )
  }
  return(sectionParentPath)
}

#' Check sectionTitle, and if blank, fill with sectionName
#' replacing all "_" and "-" with spaces
define_section_title <- function(sectionTitle, sectionName) {
  if( nchar(sectionTitle) == 0 ) {
    sectionTitle <- gsub("-", " ", gsub("_", " ", sectionName) )
  }
  return(sectionTitle)
}


confirm_parent_index <- function(sectionParentPath, authorValue, sectTemplate,
                                 sectSummaryTemplate, settings) {

  if( check_prog_dir(fs::path_dir(sectionParentPath)) == "" ) { # parent is programme

    parentIndex <- get_index_prog(sectionParentPath, settings)

    if( fs::file_exists(parentIndex) == FALSE ) {
      stop( paste0("  organisation Programme not initialised: ",
                   sectionParentPath) )
    }

  } else { # parent is a nested programme section

    parentIndex <- get_index_sect(sectionParentPath, settings)

    if( fs::file_exists(parentIndex) == FALSE ) {
      # Prog Section that has not been initialised
      # So create the parent section index
      create_programme_section(fs::path_file(sectionParentPath),
                               fs::path_dir(sectionParentPath),
                               authorValue, "", sectTemplate, sectSummaryTemplate)
    }
  }
  return(parentIndex)
}


#' Create Programme Section Directory
#'
create_sect_dir <- function(sectionParentPath, sectionName) {

  sectPath <- fs::path(sectionParentPath, sectionName)
  create_directory(sectPath, "  Made Programme Section dir: ",
                   "  Programme Section directory could not be created: ")
  return(sectPath)
}

#' Create Programme Section Index Rmd
#'
#' Fill with contents, return contents
#'
create_sect_index <- function(sectPath, sectionTitle,
                              parentIndex, tempPath, sectTemplate,
                              authorValue, orgPath, settings) {

  # define index path
  sectIndex <- get_index_sect(sectPath, settings)

  # create index Rmd file
  create_file(sectIndex, "  Made Programme Section index file: ",
              "  Programme Section index file could not be created: ")

  # read sectTemplate:
  sectContents <- read_file( fs::path(tempPath, sectTemplate) )

  # modify sectContents to include sectionTitle and authorValue
  sectContents <- gsub("{{TITLE}}", sectionTitle, sectContents, fixed=TRUE)
  sectContents <- gsub("{{AUTHOR}}", authorValue, sectContents, fixed=TRUE)

  # modify summary header/footer
  sectContents <- sub_template_param(sectContents, "{{SUMMARY_HEADER}}",
                                     settings[["SectionSummaryHeader"]], orgPath)
  sectContents <- sub_template_param(sectContents, "{{SUMMARY_FOOTER}}",
                                     settings[["SectionSummaryFooter"]], orgPath)

  # modify sectContents to include relative link to PARENT index Rmd

  parentLink <- create_hyperlink_no_ext(parentIndex, sectIndex)
  sectContents <- gsub("{{PARENTLINK}}", parentLink, sectContents, fixed=TRUE)

  # modify projects header/footer
  sectContents <- sub_template_param(sectContents, "{{PROJECTS_HEADER}}",
                                     settings[["SectionProjectsHeader"]], orgPath)
  sectContents <- sub_template_param(sectContents, "{{PROJECTS_FOOTER}}",
                                     settings[["SectionProjectsFooter"]], orgPath)

  # modify sectContents with rmarkdown-html-header content
  sectContents <- replace_markdown_header(sectContents, orgPath)

  # modify sectContents with SEP values
  sectContents <- replace_sep_values(sectContents, orgPath)

  # write to sectFile
  write_file(sectContents, sectIndex)

  cat( "  Written template to Programme Section index.Rmd file: ", sectIndex, "\n" )

  return(sectContents)

}


get_index_sect <- function(sectPath, settings) {
  fs::path(sectPath,
           paste0(settings[["SectionIndexFileNamePrefix"]],
                  fs::path_file(sectPath), ".Rmd"))
}


#' Link Programme Section Index to Parent Index
#'
#' Fill with default Programme Section Summary - updated with
#' `projectmanagr::update()`
#'
link_sect_to_parent_index <- function(sectPath, sectionName, sectContents,
                                      parentIndex, tempPath, sectSummaryTemplate,
                                      orgPath, settings) {

  # define index path
  sectIndex <- get_index_sect(sectPath, settings)

  sectSummContents <- read_file( fs::path(tempPath, sectSummaryTemplate) )

  sectSummary <- extract_sect_summ(sectContents, parentIndex, sectIndex,
                                   orgPath, settings)

  sectSummContents <- init_template_sect_summ(sectSummContents, sectionName,
                                               parentIndex, sectIndex, sectSummary,
                                              orgPath, settings)

  insert_sect_summ_parent(sectSummContents, parentIndex, orgPath, settings)

  return(sectIndex)

}

#' extract string vector that contains the programme section summary - between
#' indices of settings: `SectionSummaryHeader` & `SectionSummaryFooter` & after
#' `parLink`
extract_sect_summ <- function(sectContents, parentIndex, sectIndex,
                              orgPath, settings) {

  # collect string from programme summary section to paste into org index

  parLink <- create_hyperlink_no_ext(parentIndex, sectIndex)

  # first identify the indices between which the programme summary exists in sect Rmd
  sectSummaryHeadIndex <- match_line_index(
    load_param_vector(settings[["SectionSummaryHeader"]], orgPath),
    sectContents)
  sectSummaryFootIndex <- grep_line_index_from(
    load_param_vector(settings[["SectionSummaryFooter"]], orgPath),
    sectContents, sectSummaryHeadIndex, orgPath)-1

  # now get programme summary content to paste into org Rmd
  summaryContents <- sectContents[sectSummaryHeadIndex:sectSummaryFootIndex]
  # extract the summary vector AFTER the hyperlink from programme to org!
  sectSummary <- summaryContents[
    (match_line_index(parLink, summaryContents) +1 ) :
      length(summaryContents) ]

  return(sectSummary)

}


init_template_sect_summ <- function(sectSummContents, programmeName,
                                    parentIndex, sectIndex, sectSummary,
                                    orgPath, settings) {

  # fill sectSummContents with correct content
  sectSummContents <- sub_template_param(
    sectSummContents, "{{PROG_SECT_SUMMARY_SEP}}",
    settings[["SectionProjectSummarySep"]], orgPath)

  sectSummaryTitle <- paste0(settings[["SectionSummaryTitle"]], programmeName)
  sectSummContents <- gsub("{{PROG_SECT_SUMMARY_TITLE}}", sectSummaryTitle,
                           sectSummContents, fixed=TRUE)

  sectLink <- create_hyperlink_no_ext(sectIndex, parentIndex)
  sectSummContents <- gsub("{{PROG_SECT_LINK}}", sectLink,
                           sectSummContents, fixed=TRUE)

  sectSummContents <- sub_template_param(sectSummContents,
                                         "{{PROG_SECT_SUMMARY}}",
                                         sectSummary, orgPath)

  return(sectSummContents)
}

insert_sect_summ_parent <- function(sectSummContents, parentIndex, orgPath,
                                    settings) {


  # insert sectSummContents into parentContents
  parentContents <- read_file(parentIndex)

  if( check_prog_dir(
    fs::path_dir(fs::path_dir(parentIndex))) == "" ) { # parent is programme

  # find programme header -> footer indices:
  parentHeaderIndex <- match_line_index(
        load_param_vector(settings[["ProgProjectsHeader"]], orgPath),
        parentContents) # finds FIRST MATCH
  parentFooterIndex <- grep_line_index_from(
        load_param_vector(settings[["ProgProjectsFooter"]], orgPath),
        parentContents, parentHeaderIndex, orgPath)

  # insert progSummContents at END of programme Projects section in parentContents
   #at parentFooterIndex
  parentContents <- insert_at_indices(parentContents, parentFooterIndex,
                                      sectSummContents)

  # write to parentIndex
  write_file(parentContents, parentIndex)

  cat( "  Written Programme Section Link to Programme Index: ",
       basename(parentIndex), "\n" )

  } else { # parent is nested programme section
    # find programme section header -> footer indices:
    parentHeaderIndex <- match_line_index(
      load_param_vector(settings[["SectionProjectsHeader"]], orgPath),
      parentContents) # finds FIRST MATCH
    parentFooterIndex <- grep_line_index_from(
      load_param_vector(settings[["SectionProjectsFooter"]], orgPath),
      parentContents, parentHeaderIndex, orgPath)

    # insert sectSummContents at END of programme Projects section in parentContents
    #at parentFooterIndex
    parentContents <- insert_at_indices(parentContents, parentFooterIndex,
                                        sectSummContents)

    # write to parentIndex
    write_file(parentContents, parentIndex)

    cat( "  Written Programme Section Link to Parent Section Index: ",
         basename(parentIndex), "\n" )


  }

} #### ________________________________ ####


#' Create a New Project Document within a Programme
#'
#' Generates a new project document within an existing programme directory,
#' creating the necessary directory structure, R Markdown files, and linking
#' the new project document to the programme or section index file.
#'
#' @param projectPrefix Character. A unique alphanumeric identifier for the project.
#' @param projectName Character. The name of the new project. Must not contain spaces.
#' @param projectParentPath Character. The file path to an existing directory
#'   below a programme.
#' @param authorValue Character. The author of the project document. Defaults to
#'   the system username.
#' @param projectTitle Character. The title of the project. If empty, it will be
#'   derived from `projectName`.
#' @param projDocTemplate Character. The filename of the project document template.
#'   Default is `"Project-Doc-Template.Rmd"`.
#' @param projDocSummaryTemplate Character. The filename of the project document
#'   summary template. Default is `"Project-Doc-Summary-Template.Rmd"`.
#'
#' @details
#' This function performs the following steps:
#' 1. Ensures `projectName` does not contain spaces.
#' 2. Identifies the root organisation directory from `projectParentPath`.
#' 3. Validates that `projectParentPath` is inside a programme directory.
#' 4. Creates a new project directory and project document Rmd file.
#' 5. Populates the project document with metadata such as prefix, title, and author.
#' 6. Updates the programme or section index file to link to the new project.
#'    A section index file defines a programme section, and is created to bridge
#'    the summary of new project documents in a programme section with the main
#'    programme index.
#'
#' The function ensures that all directories and files are properly created and
#' linked within the project structure.
#'
#' @return Character. The file path of the created project document Rmd file.
#'
#' @examples
#' \dontrun{
#' # Create a new project document within a programme
#' create_project_doc(
#'   projectPrefix = "PRJ001",
#'   projectName = "DataAnalysis",
#'   projectParentPath = "/path/to/programme",
#'   authorValue = "sjwest",
#'   projectTitle = "Data Analysis Project",
#'   projDocTemplate = "Project-Doc-Template.Rmd",
#'   projDocSummaryTemplate = "Project-Doc-Summary-Template.Rmd"
#' )
#' }
#'
#' @seealso
#' - \code{\link{find_org_directory}} to locate the organisation directory.
#' - \code{\link{get_settings_yml}} for retrieving configuration settings.
#' - \code{\link{create_directory}} for creating project directories.
#' - \code{\link{write_file}} for writing content to files.
#'
#' @note
#' - The `projectName` should not contain spaces.
#' - The `projectPrefix` should be alphanumeric and unique within the programme.
#' - The function modifies files directly on disk. Ensure backups are created
#'   before running.
#' - The function relies on a structured organisation and programme directory;
#'   incorrect structures may cause failures.
#'
#' @export
create_project_doc <- function(projectPrefix, projectName, projectParentPath,
                               authorValue=Sys.info()["user"], projectTitle="",
                               projDocTemplate="Project-Doc-Template.Rmd",
                               projDocSummaryTemplate="Project-Doc-Summary-Template.Rmd") {

  cat( "\nprojectmanagr::create_project_doc():\n" )

  #### define and check args ####

  # get the orgPath from projectParentPath
  orgPath <- confirm_find_org(projectParentPath)

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  projectParentPath <- check_doc_pir_name_prefix(projectParentPath,
                                                     projectName,
                                                     projectPrefix)
  projectTitle <- define_doc_title(projectTitle, projectName)


  #### Create Project Doc ####

  tryCatch({

    projDocDir <- create_doc_dir(projectParentPath, projectPrefix)

    projDocFilePath <- create_doc_file(projectParentPath, projectPrefix,
                                projectName, settings)

    projDocContents <- fill_doc_file(projDocFilePath, projectPrefix, projectTitle,
                                     tempPath, projDocTemplate, authorValue,
                                     orgPath, settings)

    link_doc_parent(projectParentPath, projDocFilePath, projDocContents,
                    tempPath, projDocSummaryTemplate, authorValue,
                    orgPath, settings)

  }, error = function(e) {
    cat("  ====================  \n")
    cat("  Error encountered:", e$message, "\n")
    cleanup_created( c(projDocFilePath, projDocDir) )
    stop(e)  # Rethrow the error after cleanup
  })

  # return projDocFilePath
  return(projDocFilePath)

}


check_doc_pir_name_prefix <- function(projectParentPath, projectName, projectPrefix) {

  progPath <- check_prog_subdir(projectParentPath, settings)
  if( progPath == "" ) { #
    stop( paste0("  projectParentPath not within a PROGRAMME: ", projectParentPath) )
  }
  # check projectName contains NO SPACES:
  if( grepl("\\s+", projectName) ) {
    stop( paste0("  projectName contains a SPACE: ", projectName) )
  }
  # Check projectPrefix is alphanumeric (no punctuation chars) and unique to programme
  if( grepl('[[:punct:]]', projectPrefix) == TRUE ) {
    stop( paste0("  projectPrefix contains non-alphanumeric characters: ", projectPrefix) )
  }
  projDocDir <- fs::path(projectParentPath, projectPrefix)
  if( file.exists(projDocDir) == TRUE ) {
    stop( paste0("  projectPrefix already used in directory: ", projDocDir) )
  }
  return(projectParentPath)
}


define_doc_title <- function(projectTitle, projectName) {
  # Check projectTitle, and if blank, fill with projectName
  # replacing all "_" and "-" with spaces
  if( nchar(projectTitle)==0 ) {
    projectTitle <- gsub("-", " ", gsub("_", " ", projectName) )
  }
  return(projectTitle)
}


#' Create Project Directory
#'
create_doc_dir <- function(projectParentPath, projectPrefix) {

  projDocDir <- fs::path(projectParentPath, projectPrefix)
  create_directory(projDocDir, "  Made Project dir: ",
                   "  Project directory could not be created: ")
  return(projDocDir)
}


create_doc_file <- function(projectParentPath, projectPrefix,
                            projectName, settings) {

  projDocFilePath <- fs::path(projectParentPath,
                              paste0(projectPrefix, settings[["ProjectPrefixSep"]],
                                     projectName, ".Rmd") )

  # create index Rmd file
  create_file(projDocFilePath, "  Made Project document: ",
              "  Project document could not be created: ")

  return(projDocFilePath)

}

fill_doc_file <- function(projDocFilePath, projectPrefix, projectTitle,
                          tempPath, projDocTemplate,
                          authorValue, orgPath, settings) {


  # read project doc template:
  projDocContents <- read_file( fs::path(tempPath, projDocTemplate) )

  # modify projDocContents to include PREFIX projectTitle author
  projDocContents <- gsub("{{PREFIX}}", projectPrefix, projDocContents, fixed=TRUE)
  projDocContents <- gsub("{{TITLE}}", projectTitle, projDocContents, fixed=TRUE)
  projDocContents <- gsub("{{AUTHOR}}", authorValue, projDocContents, fixed=TRUE)

  # modify programme summary header/footer
  projDocContents <- sub_template_param(projDocContents, "{{SUMMARY_HEADER}}",
                                        settings[["ProjectSummaryHeader"]], orgPath)
  projDocContents <- sub_template_param(projDocContents, "{{SUMMARY_FOOTER}}",
                                        settings[["ProjectSummaryFooter"]], orgPath)

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

  return(projDocContents)

}


link_doc_parent <- function(projectParentPath, projDocFilePath, projDocContents,
                            tempPath, projDocSummaryTemplate, authorValue,
                            orgPath, settings) {

  # get programme path
  progPath <- check_prog_subdir(projectParentPath, settings)

  if( projectParentPath == progPath ) { # link doc to programme

    projDocContents <- link_prog_index_to_doc(progPath, projDocFilePath,
                                              projDocContents, settings)

    link_doc_to_prog_index(progPath, tempPath, projDocSummaryTemplate,
                           projDocFilePath, projDocContents, orgPath, settings)

  } else { # link doc to programme section

    # first check projectParentPath has prog section index - if not create one
    sectPath <- check_prog_section(projectParentPath, authorValue, settings)

    projDocContents <- link_sect_index_to_doc(sectPath, projDocFilePath,
                                              projDocContents, settings)

    link_doc_to_sect_index(sectPath, tempPath, projDocSummaryTemplate,
                           projDocFilePath, projDocContents, orgPath, settings)
  }

}

link_prog_index_to_doc <- function(progPath, projDocFilePath,
                                   projDocContents, settings) {

  programmeName <-basename(progPath)

  progLink <- hyperlink_doc_to_prog_index(progPath, programmeName,
                                          projDocFilePath, settings)

  projDocContents <- gsub("{{PROGLINK}}", progLink, projDocContents, fixed=TRUE)

  # write to projDocFilePath
  write_file(projDocContents, projDocFilePath)
  cat( "  Written template to Project document: ", projDocFilePath, "\n" )

  return(projDocContents)

}

hyperlink_doc_to_prog_index <- function(progPath, programmeName, projDocFilePath,
                                   settings) {
  progFilePath <- fs::path(progPath,
                           paste0(settings[["ProgIndexFileNamePrefix"]],
                                  programmeName, ".Rmd") )
  progLink <- create_hyperlink(programmeName, progFilePath, projDocFilePath)
  return(progLink)

}


link_doc_to_prog_index <- function(progPath, tempPath, projDocSummaryTemplate,
                                   projDocFilePath, projDocContents, orgPath,
                                   settings) {

  programmeName <-basename(progPath)

  progFilePath <- fs::path( progPath,
                            paste0(settings[["ProgIndexFileNamePrefix"]],
                                   programmeName, ".Rmd") )

  progLink <- hyperlink_doc_to_prog_index(progPath, programmeName, projDocFilePath,
                                     settings)

  # read Programme File from ORG
  progContents <- read_file( progFilePath )

  # read programme summary header template
  projDocSummaryTemplateContents <- read_file( fs::path(tempPath, projDocSummaryTemplate) )

  # create hyperlink from prog to projDoc - to insert in prog summary of projDoc
  projDocName <- substring( basename(projDocFilePath),
                            first=1, last=nchar(basename(projDocFilePath))-4)
  projDocLink <- create_hyperlink( projDocName,
                                   projDocFilePath, progFilePath )

  # collect string from projDoc summary section to paste into prog index
  # first identify the indices between which the projDoc summary exists in projDoc Rmd
  projSummaryHeadIndex <- match_line_index(
    load_param_vector(settings[["ProjectSummaryHeader"]], orgPath),
    projDocContents)
  projSummaryFootIndex <- grep_line_index_from(
    load_param_vector(settings[["ProjectSummaryFooter"]], orgPath),
    projDocContents, projSummaryHeadIndex, orgPath)-1
  # now get projDoc summary content to paste into prog Rmd
  summaryContents <- projDocContents[projSummaryHeadIndex:projSummaryFootIndex]
  projSummary <- summaryContents[
    (match_line_index(progLink, summaryContents) +1 ) : length(summaryContents) ]


  # fill projDocSummaryTemplateContents with correct content
  projSummaryTitle <- paste0(settings[["ProjectSummaryTitle"]], projDocName)
  projDocSummaryTemplateContents <- gsub("{{PROJECT_DOC_SUMMARY_TITLE}}",
                                         projSummaryTitle,
                                         projDocSummaryTemplateContents, fixed=TRUE)
  projDocSummaryTemplateContents <- gsub("{{PROJECT_DOC_LINK}}",
                                         projDocLink,
                                         projDocSummaryTemplateContents, fixed=TRUE)

  projDocSummaryTemplateContents <- sub_template_param(
    projDocSummaryTemplateContents,
    "{{PROJECT_DOC_SUMMARY_SEP}}",
    settings[["ProgProjectSummarySep"]], orgPath)

  projDocSummaryTemplateContents <- sub_template_param(
    projDocSummaryTemplateContents,
    "{{PROJECT_DOC_SUMMARY}}",
    projSummary, orgPath)


  # compute location in prog to insert the doc summary
  progProjDocHeaderIndex <- match_line_index(
    load_param_vector(settings[["ProgProjectsHeader"]], orgPath),
    progContents) # finds FIRST MATCH

  progProjDocFooterIndex <- grep_line_index_from(
    load_param_vector(settings[["ProgProjectsFooter"]], orgPath),
    progContents, progProjDocHeaderIndex, orgPath)

  # insert projDocSummaryTemplateContents at END of project summary in progContents
  # progProjDocFooterIndex
  progContents <- insert_at_indices(
    progContents,
    progProjDocFooterIndex,
    projDocSummaryTemplateContents )

  # write to progFilePath
  write_file(progContents, progFilePath)

  cat( "  Written Project Doc to Programme File: ", basename(progFilePath), "\n" )

}

check_prog_section <- function(projectParentPath, authorValue, settings) {

  # define index path
  sectIndex <- get_index_sect(projectParentPath, settings)

  if( fs::file_exists(sectIndex) == FALSE ) { # create programme section!

    cat( "  Programme Section Index not detected - generating Programme Section: ",
         projectParentPath, "\n" )
    create_programme_section(fs::path_file(projectParentPath),
                             fs::path_dir(projectParentPath),
                             authorValue)
  } else {
    cat( "  Programme Section Index detected: ", sectIndex, "\n" )
  }

  return(projectParentPath)

}


link_sect_index_to_doc <- function(sectPath, projDocFilePath,
                                   projDocContents, settings) {

  sectionName <-basename(sectPath)

  sectLink <- hyperlink_doc_to_sect_index(sectPath, sectionName,
                                          projDocFilePath, settings)

  projDocContents <- gsub("{{PROGLINK}}", sectLink, projDocContents, fixed=TRUE)

  # write to projDocFilePath
  write_file(projDocContents, projDocFilePath)
  cat( "  Written template to Project document: ", projDocFilePath, "\n" )

  return(projDocContents)

}

hyperlink_doc_to_sect_index <- function(sectPath, sectionName, projDocFilePath,
                                        settings) {
  sectFilePath <- fs::path(sectPath,
                           paste0(settings[["SectionIndexFileNamePrefix"]],
                                  sectionName, ".Rmd") )
  sectLink <- create_hyperlink(sectionName, sectFilePath, projDocFilePath)
  return(sectLink)

}


link_doc_to_sect_index <- function(sectPath, tempPath, projDocSummaryTemplate,
                                   projDocFilePath, projDocContents, orgPath,
                                   settings) {

  sectionName <-basename(sectPath)

  sectFilePath <- fs::path(sectPath,
                           paste0(settings[["SectionIndexFileNamePrefix"]],
                                  sectionName, ".Rmd") )

  sectLink <- hyperlink_doc_to_sect_index(sectPath, sectionName, projDocFilePath,
                                          settings)

  # read Programme Section Index
  sectContents <- read_file( sectFilePath )

  # read doc summary header template
  projDocSummaryTemplateContents <- read_file( fs::path(tempPath, projDocSummaryTemplate) )

  # create hyperlink from sect to projDoc - to insert in sect summary of projDoc
  projDocName <- substring( basename(projDocFilePath),
                            first=1, last=nchar(basename(projDocFilePath))-4)
  projDocLink <- create_hyperlink( projDocName,
                                   projDocFilePath, sectFilePath )

  # collect string from projDoc summary section to paste into sect index
  # first identify the indices between which the projDoc summary exists in projDoc Rmd
  projSummaryHeadIndex <- match_line_index(
    load_param_vector(settings[["ProjectSummaryHeader"]], orgPath),
    projDocContents)
  projSummaryFootIndex <- grep_line_index_from(
    load_param_vector(settings[["ProjectSummaryFooter"]], orgPath),
    projDocContents, projSummaryHeadIndex, orgPath)-1
  # now get projDoc summary content to paste into sect Rmd
  summaryContents <- projDocContents[projSummaryHeadIndex:projSummaryFootIndex]
  projSummary <- summaryContents[
    (match_line_index(sectLink, summaryContents) +1 ) : length(summaryContents) ]


  # fill projDocSummaryTemplateContents with correct content
  projSummaryTitle <- paste0(settings[["ProjectSummaryTitle"]], projDocName)
  projDocSummaryTemplateContents <- gsub("{{PROJECT_DOC_SUMMARY_TITLE}}",
                                         projSummaryTitle,
                                         projDocSummaryTemplateContents, fixed=TRUE)
  projDocSummaryTemplateContents <- gsub("{{PROJECT_DOC_LINK}}",
                                         projDocLink,
                                         projDocSummaryTemplateContents, fixed=TRUE)

  projDocSummaryTemplateContents <- sub_template_param(
    projDocSummaryTemplateContents,
    "{{PROJECT_DOC_SUMMARY_SEP}}",
    settings[["SectionProjectSummarySep"]], orgPath)

  projDocSummaryTemplateContents <- sub_template_param(
    projDocSummaryTemplateContents,
    "{{PROJECT_DOC_SUMMARY}}",
    projSummary, orgPath)


  # compute location in sect to insert the doc summary
  sectProjDocHeaderIndex <- match_line_index(
    load_param_vector(settings[["SectionProjectsHeader"]], orgPath),
    sectContents) # finds FIRST MATCH

  sectProjDocFooterIndex <- grep_line_index_from(
    load_param_vector(settings[["SectionProjectsFooter"]], orgPath),
    sectContents, sectProjDocHeaderIndex, orgPath)

  # insert projDocSummaryTemplateContents at END of project summary in sectContents
  # sectProjDocFooterIndex
  sectContents <- insert_at_indices(
    sectContents,
    sectProjDocFooterIndex,
    projDocSummaryTemplateContents )

  # write to sectFilePath
  write_file(sectContents, sectFilePath)

  cat( "  Written Project Doc to Programme Section File: ", basename(sectFilePath), "\n" )

}


#### ________________________________ ####



#' Create a New Project Note within a Project Document
#'
#' Generates a new project note within an existing project document, creating
#' the necessary directory structure, R Markdown files, and linking it to the
#' corresponding goal, deliverable, or task in the project document.
#'
#' @param projectNoteName Character. The name of the new project note. Must not
#'   contain spaces.
#' @param projectNotePath Character. The file path where the project note will
#'   be created.
#' @param selection List. Contains metadata about the selected project document.
#'   Must include:
#'   - \code{selection$rmdType}: Must be `"DOC"` to indicate a valid project doc.
#'   - \code{selection$filePath}: The file path of the project document.
#'   - \code{selection$goal}: The goal under which the note is being created.
#'   - \code{selection$deliverable}: The deliverable related to the note.
#'   - \code{selection$task}: The specific task that the note is linked to.
#'   - \code{selection$taskLine}: The line number in the project document where
#'     the task is defined.
#' @param authorValue Character. The author of the project note. Defaults to the
#'   system username.
#' @param projectNoteTitle Character. The title of the project note. If empty,
#'   it will be derived from `projectNoteName`.
#' @param projNoteTemplate Character. The filename of the project note template.
#'   Default is `"Project-Note-Template.Rmd"`.
#' @param projNoteLinkTemplate Character. The filename of the project note link
#'   template. Default is `"Project-Note-Link-Template.Rmd"`.
#' @param projNoteLinkSummaryTemplate Character. The filename of the project note
#'   link summary template. Default is `"Project-Note-Link-Summary-Template.Rmd"`.
#' @param todoTemplate Character. The filename of the project note's To-Do
#'   template. Default is `"Todo-Template.Rmd"`.
#' @param projNoteSummaryTemplate Character. The filename of the project note
#'   summary template. Default is `"Project-Note-Summary-Template.Rmd"`.
#'
#' @details
#' This function performs the following steps:
#' 1. Ensures `projectNoteName` does not contain spaces.
#' 2. Validates that `selection$rmdType` is `"DOC"`, ensuring a valid project doc.
#' 3. Identifies the root organisation directory from `projectNotePath`.
#' 4. Creates a new project note directory and an associated R Markdown file.
#' 5. Populates the project note with metadata such as title, author, and prefix.
#' 6. Links the new project note to the corresponding goal, deliverable, or task
#'    in the project document.
#' 7. Inserts a summary of the project note into the project document.
#'
#' The function ensures that all directories and files are properly created and
#' linked within the project structure.
#'
#' @return None. The function creates files and modifies the project document.
#'
#' @examples
#' \dontrun{
#' # Create a new project note within a project document
#' create_project_note(
#'   projectNoteName = "Experiment1",
#'   projectNotePath = "/path/to/project/notes",
#'   selection = list(
#'     rmdType = "DOC",
#'     filePath = "/path/to/project.Rmd",
#'     goal = "Define research objective",
#'     deliverable = "Data collection",
#'     task = "Set up experiment",
#'     taskLine = 42
#'   ),
#'   authorValue = "sjwest",
#'   projectNoteTitle = "First Experiment",
#'   projNoteTemplate = "Project-Note-Template.Rmd",
#'   projNoteLinkTemplate = "Project-Note-Link-Template.Rmd",
#'   projNoteLinkSummaryTemplate = "Project-Note-Link-Summary-Template.Rmd",
#'   todoTemplate = "Todo-Template.Rmd",
#'   projNoteSummaryTemplate = "Project-Note-Summary-Template.Rmd"
#' )
#' }
#'
#' @seealso
#' - \code{\link{find_org_directory}} to locate the organisation directory.
#' - \code{\link{get_settings_yml}} for retrieving configuration settings.
#' - \code{\link{write_file}} for writing content to files.
#' - \code{\link{link_project_note_doc}} for linking project notes to project docs.
#'
#' @note
#' - The `projectNoteName` should not contain spaces.
#' - The function modifies files directly on disk. Ensure backups are created
#'   before running.
#' - The function relies on a structured organisation and project directory;
#'   incorrect structures may cause failures.
#' - If a project note link already exists in the document, the function will
#'   fail and remove the newly created note.
#' - The `selection` list must contain `goal`, `deliverable`, and `task` values,
#'   as they determine where the project note link will be inserted.
#'
#' @export
create_project_note <- function( projectNoteName, projectNotePath,
                                 selection, authorValue=Sys.info()["user"],
                                 projectNoteTitle="",
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

  projectNotePath <- fs::path_expand(projectNotePath)

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

  linkFormed <- link_project_note_doc(selection, settings, projNoteRmdPath,
                                      projNoteRmdContents, projNoteLinkContents,
                                      projNoteLinkSummaryContents, todoContents,
                                      projNoteSummaryContents, projDocContents, orgPath)

  if( linkFormed == FALSE ) {
    # remove the project note and directory
    file.remove(projDirPath) # remove the project note dir
    file.remove(projNoteRmdPath) # remove the project note Rmd
    stop( paste0("  Creating Project Note Failed - link already exists."))
  }

} #### ________________________________ ####




#' Create a New Group Note within a Project Document
#'
#' Generates a new group note (header note and sub-note) within an existing
#' project document, creating the necessary directory structure, R Markdown
#' files, and linking it to the corresponding goal, deliverable, or task in
#' the project document.
#'
#' @param groupNoteName Character. The name of the new group note (header note).
#'   Must not contain spaces.
#' @param groupNotePath Character. The file path where the group note will be
#'   created.
#' @param selection List. Contains metadata about the selected project document.
#'   Must include:
#'   - \code{selection$rmdType}: Must be `"DOC"` to indicate a valid project doc.
#'   - \code{selection$filePath}: The file path of the project document.
#'   - \code{selection$goal}: The goal under which the note is being created.
#'   - \code{selection$deliverable}: The deliverable related to the note.
#'   - \code{selection$task}: The specific task that the note is linked to.
#'   - \code{selection$taskLine}: The line number in the project document where
#'     the task is defined.
#' @param subNoteName Character. The name of the sub-note associated with the
#'   header note.
#' @param authorValue Character. The author of the group note. Defaults to the
#'   system username.
#' @param addObjToHeader Logical. If TRUE, the project document link is added to
#'   the header note instead of the sub-note.
#' @param groupNoteTitle Character. The title of the group note. If empty, it
#'   will be derived from `groupNoteName`.
#' @param subNoteTitle Character. The title of the sub-note. If empty, it will
#'   be derived from `subNoteName`.
#' @param projNoteTemplate Character. The filename of the header note template.
#'   Default is `"Project-Header-Note-Template.Rmd"`.
#' @param subNoteTemplate Character. The filename of the sub-note template.
#'   Default is `"Project-Sub-Note-Template.Rmd"`.
#' @param headerNoteContentLinkTemplate Character. The filename of the header
#'   note content link template. Default is `"Project-Header-Note-Content-Link-Template.Rmd"`.
#' @param subNoteContentLinkTemplate Character. The filename of the sub-note
#'   content link template. Default is `"Project-Sub-Note-Content-Link-Template.Rmd"`.
#' @param projNoteLinkTemplate Character. The filename of the project note link
#'   template. Default is `"Project-Note-Link-Template.Rmd"`.
#' @param projNoteLinkSummaryTemplate Character. The filename of the project note
#'   link summary template. Default is `"Project-Note-Link-Summary-Template.Rmd"`.
#' @param todoTemplate Character. The filename of the project note's To-Do
#'   template. Default is `"Todo-Template.Rmd"`.
#' @param projNoteSummaryTemplate Character. The filename of the project note
#'   summary template. Default is `"Project-Note-Summary-Template.Rmd"`.
#' @param subNoteSummaryTemplate Character. The filename of the sub-note summary
#'   template. Default is `"Project-Sub-Note-Summary-Template.Rmd"`.
#'
#' @details
#' This function performs the following steps:
#' 1. Ensures `groupNoteName` and `subNoteName` do not contain spaces.
#' 2. Validates that `selection$rmdType` is `"DOC"`, ensuring a valid project doc.
#' 3. Identifies the root organisation directory from `groupNotePath`.
#' 4. Creates a new header note directory and an associated R Markdown file.
#' 5. Creates a sub-note directory and its corresponding R Markdown file.
#' 6. Populates both the header note and sub-note with metadata such as title,
#'    author, and prefix.
#' 7. Links the group note to the corresponding goal, deliverable, or task in
#'    the project document.
#' 8. Inserts a summary of the group note into the project document.
#'
#' The function ensures that all directories and files are properly created and
#' linked within the project structure.
#'
#' @return None. The function creates files and modifies the project document.
#'
#' @examples
#' \dontrun{
#' # Create a new group note within a project document
#' create_group_note(
#'   groupNoteName = "ExperimentOverview",
#'   groupNotePath = "/path/to/project/notes",
#'   selection = list(
#'     rmdType = "DOC",
#'     filePath = "/path/to/project.Rmd",
#'     goal = "Define research objective",
#'     deliverable = "Data collection",
#'     task = "Set up experiment",
#'     taskLine = 42
#'   ),
#'   subNoteName = "Trial1",
#'   authorValue = "sjwest",
#'   addObjToHeader = TRUE,
#'   groupNoteTitle = "Overview of Experiments",
#'   subNoteTitle = "Trial 1 Details",
#'   projNoteTemplate = "Project-Header-Note-Template.Rmd",
#'   subNoteTemplate = "Project-Sub-Note-Template.Rmd",
#'   headerNoteContentLinkTemplate = "Project-Header-Note-Content-Link-Template.Rmd",
#'   subNoteContentLinkTemplate = "Project-Sub-Note-Content-Link-Template.Rmd",
#'   projNoteLinkTemplate = "Project-Note-Link-Template.Rmd",
#'   projNoteLinkSummaryTemplate = "Project-Note-Link-Summary-Template.Rmd",
#'   todoTemplate = "Todo-Template.Rmd",
#'   projNoteSummaryTemplate = "Project-Note-Summary-Template.Rmd",
#'   subNoteSummaryTemplate = "Project-Sub-Note-Summary-Template.Rmd"
#' )
#' }
#'
#' @seealso
#' - \code{\link{find_org_directory}} to locate the organisation directory.
#' - \code{\link{get_settings_yml}} for retrieving configuration settings.
#' - \code{\link{write_file}} for writing content to files.
#' - \code{\link{link_project_note_doc}} for linking project notes to project docs.
#' - \code{\link{link_group_note_doc}} for linking group notes to project docs.
#'
#' @note
#' - The `groupNoteName` and `subNoteName` should not contain spaces.
#' - The function modifies files directly on disk. Ensure backups are created
#'   if necessary before running.
#' - The function relies on a structured organisation and project directory;
#'   incorrect structures may cause failures.
#' - If a group note link already exists in the document, the function will
#'   fail and remove the newly created note.
#' - The `selection` list must contain `goal`, `deliverable`, and `task` values,
#'   as they determine where the group note link will be inserted.
#' - If `addObjToHeader` is `TRUE`, the project document link is also added to the
#'   header note; otherwise, it is added to the sub-note only.
#'
#' @export
create_group_note  <- function( groupNoteName, groupNotePath,
                                selection, subNoteName, authorValue=Sys.info()["user"],
                                addObjToHeader=TRUE,
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
  groupNotePath <- fs::path_expand(groupNotePath)


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
                                                 headerNoteRmdContents, noteContentsHeadIndex, orgPath)

  headerNoteRmdContents <- insert_at_indices(headerNoteRmdContents, noteContentsFootIndex, subNoteContentLinkContents)


  #### write Header Note ####

  write_file(headerNoteRmdContents, headerNoteRmdPath)

  cat( "  Made Project Header Note: ", headerNoteRmdPath, "\n" )


  #### SUBNOTE : Replace markup with values ####

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

#' Insert Header Link into Subnote
#'
#' This function inserts a hyperlink to the header note into the subnote
#' contents. It creates the link using the header note file name and path,
#' replaces the "{{SUB_NOTE_CONTENT_LINK}}" placeholder in the provided
#' template, and inserts the resulting content into the subnote.
#'
#' @param subNoteContents Character vector containing the contents of the
#'        subnote Rmd file.
#' @param headerNoteFileName A string specifying the header note file name.
#' @param headerNoteRmdPath A string specifying the full file path of the
#'        header note Rmd.
#' @param subNoteRmdPath A string specifying the full file path of the
#'        subnote Rmd.
#' @param headerNoteContentLinkContents Character vector containing the template
#'        for the header link content.
#' @param settings A list of configuration settings used for loading parameter
#'        vectors and template values.
#' @param orgPath A string specifying the root directory path of the
#'        organisation.
#'
#' @details The function creates a hyperlink by calling
#' \code{create_hyperlink()} with the header note file name and paths. It then
#' substitutes the placeholder \code{"{{SUB_NOTE_CONTENT_LINK}}"} in the given
#' template using \code{sub_template_param()}. The insertion point in the
#' subnote is determined by locating header and footer markers using
#' \code{load_param_vector()}, \code{match_line_index()}, and
#' \code{grep_line_index_from()}. Finally, the generated link content is
#' inserted into the subnote via \code{insert_at_indices()}.
#'
#' @note This function assumes that the subnote contains valid header and
#' footer markers as defined in the settings. Missing markers may lead to
#' unexpected behavior.
#'
#' @examples
#' \dontrun{
#' # Read subnote contents and header link template.
#' subNoteContents <- read_file("subnote.Rmd")
#' headerTemplate <- read_file("HeaderContentLinkTemplate.Rmd")
#'
#' # Define header note details.
#' headerFileName <- "header_note.Rmd"
#' headerPath <- "/path/to/header_note.Rmd"
#' subNotePath <- "/path/to/subnote.Rmd"
#'
#' # Load project settings and organisation path.
#' settings <- get_settings_yml("/path/to/org")
#' orgPath <- "/path/to/org"
#'
#' # Insert the header link into the subnote contents.
#' newSubNoteContents <- insert_header_link_subnote(subNoteContents,
#'                            headerFileName, headerPath, subNotePath,
#'                            headerTemplate, settings, orgPath)
#'
#' # Write the updated subnote contents back to file.
#' write_file(newSubNoteContents, subNotePath)
#' }
#'
#' @seealso create_hyperlink, sub_template_param, load_param_vector,
#'          match_line_index, grep_line_index_from, insert_at_indices
insert_header_link_subnote <- function(subNoteContents, headerNoteFileName,
                                       headerNoteRmdPath, subNoteRmdPath,
                                       headerNoteContentLinkContents,
                                       settings, orgPath) {


  #### Insert header link content into subnote ####

  headerNoteContentLink <- create_hyperlink( headerNoteFileName, headerNoteRmdPath, subNoteRmdPath)
  headerNoteContentLinkContents <- sub_template_param(headerNoteContentLinkContents,
                                                      "{{SUB_NOTE_CONTENT_LINK}}",
                                                      headerNoteContentLink, orgPath)

  noteContentsHeadIndex <- match_line_index( load_param_vector(settings[["SubNoteContentsHeader"]], orgPath),
                                             subNoteContents)
  noteContentsFootIndex <- grep_line_index_from( load_param_vector(settings[["SubNoteContentsFooter"]], orgPath),
                                                 subNoteContents, noteContentsHeadIndex, orgPath)

  subNoteContents <- insert_at_indices(subNoteContents, noteContentsFootIndex, headerNoteContentLinkContents)

  subNoteContents # return

} #### ________________________________ ####


#' Add a New Sub Note to a Project Group
#'
#' This function creates a new Sub Note within a Project Group. It reads the
#' necessary template files, creates the required sub-directory and R Markdown
#' file for the Sub Note, replaces template markup with provided values, and
#' updates the Project Group Header Note with a link to the new Sub Note. It
#' then calls link_sub_note_doc() to insert all relevant links and summaries into
#' the main project document.
#'
#' @param subNoteName A string representing the name of the Project Sub Note.
#' Spaces are replaced with '-' or '_' by default.
#'
#' @param subNotePath A character string specifying the absolute directory
#' where the Sub Note will be stored. It must reside within the Project Group
#' Note Directory.
#'
#' @param selection A list with metadata about the current selection. The list
#' must include the following keys:
#'   - rmdType: A string specifying the file type. Valid values are "DOC",
#'     "HEAD", or "SUB".
#'   - filePath: The file path of the associated project document.
#'   - headerNoteLink: A relative link to the header note (used when rmdType is
#'     "DOC").
#'   - goal: A string defining the goal associated with the note.
#'   - deliverable: A string defining the deliverable linked to the note.
#'   - task: A string defining the task associated with the note.
#'   - taskLine: An integer indicating the line number in the project document
#'     where the task is defined.
#'   - addingSubNote: (Optional) A boolean that must be TRUE if rmdType is "DOC".
#'
#' @param authorValue A string representing the author's name for the note.
#' Defaults to the current system user.
#'
#' @param subNoteTitle (Optional) A title for the Sub Note. If blank, the
#' subNoteName is used with underscores and dashes replaced by spaces.
#'
#' @param subNoteTemplate A template file used to generate the Sub Note.
#' Default is "Project-Sub-Note-Template.Rmd".
#'
#' @param headerNoteContentLinkTemplate A template file for creating a content
#' link from the header note to the Sub Note. Default is
#' "Project-Header-Note-Content-Link-Template.Rmd".
#'
#' @param subNoteContentLinkTemplate A template file for creating a content link
#' within the Sub Note. Default is
#' "Project-Sub-Note-Content-Link-Template.Rmd".
#'
#' @param projNoteLinkTemplate A template file that defines the structure of the
#' Project Doc Goal/Del/Task link in the Sub Note. Default is
#' "Project-Note-Link-Template.Rmd".
#'
#' @param projNoteLinkSummaryTemplate A template file that adds a summary and
#' to-do section beneath the Project Doc link in the Sub Note. Default is
#' "Project-Note-Link-Summary-Template.Rmd".
#'
#' @param todoTemplate A template file for the to-do list section. Default is
#' "Todo-Template.Rmd".
#'
#' @param projNoteSummaryTemplate A template file used to add a Project Note
#' summary to the Project Doc. Default is
#' "Project-Note-Summary-Template.Rmd".
#'
#' @param subNoteSummaryTemplate A template file used to add a Sub Note summary
#' to the Project Doc. Default is
#' "Project-Sub-Note-Summary-Template.Rmd".
#'
#' @details
#' The function performs the following steps:
#' 1. Validates that subNoteName contains no spaces.
#' 2. Checks that the selection list is valid. The selection must include
#'    keys such as rmdType, filePath, headerNoteLink, goal, deliverable, task,
#'    and taskLine; if rmdType is "DOC", addingSubNote must be TRUE.
#' 3. Determines the header note Rmd path based on the selection.
#' 4. Reads the required template files for the Sub Note and for header note
#'    content links.
#' 5. Creates a sub-directory for the Sub Note and an associated R Markdown file.
#' 6. Replaces the markup in the Sub Note with provided parameters.
#' 7. Inserts a header link into the Sub Note and updates the Header Note with
#'    the Sub Note link.
#' 8. Calls link_sub_note_doc() to update the project document with all relevant
#'    Sub Note links and summaries.
#'
#' @return This function does not return a value. It creates and updates files on
#' disk to integrate the new Sub Note into the project documentation.
#'
#' @examples
#' \dontrun{
#' create_sub_note(
#'   subNoteName = "analysis_note",
#'   subNotePath = "/projects/my_project/notes",
#'   selection = list(
#'     rmdType = "DOC",
#'     filePath = "project_doc.Rmd",
#'     headerNoteLink = "[Header](header_note.Rmd)",
#'     goal = "Define hypothesis",
#'     deliverable = "Experiment setup",
#'     task = "Run pilot study",
#'     taskLine = 45,
#'     addingSubNote = TRUE
#'   ),
#'   authorValue = "sjwest",
#'   subNoteTitle = "Data Analysis",
#'   subNoteTemplate = "Project-Sub-Note-Template.Rmd"
#' )
#' }
#'
#' @seealso
#' - \code{\link{link_sub_note_doc}} for linking Sub Notes to Project Docs.
#' - \code{\link{find_org_directory}} to locate the organisation directory.
#' - \code{\link{get_settings_yml}} for retrieving project settings.
#' - \code{\link{insert_header_link_subnote}} for inserting header links into Sub
#'   Notes.
#'
#' @note
#' The selection list must include the keys: rmdType, filePath, headerNoteLink,
#' goal, deliverable, task, and taskLine. If rmdType is "DOC", the key
#' addingSubNote must also be TRUE. This function modifies files on disk; ensure
#' you have backups before running it. It relies on a structured project directory
#' and properly configured template files.
#'
#' @export

create_sub_note <- function( subNoteName, subNotePath,
                             selection, authorValue=Sys.info()["user"],
                             subNoteTitle="",
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
  subNotePath <- fs::path_expand(subNotePath)
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

#' Insert Header Link into Subnote
#'
#' This function inserts a hyperlink to a subnote into the header note
#' contents. The link is generated from the subnote file name and its file
#' path, then the function substitutes the "{{HEADER_NOTE_CONTENT_LINK}}"
#' placeholder in the provided template with this hyperlink. Finally, the
#' updated header note contents are returned.
#'
#' @param headerNoteRmdContents Character vector containing the contents of the
#' header note Rmd file.
#' @param subNoteFileName A string specifying the subnote file name.
#' @param subNoteRmdPath A string with the full file path of the subnote Rmd.
#' @param headerNoteRmdPath A string with the full file path of the header note
#' Rmd.
#' @param subNoteContentLinkContents Character vector containing the template
#' for the subnote content link.
#' @param settings A list of configuration settings used to load parameter
#' vectors and template values.
#' @param orgPath A string specifying the root directory path of the
#' organisation.
#'
#' @details The function first creates a hyperlink by calling
#' \code{create_hyperlink()} using the subnote file name and paths. It then
#' replaces the placeholder "{{HEADER_NOTE_CONTENT_LINK}}" in the provided
#' template using \code{sub_template_param()}. The insertion location within the
#' header note is determined by finding header and footer markers loaded from
#' the settings using \code{load_param_vector()}, \code{match_line_index()}, and
#' \code{grep_line_index_from()}. The substituted content is inserted at the
#' footer index via \code{insert_at_indices()}, and the modified header note
#' contents are returned.
#'
#' @note This function assumes that the header note contains the defined
#' header and footer markers from the settings. If these markers are missing,
#' the insertion may not occur as expected.
#'
#' @examples
#' \dontrun{
#' # Read header note contents and subnote template content.
#' headerContents <- read_file("header_note.Rmd")
#' templateContent <- read_file("SubNoteContentLinkTemplate.Rmd")
#'
#' # Define subnote details.
#' subNoteName <- "subNote1.Rmd"
#' subNotePath <- "/path/to/subNote1.Rmd"
#' headerPath <- "/path/to/header_note.Rmd"
#'
#' # Load project settings and organisation path.
#' settings <- get_settings_yml("/path/to/org")
#' orgPath <- "/path/to/org"
#'
#' # Insert the subnote link into the header note contents.
#' newHeaderContents <- insert_subnote_link_header(headerContents,
#'                        subNoteName, subNotePath, headerPath,
#'                        templateContent, settings, orgPath)
#'
#' # Write the updated header note contents back to file.
#' write_file(newHeaderContents, headerPath)
#' }
#'
#' @seealso create_hyperlink, sub_template_param, load_param_vector,
#' match_line_index, grep_line_index_from, insert_at_indices
insert_subnote_link_header <- function(headerNoteRmdContents, subNoteFileName,
                                       subNoteRmdPath, headerNoteRmdPath,
                                       subNoteContentLinkContents,
                                       settings, orgPath) {

  subNoteContentLink <- create_hyperlink( subNoteFileName, subNoteRmdPath, headerNoteRmdPath)
  subNoteContentLinkContents <- sub_template_param(subNoteContentLinkContents,
                                                   "{{HEADER_NOTE_CONTENT_LINK}}",
                                                   subNoteContentLink, orgPath)

  noteContentsHeadIndex <- match_line_index( load_param_vector(settings[["HeaderNoteContentsHeader"]], orgPath),
                                             headerNoteRmdContents)
  noteContentsFootIndex <- grep_line_index_from( load_param_vector(settings[["HeaderNoteContentsFooter"]], orgPath),
                                                 headerNoteRmdContents, noteContentsHeadIndex, orgPath)

  headerNoteRmdContents <- insert_at_indices(headerNoteRmdContents, noteContentsFootIndex, subNoteContentLinkContents)

  headerNoteRmdContents # return

}

#' Link Sub Note to Project Doc GDT
#'
#' This function links a new sub note to its associated project document
#' based on Goal-Deliverable-Task (GDT) information. It extracts GDT data
#' from the link note contents and inserts corresponding links into the sub
#' note and project document.
#'
#' @param selection A list of parameters for the selected Rmd file. Expected
#'   elements include \code{rmdType}, \code{filePath}, and optionally
#'   \code{headerNoteLink} and \code{addingSubNote}.
#' @param settings A list of configuration settings for template processing,
#'   parameter substitution, and marker definitions.
#' @param subNoteRmdPath A string specifying the full file path of the sub note
#'   Rmd file.
#' @param subNoteContents A character vector containing the sub note's content.
#' @param headerNoteRmdPath A string specifying the full file path of the header
#'   note Rmd file.
#' @param headerNoteRmdContents A character vector containing the header note's
#'   content.
#' @param projNoteLinkContents A character vector holding the project note link
#'   template.
#' @param projNoteLinkSummaryContents A character vector holding the project note
#'   link summary template.
#' @param todoContents A character vector holding the todo template.
#' @param projNoteSummaryContents A character vector with the project note summary
#'   template.
#' @param subNoteSummaryContents A character vector with the sub note summary
#'   template.
#' @param linkNoteRmdPath A string specifying the full file path of the link
#'   note Rmd file.
#' @param linkNoteRmdContents A character vector containing the link note's
#'   content.
#' @param orgPath A string specifying the root directory path of the
#'   organisation.
#'
#' @details This internal function integrates a new sub note into an existing
#'   project note group. It performs the following tasks:
#'   \enumerate{
#'     \item Extracts GDT details from the link note using
#'       \code{extract_note_obj_doc_link_GDT_summ()}.
#'     \item Iterates over each GDT to compute hyperlink data via
#'       \code{compute_doc_GDT_link()}.
#'     \item Replaces placeholders in link templates with actual values using
#'       \code{sub_note_link_params()} and \code{sub_template_param()}.
#'     \item Determines insertion points in the sub note using header and footer
#'       markers with \code{match_line_index()} and
#'       \code{grep_line_index_from()}.
#'     \item Inserts the generated links into the sub note and updates the
#'       project document.
#'   }
#'
#' @note This function assumes that the link note and sub note belong to the
#'   same project note group and that required markers and templates are
#'   present. Missing markers or templates may lead to incorrect link insertion.
#'
#' @examples
#' \dontrun{
#' # Example: Linking a new sub note to a project document.
#' selection <- list(rmdType = "SUB", filePath = "/path/to/subnote.Rmd")
#' settings <- get_settings_yml("/path/to/org")
#' subNoteRmdPath <- "/path/to/project/subnotes/subNote1.Rmd"
#' subNoteContents <- read_file(subNoteRmdPath)
#' headerNoteRmdPath <- "/path/to/project/header_note.Rmd"
#' headerNoteRmdContents <- read_file(headerNoteRmdPath)
#' projNoteLinkContents <- read_file("Project-Note-Link-Template.Rmd")
#' projNoteLinkSummaryContents <- read_file(
#'   "Project-Note-Link-Summary-Template.Rmd")
#' todoContents <- read_file("Todo-Template.Rmd")
#' projNoteSummaryContents <- read_file("Project-Note-Summary-Template.Rmd")
#' subNoteSummaryContents <- read_file(
#'   "Project-Sub-Note-Summary-Template.Rmd")
#' linkNoteRmdPath <- "/path/to/project/link_note.Rmd"
#' linkNoteRmdContents <- read_file(linkNoteRmdPath)
#' orgPath <- "/path/to/org"
#'
#' # Link the sub note with GDT-based links.
#' link_sub_note_doc(selection, settings, subNoteRmdPath, subNoteContents,
#'                   headerNoteRmdPath, headerNoteRmdContents,
#'                   projNoteLinkContents, projNoteLinkSummaryContents,
#'                   todoContents, projNoteSummaryContents,
#'                   subNoteSummaryContents, linkNoteRmdPath,
#'                   linkNoteRmdContents, orgPath)
#' }
#'
#' @seealso extract_note_obj_doc_link_GDT_summ, compute_doc_GDT_link,
#'   sub_note_link_params, sub_template_param, insert_at_indices,
#'   load_param_vector, match_line_index, grep_line_index_from, write_file,
#'   create_hyperlink
link_sub_note_doc <- function(selection, settings, subNoteRmdPath, subNoteContents,
                              headerNoteRmdPath, headerNoteRmdContents, projNoteLinkContents,
                              projNoteLinkSummaryContents, todoContents, projNoteSummaryContents,
                              subNoteSummaryContents, linkNoteRmdPath, linkNoteRmdContents, orgPath) {


  #### Set Instance Variables ####

  # subNotePath is the parent directory the subNoteRmdPath (Rmd file) sits in
  subNotePath <- fs::path_expand( dirname(subNoteRmdPath))
  subNoteFileName <- basename(subNoteRmdPath)

  #### Extract ProjDocGDTs from linkNoteRmdContents ####

  # extract each project Doc + GDT from each objective
  DocGDTsList <- extract_note_obj_doc_link_GDT_summ(linkNoteRmdContents, linkNoteRmdPath,
                                                    settings, orgPath)


  #### For each DocGDT ####

  # compute the noteObjective Header index in subNoteContents
  noteObjHeadIndex <- match_line_index( load_param_vector(settings[["NoteObjectivesHeader"]], orgPath),
                                        subNoteContents)

  # replace projNoteLinkSummaryContents summary headers
  projNoteLinkSummaryContents <- note_link_summ_params(projNoteLinkSummaryContents,
                                                       todoContents, settings, orgPath)

  for( dGDT in DocGDTsList ) {


    ##### Fill ProjDoc GDT Templates for SUB Note #####

    DocGDTList <- compute_doc_GDT_link(dGDT[["projectDocFilePath"]], subNoteRmdPath, settings,
                                       dGDT[["goal"]], dGDT[["deliverable"]], dGDT[["task"]])
    # returns list of DOC TITLE, LINK, GOAL, DEL, TASK in NAMED LIST
    # using the first path in subNoteRmdPaths - there will ALWAYS be at least one subnote!

    # replace markup in projNoteLinkContents
    subNoteLinkContents <- sub_note_link_params(projNoteLinkContents, settings, DocGDTList,
                                                projNoteLinkSummaryContents, orgPath)


    #### add Doc GDT Link to SUB Note ####

    # compute location in subNoteContents to insert the GDT Link & summary
    noteObjFootIndex <- grep_line_index_from( load_param_vector(settings[["NoteObjectivesFooter"]], orgPath),
                                              subNoteContents, noteObjHeadIndex, orgPath)

    subNoteContents <- insert_at_indices(subNoteContents, noteObjFootIndex, subNoteLinkContents)

  }

  write_file(subNoteContents, subNoteRmdPath)

  cat( "    Written GDTs to Sub Note file: ", basename(subNoteRmdPath), "\n" )

  # replace project note log sep
  projNoteSummaryContents <- sub_template_param(projNoteSummaryContents, "{{PROJECT_NOTE_LOG_SEP}}",
                                                settings[["ProjectTaskLogSep"]], orgPath)

  # replace proj note summary
  summaryContents <- extract_summary_from_link_contents(subNoteLinkContents, settings, orgPath)

  # # replace proj note summary - if NoteObjectivesTodoSectionHeader is in summaryBullet, remove everything FROM THAT LINE
  # projNoteLinkSummaryContentsTrim <- projNoteLinkSummaryContents[1 : ifelse( any(grepl(settings[["NoteObjectivesTodoSectionHeader"]],
  #                                                                                      projNoteLinkSummaryContents, fixed=TRUE)),
  #                                                                            grep(settings[["NoteObjectivesTodoSectionHeader"]],
  #                                                                                 projNoteLinkSummaryContents, fixed=TRUE)-1,
  #                                                                            length(projNoteLinkSummaryContents)) ]

  # replace in projNoteSummaryContents - in case any links are INDIVIDUAL (not group note)
  # this will be added at end of GDT section as individual project note link
  projNoteSummaryContents <- sub_template_param(projNoteSummaryContents, "{{PROJECT_NOTE_SUMMARY}}",
                                                summaryContents, orgPath)

  # replace in subNoteSummaryContents - for links that are GROUP NOTES
  # this will be added at end of the group note link in GDT section as subnote link
  subNoteSummaryContents <- sub_template_param(subNoteSummaryContents, "{{PROJECT_NOTE_SUMMARY}}",
                                               summaryContents, orgPath)

  for( dGDT in DocGDTsList ) {

    ##### Write Sub Note to each DocGDT #####

    # read projDoc
    projectDocPath <- dGDT[["projectDocFilePath"]]
    projDocContents <- read_file(projectDocPath)

    # find the GDT vector
    goalLine <- grep_line_index(dGDT[["goal"]], projDocContents, orgPath)
    delLine <- grep_line_index_from(dGDT[["deliverable"]], projDocContents, goalLine, orgPath)
    taskLine <- grep_line_index_from(dGDT[["task"]], projDocContents, delLine, orgPath)
    logLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskLogHeader"]], orgPath),
                                    projDocContents, taskLine, orgPath)
    taskFooterLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskFooter"]], orgPath),
                                           projDocContents, logLine, orgPath) # end of log section

    # determine if link exists as a group note link or as a single project note
    # find index of headernote link then the next ProjectTaskLogSep
    headerNoteName <- substr(basename(headerNoteRmdPath), 1, regexpr(".Rmd", basename(headerNoteRmdPath))-1)
    headerNoteLink <- paste0(settings[["HeaderLinkFormat"]],
                             create_hyperlink( headerNoteName, headerNoteRmdPath, projectDocPath),
                             settings[["HeaderLinkFormat"]])
    headLine <- grep(headerNoteLink, projDocContents[logLine:taskFooterLine], fixed=TRUE)

    if( length(headLine) > 0) {
      # if headLine identified, the link to this GDT must be as a group note under the header
      # get the headLine in full projDoc vector
      headLine <- grep_line_index_from(headerNoteLink, projDocContents, logLine, orgPath)
      # then the sepLine from this point
      sepLine <- grep_line_index_from(load_param_vector(settings[["ProjectTaskLogSep"]], orgPath),
                                      projDocContents, headLine, orgPath)
      # insert the new subnote at end of group note link
      insertionLine <- sepLine

      # replace sub note link
      projNoteName <- substr(basename(subNoteRmdPath), 1, regexpr(".Rmd", basename(subNoteRmdPath))-1)
      projNoteLink <- paste0(settings[["SubNoteLinkFormat"]],
                             create_hyperlink( projNoteName, subNoteRmdPath, projectDocPath),
                             settings[["SubNoteLinkFormat"]])
      snSummaryContents <- sub_template_param(subNoteSummaryContents, "{{PROJECT_NOTE_LINK}}",
                                              projNoteLink, orgPath)

      projDocContents <- insert_at_indices(projDocContents, insertionLine, snSummaryContents)

      write_file(projDocContents, projectDocPath)

      cat( "  Written Sub Note Link to Project Doc: ", basename(projectDocPath),
           " at GDT:", "\n  ", dGDT[["goal"]],"\n  ", dGDT[["deliverable"]],"\n  ", dGDT[["task"]], "\n" )

    } else {
      # if headLine not identified, the link to this GDT must be from a subnote individually linked
      # insert new subnote at end of the GDT section
      insertionLine <- taskFooterLine

      # replace sub note link
      projNoteName <- substr(basename(subNoteRmdPath), 1, regexpr(".Rmd", basename(subNoteRmdPath))-1)
      projNoteLink <- paste0(settings[["NoteLinkFormat"]],
                             create_hyperlink( projNoteName, subNoteRmdPath, projectDocPath),
                             settings[["NoteLinkFormat"]])
      snSummaryContents <- sub_template_param(projNoteSummaryContents, "{{PROJECT_NOTE_LINK}}",
                                              projNoteLink, orgPath)

      projDocContents <- insert_at_indices(projDocContents, insertionLine, snSummaryContents)

      write_file(projDocContents, projectDocPath)

      cat( "  Written Project Note Link to Project Doc: ", basename(projectDocPath),
           " at GDT:", "\n  ", dGDT[["goal"]],"\n  ", dGDT[["deliverable"]],"\n  ", dGDT[["task"]], "\n" )

    }
  } # end for
} #### ________________________________ ####

#' Create Insertable Content in a Project Note
#'
#' This function declares new insertable content based on a content
#' declaration template and inserts it into a selected Project Note.
#' It creates a separate Rmd file (the content source) that initially is blank,
#' which can be modified to include specific documentation such as SOPs or
#' protocol logs.
#'
#' @param selection A list representing the current Project Note selection.
#'   It must include:
#'     - filePath: The path of the current Project Note Rmd.
#'     - originalLineNumber: The line number where content is to be inserted.
#'     - rmdType: The type of Rmd file; must be "NOTE" or "SUB".
#'
#' @param contentName A string specifying the name of the content.
#'   It must not contain any spaces.
#'
#' @param contentDescription A string describing the content. This is shown
#'   to users when choosing content to insert.
#'
#' @param contentSourcePath A string specifying the path where the content
#'   directory and its Rmd file will be created. It is recommended to place
#'   it within the Project Note's directory.
#'
#' @param projectNoteTitle (Optional) A title for the content. If left blank,
#'   contentName is used with underscores and dashes replaced by spaces.
#'
#' @param contentDeclarationTemplate A template file for content declaration,
#'   located in the .config/templates/ directory. Default is
#'   "Protocol-Declaration-Template.Rmd".
#'
#' @param contentSourceTemplate A template file for the content source, found
#'   in the .config/templates/ directory. Default is
#'   "Protocol-Source-Template.Rmd".
#'
#' @details
#' The function executes the following steps:
#' 1. Determines the content directory and Rmd file path using contentName and
#'    contentSourcePath.
#' 2. Creates a relative hyperlink from the Project Note to the new content Rmd.
#' 3. Reads the content declaration and content source templates from the
#'    templates directory.
#' 4. Replaces placeholders in the content declaration template with the
#'    provided content title, description, and the hyperlink.
#' 5. Inserts the modified content declaration into the Project Note at the
#'    specified insertion point.
#' 6. Creates the content directory and writes the content source template to
#'    a new Rmd file.
#' 7. Updates the Project Note with the new content declaration.
#'
#' @return A string representing the path to the created content Rmd file.
#'
#' @examples
#' \dontrun{
#'   create_content(
#'     selection = list(
#'       filePath = "project_note.Rmd",
#'       originalLineNumber = 25,
#'       rmdType = "NOTE"
#'     ),
#'     contentName = "SOP_Fix",
#'     contentDescription = "Standard operating procedure for fixing the pump.",
#'     contentSourcePath = "/projects/my_project/notes",
#'     projectNoteTitle = "Pump Fix SOP",
#'     contentDeclarationTemplate = "Protocol-Declaration-Template.Rmd",
#'     contentSourceTemplate = "Protocol-Source-Template.Rmd"
#'   )
#' }
#'
#' @seealso
#' \code{\link{create_hyperlink}}, \code{\link{read_file}},
#' \code{\link{write_file}}, \code{\link{get_settings_yml}},
#' \code{\link{find_org_directory}}, \code{\link{insert_at_indices}}
#'
#' @note
#' The function assumes that the current file in the selection is a Project Note.
#' The selection must include keys 'filePath' and 'originalLineNumber'. The
#' contentName must not have spaces, and the content declaration template must
#' define placeholders for CONTENT_TITLE_FIELD, CONTENT_TITLE,
#' CONTENT_DESCRIPTION_FIELD, CONTENT_DESCRIPTION, CONTENT_SOURCE_FIELD, and
#' CONTENT_SOURCE_LINK.
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

  contentDeclarationContents <- read_file( fs::path( tempPath, contentDeclarationTemplate) )
  contentSourceContents <- read_file( fs::path( tempPath, contentSourceTemplate) )
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

} #### ________________________________ ####

#' Create Weekly Journal
#'
#' This function creates a weekly journal file for an organisation if it does
#' not already exist. If the journal file exists, it returns the path to the
#' existing file. The journal is stored in the directory specified in the
#' settings under "WeeklyJournalDir". Optionally, TODOs can be extracted to
#' this file using the extract_todos() function.
#'
#' @param date A string or Date object representing the start date for the
#'   Weekly Journal. It is converted to the Monday of the current week. The date
#'   should be in 'YYYY-MM-DD' format.
#'
#' @param organisationPath A string representing the path to the Organisation
#'   where the journal is created and saved.
#'
#' @param authorValue A string representing the author name for the Programme
#'   index file.
#'
#' @param journalFileNameTemplate A string that defines the journal file name.
#'   Placeholders for {{YYYY}}, {{MM}}, {{DD}}, and {{ORGNAME}} are replaced with
#'   the corresponding values.
#'
#' @param journalTemplate A template file name from the templates directory that
#'   defines the layout for the weekly journal.
#'
#' @details
#' The function first converts the input date to the Monday of the current week
#' and extracts the year, month, and day. It then determines the organisation
#' directory using the given organisationPath. Next, it creates the necessary
#' directories (the journal root and a year subdirectory) and generates a journal
#' Rmd file using the provided journal template. If the journal file already exists,
#' the function does not overwrite it and returns its path instead.
#'
#' @return A string representing the path to the weekly journal Rmd file.
#'
#' @examples
#' \dontrun{
#'   create_weekly_journal(
#'     date = "2023-04-10",
#'     organisationPath = "/path/to/organisation",
#'     authorValue = "John Doe",
#'     journalFileNameTemplate = "{{YYYY}}-{{MM}}-{{DD}}_{{ORGNAME}}",
#'     journalTemplate = "Weekly-Work-Journal-Template.Rmd"
#'   )
#' }
#'
#' @seealso
#' \code{\link{find_org_directory}}, \code{\link{get_settings_yml}},
#' \code{\link{create_directory}}, \code{\link{read_file}},
#' \code{\link{write_file}}, \code{\link{extract_todos}}
#'
#' @note
#' The function converts the date to a Date object if it is not already. It
#' relies on proper configuration in the YAML settings and assumes that the
#' organisation directory structure is correctly set up.
#'
#' @export
create_weekly_journal <- function(date=lubridate::today(),
                                  organisationPath=getwd(),
                                  authorValue=Sys.info()["user"],
                                  journalFileNameTemplate="{{YYYY}}-{{MM}}-{{DD}}_{{ORGNAME}}",
                                  journalTemplate="Weekly-Work-Journal-Template.Rmd") {


  #### Instance Variables ####

  if( lubridate::is.Date(date) == FALSE ) {
    date <- lubridate::ymd(date) # parse the date and convert to ymd format
  }

  # convert the date to the Monday of current week if needed
  date <- lubridate::floor_date(date, unit="week", week_start=1)

  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")


  #### Identify root of ORGANISATION ####

  #check_org_dir()

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


  #### Create Journal Dirs ####

  # create journal root Dir:
  journalPath <- fs::path(orgPath, settings[["WeeklyJournalDir"]])

  create_directory(journalPath,
                   "  Made Journal dir: ",
                   "  Journal directory could not be created: ")

  # create journal year Dir:
  journalYearPath <- fs::path(journalPath, year)

  create_directory(journalYearPath,
                   "  Made Journal Year dir: ",
                   "  Journal Year directory could not be created: ")


  #### create Journal Rmd file ####

  # modify journalFileNameTemplate - with any variable syntax
  journalFileNameTemplate <- gsub('{{YYYY}}', year, journalFileNameTemplate, fixed = TRUE)
  journalFileNameTemplate <- gsub('{{MM}}', month, journalFileNameTemplate, fixed = TRUE)
  journalFileNameTemplate <- gsub('{{DD}}', day, journalFileNameTemplate, fixed = TRUE)

  journalFileNameTemplate <- gsub('{{ORGNAME}}', orgName, journalFileNameTemplate, fixed = TRUE)

  journalRmdPath <- fs::path(journalYearPath, paste0(journalFileNameTemplate, ".Rmd"))

  # deal with possibility file may already exists
  if( fs::file_exists(journalRmdPath) ) {

    # if it exists, report this, and do not write to the file!
    cat( "  Journal .Rmd file Exists: ",journalRmdPath, "\n" )

  } else {

    create_file(journalRmdPath,
                "  Made Journal .Rmd file: ",
                "  Journal .Rmd file could not be created: ")

    # read journalTemplate:
    journalContents <- read_file( paste0(tempPath, .Platform$file.sep, journalTemplate) )

    # fill template

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

    # modify journalContents with rmarkdown-html-header content
    journalContents <- replace_markdown_header(journalContents, orgPath)

    # modify journalContents with SEP values
    journalContents <- replace_sep_values(journalContents, orgPath)

    # write to journalFile
    write_file(journalContents, journalRmdPath)

    cat( "  Written template to Journal .Rmd file: ", journalRmdPath, "\n" )

  } # end if journalRmdPath exists is FALSE

  return(journalRmdPath) # return the path

}


#' Generate Plaintext Calendar
#'
#' This internal function generates a plaintext calendar for insertion
#' into a weekly journal. The calendar is arranged as a matrix with columns
#' for each day of the week (starting with Sunday) and includes dates from
#' the first day of the month to the Saturday after the month's end.
#'
#' @param year A string or numeric value representing the year (YYYY).
#' @param month A string or numeric value representing the month (MM).
#' @param day A string or numeric value representing the day (DD). This is used
#'   only for header display purposes.
#' @param calendar_header (Optional) A string used as the header for the
#'   calendar. Default is "# DAILY LOG :".
#'
#' @return A string containing the formatted plaintext calendar.
#'
#' @details
#' The function constructs a date object for the first day of the given
#' month and year, and determines the last day of the month. It then
#' extends the date range to fill the calendar grid up to the following
#' Saturday. A 7x7 matrix is built with the day names as column headers and
#' date numbers filled in appropriately. Blank spaces are inserted where
#' no date exists, and the matrix is converted into a formatted string.
#'
#' @examples
#' \dontrun{
#'   # Generate a calendar for April 2023 with a custom header.
#'   cal <- generate_plaintext_calendar("2023", "04", "01",
#'                                        calendar_header="# WEEKLY LOG :")
#'   cat(cal)
#' }
#'
#' @seealso
#' \code{\link[lubridate]{floor_date}}, \code{\link{as.Date}},
#' \code{\link{seq.Date}}, \code{\link{format}}
#'
#' @note
#' This function assumes that the input year, month, and day are provided in
#' valid 'YYYY', 'MM', and 'DD' formats. It is designed for internal use
#' and may require further formatting for presentation in different contexts.
#'
generate_plaintext_calendar <- function(year, month, day,
                                        calendar_header="# DAILY LOG :") {
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
#' Creates RMarkdown content for 7 days starting from the provided year-
#' month-day date. It returns a character vector with RMarkdown headers
#' and separator lines for each day.
#'
#' @param year A string or numeric value representing the year (YYYY).
#' @param month A string or numeric value representing the month (MM).
#' @param day A string or numeric value representing the day (DD) from which
#'   the log begins.
#' @param separator_lines A string specifying the separator lines to insert
#'   before each day's header. Default is "{{SEP02}}".
#'
#' @details
#' This function creates a weekly daily log in RMarkdown format. It computes
#' the date range by adding 6 days to the starting date. For each day in the
#' range, it generates an uppercase header that includes the weekday, day,
#' month, and year. Separator lines are added before each header. The result
#' is returned as a character vector of RMarkdown text lines.
#'
#' @return A character vector containing the RMarkdown lines for the weekly
#' daily log.
#'
#' @examples
#' \dontrun{
#'   # Generate a weekly log for April 10, 2023.
#'   md_lines <- generate_weekly_rmarkdown("2023", "04", "10")
#'   cat(md_lines, sep = "\n")
#' }
#'
#' @seealso
#' \code{\link{as.Date}}, \code{\link{format}}, \code{\link{toupper}},
#' \code{\link{paste}}, \code{\link{while}}
#'
#' @note
#' Ensure the year, month, and day are provided in valid formats. The function
#' assumes the input date is valid and performs minimal error checking.
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

} #### ________________________________ ####


