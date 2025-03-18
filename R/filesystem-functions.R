#' Find Organisation Root Directory
#'
#' First searches for an Organisation Root Directory in the `path` parameter,
#' by recursively attempting to identify the `.config/` and
#' `.config/templates/` directories - via `find_org_path()`.
#'
#' If this fails, and an RStudio session is available, the filesystem path of
#' the currently active document in the Source Editor is also searched - via
#' `find_org_rstudio_editor()`
#'
#' If an Organisation path is identified, it is returned, otherwise
#' the function returns a BLANK string "".
#'
#' @param path an absolute path in the filesystem - should be within an
#' Organisation.
#'
#' @export
find_org_directory <- function( path = getwd() ) {

  orgPath <- find_org_path(fs::path_expand(path))
  if( orgPath != "") {
    return(orgPath)
  }

  # else check rstudio editor path if available
  orgPath <- find_org_rstudio_editor()
  return(orgPath)
}


#' find organisation root directory from passed path
#'
#' Used by `find_org_directory()` as first pass attempt to identify an
#' organisation root directory.
find_org_path <- function(path) {

  # look for the .config/ and templates/ dirs:
  confPath <- get_config_dir(path)
  tempPath <- get_template_dir(path)

  # get the root of current path - to exist recursive search
  path2 <- unlist(fs::path_split(path))[1]

  #### recursive search for org directory ####
  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    path2 <- path # save in placeholder
    path <- dirname(path)
    if( path2 == path ) {
      path <- ""
      break
    }
    confPath <- get_config_dir(path)
    tempPath <- get_template_dir(path)
  }
  return(fs::path(path))
}

#' From an absolute path compute the relative path from the Organisation Root.
#'
get_relative_path_org <- function(path) {
  orgPath <- find_org_path(path)
  fs::path_rel(path, start = fs::path_dir(orgPath) )
}


#' confirm the found org
#'
#' Confirms by checking returned value is not blank.
confirm_find_org <- function(organisationPath) {
  orgPath <- find_org_directory(organisationPath)

  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  organisationPath is not in an Organisation: ", organisationPath) )
  }
  # now, orgPath should be the root dir of the organisation

  return(orgPath)
}

#' Check Dirs for Organisations
#'
#' Simple wuick check of directories in `dirs` for presence of any ProjectManagr
#' Organisations.
check_dirs_org <- function(dirs) {

  return(dirs[fs::file_exists(get_settings_yml_file(dirs))])

}


#` confirm rmd_path
#'
#' Path in org && Project Doc or Note (subdir of programme)
#' && absolute + normalised
#'
#' @param rmd_path Path to Rmd file to check.
confirm_rmd_path <- function(rmd_path) {

  # if not an absolute path:
  if( R.utils::isAbsolutePath(rmd_path) == FALSE ) {
    rmd_path <- R.utils::getAbsolutePath(rmd_path )
  }


  orgPath <- find_org_directory(dirname(rmd_path))

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, rmd_path is not inside a PROGRAMME sub-dir!
    stop( paste0("  rmd_path is not a Project Doc or Note - not in a sub-dir of a PROGRAMME Directory: ", rmd_path) )
  }
  # now, orgPath should be the root dir of the organisation

  settings <- get_settings_yml(orgPath)

  # CONFIRM rmd_path is a project doc or note:
  rmd_type <- get_file_type(rmd_path, settings)

  # normalize path - remove HOME REF ~
  rmd_path <- fs::path_expand(rmd_path)

  # return
  rmd_path
}


#' get prefix from path
#'
#' @param filePath Path to Project Note or Doc Rmd file.
#'
#' @param settings List from `.config/settings.yml` file.
get_prefix <- function(filePath, settings) {

  substr(basename(filePath), 1,
         regexpr(settings[["ProjectPrefixSep"]], basename(filePath), fixed=TRUE)-1 )
}


#' Determine File Type
#'
#' Identifies the type of file based on its file path, naming conventions,
#' and organizational settings. The function distinguishes between project
#' documents, notes, and their subtypes.
#'
#' @param filePath Character string. The full path to the file whose type needs
#'   to be determined. This should be a file within the structured project directory.
#' @param settings List. A named list containing project-specific configuration
#'   settings.
#'   The following settings are expected:
#'   - \code{ProjectPrefixSep}: Separator string between the prefix and the file name.
#'   - \code{ProjectIndexSep}: Separator string between prefix characters and numbers.
#'   - \code{GroupNotePrefixSep}: Separator string for major and minor numbering in notes.
#'   - \code{HeaderNotePrefix}: String indicating the header note's minor numbering.
#'
#' @details
#' The function parses the \code{filePath} and compares its naming and location
#' against expected patterns based on \code{settings}. It evaluates the prefix
#' and other components to classify the file into one of the following types:
#' - \code{"DOC"}: A project document.
#' - \code{"NOTE"}: A simple note.
#' - \code{"HEAD"}: A header note.
#' - \code{"SUB"}: A sub-note.
#' - \code{"UNKNOWN"}: A file that does not match any known project file type.
#'
#' The function uses the following logic:
#' - Project documents are identified by mismatches between the prefix and the
#'   parent directory name.
#' - Header notes are identified by specific substrings in the prefix
#'   (e.g., \code{"-00~_"}).
#' - Sub-notes are identified by additional separators in their prefixes and the
#'   naming of their parent directories.
#' - Simple notes are identified by the presence of a single separator in their
#'   prefixes.
#'
#' @return
#' A character string indicating the file type. Possible values:
#' - \code{"DOC"}
#' - \code{"NOTE"}
#' - \code{"HEAD"}
#' - \code{"SUB"}
#' - \code{"UNKNOWN"}
#'
#' @examples
#' # Example settings
#' settings <- list(
#'   ProjectPrefixSep = "~_",
#'   ProjectIndexSep = "~",
#'   GroupNotePrefixSep = "-",
#'   HeaderNotePrefix = "00"
#' )
#'
#' # Example file path
#' filePath <- "/project/directory/ABC~001~_file.Rmd"
#'
#' # Determine file type
#' get_file_type(filePath, settings)
#'
#' @note
#' This function assumes that the project directory structure adheres to the naming conventions
#' defined in the \code{settings} parameter. Misconfigured settings or unconventional file names
#' may lead to incorrect classifications.
#'
#' @seealso
#' - \code{\link{basename}} and \code{\link{dirname}} for extracting components of file paths.
#'
get_file_type <- function( filePath, settings ) {


  #### extract prefix ####

  # file types will depend on the LOCATION and FILENAME PREFIX

  # Project Docs & Notes : These both will contain the prefixSep && have accompanying directory named prefix
    # DOCS : Can be identified by fact their prefix does NOT match the parent directory they sit in
    # NOTES : Can be identified by fact their prefix DOES match parent directory they sit in

  # get prefix string - extracts basename from start up to END OF ProjectPrefixSep
   # if ProjectPrefixSep doesnt exist this will be BLANK
  prefixSep <- substr(basename(filePath), 1,
                      regexpr(settings[["ProjectPrefixSep"]], basename(filePath), fixed=TRUE)
                      +(nchar(settings[["ProjectPrefixSep"]])-1) )
   # this is useful as can identify the headerNote complete string: "-00~_"

  # extracts prefix up to JUST BEFORE ProjectPrefixSep
  prefix <- substr(basename(filePath), 1,
                   regexpr(settings[["ProjectPrefixSep"]], basename(filePath), fixed=TRUE)-1 )

  prefixStr <- prefix # set all values initially
  # get just the STRING first part from prefix
  # project notes have a prefix SRING then a NUMBER - so want to remove possible number
  # these are separated by ~ - so return string up to but not including '~' : ProjectIndexSep

  # this code works with `prefix` as a vector!
  projIndexSepExists <- regexpr(settings[["ProjectIndexSep"]], prefix, fixed=TRUE) > -1
  prefixStr[projIndexSepExists] <- substr(prefix[projIndexSepExists], 1,
                                          regexpr(settings[["ProjectIndexSep"]], prefix[projIndexSepExists], fixed=TRUE)-1 )

  # old code only works with individual strings..
  # if( regexpr(settings[["ProjectIndexSep"]], prefix, fixed=TRUE) > -1 ) {
  #   prefixStr <- substr(prefix, 1,
  #                       regexpr(settings[["ProjectIndexSep"]], prefix, fixed=TRUE)-1 )
  # } else {
  #   prefixStr <- prefix
  # }


  # project notes subtypes are defined by FILENAME PREFIX:
   # looking at the PREFIX of the name will help identify the file
   # PREFIX layouts
    # simple note : PREFIXSTR~001~_...Rmd
    # header note : PREFIXSTR~001-00~_...Rmd
    # subnote     : PREFIXSTR~001-001~_...Rmd
  # PREFIXSTR will be the char string derived from parent DIR
  # first "~" string is ProjectIndexSep in settings - splits char string from numbering
  # "~_" string is ProjectPrefixSep in settings - splits prefix from filename
  # header & subnote: "-" string is GroupNotePrefixSep in settings - splits major and minor numbering
  # header : "00" is HeaderNotePrefix in settings - defines the header note minor numbering

  # Will avoid using REG EXPR - in case special chars are selected for Prefix components
   # will compose the grepl() functions with `fixed=TRUE` with each sub-string to match

  # identifying a header note - will contain the complete headerNote Prefix defined in settings
  # with default settings this creates the single substring : "-00~_"
  headerTypeStr <- paste0(settings[["GroupNotePrefixSep"]],
                          settings[["HeaderNotePrefix"]],
                          settings[["ProjectPrefixSep"]])

  # identifying a sub note - will contain two sub-strings:
   # ProjectIndexSep : ~
   # GroupNotePrefixSep : -

 # identifying a simple note - will contain one sub-string:
  # ProjectIndexSep : ~


  #### parse filePath & prefix ####

  if(prefix == "") {
    # no prefix identified - must not be project doc or project note
    TYPE <- "UNKNOWN"

  } else if( basename(dirname(filePath)) != prefixStr ) {

    # project docs can be defined by a mismatch of parent directory name and prefix STRING name
    if( startsWith(basename(dirname(filePath)), prefixStr) == TRUE ) {
      TYPE <- "SUB" # subtle bug: sub note parent only STARTS WITH prefixStr, so deal with this exception
    } else {
      TYPE <- "DOC"
    }

  } else if( grepl(headerTypeStr, prefixSep, fixed = TRUE) ) {
    # headerNote will contain the full headerTypeStr in prefixSep
     # using prefixSep here to include the whole ProjectPrefixSep string!
    TYPE <- "HEAD"

  } else if( grepl(settings[["ProjectIndexSep"]], prefix, fixed = TRUE) &&
             grepl(settings[["GroupNotePrefixSep"]], prefix, fixed = TRUE) &&
             endsWith(basename(dirname(filePath)),
                      paste0(settings[["GroupNotePrefixSep"]], settings[["HeaderNotePrefix"]]))  )  {
    # subNotes are differentiated from simple notes by inclusion of GroupNotePrefixSep
     # need to use prefix here, as the GroupNotePrefixSep string is in the ProjectPrefixSep by default!
    # AND subNotes will be in a directory whose name will end with GroupNotePrefixSep + HeaderNotePrefix : '-00' by default
    TYPE <- "SUB"

  } else if( grepl(settings[["ProjectIndexSep"]], prefix, fixed = TRUE) ) {
    # simple notes will contain the ProjectIndexSep in the prefix
    TYPE <- "NOTE"
  }

  TYPE
}


#' Get File Types
#'
#' Get types of a vector of file paths.
#'
#' Determine if the current file is a PROJECT DOC (it is inside the PROJECTS/ Directory),
#' a HEADER GROUP NOTE (contains the string "-00~_"), or a SIMPLE or SUB Project Note (any
#' other file).
#'
#' Returns the relevant String:  DOC, HEAD, SUB, NOTE, or UNKNOWN for unidentified file.
#'
#' @param filePath the Absolute path to the file.
#'
#' @param settings List from `.config/settings.yml` file.
#'
get_file_types <- function( filePaths, settings ) {

  types <- c()

  for(fps in filePaths) {
    types <- c(types, get_file_type(fps, settings))
  }
  types
}



#' Get Date
#'
#' internal function to get date in YYYY MM DD format
#'
#' @param timezone Timezone code to use. Default "UTC".
#'
#' @param split Chracter to split YYYY & MM & DD. Default "/".
#'
get_date <- function(timezone = "UTC", split="/") {
  #### compute date with timezone param ####
  datetime <- lubridate::now(timezone) #Sys.time()

  # round to nearest minute:
  datetime <- round(datetime,"mins")

  # convert to POSIXlt:
  datetime <- as.POSIXlt(datetime)

  # round to nearest 5 min - not using now keep round to nearest 1min!
  #datetime$min <- (datetime$min + 5/2) %/% 5 * 5

  # format datetime to use "/" and have a ":" between date and time
  datetime_split <- strsplit(as.character(datetime), " ")
  date <- gsub("-", split, datetime_split[[1]][1] )

  date
}


#' internal function to get datetime in YYYY/MM/DD:hh:mm format
#'
#' @param timezone Timezone code to use. Default "UTC".
#'
#' @param split Chracter to split YYYY & MM & DD. Default "/".
#'
#' @param splitTime Chracter to split DD & hh & mm. Default ":".
#'
get_datetime <- function(timezone = "UTC", split="-", splitTime=":") {

  datetime <- lubridate::now(timezone) #Sys.time()

  # round to nearest minute:
  datetime <- round(datetime,"mins")

  # convert to POSIXlt:
  datetime <- as.POSIXlt(datetime)

  # round to nearest 5 min - not using now keep round to nearest 1min!
  #datetime$min <- (datetime$min + 5/2) %/% 5 * 5

  # format datetime to use "/" and have a ":" between date and time
  datetime_split <- strsplit(as.character(datetime), " ")
  datetime_split[[1]][1] <- gsub("-", split, datetime_split[[1]][1] )

  datetime_colon <- paste0( datetime_split[[1]][1], splitTime, datetime_split[[1]][2] )

  # remove seconds:
  datetime_colon <- substr(datetime_colon, 1, nchar(datetime_colon)-3)

  datetime_colon

}



#' Get Next Simple Prefix
#'
#' This method extracts the Prefix String from projectNotePath (isolated by extracting
#' all letters and numbers up to the `projIndexSep`).  It then checks the
#' Rmd Files in projectNotePath that begin with prefixString to compute the next SIMPLE
#' Prefix - +1 from the highest prefix value.
#'
#' NOTE must use Rmd files and not the Project
#' Note DIRs, as the DIRs can be symlinks, and if not active are not returned as existing!
#'
#' projectNotePath - must be a path to a VALID DIR for Project Notes - a SubDir in a PROGRAMME DIR.
#'
#' @param projectNotePath Is the path the new simple project note is to be placed
#' into.
#'
#' @param settings List from `.config/settings.yml` file.
#'
get_next_simple_prefix <- function(projectNotePath, settings) {

  #### Set Instance Variables ####

  projIdentifierSep <- load_param_vector(settings[["ProjectIdentifierSep"]]) # "_"
  projPrefixSep <- load_param_vector(settings[["ProjectPrefixSep"]]) #  "~_"
  projIndexSep <- load_param_vector(settings[["ProjectIndexSep"]]) # "~"
  groupIndexSep <- load_param_vector(settings[["GroupNotePrefixSep"]]) # "-"


  #### compute project note prefix ####

  # get the selected directory's BASENAME - to extract the prefix:
  pir <- basename(projectNotePath)

  # Extract PREFIX - up to ProjectIdentifierSep
  if(regexpr(projIdentifierSep, pir, fixed=TRUE) > 0) {
    prefix <- substring(pir, first=1, last=regexpr(projIdentifierSep,
                                                   pir, fixed=TRUE)-1 )
  } else {
    prefix <- pir
  }

  # define the Regular Exp for this prefix:
  prefixRegExp <- paste("^", prefix, ".*", sep="")

  # look in projectNotePath for any files - if so, compute Prefix from these, otherwise generate new Prefix for file:
  subFiles <- list.files(projectNotePath) # get all files
  subFiles <- subFiles[endsWith(subFiles, paste0(".", settings[["FileType"]]) )] # filter for filetype
  subFiles <- subFiles[ regexpr(projPrefixSep, subFiles) >= 1 ] # filter for files that contain projPrefixSep
  subFiles <- subFiles[ startsWith(subFiles, pir) ] # filter for files that start with name of parent
    # ALL PROJECT NOTES HAVE THIS FORMAT!
  # now CREATE subDirs from subFiles - substring at index of "~_"
  subDirs <- substr(subFiles, 1, regexpr(projPrefixSep, subFiles)-1 )

  # filter subNoteDirs to only contain content that starts with the majorPrefix:
  subDirs <- subDirs[grepl(prefixRegExp, subDirs)]

  if(length(subDirs) > 0 ) {

    # extract all MAJOR NUMBERING of subDirs (numbers BETWEEN projIndexSep (~) and
      # groupIndexSep (-) for GROUP HEADERS, and AFTER ~ for SINGLE NOTES)

    subVals <- integer( length(subDirs) ) # new integer vector

    for(l in 1:length(subDirs) ) {

      # remove the prefix string (same as parent directory name) to identify the major numbering
      subDirNums <- substring(subDirs[l], first=regexpr(pir, subDirs[l], fixed=TRUE)+nchar(pir))

      if( regexpr(groupIndexSep, subDirNums, fixed=TRUE) > 0 ) {

        # this is a GROUP HEADER DIR - extract the integer between ~ and -
        subVals[l] <- as.integer(
          substring( subDirNums,
                     first=regexpr(projIndexSep, subDirNums, fixed=TRUE)
                           + nchar(projIndexSep),
                     last=regexpr(groupIndexSep, subDirNums, fixed=TRUE)
                          - nchar(groupIndexSep) )  )
      } else {

        # this is a SINGLE DIR - extract the integer AFTER projIndexSep: ~
        subVals[l] <- as.integer(
                       substring( subDirNums,
                         first=regexpr(projIndexSep, subDirNums, fixed=TRUE)
                                         + nchar(projIndexSep) )  )
      }
    }

    # subVals now contains all the index values - copmute the MAX, and then add 1 for NEXT index:
    nextIndex <- max(subVals) +1
    if(nextIndex>99) {

      nextIndexChar <- paste("", nextIndex, sep="")

      } else if(nextIndex>9) {
      nextIndexChar <- paste("0", nextIndex, sep="")

      } else {
      nextIndexChar <- paste("00", nextIndex, sep="")

      }

    simplePrefix <- paste(prefix, projIndexSep, nextIndexChar, sep="")

  } else {
    # else this is the first note in this DIR - set index to 001
    simplePrefix <- paste(prefix, projIndexSep, "001", sep="")
  }

  # return simplePrefix:
  simplePrefix

}

#' get next header prefix from path
#'
#' @param groupNotePath Path to group Note.
#'
#' @param settings List from `.config/settings.yml` file.
#'
get_next_header_prefix <- function(groupNotePath, settings) {

  # get next simple Note Prefix
  prefix <- get_next_simple_prefix(groupNotePath, settings)

  #### Return Simple Note Prefix ####

  # suffixed with GroupNotePrefixSep + HeaderNotePrefix
  paste0(prefix, settings[["GroupNotePrefixSep"]], settings[["HeaderNotePrefix"]])

}


#' Get Next SubNote Prefix
#'
#' This method extracts the Prefix String from headerNoteDir, then checks the
#' DIRs in headerNoteDir that begin with prefixString to compute the next Group
#' Prefix - +1 from the highest prefix value.
#'
#' @param headerNoteDir The Group Header Note Directory.
#'
#' @param settings List from `.config/settings.yml` file.
#'
get_next_subnote_prefix <- function(headerNoteDir, settings) {


  #### Set Instance Variables ####

  projIdentifierSep <- load_param_vector(settings[["ProjectIdentifierSep"]]) # "_"
  projPrefixSep <- load_param_vector(settings[["ProjectPrefixSep"]]) #  "~_"
  projIndexSep <- load_param_vector(settings[["ProjectIndexSep"]]) # "~"
  groupIndexSep <- load_param_vector(settings[["GroupNotePrefixSep"]]) # "-"


  #### compute group note prefix ####

  headerNotePrefix <- basename(headerNoteDir) # prefix string plus major index plus header '-00'
  headerNotePrefixDir <- basename( dirname(headerNoteDir) ) # string prefix is derived from!

  # get the ROOT of the Prefix - string up to groupIndexSep
  headPrefixNoString <- substring(headerNotePrefix, first=nchar(headerNotePrefixDir)+1)
  majorPrefix <- substring(headerNotePrefix, first=1,
                           last= nchar(headerNotePrefixDir)+regexpr(groupIndexSep, headPrefixNoString)[1])
  # add support for headerNotePrefix incase it contains groupIndexSep char!

  # define the Regular Exp for this prefix:
  majPrefixRegExp <- paste("^", majorPrefix, ".*", sep="")

  # look in headerNoteDir for any files - if so, compute Prefix from these, otherwise generate new Prefix for file:
  #subNoteDirs <- list.dirs( headerNoteDir, full.names=FALSE, recursive=FALSE )
  subNoteFiles <- list.files(headerNoteDir)
  subNoteFiles <- subNoteFiles[endsWith(subNoteFiles, ".Rmd")]
  # now CREATE subNoteDirs from subNoteFiles - substring at index of "~_"
  subNoteDirs <- substr(subNoteFiles, 1, regexpr(projPrefixSep, subNoteFiles)-1 )


  # filter subNoteDirs to only contain content that starts with the majorPrefix:
  subNoteDirs <- subNoteDirs[grepl(majPrefixRegExp, subNoteDirs)]

  if(length(subNoteDirs) > 0 ) {

    # compute NEXT subNote Prefix from the DIRs in subNoteDirs:
    subNoteNums <- substring(subNoteDirs, first=nchar(headerNotePrefixDir)+1)
    subNoteVals <- as.integer(substring(subNoteNums, first=regexpr(groupIndexSep, subNoteNums, fixed=TRUE)+1  ) )

    nextVal <- max(subNoteVals) +1

    if(nextVal>99) {
      nextValChar <- paste("", nextVal, sep="")

    } else if(nextVal>9) {
      nextValChar <- paste("0", nextVal, sep="")

    } else {
      nextValChar <- paste("00", nextVal, sep="")

    }

    subNotePrefix <- paste(majorPrefix, nextValChar, sep="")

  } else {
    # else, its the first subnote!
    # create subNotePrefix by adding a ".001" to end of majorPrefix:
    subNotePrefix <- paste0(majorPrefix, "001")

  }

  subNotePrefix

} #### ________________________________ ####



#' Read File
#'
#' Returns a string vector of text file at `filePath`.
#'
#' @param filPath path to file to read
#'
read_file <- function(filePath) {

  fileConn <- file(filePath)
  fileContents <- readLines(fileConn)
  close(fileConn)
  fileContents # return
}



#' Write File
#'
#' Writes `contents` to `filePath`.
#'
#' @param contents Character vector to write to filePath
#'
#' @param filPath path to file to write
#'
write_file <- function(contents, filePath) {
  fileConn <- file(filePath)
  writeLines(contents, fileConn)
  close(fileConn)
}

create_directory <- function(path, log, error) {
  fs::dir_create(path)
  confirm_dir(path, error)
  cat( log, path, "\n" )
}


create_file <- function(path, log, error) {
  fs::file_create(path)
  confirm_file(path, error)
  cat( log, path, "\n" )
}

#' Confirm Directories Exist
#'
#' Checks if all directories in the given list exist. If any are missing, the
#' organisation directory is deleted and an error is raised.
#'
#' @param dirPaths Character vector. A list of directory paths to check.
#' @param errMsg Character. Error message to display if a directory does not exist.
#' @param orgPath Character. The root path of the organisation, which will be
#'  removed if any check fails.
confirm_dirs <- function(dirPaths, errMsg, orgPath) {
  if( any(fs::dir_exists(dirPaths) == FALSE) ) {
    dp <- dirPaths[fs::dir_exists(dirPaths) == FALSE]
    fs::dir_delete(orgPath) # remove org
    stop( paste0(errMsg, paste(dp, collapse='\n  ')) )
  }
}

#' Confirm a Single Directory Exists
#'
#' Checks if a given directory exists. If not, raises an error.
#'
#' @param dirPath Character. Path to the directory to check.
#' @param errMsg Character. Error message to display if the directory does not
#' exist.
confirm_dir <- function(dirPath, errMsg) {
  if( fs::dir_exists(dirPath) == FALSE ) {
    stop( paste0(errMsg, dirPath) )
  }
}

#' Confirm a File Exists
#'
#' Checks if a given file exists. If not, raises an error.
#'
#' @param filePath Character. Path to the file to check.
#' @param errMsg Character. Error message to display if the file does not exist.
confirm_file <- function(filePath, errMsg) {
  if( fs::file_exists(filePath) == FALSE ) {
    stop( paste0(errMsg, filePath) )
  }
} #### ________________________________ ####


#' Check a path is the org root dir
#'
#' @param fileSystemPath String of a filesystem path.
#'
check_org_dir <- function(fileSystemPath) {

  # look for the .config/ and templates/ dirs:
  confPath <- get_config_dir(fileSystemPath)
  tempPath <- get_template_dir(fileSystemPath)
  settings <- get_settings_yml_file(fileSystemPath)

  # of confPath and tempPath exist, this is the org (root) dir
  if( fs::dir_exists(confPath) &
      fs::dir_exists(tempPath) &
      fs::file_exists(settings) ) {
    return(TRUE)
  } else{
    return(FALSE)
  }
}



#' Check Programme Dir
#'
#' Checks fileSystemPath is pointing to a PROGRAMME directory.
#' A Programme Directory is a direct sub-dir to a Organisation
#' directory, so this method establishes this is the case by checking
#' the parent Dir to fileSystemPath, to see if '.config/' and
#' '.config/templates/' exist.
#'
#' Secondly, it checks the 'PROJECTS/' directory exists in this
#' putative Programme Directory.
#'
#' If a Programme path is confirmed, it is returned, otherwise
#' the function returns a BLANK string "".
#'
#' @param fileSystemPath Path to possible programme directory
#'
#' @param settings List from `.config/settings.yml` file.
#'
check_prog_dir <- function( fileSystemPath, settings ) {

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname(fileSystemPath)
  is_org <- check_org_dir(orgPath)

  if(  !is_org  ) {
    fileSystemPath <- ""
  }

  fileSystemPath
}


#' Check Programme SubDir
#'
#' Checks fileSystemPath is pointing inside a PROGRAMME directory.
#' A Programme Directory is a direct sub-dir to a Organisation
#' directory, so this method establishes this is the case by recursively checking
#' the parent Dir to fileSystemPath, to see if '.config/' and
#' '.config/templates/' exist.
#'
#' If a Programme path is confirmed, it is returned, otherwise
#' the function returns a BLANK string "".
#'
#' @param fileSystemPath Path to possible programme directory
#'
#' @param settings List from `.config/settings.yml` file.
#'
check_prog_subdir <- function( fileSystemPath, settings ) {

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  path <- dirname(fileSystemPath)
  progPath <- fileSystemPath

  # get config templates settings yml
  confPath <- get_config_dir(path)
  tempPath <- get_template_dir(path)

  if(  !( all(file.exists(confPath)) && all(file.exists(tempPath)) )  ) {
    fileSystemPath <- ""
  }

  path2 <- "/" # use this as placeholder of PREVIOUS orgPath - if orgPath == orgPath2, then have not found config or template!

  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    path2 <- path # save in placeholder
    path <- dirname(path)
    if( path2 == path ) {
      path <- ""
      progPath <- ""
      break
    }
    confPath <- get_config_dir(path)
    tempPath <- get_template_dir(path)
    progPath <- dirname(progPath)
  }

  # fileSystemPath is either a PROGRAMME DIR or blank - return
  progPath

}


#' Check Programme File
#'
#' Checks fileSystemPath is inside a PROGRAMME directory.
#' A Programme Directory is a direct sub-dir to a Organisation
#' directory, so this method establishes this is the case by checking
#' the parent Dir to fileSystemPath, to see if '.config/' and
#' '.config/templates/' exist.
#'
#' Secondly, it checks the 'PROJECTS/' directory exists in this
#' putative Programme Directory.
#'
#' If a Programme path is confirmed, it returns the fileSystemPath, otherwise
#' the function returns a BLANK string "".
#'
#' @param fileSystemPath Path to possible programme directory
#'
check_prog_file <- function( fileSystemPath ) {

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname(fileSystemPath)

  # set confPath + tempPath:
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)

  if(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    fileSystemPath <- ""
  }

  # fileSystemPath is therefore in a PROGRAMME DIR

  fileSystemPath

}


#' Check Project Note
#'
#' Checks fileSystemPath is inside a sub-dir to a PROGRAMME directory.
#' A Programme Directory is a direct sub-dir to a Organisation
#' directory, so this method establishes this is the case by checking
#' the parent Dir to fileSystemPath, to see if '.config/' and
#' '.config/templates/' exist.
#'
#' Secondly, it checks the 'PROJECTS/' directory exists in this
#' putative Programme Directory.
#'
#' Finally, it confirms the Project Note is of type NOTE.
#'
#' If a Programme path is confirmed, it returns the fileSystemPath, otherwise
#' the function returns a BLANK string "".
#'
#' @param fileSystemPath Path to possible project note
#'
check_proj_note <- function( fileSystemPath ) {

  returnPath <- ""

  if( find_prog_dir(fileSystemPath) != "" ) {
    # the fileSystemPath is in a Programme Directory

    if( basename(dirname(fileSystemPath)) != "PROJECTS" ) {
      # the filesystemPath DIR is NOT PROJECTS - therefore NOT a Project Doc

      if(get_file_type(fileSystemPath) == "NOTE" ) {
        # the file type of fileSystemPath is NOTE - return fileSystemPath
        returnPath <- fileSystemPath
      }
    }
  }

  # returnPath:
  returnPath

}


#' Check Programme Sub Dir
#'
#' Searches fileSystemPath's parent directories to identify
#' a Project Organisation directory.  This is identified by
#' finding a '.config/' directory and a '.config/templates/' directory.
#'
#' For the fileSystemPath to be successfully returned, the directory
#' MUST be at least in level below a PROGRAMME directory.
#'
#' The original path is returned if identified, otherwise
#' the function returns a BLANK string "".
#'
#' @param fileSystemPath Path to location inside programme directory
#'
check_prog_sub_dir <- function( fileSystemPath ) {

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname(dirname(fileSystemPath))

  # set confPath + tempPath:
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)

  orgPath2 <- "/" # use this as placeholder of PREVIOUS orgPath - if orgPath == orgPath2, then have not found config or template!

  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    orgPath2 <- orgPath # save in placeholder
    orgPath <- dirname(orgPath)
    if( orgPath2 == orgPath ) {
      fileSystemPath <- ""
      break
    }
    # set confPath + tempPath:
    confPath <- get_config_dir(orgPath)
    tempPath <- get_template_dir(orgPath)
  }

  #if(fileSystemPath == "") {
  #  stop(paste0("  Dir is NOT inside a PROGRAMME: ", fileSystemPath))
  #}

  fileSystemPath

}


#' Find Programme Dir
#'
#' Recursively searches fileSystemPaths to identify a
#' Programme directory.  This is identified by identifying the organisation root
#' directory from each parent direcotry searched.
#'
#' If Programme paths are identified, they are  returned, otherwise
#' the function returns BLANK strings "".
#'
#' @param fileSystemPaths a vector of paths to identify programme paths from.
#'
find_prog_dir <- function(fileSystemPaths) {

  # ensure path is absolute
  fileSystemPaths <- fs::path_expand(fileSystemPaths)

  root <- "/" # use to identify the root

  # vector and index to store parsed filesystempaths
  fsp <- c("")
  fspi <- 1

  # for each path
  for(f in fileSystemPaths) {

    # initially check the PARENT DIRECTORY is the root of an organisation
    f_is_org <- check_org_dir(dirname(f))

    # recursively search for orgPath in parent directory
    while(  !f_is_org  ) {
      f <- dirname(f)
      if( root == f ) { # break if reached filesystem root
        f <- ""
        break
      }
      f_is_org <- check_org_dir(dirname(f))
    }

    # add returning vector and increment index
    fsp[fspi] <- f
    fspi <- fspi+1
  }

  # return fsp
  fsp
}


#' Find Programme Dirs from Organisation path
#'
#' Searches the orgPath to identify all programmes. Programme paths are returned
#' after extracting the names from the status.yml file under PROGRAMMES section.
#'
#' @param orgPath Path to a projectmanagr organisation.
#'
#' @param settings List object of extracted metadata from organisation
#' `.config/settings.yml` file.
#'
#' @return programmePaths character vector containing all identified programme
#' absolute paths.
#'
find_prog_dirs <- function( orgPath, settings ) {

  # get all Programme DIRs from status.yml

  # load status file for projectmanagr org status
  # contains information on contents DIRs && index of contents in those files with retrieval datetime
  status <- get_status_yml(orgPath, settings)

  progNames <- lapply(status['PROGRAMMES'], names)

  progPaths <- fs::path(orgPath, progNames$PROGRAMMES)

  progPaths <- progPaths[fs::dir_exists(progPaths)]

  progPaths

}


#' Find Group Note Header Rmd Path
#'
#' From the `subNoteRmdPath` - an absolute path.
#'
#' @param subNoteRmdPath Path to sub note Rmd file
#'
#' @param settings List from `.config/settings.yml` file.
#'
find_header_Rmd_path <- function( subNoteRmdPath, settings ) {

  subNoteRmdPath <- fs::path_expand( subNoteRmdPath) # expand tilde

  # header note prefix will be dir base name
  headerNotePrefix <- basename( dirname(subNoteRmdPath))
  # header note will be in the parent dir
  headerNotePath <- dirname( dirname(subNoteRmdPath) )

  # look for headernote with prefix same as headerNotePath basename
  # AND that contains the projectPrefixSep
  # AND that ends with .Rmd
  files <- list.files(headerNotePath)
  files <- grep(settings[["ProjectPrefixSep"]], files, fixed=TRUE, value=TRUE)
  files <- grep(headerNotePrefix, files, fixed=TRUE, value=TRUE)
  files <- grep(".Rmd", files, fixed=TRUE, value=TRUE)

  paste0(headerNotePath, .Platform$file.sep, files) # return headerRmdPath

}


#' Compute Project Indices
#'
#' From the PROJECTS dir in a programme (`projsPath`), compute the available indices
#' for creating new project doc and DIR, based on all DIRs in `projsPath` that start
#' with `programmePrefix` and return a numeric array of these.
#'
compute_project_indices <- function(projsPath, programmePrefix) {

  # read all files in projsPath that start with prefix:
  #directories <- dir(projsPath, recursive = FALSE, full.names = FALSE, pattern= paste(programmePrefix,"[0-9]{1,}[~]{1}[_]{1}", sep="")  )
  directories <- list.dirs(projsPath, full.names=FALSE, recursive=FALSE)
  directories <- directories[startsWith(directories, programmePrefix)]

  #projectIndexes <- sapply( directories, function(x)
  #              substr(x, gregexpr(programmePrefix, x)[[1]][1]+nchar(programmePrefix),
  #                        gregexpr(settings[["ProjectIndexSep"]], x)[[1]][1]-1 )  )

  projectIndexes <- sapply( directories, function(x)
    substr(x, gregexpr(programmePrefix, x)[[1]][1]+nchar(programmePrefix),
           nchar(x) )  )

  projectIndexMax <- sort( as.numeric(projectIndexes) )[length(projectIndexes)]

  if( length(projectIndexMax) == 0 ) {

    indexes <- 1 #this ensures if there are NO directories that match the glob above, that the index is set to 1!

  } else {

    # get any missing values:
    indexes <- setdiff( seq(from = 1, to = projectIndexMax, by =1), sort( as.numeric(projectIndexes) ))

    # add +1 on projectIndexMax to indexes
    indexes[length(indexes)+1] <- projectIndexMax+1

  }

  indexes # return

}

#' Compute the next available project index
#'
#' Returns first result from compute_project_indices, appending a 0 if
#' the returned number is below 10.
#'
compute_project_index <- function(projsPath, programmePrefix) {

  projIndices <- compute_project_indices(projsPath, programmePrefix)
  projectIndex <- projIndices[1] # use first available index

  # if projectIndex is only one digit, append "0" to front:
  if(projectIndex < 10 ) {
    projectIndex <- paste("0", projectIndex, sep="")
  } else {
    projectIndex <- paste("", projectIndex, sep="")
  }
}



#' Get config dir
#'
#' This is set as `.config` GLOBALLY - used to identify
#' a Project Organisation root directory.
#'
get_config_dir <- function(orgPath) {
  confPath <- fs::path(orgPath, ".config")
  return(confPath)
}


#' Get template dir
#'
#' This is set as `.config/templates` GLOBALLY - used to identify
#' a Project Organisation root directory.
#'
get_template_dir <- function(orgPath) {
  tempPath <- fs::path(get_config_dir(orgPath), "templates")
  return(tempPath)
}
#' Get settings yml file
#'
#' This is set as `.config/settings.yml` GLOBALLY - stores all settings for
#' projectmanagr organisation.
#'
get_settings_yml_file <- function(orgPath) {
  settingsYml <- fs::path(get_config_dir(orgPath), "settings.yml")
  return(settingsYml)
}

#' Get settings yml
#'
#' This is set as `.config/settings.yml` GLOBALLY - stores all settings for
#' projectmanagr organisation.
#'
get_settings_yml <- function(orgPath) {
  settingsYml <- get_settings_yml_file(orgPath)
  settings <- yaml::yaml.load( yaml::read_yaml( settingsYml ) )
  return(settings)
}


#' Get status yml file
#'
#' This is stored in `.config` GLOBALLY - filename is derived from settings
#' in `ConfigStatusYamlFile` parameter
#'
get_status_yml_file <- function(orgPath, settings) {
  statusYml <- fs::path(get_config_dir(orgPath), settings[["ConfigStatusYamlFile"]])
  return(statusYml)
}

#' Get status yml
#'
#' This is stored in `.config` GLOBALLY - filename is derived from settings
#' in `ConfigStatusYamlFile` parameter
#'
get_status_yml <- function(orgPath, settings) {
  statusYml <- get_status_yml_file(orgPath, settings)
  status <- yaml::yaml.load( yaml::read_yaml( statusYml ) )
  return(status)
}


#' Get volumes dir
#'
#' Stores volume mounts. This is set as `volumes/` by default, stored in
#' settings.
#'
get_volumes_dir <- function(orgPath, settings) {
  volPath <- fs::path(orgPath, settings[["VolumesDir"]])
  return(volPath)
}

#' Get site dir
#'
#' Stores html site of org.  This is set as `site/` by default, stored in
#' settings.
#'
get_site_dir <- function(orgPath, settings) {
  sitePath <- fs::path(orgPath, settings[["SiteDir"]])
  return(sitePath)
}


#' Get weekly journal dir
#'
#' Stores weekly journal Rmd files.  This is set as `weekly-journal/` by default,
#' stored in settings.
#'
get_weekly_journal_dir <- function(orgPath, settings) {
  weeklyjournalPath <- fs::path(orgPath, settings[["WeeklyJournalDir"]])
  return(weeklyjournalPath)
}


#' Get Project Doc Dir Path
#'
#' Retrieves the projectDoc DIR from the projectDoc path:
#'
#' - Removes the projectDocTitle and EXT from the path, and returns
#' the ProjectDoc Dir - the path plus the projectDoc PREFIX.
#'
get_project_doc_dir_path <- function( projectDocPath, settings ) {

  substring(projectDocPath, first=1,
            last=regexpr(settings[["ProjectPrefixSep"]], projectDocPath, fixed=TRUE)-1 )

}


#' Get Project Note Dir Path
#'
#' Retrieves the project Note DIR from the project Note path:
#'
get_project_note_dir_path <- function( projectNoteFilePath, settings ) {

  substring(projectNoteFilePath, first=1,
            last=regexpr(settings[["ProjectPrefixSep"]], projectNoteFilePath, fixed=TRUE)-1 )

}


#' Get Project Note Path
#'
#' Compute a Project Note Path from a fileSystem path and a projectNote Name.
#'
#' The fileSystemPath must be a DIRECTORY and sub-dir in a PROGRAMME DIRECTORY
#' - either a project Document DIR, or another Directory in a PROGRAMME for
#' storing Project Notes.
#'
#' The NAME of the selected Directory is used to derive the PREFIX for files within this
#' Directory - it uses all LETTERS and NUMBERS UP TO the first "_" in the directory name.
#' If no Underscore exists, will use the whole Directory name as the Prefix for the projectNote.
#'
get_project_note_path <- function( fileSystemPath, projectNoteName, settings ) {

  # search inside fileSystemPath to identify any DIRECTORIES that start with prefix, and if so, compute the NEXT INDEX:
  # NB take care of both SIMPLE and GROUP directories (Prefix~NUMBER for simple, Prefix~NUMBER-00 for GROUP)
  prefix <- get_next_simple_prefix(fileSystemPath, settings)

  paste0(fileSystemPath, .Platform$file.sep, prefix, settings[["ProjectPrefixSep"]], projectNoteName, ".Rmd")

}

#' Return the Project Note Rmd Path from a Project DIR Path
#'
#'
get_project_note_pathFromDir <- function( projectNoteDir ) {

  project_note_paths <- c()

  for( i in 1:length(projectNoteDir) ) {

    fileList <- list.files( dirname(projectNoteDir[i]) )

    proj_note_path <- fileList[startsWith(fileList, basename(projectNoteDir[i])) & endsWith(fileList, ".Rmd")]

    project_note_paths <- c(project_note_paths, paste0(dirname(projectNoteDir[i]), .Platform$file.sep, proj_note_path) )

  }

  # return projectNotePaths:
  project_note_paths

}


#' Get Project Header Note Path
#'
#' Compute a Project Header Note Path from a fileSystem path and a projectNote Name.
#'
#' The fileSystemPath must be a DIRECTORY and sub-dir in a PROGRAMME DIRECTORY
#' - either a project Document DIR, or another Directory in a PROGRAMME for
#' storing Project Notes.
#'
#' The NAME of the selected Directory is used to derive the PREFIX for files within this
#' Directory - it uses all LETTERS and NUMBERS UP TO the first "_" in the directory name.
#' If no Underscore exists, will use the whole Directory name as the Prefix for the projectNote.
#'
get_header_note_path <- function( fileSystemPath, projectHeaderNoteName, settings ) {

  prefix <- get_next_header_prefix(fileSystemPath, settings)
  paste0(fileSystemPath, .Platform$file.sep, prefix, settings[["ProjectPrefixSep"]], projectHeaderNoteName, ".Rmd")

}


#' Get Project Header Note directory path
#'
#' Compute a Project Header Note Directory Path from a fileSystem path.
#'
#' The fileSystemPath must be a DIRECTORY and sub-dir in a PROGRAMME DIRECTORY
#' - either a project Document DIR, or another Directory in a PROGRAMME for
#' storing Project Notes.
#'
#' The NAME of the selected Directory is used to derive the PREFIX for files within this
#' Directory - it uses all LETTERS and NUMBERS UP TO the first "_" in the directory name.
#' If no Underscore exists, will use the whole Directory name as the Prefix for the projectNote.
#'
get_next_header_note_dir <- function( fileSystemPath, projectHeaderNoteName, settings ) {

  prefix <- get_next_header_prefix(fileSystemPath, settings)
  paste0(fileSystemPath, .Platform$file.sep, prefix)

}


#' Get Project Header Note directory path from header note path
#'
#' Compute a Project Header Note Directory Path from a fileSystem path.
#'
#' The fileSystemPath must be a DIRECTORY and sub-dir in a PROGRAMME DIRECTORY
#' - either a project Document DIR, or another Directory in a PROGRAMME for
#' storing Project Notes.
#'
#' The NAME of the selected Directory is used to derive the PREFIX for files within this
#' Directory - it uses all LETTERS and NUMBERS UP TO the first "_" in the directory name.
#' If no Underscore exists, will use the whole Directory name as the Prefix for the projectNote.
#'
get_header_note_dir_path <- function( headerNoteFilePath, settings ) {

  get_project_note_dir_path(headerNoteFilePath, settings)

}


#' Get Project Sub Note Path
#'
#' Compute a Project Sub Note Path from a fileSystem path and a projectNote Name.
#'
#' The headerNoteDir must be the directory of a header note.
#'
#' The NAME of the selected Directory is used to derive the PREFIX for files within this
#' Directory - it uses all LETTERS and NUMBERS UP TO the first "_" in the directory name.
#' If no Underscore exists, will use the whole Directory name as the Prefix for the projectNote.
#'
get_sub_note_path <- function( headerNoteDir, projectSubNoteName, projectHeaderNoteName, settings ) {

  prefix <- get_next_subnote_prefix(headerNoteDir, settings)
  paste0(headerNoteDir, .Platform$file.sep, prefix, settings[["ProjectPrefixSep"]], projectSubNoteName, ".Rmd")

}

#' Get Project Note Paths
#'
#' Get project note paths from directory tree.  Assumes `parentDirectory` is in
#' Organisation, returns blank vector if no project notes are identified.
#'
get_project_note_paths <- function(parentDirectory, settings) {

  # project note file list
  pnv <- c()
  i <- 1

  # # get all files matching FileType used in Rmd
  # pd <- fs::path_expand(parentDirectory)
  # ft <- paste0("*.", settings[["FileType"]])
  # fl <- list.files(pd, pattern = ft, recursive=TRUE, full.names=TRUE)
  orgPath <- find_org_directory(parentDirectory)

  # get config templates settings yml
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  settings <- get_settings_yml(orgPath)

  # get dirs in root to EXCLUDE from search
  volPath <- get_volumes_dir(orgPath, settings)
  sitePath <- get_site_dir(orgPath, settings)
  weeklyjournalPath <- get_weekly_journal_dir(orgPath, settings)

  fileList <- list()
  filePaths <- get_file_list_to_project_notes(
    fileList, parentDirectory, settings,
    pathExclusions = c(confPath, volPath, sitePath, weeklyjournalPath) )
  # get all Project Docs/Notes inside path - they all contain "~_" in filename and end with .Rmd
  fl <- fs::path(unlist(filePaths))

  # check each file path
  for(f in fl) {
    # if a project note add to vector
    if( get_file_type(f, settings) == "HEAD" |
        get_file_type(f, settings) == "SUB" |
        get_file_type(f, settings) == "NOTE" ) {
      pnv[i] <- fs::path_expand(f)
      i <- i+1
    }
  }

  # return as fs path object
  fs::path(pnv)
}


#' get file list down to project notes
#'
#' Traverses all directory tree in `dl` but only get fileList recursively but
#' only down to project note parent dir level.
#'
#' This method makes parsing all plaintext Project Notes across an Organisation
#' much more efficient!
#'
#' @param fileList List of files recursively retrieved by this function.
#' @param dl DirsList - a list of directory paths.
#' @param settings projectmanagr settings list.
#' @param fileExtensions File extensions of files to list.
#' @param pathExclusions Directory Paths which should be EXCLUDED from file
#' search. Typically want to exclude the config and volumes directories in the
#' root of an Organisation.
#' @param retrievalDateTimeCutoff Any project notes last modified BEFORE the
#' cutoff time will not be returned. If NULL ignored. This is a lubridate datetime
#' object, made by parsing a datetime string through `lubridate::ymd_hm()`
#'
get_file_list_to_project_notes <- function(fileList, dl, settings,
                                           fileExtensions = list("Rmd"),
                                           pathExclusions = c(),
                                           retrievalDateTimeCutoff = NULL ) {


  # testing:
  #fileList
  #dl <- orgPath
  #settings
  #fileExtensions = list("Rmd")
  #pathExclusions = c(confPath, volPath, sitePath, weeklyjournalPath)
  #retrievalDateTimeCutoff = dt

  #### Set Instance Variables ####

  # get important delimiters from settings
  #projIdentifierSep <- load_param_vector(settings[["ProjectIdentifierSep"]]) # "_"
  #projPrefixSep <- load_param_vector(settings[["ProjectPrefixSep"]]) #  "~_"
  #projIndexSep <- load_param_vector(settings[["ProjectIndexSep"]]) # "~"
  #groupIndexSep <- load_param_vector(settings[["GroupNotePrefixSep"]]) # "-"


  #### get all files from dl ####

  if( is.null(retrievalDateTimeCutoff) ) { # no cutoff filter

    # get each file with extension from all dirs in dl
    for(fe in fileExtensions) {
      # get local copy of fl to check for project notes
      fl <- paste0( dl, .Platform$file.sep,
                    list.files(path = dl, pattern = paste0("*.",fe),
                               all.files = TRUE) )
      fl <- fl[fl!=paste0(dl, .Platform$file.sep)] # remove any instances of just dl/ - if no files found!
      fileList <- c(fileList, fl )
    }

  } else { # filter for cutoff datetime

    # get each file with extension from all dirs in dl
    for(fe in fileExtensions) {
      # get local copy of fl to check for project notes
      fl <- paste0( dl, .Platform$file.sep,
                    list.files(path = dl, pattern = paste0("*.",fe),
                               all.files = TRUE) )
      fl <- fl[fl!=paste0(dl, .Platform$file.sep)] # remove any instances of just dl/ - if no files found!

      # filter for datetime - only keep files
      flf <- fl[lubridate::as_datetime(file.info(fl)$ctime, tz='UTC') > retrievalDateTimeCutoff]

      fileList <- c(fileList, flf )
    }

  }


  #### get next level of sub-dirs from dl ####

  dls <- list.dirs(path = dl, recursive=FALSE)

  # remove any directories that match pathExclusions
  dls <- dls[ !dls %in% pathExclusions ]


  #### filter next level of sub-dirs to remove SIMPLE & SUB NOTE DIRS ####

  types <- get_file_types(fl, settings)
  fl_ex <- fl[types=="NOTE" | types=="SUB"] # excluding dirs belonging to simple notes and subnotes
  fl_ex_dir <- get_project_note_dir_path(fl_ex, settings) # get the note's dirs

  # remove the project note dirs from dls
  for(fled in fl_ex_dir) {
    dls <- dls[dls != fled]
  }


  #### recursively get more files from next level of sub-dirs ####

  # recurses with each set of dls retrieved!
  for(d in dls) {
    fileList <- get_file_list_to_project_notes(
      fileList, d, settings,
      fileExtensions, pathExclusions, retrievalDateTimeCutoff)
  }

  #### return list of files down to project note dirs ####

  # return fileList
  fileList


}




#' Get Path from Link
#'
#' Extracts the path from a markdown link - content between `](` && `)`.
#'
#' @param linkString String containing at least one link.
#'
#' @param linkIndex If the string contains more than one link, which link to
#' get the path from. Default is the first (`1`).
#'
get_path_from_link <- function(linkString, linkIndex=1) {

  firstPos <- (gregexpr("](", linkString, fixed=TRUE)[[1]][linkIndex])+2
  lastPos <- (gregexpr(")", linkString, fixed=TRUE)[[1]][linkIndex])-1
  substr(linkString, firstPos, lastPos)

}


#' Get Absolute Path from full + relative paths
#'
#' Collapses paths to one absolute path
#'
#' @param fullPath initial full (absolute) path as source.
#'
#' @param relativePath A relative path from the fullPath, to collapse.
get_absolute_path <- function(fullPath, relativePath) {

  R.utils::getAbsolutePath( paste0(fullPath, .Platform$file.sep, relativePath))

}


