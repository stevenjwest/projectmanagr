
#' Split File Path and Header Anchor
#'
#' Splits a string containing a file path and optional Markdown-style section
#' header (denoted by `#`) into two components: the base file path and the
#' section anchor.
#'
#' This utility is designed to support functions in `hyperlink-functions.R`
#' that construct or parse hyperlinks to R Markdown (`.Rmd`) files, where
#' links may point to entire files or to specific sections within them.
#'
#' For example, a link like `"notes/myDoc.Rmd#summary-section"` contains both a
#' file path and a header anchor. This function extracts:
#' - `path = "notes/myDoc.Rmd"`
#' - `header = "#summary-section"`
#'
#' @param kPath Character string. The combined file path and optional header
#' anchor (e.g., `"file.Rmd#section-name"`).
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{path}{The base file path (before `#`).}
#'   \item{header}{The header anchor (including leading `#`), or an empty string if not present.}
#' }
#'
#' @examples
#' split_path_header("file.Rmd#section1")
#' # returns list(path = "file.Rmd", header = "#section1")
#'
#' split_path_header("file.Rmd")
#' # returns list(path = "file.Rmd", header = "")
#'
#' @seealso \code{\link{create_hyperlink}}, \code{\link{create_hyperlink_section}}
#'
#' @keywords internal

split_path_header <- function(kPath) {

  # does kpath contain the path#header separator - # ??
  if( length(grep('#', kPath, fixed=TRUE)) == 1 ) {
    headI <- gregexpr('#', kPath)[[1]]
    sPath <- substr(kPath, 1, (headI-1))
    sHeader <- substr(kPath, headI, nchar(kPath))
  } else {
    sPath <- kPath
    sHeader <- ""
  }

  # return named list
  list(
    path = sPath,
    header = sHeader
  )
}

#' create link from Project Doc to Project Note
#'
#' For inserting into a Project Doc under a GDT to form a link between the Note
#' and Doc GDT.
doc_note_link <- function(projNoteRmdPath, projectDocPath, settings) {
  paste0(settings[["NoteLinkFormat"]],
         create_hyperlink(
           get_prefix_name_from_file_path(projNoteRmdPath, settings),
           projNoteRmdPath, projectDocPath),
         settings[["NoteLinkFormat"]])
}


#' create link from Project Doc to Project Sub Note
#'
#' For inserting into a Project Doc under a GDT to form a link between the Note
#' and Doc GDT.
doc_head_link <- function(headerNoteRmdPath, projectDocPath, settings) {
  paste0(settings[["HeaderLinkFormat"]],
         create_hyperlink(
           get_prefix_name_from_file_path(headerNoteRmdPath, settings),
           headerNoteRmdPath, projectDocPath),
         settings[["HeaderLinkFormat"]])
}

#' create link from Project Doc to Project Sub Note
#'
#' For inserting into a Project Doc under a GDT to form a link between the Note
#' and Doc GDT.
doc_sub_link <- function(projSubRmdPath, projectDocPath, settings) {
  paste0(settings[["SubNoteLinkFormat"]],
         create_hyperlink(
           get_prefix_name_from_file_path(projSubRmdPath, settings),
           projSubRmdPath, projectDocPath),
         settings[["SubNoteLinkFormat"]])
}


#' Create R Markdown Hyperlink with Relative Path
#'
#' Generates a Markdown-formatted hyperlink pointing from one R Markdown file
#' (`fromFilePath`) to another (`toFilePath`). The visible link text is specified
#' by `toFileName`. The link target is computed as a relative path from the source
#' file to the destination file, and then minimally cleaned to remove leading
#' `../` segments for more concise Rmd hyperlink rendering.
#'
#' This function is used within the `projectmanagr` package to build structured
#' cross-references between Project Docs, Project Notes, and Sub-Notes, enabling
#' traceable documentation of research goals, deliverables, and experiments.
#'
#' @param toFileName Character. The display text of the hyperlink (e.g., `"note-001"`).
#' Typically derived from the filename prefix or a custom label.
#'
#' @param toFilePath Character. Full or relative path to the **target** `.Rmd` file.
#' This is the destination file the link should point to.
#'
#' @param fromFilePath Character. Full or relative path to the **source** `.Rmd` file.
#' This is the file where the hyperlink will be inserted.
#'
#' @return A character string containing a properly formatted Markdown hyperlink,
#' e.g., \code{"[note-001](notes/note-001.Rmd)"}.
#'
create_hyperlink <- function(toFileName, toFilePath, fromFilePath) {
  NoteLink <- R.utils::getRelativePath(toFilePath, relativeTo=fromFilePath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`

  HyperLink <- paste0(
    "[",
    toFileName,
    "](",
    NoteLink,
    ")")
  return(HyperLink)
}


#' Create Hyperlink Without File Extension in Link Text
#'
#' Generates a Markdown-formatted hyperlink pointing from one `.Rmd` file
#' (`fromFilePath`) to another (`toFilePath`), using the **base name without file
#' extension** as the link text.
#'
#' This is a convenience wrapper for \code{\link{create_hyperlink}} used when the
#' display name of the link should omit `.Rmd` or other file extensions.
#'
#' @param toFilePath Character. Full or relative path to the **target** `.Rmd` file.
#'
#' @param fromFilePath Character. Full or relative path to the **source** `.Rmd` file
#' from which the relative link should be computed.
#'
#' @return A character string containing a properly formatted Markdown hyperlink,
#' using the file name without extension as the link text.
#'
create_hyperlink_no_ext <- function(toFilePath, fromFilePath) {

  toFileName <- basename(toFilePath)
  toFileNameNoExt <- substring(toFileName,
                               first=1,
                               last=regexpr("\\.[^\\.]*$", toFileName)-1 )

  create_hyperlink(toFileNameNoExt,
                   toFilePath,
                   fromFilePath)
}


#' Create Hyperlink Section
#'
#' Creates a string for hyperlink in Rmd, using \code{toFileName : toFileSection}
#' as the hyperlink text, and generating a RELATIVE LINK to
#' \code{toFilePath#toFileSection} from \code{fromFilePath}.
#'
#' Internally, the heading portion of the link (the anchor after `#`) is generated
#' in a way similar to how pandoc creates HTML anchors for headings: all characters
#' are lowercased, punctuation replaced with a dash, etc.
#'
#' @param toFileName Character. A short name for the file you're linking to.
#' @param toFileSection Character. The Markdown header (section) in \code{toFilePath}.
#' @param toFilePath Character. Path to the target Rmd file.
#' @param fromFilePath Character. Path to the Rmd file from which the link is created.
#'
#' @return A character string containing the relative link suitable for R Markdown.
#'
#' @examples
#' \dontrun{
#'   create_hyperlink_section(
#'     "myDoc.Rmd",
#'     "Section Title!",
#'     "path/to/myDoc.Rmd",
#'     "path/to/currentDoc.Rmd"
#'   )
#' }
create_hyperlink_section <- function(toFileName, toFileSection, toFilePath, fromFilePath) {

  NoteLink <- R.utils::getRelativePath(toFilePath, relativeTo = fromFilePath)
  NoteLink <- substring(NoteLink, first = 4, last = nchar(NoteLink)) # remove first ../

  NoteLink <- link_add_section(NoteLink, toFileSection)
  HyperLink <- paste0(
    "[",
    toFileName,
    " : ",
    gsub('#', '', toFileSection, fixed = TRUE),
    "](",
    NoteLink,
    ")"
  )
  HyperLink
}


#' Add Section to Link
#'
#' \code{NoteLink} is just a path to a file to link to.
#' \code{toFileSection} is the Markdown header of a section in \code{NoteLink}.
#'
#' This function appends the header as an HTML-friendly anchor, using pandoc-like
#' rules. For example, a heading `My Special--Section!` becomes `#my-special-section`.
#'
#' @param NoteLink Character. A path to the file (without the section).
#' @param toFileSection Character. The actual heading text in the Rmd file to link to.
#'
#' @return A character string with the section anchor appended (e.g. "#my-special-section").
#'
#' @examples
#' \dontrun{
#'   link_add_section("path/to/myDoc.Rmd", "My Special--Section!")
#' }
link_add_section <- function(NoteLink, toFileSection) {
  anchor <- sanitize_header_for_anchor(toFileSection)
  paste0(NoteLink, "#", anchor)
}

#' Sanitize Markdown Header for Anchor Link
#'
#' Converts a Markdown header string into a standardized anchor format
#' suitable for internal hyperlinking, following rules similar to those used
#' by Pandoc when generating HTML IDs from headers.
#'
#' This function is a core utility for generating internal section links between
#' `.Rmd` files in the `projectmanagr` package, ensuring that links to specific
#' headers (e.g., using \code{#my-section}) match the actual anchors created
#' during R Markdown rendering.
#'
#' @param txt Character string. The raw header text from a Markdown section (e.g.,
#' `"My Special--Section!"`).
#'
#' @return A character string representing a sanitized anchor. All characters are
#' converted to lowercase, sequences of non-alphanumeric characters are replaced
#' by a single dash (`-`), and leading or trailing dashes are stripped.
#'
#' @seealso \code{\link{create_hyperlink_section}}, \code{\link{link_add_section}}
#'
#' @keywords internal
sanitize_header_for_anchor <- function(txt) {
  # Convert to lower case
  txt <- tolower(txt)
  # Replace any sequence of non-alphanumeric characters with a single dash
  txt <- gsub("[^[:alnum:]]+", "-", txt)
  # Remove leading or trailing dashes
  txt <- gsub("^-+|-+$", "", txt)
  # That's enough for standard pandoc-like anchor generation
  txt
}


#' Update Filenames in Links in Files within a Directory Tree
#'
#' Updates references to a specific filename in files within a directory tree,
#' replacing the old filename with a new name. This includes replacing the
#' corresponding links within the file contents. Processes specific file
#' extensions and excludes certain directories from processing.
#'
#' @param oldFileName Character string of the basename of the file, including prefix
#' and extension of the old filename to be replaced. Must not contain spaces.
#' @param newName Character string. The new name to replace the old filename. Must
#' not contain spaces.
#' @param dirTree Character string. The path to the root directory containing the
#' files to be updated.
#' @param settings List. Configuration details, including the \code{ProjectPrefixSep}
#' and \code{VolumesDir}.
#' @param fileExtensions List. A list of file extensions to include in the search
#' (e.g., \code{list("Rmd")}). Defaults to \code{list("Rmd")}.
#'
#' @details
#' This function identifies all files matching the specified extensions in the
#' directory tree. It processes each file, searching for links or references
#' that match the \code{oldFileName}, and replaces them with the \code{newName}.
#'
#' The function ensures:
#' - Both \code{oldFileName} and \code{newName} do not contain spaces.
#' - Excluded paths (e.g., configuration or volume directories) are not processed.
#' - Links are reconstructed using the project prefix and the provided settings.
#'
#' Files are updated in place, and any changes made are logged to the console.
#'
#' @return
#' The function does not return a value but modifies files in place as necessary.
#'
#' @examples
#' # Update file references in Rmd files within the directory
#' update_links_filenames(
#'   oldFileName = "project_old.Rmd",
#'   newName = "project_new",
#'   dirTree = "/path/to/directory",
#'   settings = list(ProjectPrefixSep = "_", VolumesDir = "volumes"),
#'   fileExtensions = list("Rmd")
#' )
#'
#' @seealso
#' - \code{\link{find_org_directory}} for locating the ORG directory.
#' - \code{\link{get_file_list_to_project_notes}} for recursive file traversal.
#' - \code{\link{read_file}} and \code{\link{write_file}} for file I/O operations.
#'
#' @note
#' - Ensure that both \code{oldFileName} and \code{newName} do not contain spaces.
#' - Files are modified in place. Backup your directory before using this function.
#'
#' @export
update_links_filenames <- function( oldFileName, newName, dirTree, settings,
                                    fileExtensions = list("Rmd") ) {

  #### set and check  variables ####
  # check oldFileName contains NO SPACES:
  if( grepl("\\s+", oldFileName) ) {
    stop( paste0("  oldFileName contains a SPACE: ",oldFileName) )
  }

  # check newName contains NO SPACES:
  if( grepl("\\s+", newName) ) {
    stop( paste0("  newName contains a SPACE: ",newName) )
  }

  # make dirTree full path
  dirTree <- fs::path_expand(dirTree)

  # get orgPath from dirTree
  orgPath <- confirm_find_org(dirTree)

  # get config path to exclude files from (in case dirTree is orgPath!)
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  volPath <- paste0(orgPath, .Platform$file.sep, settings[["VolumesDir"]])

  # split oldFileName into PREFIX, NAME, EXTENSION
  result <- split_proj_file_prefix_name_ext(oldFileName, settings)
  oldPrefix <- result$prefix
  oldName   <- result$name
  oldFileNameExt    <- result$ext

  # construct oldLink: prefix plus oldName (no extension) for replacing
  oldLink <- paste0(oldPrefix, settings[["ProjectPrefixSep"]], oldName)

  # construct newLink: prefix plus newName (no extension) for replacing
  newLink <- paste0(oldPrefix, settings[["ProjectPrefixSep"]], newName)


  #### get file list ####

  files <- get_file_list_to_project_notes(
    dirTree, settings,
    fileExtensions = fileExtensions,
    pathExclusions = c(confPath, volPath))


  #### replace oldLink with newLink ####

  for(fl in files) {

    # read file:
    contents <- read_file(fl)
    linkLines <- grepl(oldLink, contents, fixed=TRUE )
    if( any( linkLines ) ) {
      # replace oldName with newName in contents in link lines too
      contents[linkLines] <- gsub(oldName, newName, contents[linkLines], fixed=TRUE)
      # better to just replace old and new names - ensures names without prefix are properly renamed
      # and just do this replacement on the lines with oldLink in them
      # replace oldLink with newLink in contents
      #contents[linkLines] <- gsub(oldLink, newLink, contents[linkLines], fixed=TRUE)
      # and save file
      cat( "    replaced link(s) in file:",
           fs::path_rel(fs::path(fl), start=fs::path(dirTree)) ,"\n" )
      write_file(contents, fl)
    }
  }
}


#' Update Links in Files within a Directory Tree
#'
#' Searches for and replaces old link suffixes with new link suffixes in files
#' located in a specified directory tree. Optionally replaces specific strings
#' within the same files. Recursively processes subdirectories while excluding
#' specific paths.
#'
#' @param oldLinkSuffix Character string. The old link suffix to be replaced (e.g., \code{"old_suffix"}).
#' @param newLinkSuffix Character string. The new link suffix to replace the old suffix (e.g., \code{"new_suffix"}).
#' @param dirTree Character string. The path to the root directory containing files to be updated.
#' @param settings List. Contains configuration details such as the volumes directory key (\code{"VolumesDir"}).
#' @param oldLinkString Character string. Optional. A specific string within files to replace. Default is an empty string (\code{""}).
#' @param newLinkString Character string. Optional. The replacement for \code{oldLinkString}. Default is an empty string (\code{""}).
#' @param fileExtensions List. A list of file extensions (e.g., \code{list("Rmd")}) to include in the search. Defaults to \code{list("Rmd")}.
#'
#' @details
#' The function normalizes the input directory path and identifies the ORG directory.
#' It generates a list of files to be processed, excluding specific directories
#' such as the configuration and volume directories. For each file, it reads
#' the contents, identifies occurrences of \code{oldLinkSuffix}, replaces them
#' with \code{newLinkSuffix}, and optionally replaces \code{oldLinkString} with
#' \code{newLinkString}.
#'
#' The directory tree is traversed recursively, but only down to the project note
#' parent directory level. Any file changes are saved, and the relative paths
#' of modified files are logged to the console.
#'
#' @return
#' The function does not return a value but modifies files in place as necessary.
#'
#' @examples
#' # Update links in Rmd files within the given directory
#' update_links(
#'   oldLinkSuffix = "_old",
#'   newLinkSuffix = "_new",
#'   dirTree = "/path/to/directory",
#'   settings = list(VolumesDir = "volumes"),
#'   fileExtensions = list("Rmd")
#' )
#'
#' # Replace specific strings in addition to link suffixes
#' update_links(
#'   oldLinkSuffix = "_v1",
#'   newLinkSuffix = "_v2",
#'   dirTree = "/path/to/directory",
#'   settings = list(VolumesDir = "volumes"),
#'   oldLinkString = "http://old.link",
#'   newLinkString = "http://new.link",
#'   fileExtensions = list("Rmd", "txt")
#' )
#'
#' @seealso
#' - \code{\link{find_org_directory}} for locating the ORG directory.
#' - \code{\link{get_file_list_to_project_notes}} for recursive file traversal.
#' - \code{\link{read_file}} and \code{\link{write_file}} for file I/O operations.
#'
#' @note
#' - Ensure that the \code{settings} list contains a valid \code{"VolumesDir"} key.
#' - Files are modified in place. It is recommended to back up your directory before using this function.
#'
#' @export
update_links <- function( oldLinkSuffix, newLinkSuffix, dirTree, settings,
                          oldLinkString="", newLinkString="", fileExtensions = list("Rmd") ) {

  # make dirTree full path
  dirTree <- fs::path_expand(dirTree)

  # get orgPath from dirTree
  orgPath <- find_org_directory(dirTree)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ",
                 projectNotePath) )
  }

  # get config path to exclude files from (in case dirTree is orgPath!)
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  volPath <- paste0(orgPath, .Platform$file.sep, settings[["VolumesDir"]])


  #### get file list ####

  files <- get_file_list_to_project_notes(
    dirTree, settings,
    fileExtensions = fileExtensions,
    pathExclusions = c(confPath, volPath))


  #### replace oldLinkSuffix with newLinkSuffix ####

  for(fl in files) {

    contents <- read_file(fl)
    cL <- grepl(oldLinkSuffix, contents, fixed=TRUE )
    if( any(cL) ) {
      # replace oldLinkSuffix with newLinkSuffix in contents
      contents[cL] <- gsub(oldLinkSuffix, newLinkSuffix, contents[cL], fixed=TRUE)
      if(oldLinkString!="" && newLinkString!="") {
        contents[cL] <- gsub(oldLinkString, newLinkString, contents[cL], fixed=TRUE)
      }
      # and save file
      cat( "    replaced link(s) in file:",
           fs::path_rel(fs::path(fl), start=fs::path(dirTree)) ,"\n" )
      write_file(contents, fl)
    }
  }
}



#' Update Headers
#'
#' Updates every header in all Project Notes within `dirTree`, replacing
#' `oldHeader` with `newHeader`.
#'
#' @param oldHeader Old header string to be updated - must start with `#` to
#' denote a markdown header.
#'
#' @param newHeader New header string to replace `oldHeader` - must start with
#' `#` to denote a markdown header.
#'
#' @param dirTree Directory tree to search for files for replacing links in.
#'
#' @param settings projectmanagr settings list.
#'
#' @param fileExtensions List of file extensions indicating what types of files
#' should be searched for links and have links replaced.  Must be plaintext file
#' type.  Default is Rmd files only.
#'
#' @export
update_headers <- function( oldHeader, newHeader, dirTree, settings,
                            oldHeaderString="", newHeaderString="", fileExtensions = list("Rmd") ) {


  # make dirTree full path
  dirTree <- fs::path_expand(dirTree)

  # get orgPath from dirTree
  orgPath <- find_org_directory(dirTree)
  if(orgPath == "" ) { # only if orgPath not identified
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ",
                 projectNotePath) )
  }

  # get config path to exclude files from (in case dirTree is orgPath!)
  confPath <- get_config_dir(orgPath)
  tempPath <- get_template_dir(orgPath)
  volPath <- paste0(orgPath, .Platform$file.sep, settings[["VolumesDir"]])


  #### get file list ####

  files <- get_file_list_to_project_notes(
    dirTree, settings,
    fileExtensions = fileExtensions,
    pathExclusions = c(confPath, volPath))


  #### replace oldHeader with newHeader ####

  for(fl in files) {

    contents <- read_file(fl)
    cL <- startsWith(contents, oldHeader) # use starts with to remove blank space
    if( any(cL) ) {
      # replace oldHeader with newHeader in contents
      contents[cL] <- gsub(oldHeader, newHeader, contents[cL], fixed=TRUE)
      if(oldHeaderString!="" && newHeaderString!="") {
        contents[cL] <- gsub(oldHeaderString, newHeaderString, contents[cL], fixed=TRUE)
      }
      # and save file
      cat( "    replaced link(s) in file:", fl ,"\n" )
      write_file(contents, fl)
    }
  }
}

