#' Add a New Protocol
#'
#' This Function adds a new Protocol to a Project Note - saved in the PROGRAMMES' SOP/ Dir.
#' The Protocol is formed using a Rmd template, and uses the tufte::tufte_handout format to
#' generate a PDF of the protocol.
#'
#' Protocols are stored in the SOP/ directory inside the PROGRAMME Directory.  Each Protocol
#' exists in its own directory, to keep its compiled files together.
#'
#' The Protocol source Rmd will link to its creating Project Note, and the Project Note will link to the
#' compiled PDF of the Protocol.
#'
#' @param projectNotePath The ABSOLUTE path of the Project Note.
#' @param protocolName The name of the Protocol, a Title with all SPACES replaced
#' with - or _.
#' #' @param protocolTitle The title of the Protocol, by default the name with all - and _ replaced
#' with SPACES.
#' @param protocolTemplate Template to use, as found in the `config/templates/` directory.  Default is
#' "Protocol-Template-Tufte.Rmd"
#'
#' @export
addProtocol <- function( projectNotePath, protocolName, protocolTitle="", protocolTemplate="Protocol-Template-Tufte.Rmd"  ) {

  cat( "\nprojectmanagr::addProtocol():\n" )

  # Check protocolName contains NO SPACES:
  if( grepl("\\s+", protocolName) ) {
    stop( paste0("  protocolName contains a SPACE: ", protocolName) )
  }

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(protocolTitle)==0 ) {
    protocolTitle = gsub("-", " ", gsub("_", " ", protocolName) )
  }

  # Check projectNotePath is a sub-dir in a Programme DIR, which is a sub-dir to the root of an ORGANISATION:
    # run dirname TWICE as want to ensure projectNotePath is a sub-dir in a Programme!
  orgPath <- dirname( dirname(projectNotePath) )

  orgPath <- findOrgDir(orgPath)

  if(orgPath == "" ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, projectNotePath is not inside a PROGRAMME sub-dir!
    stop( paste0("  projectNotePath is not in a sub-dir of a PROGRAMME Directory: ", projectNotePath) )
  }
  # now, orgPath should be the root dir of the organisation

  # set confPath + tempPath:
  confPath <- paste0( orgPath, .Platform$file.sep, "config" )
  tempPath <- paste0( confPath, .Platform$file.sep, "templates" )

  # normalize path - remove HOME REF ~
  projectNotePath <- normalizePath(projectNotePath)


  # get the progPath:
  progPath <- findProgDir(projectNotePath)

  # get SOP path:
  protocolsPath <- paste0(progPath, .Platform$file.sep, "SOP")


  # CODE TO DEFINE THE PROJECT NOTE PREFIX DIR PATH:
  # get the project Parent DIR, projectName, projectDIR:
  #projectParentDir <- dirname(projectNotePath)
  #projectPrefixName <- basename(projectNotePath)
  #projectName <- substr(projectPrefixName, regexpr("~_", projectPrefixName)+2, nchar(projectPrefixName))
  #projectPrefix <- substr(projectPrefixName, 1, regexpr("~_", projectPrefixName)-1 )

  #projectPrefixDirPath <- paste0( projectParentDir, .Platform$file.sep, projectPrefix )

  # noramlise path - as this is a SYMLINK, want the actual destination path!
  #projectPrefixDirPath <- normalizePath(projectPrefixDirPath)



  # read Protocol template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, protocolTemplate, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)

  # Save Protocol to the project note DIR using its Name:
  protocolDirPath <- paste( protocolsPath, .Platform$file.sep, protocolName, sep="")
  done <- dir.create(protocolDirPath)

  if(!done) {
    stop( paste0("  Protocol Path could not be created: ", protocolDirPath) )
  }

  protocolPath <- paste( protocolDirPath, .Platform$file.sep, protocolName, ".Rmd", sep="")
  done <- file.create( protocolPath )

  if(!done) {
    stop( paste0("  Protocol could not be created: ", protocolPath) )
  }

  cat( "  Made Protocol File: ", protocolPath, "\n" )


  # extract the Author value from the settings.yml file:
  settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  authorValue <- settings[["Author"]]

  # modify templateContents to include PREFIX and projectTitle
  templateContents <- gsub("{{TITLE}}", protocolTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)



  ### compute Project Source Doc RELATIVE LINK:


  DocLink <- R.utils::getRelativePath(projectNotePath, relativeTo=protocolPath)
  DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # remove first `../`
  # DocLink <- paste( substring(DocLink, first=1, last=nchar(DocLink)-4), "/", sep="")
  # for now have left links as ".Rmd", but this needs to be ".html", or "/" in a rendered website!
  # Its set as ".Rmd" as ".Rmd" links can be navigated in RStudio!  CONVERT LINKS in update() function

  DocName <- basename(projectNotePath)
  DocName <- gsub("-", " ",  gsub("_", " ", substring(DocName, first=1, last=nchar(DocName)-4) )  )

  DocTitleLink <- paste( "[", DocName, "](", DocLink, ")", sep="" )


  templateContents <- gsub("{{PROJECT_NOTE_LINK}}", DocTitleLink, templateContents, fixed=TRUE)
  #templateContents <- gsub("{{PROJECT_NOTE_TITLE}}", DocName, templateContents, fixed=TRUE)


  # write to protocolFile
  fileConn <- file(protocolPath)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written Title and Author to Protocol file: ", basename(protocolPath), "\n" )

}
