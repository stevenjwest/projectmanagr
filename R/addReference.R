#' Add a New Reference
#'
#' This Function adds a new Reference to a Project Note - saved in the PROGRAMMES' REF/ Dir.
#' The Reference is formed using an Rmd template: Reference-Template.Rmd from the projectmanagr
#' package. Cheatheets compile to PDF as standard, and can therefore be used as independent files.
#'
#' References are stored in the REF/ directory inside the PROGRAMME Directory.  Each Reference
#' exists in its own directory, to keep its compiled files together.
#'
#' The Reference source Rmd will link to its creating Project Note, and the Project Note will link to the
#' compiled PDF of the Reference.
#'
#' References can further be inserted into new Project Notes, using the insertProcotol() function, which
#' allows convenient insertion of documentation into new Notes.
#'
#' @param projectNotePath The ABSOLUTE path of the Project Note.
#'
#' @param referenceName The name of the reference, a Title with all SPACES replaced
#' with - or _.
#'
#' @param referenceTitle The title of the reference, by default the name with all - and _ replaced
#' with SPACES.
#'
#' @param referenceTemplate Template to use, as found in the `config/templates/` directory.  Default is
#' "Reference-Template.Rmd"
#'
#' @export
addReference <- function( projectNotePath, referenceName, referenceTitle="", referenceTemplate="Reference-Template.Rmd"  ) {

  cat( "\nprojectmanagr::addReference():\n" )

  # Check referenceName contains NO SPACES:
  if( grepl("\\s+", referenceName) ) {
    stop( paste0("  referenceName contains a SPACE: ", referenceName) )
  }

  # Check referenceTitle, and if blank, fill with referenceName, replacing all "_" and "-" with spaces
  if( nchar(referenceTitle)==0 ) {
    referenceTitle = gsub("-", " ", gsub("_", " ", referenceName) )
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

  # get REF path:
  refPath <- paste0(progPath, .Platform$file.sep, "REF")


  # CODE TO DEFINE THE PROJECT NOTE PREFIX DIR PATH:
  # get the project Parent DIR, projectName, projectDIR:
  #projectParentDir <- dirname(projectNotePath)
  #projectPrefixName <- basename(projectNotePath)
  #projectName <- substr(projectPrefixName, regexpr("~_", projectPrefixName)+2, nchar(projectPrefixName))
  #projectPrefix <- substr(projectPrefixName, 1, regexpr("~_", projectPrefixName)-1 )

  #projectPrefixDirPath <- paste0( projectParentDir, .Platform$file.sep, projectPrefix )

  # noramlise path - as this is a SYMLINK, want the actual destination path!
  #projectPrefixDirPath <- normalizePath(projectPrefixDirPath)


  # read reference template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, referenceTemplate, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)

  # Save reference to the project note DIR using its Name:
  referenceDirPath <- paste( refPath, .Platform$file.sep, referenceName, sep="")
  done <- dir.create(referenceDirPath)

  if(!done) {
    stop( paste0("  reference Path could not be created: ", referenceDirPath) )
  }

  referencePath <- paste( referenceDirPath, .Platform$file.sep, referenceName, ".Rmd", sep="")
  done <- file.create( referencePath )

  if(!done) {
    stop( paste0("  reference could not be created: ", referencePath) )
  }

  cat( "  Made reference File: ", referencePath, "\n" )


  # extract the Author value from the settings.yml file:
  #settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  #settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  #authorValue <- settings[["Author"]]
  authorValue <- Sys.info()["user"] # use username as author instead

  # modify templateContents to include PREFIX and projectTitle
  templateContents <- gsub("{{TITLE}}", referenceTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)


  ### compute Project Source Doc RELATIVE LINK:

  DocLink <- R.utils::getRelativePath(projectNotePath, relativeTo=referencePath)
  DocLink <- substring(DocLink, first=4, last=nchar(DocLink)) # remove first `../`
  # DocLink <- paste( substring(DocLink, first=1, last=nchar(DocLink)-4), "/", sep="")
  # for now have left links as ".Rmd", but this needs to be ".html", or "/" in a rendered website!
  # Its set as ".Rmd" as ".Rmd" links can be navigated in RStudio!  CONVERT LINKS in update() function

  DocName <- basename(projectNotePath)
  DocName <- gsub("-", " ",  gsub("_", " ", substring(DocName, first=1, last=nchar(DocName)-4) )  )

  DocTitleLink <- paste( "[", DocName, "](", DocLink, ")", sep="" )


  templateContents <- gsub("{{PROJECT_NOTE_LINK}}", DocTitleLink, templateContents, fixed=TRUE)
  #templateContents <- gsub("{{PROJECT_NOTE_TITLE}}", DocName, templateContents, fixed=TRUE)


  # write to referenceFile
  fileConn <- file(referencePath)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written Title and Author to reference file: ", basename(referencePath), "\n" )

}
