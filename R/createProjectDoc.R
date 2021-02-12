#' Create a New Project
#'
#' Generates a new Project inside a Programme in an Organisation.  The fileSystemPath
#' must be in the Programme directory, which is the sub-Dir to the root ORGANISATION
#' dir.
#'
#' The project is automatically numbered, by reading the current projects and determining
#' the next number in the sequence, as well as deriving the Programme Prefix.
#'
#' User must supply the project name, which must contain NO SPACES, and optionally a project
#' title.  If no title is provided, the Title is derived from the project name, replacing any
#' "_" & "-" with " ". The default fileSystemPath is the working directory.
#'
#' @param projectName Name of Project - must NOT contain a space.
#'
#' @param projectTitle Title of project - typically the projectName with "_" & "-" replaced with spaces.
#'
#' @param fileSystemPath Path to insert the Project into.  This must be a Programme dir, one level below the
#' organisation, and containing a PROJECTS/ directory.  The Project will be placed into the PROJECTS/ directory.
#' If none found, the method will end without making a Project
#'
#' @param projDocTemplate Rmd template used to create the Project Doc - default is "Project-Doc-Template.Rmd"
#'
#' @param projectIndex If 0, the function will determine the projectIndex by searching the PROJECTS/ directory.
#' Otherwise, the projectIndex is used to number the Project in its Prefix.
#'
#' @export
createProjectDoc <- function(projectName, projectTitle="", fileSystemPath=getwd(),
                             projDocTemplate="Project-Doc-Template.Rmd", projectIndex=0 ) {

  cat( "\nprojectmanagr::createProjectDoc():\n" )

  # check projectName contains NO SPACES:
  if( grepl("\\s+", projectName) ) {
    stop( paste0("  projectName contains a SPACE: ", projectName) )
  }

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  progPath <- checkProgDir(fileSystemPath)

  if(  progPath == ""  ) {
    # the search reached the root of the filesystem without finding the Organisation files,
    # therefore, fileSystemPath is not inside a PROGRAMME sub-dir!
    stop( paste0("  fileSystemPath is not in a PROGRAMME Directory: ", fileSystemPath) )
  }

  # get the orgPath from fileSystemPath - confirmed above to sit in an organisation!
  orgPath <- findOrgDir(fileSystemPath)
  # set confPath + tempPath:
  confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")

  # fileSystemPath is therefore in a PROGRAMME DIR


  # extract the PROGRAMME NAME from the fileSystemPath:
  programmeName <-basename(progPath)

  # define the projects path - PROJECTS/ dir in progPath
  projsPath <- paste( progPath, .Platform$file.sep, "PROJECTS", sep="")


  # extract the programme prefix from its config file:
  statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )
  programmePrefix <- status[["PROGRAMMES"]][[programmeName]][["programmePrefix"]]


  if(projectIndex < 1) { # if projectIndex is below 1 (default is 0), then try to identify what projectIndex should be
                         # by looking at DIR numbers:

    # read all DIRs in projsPath that start with prefix:
    directories <- dir(projsPath, recursive = FALSE, full.names = FALSE, pattern= paste(programmePrefix,"[0-9]{1,}[~]{1}[_]{1}", sep="")  )

    projectIndexes <- sapply( directories, function(x)
                  substr(x, gregexpr(programmePrefix, x)[[1]][1]+nchar(programmePrefix), gregexpr("~", x)[[1]][1]-1 )  )

    projectIndex <- sort( as.numeric(projectIndexes) )[length(projectIndexes)]

    projectIndex <- projectIndex+1 # add one to max projectIndex

    if(length(projectIndex) == 0 ) {

      projectIndex <- 1 #this ensures if there are NO directories that match the glob above, that the index is set to 1!

    }

    # if projectIndex is only one digit, append "0" to front:
    if(projectIndex < 10 ) {

      projectIndex <- paste("0", projectIndex, sep="")

    } else {

      projectIndex <- paste("", projectIndex, sep="")

    }

  } else { # else, if projectIndex was set to be above 0, then use this number!

    if(projectIndex < 10 ) {

        projectIndex <- paste("0", projectIndex, sep="")

    } else {

      projectIndex <- paste("", projectIndex, sep="")

    }

  }


  ### CREATING THE PROJECT: ###

  # create Dir:
  projPath = paste(projsPath, .Platform$file.sep, programmePrefix, projectIndex, sep="")
  done <- dir.create(projPath)

  if(!done) {
    stop( paste0("  Project directory could not be created: ", projPath) )
  }

  cat( "  Made Project dir: ",projPath, "\n" )


  # create Rmd file:
  projFile = paste(projsPath, .Platform$file.sep, programmePrefix, projectIndex, "~_", projectName, ".Rmd", sep="")
  done <- file.create(projFile)

  if(!done) {
    stop( paste0("  Project file could not be created: ", projFile) )
  }

  cat( "  Made Project file: ", projFile, "\n" )


  # read project doc template:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, projDocTemplate, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)

  # Check projectTitle, and if blank, fill with projectName, replacing all "_" and "-" with spaces
  if( nchar(projectTitle)==0 ) {
    projectTitle = gsub("-", " ", gsub("_", " ", projectName) )
  }


  # extract the Author value from the settings.yml file: NOW USE username
  #settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  #settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  #authorValue <- settings[["Author"]]
  authorValue <- Sys.info()["user"] # use username as author instead

  # modify templateContents to include PREFIX and projectTitle
  templateContents <- gsub("{{PREFIX}}", paste(programmePrefix, projectIndex, sep=""), templateContents, fixed=TRUE)
  templateContents <- gsub("{{TITLE}}", projectTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)

  # write to projFile
  fileConn <- file(projFile)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written template to Project file: ", projFile, "\n" )


  # Write PROJECT to the status.yml file:
  # NO LONGER USING STATUS.YML TO HOLD DATA ON ALL DOCS AND NOTES

  # Read the status.yml file first into a LIST:
  #statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  #status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  # add programmeName, programmePrefix, projectIndex under the FULL projectName (including prefix, index and name)
    # in the "PROJECTS" section of the status.yml List:
  #attrs <- list(programmeName, programmePrefix, projectIndex, as.character(file.info(projFile)[,5]) )
  #names(attrs) <- c("programmeName", "programmePrefix", "projectIndex", "creationTime")
  #status[["PROJECTS"]][[ paste(programmePrefix, projectIndex, "~_", projectName, sep="") ]] <- attrs
  # can retrieve the programmePrefix with call to:
    # status[["PROJECTS"]][[projectName]][["programmeName"]]
    # status[["PROJECTS"]][[projectName]][["projectPrefix"]]

  # Write status list to the statusFile:
  #yaml::write_yaml( yaml::as.yaml(status), statusFile )

  #cat( "  Written PROJECT to Status.yml file: ", statusFile, "\n" )


  ### DEPRECATED - WRITE PROJECT DOC TO ORGANISATION INDEX FILE:

  # read Organisation Index File: orgName in status
  #orgIndexPath = paste(orgPath, .Platform$file.sep, "index_", status[["orgName"]], ".Rmd", sep="")
  #orgIndexFileConn <- file( orgIndexPath )
  #orgIndexContents <- readLines( orgIndexFileConn )
  #close(orgIndexFileConn)


  # create the projIndexLink:
  #NoteLink <- R.utils::getRelativePath(projFile, relativeTo=orgIndexPath)
  #NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  #projIndexLink <- paste("* [", projectTitle, "](", NoteLink, ")",  sep="")

  # create the Vector, including Whitespace and Summary information:
  #projIndexLinkVector <- c( "", "", "", projIndexLink, "" )

  # compute place to insert the project doc link:
  # First get the line index containing containing the programmeName
  #line <- grepLineIndex(programmeName, orgIndexContents)

  # Then get the NEXT line that starts with ##
  #line <- computeNextLine(line, orgIndexContents)

  # Insert projIndexLinkVector to orgIndexContents:
  #orgIndexContents <- c(orgIndexContents[1:(line-1)], projIndexLinkVector, orgIndexContents[(line+1):length(orgIndexContents)])


  # write to orgIndexPath
  #orgIndexFileConn <- file( orgIndexPath )
  #writeLines(orgIndexContents, orgIndexFileConn)
  #close(orgIndexFileConn)

  #cat( "  Written Project Doc to Org File: ", basename(orgIndexPath), "\n" )



  ### WRITE PROJECT DOC TO PROGRAMME INDEX FILE:

  # read Programme Index File:
  progIndexPath = paste(progPath, .Platform$file.sep, "index_", programmeName, ".Rmd", sep="")
  progIndexFileConn <- file( progIndexPath )
  progIndexContents <- readLines( progIndexFileConn )
  close(progIndexFileConn)

  # create the projIndexLink:
  NoteLink <- R.utils::getRelativePath(projFile, relativeTo=progIndexPath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  projIndexLink <- paste("* [", projectTitle, "](", NoteLink, ")",  sep="")

  # create the Vector, including Whitespace and Summary information:
  projIndexLinkVector <- c( "", "", "", "---", "", "", "", paste("## ",projectTitle, sep=""), "", "", projIndexLink, "" )
  #projIndexLinkVector <- c( "", "", "", projIndexLink, "" )

  # compute place to insert the project doc link:
  # First get the line index containing containing the header "------"
    # This is at the END of the # Projects section.
  line <- grepLineIndex("------", progIndexContents)

  # Then find the PREVIOUS line that has ANY content, and return the index +1:
  line <- computePreviousLineIndex(line, progIndexContents)

  # Insert projIndexLinkVector to progIndexContents:
  progIndexContents <- c(progIndexContents[1:(line-1)], projIndexLinkVector, progIndexContents[(line+1):length(progIndexContents)])


  # write to progIndexPath
  progIndexFileConn <- file( progIndexPath )
  writeLines(progIndexContents, progIndexFileConn)
  close(progIndexFileConn)

  cat( "  Written Project Doc to Programme File: ", basename(progIndexPath), "\n" )



}
