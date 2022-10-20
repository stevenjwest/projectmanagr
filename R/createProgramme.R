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
#' @param programmePrefix PREFIX used for all new PROJECTS in this PROGRAMME - should use ALL CAPS, and
#' NO NUMBERS.  Must not contain a space. Ideally two or three letters long.
#'
#' @param programmeTitle Title of programme - typically the programmeName with "_" & "-" replaced with spaces.
#'
#' @param organisationPath Path to insert the PROGRAMME into.  If this is not an Organisation Directory, will search up
#' the directory tree to attempt to find one.  If none found, the method will end without making a PROGRAMME.
#'
#' @param progTemplate The Rmd file to use as a template to create the Programme.  This is set to "Programme-Template.Rmd" in
#' projectmanagr.
#'
#' @export
createProgramme <- function(programmeName, programmePrefix, programmeTitle="", organisationPath=getwd(),
                            progTemplate="Programme-Template.Rmd" ) {

  cat( "\nprojectmanagr::createProgramme():\n" )

  # check programmeName contains NO SPACES:
  if( grepl("\\s+", programmeName) ) {
    stop( paste0("  programmeName contains a SPACE: ", programmeName) )
  }

  # check programmePrefix contains NO SPACES:
  if( grepl("\\s+", programmePrefix) ) {
    stop( paste0("  programmePrefix contains a SPACE: ", programmePrefix) )
  }

  # Search for root of an ORGANISATION:

  # look for the .config/ and templates/ dirs:
  confPath = paste(organisationPath, .Platform$file.sep, "config" , sep="")
  tempPath = paste(confPath, .Platform$file.sep, "templates" , sep="")

  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    organisationPath <- dirname(organisationPath)
    if(nchar(organisationPath) <=1) {
      stop( paste0("  Could not identify ORGANISATION in organisationPath: ",organisationPath) )
    }
    confPath = paste(organisationPath, .Platform$file.sep, "config" , sep="")
    tempPath = paste(confPath, .Platform$file.sep, "templates", sep="")
  }

  # now organisationPath should contain the path to the ORG ROOT DIR
  orgPath <- organisationPath


  ### CREATING A PROGRAMME: ###

  # create PROG Dir:
  progPath = paste(orgPath, .Platform$file.sep, programmeName, sep="")
  done <- dir.create(progPath)

  if(!done) {
    stop( paste0("  Programme directory could not be created: ", progPath) )
  }

  cat( "  Made Programme dir: ", progPath, "\n" )

  # create PROJECTS dir:
  projsPath = paste(progPath, .Platform$file.sep, "PROJECTS", sep="")
  done <- dir.create(projsPath)

  if(!done) {
    stop( paste0("  PROJECTS directory could not be created: ", projsPath) )
  }

  cat( "  Made PROJECTS dir: ",projsPath, "\n" )



  # create SOP dir:
  templatesPath = paste(progPath, .Platform$file.sep, "SOP", sep="")
  done <- dir.create(templatesPath)

  if(!done) {
    stop( paste0("  SOP directory could not be created: ", templatesPath) )
  }

  cat( "  Made SOP dir: ",templatesPath, "\n" )



  # create REF dir:
  templatesPath = paste(progPath, .Platform$file.sep, "REF", sep="")
  done <- dir.create(templatesPath)

  if(!done) {
    stop( paste0("  REF directory could not be created: ", templatesPath) )
  }

  cat( "  Made REF dir: ",templatesPath, "\n" )


  # create Rmd file:
  progFile = paste(progPath, .Platform$file.sep, "index_", programmeName, ".Rmd", sep="")
  done <- file.create(progFile)

  if(!done) {
    stop( paste0("  Programme index.Rmd file could not be created: ", progFile) )
  }

  cat( "  Made Programme index.Rmd file: ",progFile, "\n" )


  # read progTemplate:
  templateFileConn <- file( paste( tempPath, .Platform$file.sep, progTemplate, sep="") )
  templateContents <- readLines( templateFileConn )
  close(templateFileConn)

  # Check programmeTitle, and if blank, fill with programmeName, replacing all "_" and "-" with spaces
  if( nchar(programmeTitle) == 0 ) {
    programmeTitle = gsub("-", " ", gsub("_", " ", programmeName) )
  }

  # Read the status.yml file first into a LIST:
  statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  # extract the Author value from the settings.yml file:
  #settingsFile = paste( confPath, .Platform$file.sep, "settings.yml", sep="" )
  #settings <- yaml::yaml.load( yaml::read_yaml( settingsFile ) )
  #authorValue <- settings[["Author"]]
  authorValue <- Sys.info()["user"] # use username as author instead

  # modify templateContents to include programmeTitle and authorValue
  templateContents <- gsub("{{TITLE}}", programmeTitle, templateContents, fixed=TRUE)
  templateContents <- gsub("{{AUTHOR}}", authorValue, templateContents, fixed=TRUE)

  # modify templateContents to include relative link to org index Rmd
  orgIndexPath = paste(orgPath, .Platform$file.sep, "index_", status[["orgName"]], ".Rmd", sep="")
  orgTitle <- substring(basename(orgIndexPath), first=1, last=nchar(basename(orgIndexPath))-4)
  NoteLink <- R.utils::getRelativePath(orgIndexPath, relativeTo=progFile)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  orgLink <- paste("[", orgTitle, "](", NoteLink, ")",  sep="")
  templateContents <- gsub("{{ORGLINK}}", orgLink, templateContents, fixed=TRUE)

  # write to projFile
  fileConn <- file(progFile)
  writeLines(templateContents, fileConn)
  close(fileConn)

  cat( "  Written template to Programme index.Rmd file: ", progFile, "\n" )


  # Write to the status.yml file the Programme Prefix

  # add the programmePrefix under the programmeName in the "PROGRAMMES" section of the status.yml List:
  attrs <- list(programmePrefix, programmeTitle, as.character(file.info(progFile)[,5]) )
  names(attrs) <- c("programmePrefix", "programmeTitle", "creationTime")
  status[["PROGRAMMES"]][[programmeName]] <- attrs
  # can retrieve the programmePrefix with call to:  status[["PROGRAMMES"]][[programmeName]][["programmePrefix"]]

  # Write status list to the statusFile:
  yaml::write_yaml( yaml::as.yaml(status), statusFile )

  cat( "  Written PROGRAMME to Status.yml file: ", statusFile, "\n" )


  ### WRITE PROGRAMME TO ORGANISATION INDEX FILE:

  # read Organisation Index File: orgName in status
  orgIndexPath <- paste(orgPath, .Platform$file.sep, "index_", status[["orgName"]], ".Rmd", sep="")
  orgIndexFileConn <- file( orgIndexPath )
  orgIndexContents <- readLines( orgIndexFileConn )
  close(orgIndexFileConn)

  # create the progIndexLink:
  NoteLink <- R.utils::getRelativePath(progFile, relativeTo=orgIndexPath)
  NoteLink <- substring(NoteLink, first=4, last=nchar(NoteLink)) # remove first `../`
  progIndexLink <- paste("[", programmeName, "](", NoteLink, ")",  sep="")

  # read string from prog summary to paste into org index
  summaryStartIndex <- matchLineIndex(orgLink, templateContents) # finds FIRST MATCH
  #summaryStartIndex <- computeNextLineIndex((summaryStartIndex+1), templateContents )-1
  summaryEndIndex <- grepLineIndexFrom("----", templateContents, summaryStartIndex)-1
  summaryStartIndex <- summaryStartIndex+1 # to remove the org link

  if(summaryStartIndex == summaryEndIndex) {
    # start and end both found line '----' - there is no summary info to add!
    progSummary <- c(progIndexLink)
  }
  else {
    # fill summary with lines
    progSummary <- c(progIndexLink)

    for(i in (summaryStartIndex):(summaryEndIndex-1) ) {

      progSummary <- c(progSummary, templateContents[i])

    }
  }
  #progIndexLinkBullet <- c(progIndexLink, "", "", "", "## AIMS", "",
  #                         "", "* LIST_AIMS", "", "", "", "## PROGRESS", "",
  #                         "", "* ADD_PROGRESS", "" )

  # create the Vector, including Whitespace and Summary information:
  progIndexLinkVector <- c( "", "", "---", "", "", "", paste("# ",programmeName, sep=""), "", "", progSummary )

  # compute place to insert the project note link:
  # get the line selected in the projectDoc - [["originalLine"]]
  line <- computeLastLineIndex(orgIndexContents)

  # Insert projectNoteLinkVector to orgIndexContents:
  #orgIndexContents <- c(orgIndexContents[1:(line-1)], progIndexLinkVector, orgIndexContents[(line+1):length(orgIndexContents)])
  orgIndexContents <- c(orgIndexContents[1:(line-1)], progIndexLinkVector, orgIndexContents[(line+1):length(orgIndexContents)])


  # write to orgIndexPath
  orgIndexFileConn <- file( orgIndexPath )
  writeLines(orgIndexContents, orgIndexFileConn)
  close(orgIndexFileConn)

  cat( "  Written Programme Link to Org File: ", basename(orgIndexPath), "\n" )


}
