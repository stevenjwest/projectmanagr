#' Create a New Programme
#'
#' Generates a new Programme at the top level of the Organisation.  If the
#' fileSystemPath is not at the top of the Organisation, will traverse until
#' it is found.  User must supply the programmeName and programmePrefix.  The
#' programmeName must NOT contain a space, an optional programmeTitle (which
#' if not supplied will default to the programmeName, replacing "_" & "-" with
#' " ").  The default fileSystemPath is the working directory.
#'
#'
#' @export
createProgramme <- function(programmeName, programmePrefix, programmeTitle="", fileSystemPath=getwd() ) {

  # check programmeName contains NO SPACES:
  if( grepl("\\s+", programmeName) ) {
    stop( cat("programmeName contains a SPACE: ",programmeName, "\n") )
  }

  # Check fileSystemPath is at the root of an ORGANISATION:

  # look for the .config/ and templates/ dirs:
  confPath = paste(fileSystemPath, .Platform$file.sep, "config" , sep="")
  tempPath = paste(confPath, .Platform$file.sep, "templates" , sep="")

  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    fileSystemPath <- dirname(fileSystemPath)
    if(nchar(fileSystemPath) <=1) {
      stop( cat("Could not identify ORGANISATION in fileSystemPath: ",fileSystemPath, "\n") )
    }
    confPath = paste(fileSystemPath, .Platform$file.sep, "config" , sep="")
    tempPath = paste(confPath, .Platform$file.sep, "templates", sep="")
  }

  # now fileSystemPath should contain the path to the ORG ROOT DIR


  ### CREATING A PROGRAMME: ###

  # create Dir:
  progPath = paste(fileSystemPath, .Platform$file.sep, programmeName, sep="")
  done <- dir.create(progPath)

  if(!done) {
    stop( cat("Programme directory could not be created: ", progPath, "\n") )
  }

  cat( "Made Programme dir: ",progPath, "\n" )


  # create PROJECTS dir:
  projsPath = paste(progPath, .Platform$file.sep, "PROJECTS", sep="")
  done <- dir.create(projsPath)

  if(!done) {
    stop( cat("PROJECTS directory could not be created: ", projsPath, "\n") )
  }

  cat( "  Made PROJECTS dir: ",projsPath, "\n" )


  # create SOP dir:
  sopPath = paste(progPath, .Platform$file.sep, "SOP", sep="")
  done <- dir.create(sopPath)

  if(!done) {
    stop( cat("SOP directory could not be created: ", sopPath, "\n") )
  }

  cat( "  Made SOP dir: ",sopPath, "\n" )


  # create Rmd file:
  progFile = paste(progPath, .Platform$file.sep, "index.Rmd", sep="")
  done <- file.create(progFile)

  if(!done) {
    stop( cat("Programme index.Rmd file could not be created: ", progFile, "\n") )
  }

  # TODO need to actually FILL the index.Rmd file!

  # Check programmeTitle, and if blank, fill with programmeName, replacing all "_" and "-" with spaces
  if( nchar(programmeTitle) == 0 ) {
    programmeTitle = gsub("-", " ", gsub("_", " ", programmeName) )
  }


  # Write to the status.yml file:  IS THIS NEEDED?  The Programme is NOT UPDATEABLE!  But may need to RETRIEVE information on it!

  # Read the status.yml file first into a LIST:
  statusFile = paste( confPath, .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )

  # add the programmePrefix under the programmeName in the "PROGRAMMES" section of the status.yml List:
  attrs <- list(programmePrefix, as.character(file.info(progFile)[,5]) )
  names(attrs) <- c("programmePrefix", "creationTime")
  status[["PROGRAMMES"]][[programmeName]] <- attrs
  # can retrieve the programmePrefix with call to:  status[["PROGRAMMES"]][[programmeName]][["programmePrefix"]]

  # Write status list to the statusFile:
  yaml::write_yaml( yaml::as.yaml(status), statusFile )

  cat( "  Written PROGRAMME to Status.yml file: ", statusFile, "\n" )

}
