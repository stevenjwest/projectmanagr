#' Check Programme Dir
#'
#' Checks fileSystemPath is pointing to a PROGRAMME directory.
#' A Programme Directory is a direct sub-dir to a Organisation
#' directory, so this method establishes this is the case by checking
#' the parent Dir to fileSystemPath, to see if 'config/' and
#' 'config/templates/' exist.
#'
#' Secondly, it checks the 'PROJECTS/' directory exists in this
#' putative Programme Directory.
#'
#' If a Programme path is confirmed, it is returned, otherwise
#' the function returns a BLANK string "".
#'
#'
checkProgDir <- function( fileSystemPath ) {

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname(fileSystemPath)

  # look for the config/ and templates/ dirs:
  confPath = paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath = paste(confPath, .Platform$file.sep, "templates" , sep="")

  if(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    fileSystemPath <- ""
  }

  # fileSystemPath is therefore in a PROGRAMME DIR

  # also check if the PROJECTS/ dir is in the current DIR, and if not, exit:
  projsPath <- paste(fileSystemPath, .Platform$file.sep, "PROJECTS" , sep="")

  if(  !( file.exists(projsPath) )  ) {
    fileSystemPath <- ""
  }

  fileSystemPath

}


#' Check Programme File
#'
#' Checks fileSystemPath is inside a PROGRAMME directory.
#' A Programme Directory is a direct sub-dir to a Organisation
#' directory, so this method establishes this is the case by checking
#' the parent Dir to fileSystemPath, to see if 'config/' and
#' 'config/templates/' exist.
#'
#' Secondly, it checks the 'PROJECTS/' directory exists in this
#' putative Programme Directory.
#'
#' If a Programme path is confirmed, it returns the fileSystemPath, otherwise
#' the function returns a BLANK string "".
#'
#'
checkProgFile <- function( fileSystemPath ) {

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname(fileSystemPath)

  # look for the config/ and templates/ dirs:
  confPath = paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath = paste(confPath, .Platform$file.sep, "templates" , sep="")

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
#' the parent Dir to fileSystemPath, to see if 'config/' and
#' 'config/templates/' exist.
#'
#' Secondly, it checks the 'PROJECTS/' directory exists in this
#' putative Programme Directory.
#'
#' Finally, it confirms the Project Note is of type NOTE.
#'
#' If a Programme path is confirmed, it returns the fileSystemPath, otherwise
#' the function returns a BLANK string "".
#'
#'
checkProjNote <- function( fileSystemPath ) {

  returnPath <- ""

  if( findProgDir(fileSystemPath) != "" ) {
    # the fileSystemPath is in a Programme Directory

    if( basename(dirname(fileSystemPath)) != "PROJECTS" ) {
      # the filesystemPath DIR is NOT PROJECTS - therefore NOT a Project Doc

      if(getFileType(fileSystemPath) == "NOTE" ) {
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
#' finding a 'config/' directory and a 'config/templates/' directory.
#'
#' For the fileSystemPath to be successfully returned, the directory
#' MUST be at least in level below a PROGRAMME directory.
#'
#' If an Organisation path is identified, it is returned, otherwise
#' the function returns a BLANK string "".
#'
#'
checkProgSubDir <- function( fileSystemPath ) {

  # Check fileSystemPath is in a Programme DIR, a sub-dir to the root of an ORGANISATION:
  orgPath <- dirname(dirname(fileSystemPath))

  # look for the config/ and templates/ dirs:
  confPath = paste(orgPath, .Platform$file.sep, "config" , sep="")
  tempPath = paste(confPath, .Platform$file.sep, "templates" , sep="")

  orgPath2 <- "/" # use this as placeholder of PREVIOUS orgPath - if orgPath == orgPath2, then have not found config or template!

  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    orgPath2 <- orgPath # save in placeholder
    orgPath <- dirname(orgPath)
    if( orgPath2 == orgPath ) {
      fileSystemPath <- ""
      break
    }
    confPath <- paste(orgPath, .Platform$file.sep, "config" , sep="")
    tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")
  }

  if(fileSystemPath == "") {
    stop(paste0("  Dir is NOT inside a PROGRAMME: ", fileSystemPath))
  }

  fileSystemPath

}


#' Find Programme Dir
#'
#' Searches fileSystemPath's parent directories to identify
#' a Programme directory.  This is identified by
#' finding a 'PROJECTS/' directory and a 'SOP' directory.
#'
#' If a Programme path is identified, it is returned, otherwise
#' the function returns a BLANK string "".
#'
#'
findProgDir <- function( fileSystemPath ) {

  # Check fileSystemPath is in a PROGRAMME:

  # look for the PROJECTS/ and SOP/ dirs:
  projPath <- paste(fileSystemPath, .Platform$file.sep, "PROJECTS" , sep="")
  tempPath <- paste(fileSystemPath, .Platform$file.sep, "SOP" , sep="")

  fileSystemPath2 <- "/" # use this as placeholder of PREVIOUS fileSystemPath
                         # if fileSystemPath == fileSystemPath2, then have not found projects or template!

  while(  !( file.exists(projPath) && file.exists(tempPath) )  ) {
    fileSystemPath2 <- fileSystemPath # save in placeholder
    fileSystemPath <- dirname(fileSystemPath)
    if( fileSystemPath2 == fileSystemPath ) { # break if reached filesystem root
      fileSystemPath <- ""
      break
    }
    projPath <- paste(fileSystemPath, .Platform$file.sep, "PROJECTS" , sep="")
    tempPath <- paste(fileSystemPath, .Platform$file.sep, "SOP", sep="")
  }

  fileSystemPath

}


#' Find Organisation Dir
#'
#' Searches fileSystemPath's parent directories to identify
#' a Project Organisation directory.  This is identified by
#' finding a 'config/' directory and a 'config/templates/' directory.
#'
#' If an Organisation path is identified, it is returned, otherwise
#' the function returns a BLANK string "".
#'
#' @param fileSystemPath an absolute path in the filesystem - should be within an Organisation.
#'
findOrgDir <- function( fileSystemPath ) {

  # Check fileSystemPath is at the root of an ORGANISATION:

  # look for the config/ and templates/ dirs:
  confPath <- paste(fileSystemPath, .Platform$file.sep, "config" , sep="")
  tempPath <- paste(confPath, .Platform$file.sep, "templates" , sep="")

  fileSystemPath2 <- "/" # use this as placeholder of PREVIOUS orgPath - if orgPath == orgPath2, then have not found config or template!

  while(  !( file.exists(confPath) && file.exists(tempPath) )  ) {
    fileSystemPath2 <- fileSystemPath # save in placeholder
    fileSystemPath <- dirname(fileSystemPath)
    if( fileSystemPath2 == fileSystemPath ) {
      fileSystemPath <- ""
      break
    }
    confPath <- paste(fileSystemPath, .Platform$file.sep, "config" , sep="")
    tempPath <- paste(confPath, .Platform$file.sep, "templates", sep="")
  }

  fileSystemPath

}


#' Get File Type
#'
#' Determine if the current file is a PROJECT DOC (it is inside the PROJECTS/ Directory),
#' a HEADER GROUP NOTE (contains the string "-00~_"), or a SIMPLE or SUB Project Note (any
#' other file).
#'
#' Returns the relevant String:  DOC, HEAD, NOTE
#'
#'@param fileSystemPath the Absolute path to the file.
#'
#'
getFileType <- function( fileSystemPath ) {

  TYPE <- ""

  #cat( "\ngetFileType - fileSystemPath: ", fileSystemPath, "\n" )

  #cat( "\ngetFileType - basename(dirname(fileSystemPath)): ", basename(dirname(fileSystemPath)), "\n" )

  if(basename(dirname(fileSystemPath)) == "PROJECTS") {
    TYPE <- "DOC"
  }
  else if( grepl("-00~", fileSystemPath, fixed = TRUE) ) {
    TYPE <- "HEAD"
  }
  else {
    TYPE <- "NOTE"
  }

  TYPE

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
getHeaderNotePath <- function( fileSystemPath, projectHeaderNoteName ) {

  # search inside fileSystemPath to identify any DIRECTORIES that start with prefix, and if so, compute the NEXT INDEX:
  # NB take care of both SIMPLE and GROUP directories (Prefix~NUMBER for simple, Prefix~NUMBER-00 for GROUP)
  prefix <- paste( getNextSimplePrefix(fileSystemPath), "-00", sep="")

  paste(fileSystemPath, .Platform$file.sep, prefix, "~_", projectHeaderNoteName, ".Rmd", sep="")

}


#' Get Next Group Prefix
#'
#' This method extracts the Prefix String from headerNotePath, then checks the
#' DIRs in headerNotePath that begin with prefixString to compute the next Group
#' Prefix - +1 from the highest prefix value.
#'
#' headerNotePath - must be the path to the headerNote - DIR or RMD file.
#'
#'
getNextGroupPrefix <- function( headerNotePath ) {

  # check if headerNotePath is a DIR or RMD file:
  if(regexpr("~_", headerNotePath, fixed=TRUE) == -1) {

    headerNoteDir <- headerNotePath

    # get prefix - extract after last file.sep:
    headerNotePrefix <- substring(headerNotePath, regexpr("\\/[^\\/]*$", headerNotePath)+1 )

    } else {

      # extract headerNoteDir from path - from the ~_ separator, replace fileName with file.sep
      headerNoteDir <- paste( substring(headerNotePath, 1, regexpr("~_", headerNotePath, fixed=TRUE)-1 ), sep="")

      # get prefix - extract between last file.sep and "~_" string:
      headerNotePrefix <- substring(headerNotePath, regexpr("\\/[^\\/]*$", headerNotePath)+1, regexpr("~_", headerNotePath, fixed=TRUE)-1 )

    }

  # get the ROOT of the Prefix:
  majorPrefix <- substring(headerNotePrefix, first=1, last=nchar(headerNotePrefix)-2)

  # define the Regular Exp for this prefix:
  majPrefixRegExp <- paste("^", majorPrefix, ".*", sep="")

  # look in headerNoteDir for any files - if so, compute Prefix from these, otherwise generate new Prefix for file:
  #subNoteDirs <- list.dirs( headerNoteDir, full.names=FALSE, recursive=FALSE )
  subNoteFiles <- list.files(headerNoteDir)
  subNoteFiles <- subNoteFiles[endsWith(subNoteFiles, ".Rmd")]
  # now CREATE subNoteDirs from subNoteFiles - substring at index of "~_"
  subNoteDirs <- substr(subNoteFiles, 1, regexpr("~_", subNoteFiles)-1 )


  # filter subNoteDirs to only contain content that starts with the majorPrefix:
  subNoteDirs <- subNoteDirs[grepl(majPrefixRegExp, subNoteDirs)]

  if(length(subNoteDirs) > 0 ) {

    # compute NEXT subNote Prefix from the DIRs in subNoteDirs:
    subNoteVals <- as.integer(substring(subNoteDirs, first=regexpr("-", subNoteDirs, fixed=TRUE)+1  ) )

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
    # else, its the first subnote! create subNotePrefix by adding a "1" to end of headerNotePrefix:
      subNotePrefix <- paste(headerNotePrefix, "1", sep="")

    }

  subNotePrefix

}


#' Get Next Simple Prefix
#'
#' This method extracts the Prefix String from fileSystemPath (isolated by extracting
#' all letters and numbers up to the first "_").  It then checks the
#' Rmd Files in fileSystemPath that begin with prefixString to compute the next SIMPLE
#' Prefix - +1 from the highest prefix value.
#'
#' NOTE must use Rmd files and not the Project
#' Note DIRs, as the DIRs can be symlinks, and if not active are not returned as existing!
#'
#' fileSystemPath - must be a path to a VALID DIR for Project Notes - a SubDir in a PROGRAMME DIR.
#'
#'
getNextSimplePrefix <- function( fileSystemPath ) {

  # get the selected directory's BASENAME - to extract the prefix:
  pir <- basename(fileSystemPath)

  # Extract PREFIX - up to first "_"
  if(regexpr("_", pir, fixed=TRUE) > 0) {
    prefix <- substring(pir, first=1, last=regexpr("_", pir, fixed=TRUE)-1 )
  }
  else {
    prefix <- pir
  }

  # define the Regular Exp for this prefix:
  prefixRegExp <- paste("^", prefix, ".*", sep="")

  # look in headerNoteDir for any files - if so, compute Prefix from these, otherwise generate new Prefix for file:
  #subDirs <- list.dirs( fileSystemPath, full.names=FALSE, recursive=FALSE )
  subFiles <- list.files(fileSystemPath)
  subFiles <- subFiles[endsWith(subFiles, ".Rmd")]
  # now CREATE subDirs from subFiles - substring at index of "~_"
  subDirs <- substr(subFiles, 1, regexpr("~_", subFiles)-1 )

  # filter subNoteDirs to only contain content that starts with the majorPrefix:
  subDirs <- subDirs[grepl(prefixRegExp, subDirs)]

  if(length(subDirs) > 0 ) {

    # extract all MAJOR NUMBERING of subDirs (numbers BETWEEN ~ and - for GROUP HEADERS, and AFTER ~ for SINGLE NOTES)

    subVals <- integer( length(subDirs) )

    for(l in 1:length(subDirs) ) {

      if( regexpr("-", subDirs[l], fixed=TRUE) > 0 ) {
        # this is a GROUP HEADER DIR - extract the integer between ~ and -

        subVals[l] <- as.integer( substring(
                                    subDirs[l],
                              first=regexpr("~", subDirs[l], fixed=TRUE)+1,
                              last=regexpr("-", subDirs[l], fixed=TRUE)-1
                          )
                      )
      } else {
        # this is a SINGLE DIR - extract the integer AFTER ~

        subVals[l] <- as.integer( substring(
                                    subDirs[l],
                              first=regexpr("~", subDirs[l], fixed=TRUE)+1
                          )
                      )
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

    simplePrefix <- paste(prefix, "~", nextIndexChar, sep="")

  } else {
    # else this is the first note in this DIR - set index to 001
    simplePrefix <- paste(prefix, "~001", sep="")
  }

  # return simplePrefix:
  simplePrefix

}


#' Get Project Doc Dir Path
#'
#' Retrieves the projectDoc DIR from the projectDoc path:
#'
#' - Removes the projectDocTitle and EXT from the path, and returns
#' the ProjectDoc Dir - the path plus the projectDoc PREFIX.
#'
getProjectDocDirPath <- function( projectDocPath ) {

  substring(projectDocPath, first=1, last=regexpr("~_", projectDocPath, fixed=TRUE)-1 )

}


#' Get Project Note Dir Path
#'
#' Retrieves the project Note DIR from the project Note path:
#'
getProjectNoteDirPath <- function( projectDocPath ) {

  substring(projectDocPath, first=1, last=regexpr("~_", projectDocPath, fixed=TRUE)-1 )

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
getProjectNotePath <- function( fileSystemPath, projectNoteName ) {

  # search inside fileSystemPath to identify any DIRECTORIES that start with prefix, and if so, compute the NEXT INDEX:
  # NB take care of both SIMPLE and GROUP directories (Prefix~NUMBER for simple, Prefix~NUMBER-00 for GROUP)
  prefix <- getNextSimplePrefix(fileSystemPath)

  paste(fileSystemPath, .Platform$file.sep, prefix, "~_", projectNoteName, ".Rmd", sep="")

}

#' Return the Project Note Rmd Path from a Project DIR Path
#'
#'
getProjectNotePathFromDir <- function( projectNoteDir ) {

  project_note_paths <- c()

  for( i in 1:length(projectNoteDir) ) {

    fileList <- list.files( dirname(projectNoteDir[i]) )

    proj_note_path <- fileList[startsWith(fileList, basename(projectNoteDir[i])) & endsWith(fileList, ".Rmd")]

    project_note_paths <- c(project_note_paths, paste0(dirname(projectNoteDir[i]), .Platform$file.sep, proj_note_path) )

  }

  # return projectNotePaths:
  project_note_paths

}


#' Get Project Path
#'
#' Assumes fileSystemPath is a PROGRAMME DIRECTORY, containing a 'PROJECTS/' Directory.
#'
getProjectPath <- function( fileSystemPath, projectName ) {

  # identify the Organisation Dir:
  orgPath <- findOrgDir(fileSystemPath)

  # specify the Projects Dir:
  projsPath <- paste(fileSystemPath, .Platform$file.sep, "PROJECTS" , sep="")

  # extract the PROGRAMME NAME from the fileSystemPath:
  programmeName <-basename(fileSystemPath)


  # extract the programme prefix from its config file:
  statusFile = paste( orgPath, .Platform$file.sep, "config", .Platform$file.sep, "status.yml", sep="" )
  status <- yaml::yaml.load( yaml::read_yaml( statusFile ) )
  programmePrefix <- status[["PROGRAMMES"]][[programmeName]][["programmePrefix"]]


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

  paste(projsPath, .Platform$file.sep, programmePrefix, projectIndex, "~_", projectName, ".Rmd", sep="")

}


#' Get Sub Note Path form Project Doc
#'
#' Assumes projectDocPath is the path to the Project Doc.
#'
#' The headerNoteRelPath is assumed to be the RELATIVE path from projectDocPath
#' to the headerNote.
#'
#'
getSubNotePathFromDoc <- function( projectDocPath, headerNoteRelPath, subNoteName ) {

  # compute the headerNotePath from projectDocPath and headerNoteRelPath (rel to ProjectDocPath!)
  headerNotePath <- computePath(projectDocPath, headerNoteRelPath)

  # extract headerNoteDir from path - from the ~_ separator, replace fileName with file.sep
  headerNoteDir <- paste( substring(headerNotePath, 1, regexpr("~_", headerNotePath, fixed=TRUE)-1 ), sep="")

  # get the NEXT subNote Prefix:
  subNotePrefix <- getNextGroupPrefix(headerNoteDir)

  # SubNote will be in the headerNoteDir:
  subNotePath <- paste(headerNoteDir, .Platform$file.sep, subNotePrefix, "~_", subNoteName, ".Rmd", sep="" )

  subNotePath


}


#' Get Sub Note Path
#'
#' Assumes projectDocPath is the path to the Project Doc.
#'
#' The headerNoteRelPath is assumed to be the RELATIVE path from projectDocPath
#' to the headerNote.
#'
#'
getSubNotePathFromHead <- function( headerNotePath, subNoteName ) {

  # compute the headerNotePath from projectDocPath and headerNoteRelPath (rel to ProjectDocPath!)
  #headerNotePath <- computePath(projectDocPath, headerNoteRelPath)

  # extract headerNoteDir from path - from the ~_ separator, replace fileName with file.sep
  headerNoteDir <- paste( substring(headerNotePath, 1, regexpr("~_", headerNotePath, fixed=TRUE)-1 ), sep="")

  # get the NEXT subNote Prefix:
  subNotePrefix <- getNextGroupPrefix(headerNoteDir)

  # SubNote will be in the headerNoteDir:
  subNotePath <- paste(headerNoteDir, .Platform$file.sep, subNotePrefix, "~_", subNoteName, ".Rmd", sep="" )

  subNotePath


}


#' List all SubNotes that are below the Header Note.
#'
#' This Function returns a CHARACTER VECTOR of absolute paths to each subnote under the header note at headerNotePath.
#'
#' @param headerNotePath The path to the Header Note.  This MUST be in a sub-directory or lower inside a
#'   PROGRAMME Directory.  This MUST be an ABSOLUTE PATH.
#'
listSubNotePaths <- function( headerNotePath ) {

  #  Determine headerNote PREFIX
  projectNotePrefix <- substring( basename(headerNotePath), first=1, last=regexpr("~_", basename(headerNotePath), fixed=TRUE)-1 )

  # Check headerNotePrefix IS a HEADER NOTE (ending with "-00"):
  if( regexpr("-", projectNotePrefix) == -1 || substring(projectNotePrefix, regexpr("-", projectNotePrefix)+1) != "00" ) {
    # Single OR SUB NOTE:  This method is not designed to deal with these Notes - STOP:
    stop( paste0("  headerNotePath is to a Single Note or Sub Note of a Group Project Note: ", headerNotePath, " Use addLinkProjectNote() Function.") )
  }

  # get headerNoteDir
  headerNoteDir <- paste(dirname(headerNotePath), .Platform$file.sep, projectNotePrefix, sep="" )

  # extract each file from dir - file defined by name containing string "~_"
  file_list <- list.files(headerNoteDir)
  file_list <- file_list[ regexpr("~_", file_list) > 0  ]
  file_list <- paste(headerNoteDir, .Platform$file.sep, file_list, sep="" )

  file_list

}


#' Compute Path
#'
#' Compute Path from the fullPath combined with the relPath.
#'
#' Relative Path is used to adjust the fullPath, and returns a fullPath
#' that is directed to the relativePath location.
#'
#' Examples
#'
#' 01:
#'
#' fullPath <- "/Users/user/00_ORG/01-PROGRAMME/PROJECTS/P01/P01~001~_Project_Doc.Rmd"
#'
#' relPath <- "../../LAB/LAB01-00~_Exp_01.Rmd"
#'
#' Output: /Users/user/00_ORG/01-PROGRAMME/LAB/LAB01-00~_Exp_01.Rmd
#'
#' 02:
#'
#' fullPath <- "/Users/user/00_ORG/01-PROGRAMME/PROJECTS/P01/P01~001~_Project_Doc.Rmd"
#'
#' relPath <- "../../LAB"
#'
#' Output: /Users/user/00_ORG/01-PROGRAMME/LAB
#'
#' 03:
#'
#' fullPath <- "/Users/user/00_ORG/01-PROGRAMME/PROJECTS/P01/P01~001~_Project_Doc.Rmd"
#'
#' relPath <- "../../LAB/"
#'
#' Output: /Users/user/00_ORG/01-PROGRAMME/LAB/
#'
#'
computePath <- function( fullPath, relPath ) {

  # first TRIM relPath to path with no pir jumps ("../"), and count number of "../" in the string
  parentDir <- 0

  while(TRUE) {
    if( substring(relPath, 1, 3) == "../"  ) {
      relPath <- substring(relPath, 4)
      parentDir <- parentDir + 1 # count the number of 'jumps'
    }
    else {
      break
    }
  }


  # next, trim fullPath to dir, then each dir by number of jumps (parentDir)
  fullPath <- substring(fullPath, 1, regexpr("\\/[^\\/]*$", fullPath)-1 ) # trim to DIR (if this is a DIR it is NOT moved up to parent!)
  if(parentDir > 0) {
    for(l in 1:parentDir) {
      fullPath <- dirname(fullPath)
    }
  }

  newPath <- paste(fullPath, .Platform$file.sep, relPath, sep="")

  newPath

}


#' Compute Sub Note Path
#'
#' Assumes projectDocPath is the path to the Project Doc.
#'
#' The headerNoteRelPath is assumed to be the RELATIVE path from projectDocPath
#' to the headerNote.
#'
#'
computeSubNotePath <- function( headerNotePath, subNoteName ) {

  # extract headerNoteDir from path - from the ~_ separator, replace fileName with file.sep
  headerNoteDir <- paste( substring(headerNotePath, 1, regexpr("~_", headerNotePath, fixed=TRUE)-1 ), sep="")

  # get the NEXT subNote Prefix:
  subNotePrefix <- getNextGroupPrefix(headerNoteDir)

  # SubNote will be in the headerNoteDir:
  subNotePath <- paste(headerNoteDir, .Platform$file.sep, subNotePrefix, "~_", subNoteName, ".Rmd", sep="" )

  subNotePath


}


#' Volumes Add Programme
#'
#' Add Programme DIR named programmeName to each data dir in volumes/ in the given orgPath
#'
volumesAddProgramme <- function(orgPath, programmeName) {

  volPath <- paste(orgPath, .Platform$file.sep, "volumes", .Platform$file.sep, sep="")

  dirs <- list.dirs(path=paste(orgPath, .Platform$file.sep, "volumes", sep=""), full.names=TRUE, recursive=FALSE)

    lapply(dirs, function(x) {

        dir.create(paste(dirs, .Platform$file.sep, programmeName, sep="") )

      })

}




