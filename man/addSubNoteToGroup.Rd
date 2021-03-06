% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addSubNoteToGroup.R
\name{addSubNoteToGroup}
\alias{addSubNoteToGroup}
\title{Add a New Sub Note to a Project Group}
\usage{
addSubNoteToGroup(
  subNoteName,
  subNotePrefix,
  subNoteDir,
  selection,
  subNoteTitle = "",
  subNoteTemp = "Project-Sub-Note-Template.Rmd"
)
}
\arguments{
\item{subNoteName}{The name of the Project Sub Note, a Title with all SPACES replaced
with - or _.}

\item{subNotePrefix}{The whole subNotePrefix, including identifier and Major
Numbering, separated by ~, and finally the Minor Numbering system (or subNote numbering),
separated by -.  User needs to define this.}

\item{subNoteDir}{The directory where the Sub Note will be stored.  This will be the Project Group
Note Directory.  Must be an ABSOLUTE path!}

\item{selection}{List containing the Goal, Del, Task selected from the Project Doc, as well as other useful
information - lines of Task/Del/Goal, projectDoc path content of selection line.  Mainly for validating the
input (its not actually used in this function!  But the selection is used in RStudio Addins to determine whether
to run addSubNoteToGroup, addProjectNote, or addProjectNoteGroup - may omit?).  See `cursorSelection()` or
`userSelection()`.  Selection MUST be on a HeaderNote!}

\item{subNoteTitle}{OPTIONAL title for the Project Note.  Default is to use subNoteName and replace
all _ and - with SPACES.}

\item{subNoteTemp}{OPTIONAL template to use, as found in the `config/templates/` directory of the Organisation.
Default is `Project-Sub-Note-Template.Rmd`}
}
\description{
This Function adds a Sub Project Note to a Project Group: The subNote is placed into the Project
Group Directory, links to the subNote are added to the project doc(s) and the project group header
note, and the subnote is written to any Programme Index files necessary, under the project doc(s).
}
