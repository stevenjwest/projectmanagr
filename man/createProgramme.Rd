% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createProgramme.R
\name{createProgramme}
\alias{createProgramme}
\title{Create a New Programme}
\usage{
createProgramme(
  programmeName,
  programmePrefix,
  programmeTitle = "",
  fileSystemPath = getwd(),
  progTemplate = "Programme-Template.Rmd"
)
}
\arguments{
\item{programmeName}{Name of Progamme - must NOT contain a space.}

\item{programmePrefix}{PREFIX used for all new PROJECTS in this PROGRAMME - should use ALL CAPS, and
NO NUMBERS.  Must not contain a space}

\item{programmeTitle}{Title of programme - typically the programmeName with "_" & "-" replaced with spaces.}

\item{fileSystemPath}{Path to insert the PROGRAMME into.  If this is not an Organisation Directory, will search up
the directory tree to attempt to find one.  If none found, the method will end without making a PROGRAMME.}

\item{progTemplate}{The Rmd file to use as a template to create the Programme.  This is set to "Programme-Template.Rmd" in
projectmanagr.}
}
\description{
Generates a new Programme at the top level of the Organisation.  If the
fileSystemPath is not at the top of the Organisation, will traverse until
it is found.  This function will halt if no Organisation is found - defined
by the presence of config/ and config/templates dirs ar root.
}
\details{
User must supply the programmeName and programmePrefix.  The
programmeName must NOT contain a space, an optional programmeTitle (which
if not supplied will default to the programmeName, replacing "_" & "-" with
" ").  The default fileSystemPath is the working directory.
}
