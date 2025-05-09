% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-addins.R
\name{addin_open_daily_journal}
\alias{addin_open_daily_journal}
\title{Addin to Open Daily Journal}
\usage{
addin_open_daily_journal()
}
\description{
A Shiny gadget that displays the current ORG directory path and a calendar
interface. The user can select a date, which determines the day for the
journal. If a journal for the selected day already exists, it will
be opened. Otherwise, a new journal is created using the
\code{create_daily_journal()} function.
}
\details{
The function uses the current working directory of the active document to
identify the ORG directory and settings. The user is presented with a
calendar interface to select a date. If the ORG directory is not found,
an error is displayed, and the function terminates.

Internally, the function:
\itemize{
\item Ensures the working directory is set to the active document's path.
\item Finds the ORG directory.
\item Retrieves settings from a YAML file in the ORG directory.
\item Displays a Shiny gadget with the appropriate UI and server components.
}
}
\note{
This function is intended to be used interactively as an RStudio Addin.
}
\examples{
# Open the daily journal gadget:
projectmanagr::addin_open_daily_journal()

}
\seealso{
\itemize{
\item \code{\link{create_daily_journal}} for creating journal files.
\item \code{\link{find_org_directory}} for locating the ORG directory.
\item \code{\link{get_settings_yml}} for retrieving settings from a YAML file.
\item \code{\link{addin_error_org}} for handling ORG-related errors.
}
}
