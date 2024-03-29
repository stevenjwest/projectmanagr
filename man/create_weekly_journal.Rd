% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-functions.R
\name{create_weekly_journal}
\alias{create_weekly_journal}
\title{Create Weekly Journal}
\usage{
create_weekly_journal(
  date = lubridate::today(),
  organisationPath = getwd(),
  journalFileNameTemplate = "{{YYYY}}-{{MM}}-{{DD}}_{{ORGNAME}}",
  journalTemplate = "Weekly-Work-Journal-Template.Rmd",
  openJournal = TRUE
)
}
\arguments{
\item{date}{The start date of the Weekly Journal - typically a Monday.  This
should be in 'YYYY-MM-DD' format, and can be a String. Can create a Date
object with code \code{as.Date(paste(year, month, "01", sep = "-"))}.}

\item{organisationPath}{The path to the Organisation where the weekly
journal is created & saved.}

\item{journalFileNameTemplate}{A string that defines the journal File Name.
YYYY & MM & DD are replaced with the year, month, day in the date arg.}

\item{journalTemplate}{File in template/s dir that indicates the layout
for the weekly journal.}
}
\description{
Saved in directory indicated in settings under "WeeklyJournalDir".
}
\details{
Can optionally extract TODOs to this file with the \code{extract_todos()} function.
}
