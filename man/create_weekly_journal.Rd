% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-functions.R
\name{create_weekly_journal}
\alias{create_weekly_journal}
\title{Create Weekly Journal}
\usage{
create_weekly_journal(
  date = lubridate::today(),
  organisationPath = getwd(),
  authorValue = Sys.info()["user"],
  journalFileNameTemplate = "{{YYYY}}-{{MM}}-{{DD}}_{{ORGNAME}}",
  journalTemplate = "Weekly-Work-Journal-Template.Rmd"
)
}
\arguments{
\item{date}{A string or Date object representing the start date for the
Weekly Journal. It is converted to the Monday of the current week. The date
should be in 'YYYY-MM-DD' format.}

\item{organisationPath}{A string representing the path to the Organisation
where the journal is created and saved.}

\item{authorValue}{A string representing the author name for the Programme
index file.}

\item{journalFileNameTemplate}{A string that defines the journal file name.
Placeholders for {{YYYY}}, {{MM}}, {{DD}}, and {{ORGNAME}} are replaced with
the corresponding values.}

\item{journalTemplate}{A template file name from the templates directory that
defines the layout for the weekly journal.}
}
\value{
A string representing the path to the weekly journal Rmd file.
}
\description{
This function creates a weekly journal file for an organisation if it does
not already exist. If the journal file exists, it returns the path to the
existing file. The journal is stored in the directory specified in the
settings under "WeeklyJournalDir". Optionally, TODOs can be extracted to
this file using the extract_todos() function.
}
\details{
The function first converts the input date to the Monday of the current week
and extracts the year, month, and day. It then determines the organisation
directory using the given organisationPath. Next, it creates the necessary
directories (the journal root and a year subdirectory) and generates a journal
Rmd file using the provided journal template. If the journal file already exists,
the function does not overwrite it and returns its path instead.
}
\note{
The function converts the date to a Date object if it is not already. It
relies on proper configuration in the YAML settings and assumes that the
organisation directory structure is correctly set up.
}
\examples{
\dontrun{
  create_weekly_journal(
    date = "2023-04-10",
    organisationPath = "/path/to/organisation",
    authorValue = "John Doe",
    journalFileNameTemplate = "{{YYYY}}-{{MM}}-{{DD}}_{{ORGNAME}}",
    journalTemplate = "Weekly-Work-Journal-Template.Rmd"
  )
}

}
\seealso{
\code{\link{find_org_directory}}, \code{\link{get_settings_yml}},
\code{\link{create_directory}}, \code{\link{read_file}},
\code{\link{write_file}}, \code{\link{extract_todos}}
}
