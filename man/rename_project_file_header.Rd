% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename-functions.R
\name{rename_project_file_header}
\alias{rename_project_file_header}
\title{Rename a Project File Section Header}
\usage{
rename_project_file_header(selection, headerName)
}
\arguments{
\item{selection}{Selection from projectmanagr Project file, made on the
Header (starts with \verb{#}) to be renamed.}

\item{headerName}{The new name to use for the selected Header}
}
\description{
Section Headers defined in Markdown plaintext files, beginning with \verb{#}. This
method ensures all links to a file header are appropriately updated across
the ProjectManagr Organisation.
}
