% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document-functions.R
\name{update_project_docs}
\alias{update_project_docs}
\title{updating all project docs in one function for efficiency}
\usage{
update_project_docs(projectDocPaths, settings, orgPath)
}
\description{
As one project note can contain MULTIPLE GDTs to Project Notes, and want to
efficiently update the projDoc GDTs, need to keep track of which project
notes have been updated.
}
\details{
Every time a project note is opened, ALL OF ITS GDT SUMMARY SECTIONS are updated.
Once updated do not want to update again, so add its full path to a character
vector to check against before opening and updating any further project notes.
}
