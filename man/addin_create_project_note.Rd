% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-addins.R
\name{addin_create_project_note}
\alias{addin_create_project_note}
\title{Add a New Project Note Addin}
\usage{
addin_create_project_note()
}
\description{
Generates a Shiny Gadget for adding a new Project Note inside
a Project Doc.
}
\details{
The User must select a Task or Project Header Note Link in a
Project Document.  If a Project Task is selected, the User
can build a new Project Note - either a Simple Note or a Group
Note (Header plus one SubNote).  If a Project Header Note Link
is selected, the User can ADD a new SubNote to this Group Note.

User selects a destination in
the file system (MUST be a Programme Dir), Project name, and
Project title (for the html page).

Stipulates any errors in the input, and can only be completed
when these errors have been resolved.
}
