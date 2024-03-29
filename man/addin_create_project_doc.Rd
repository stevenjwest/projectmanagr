% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-addins.R
\name{addin_create_project_doc}
\alias{addin_create_project_doc}
\title{Create a New Project Document Addin}
\usage{
addin_create_project_doc()
}
\description{
Generates a Shiny Gadget for creating a new Project Doc inside
a Programme.
}
\details{
User selects a Programme, then destination in the file system (MUST be within
a Programme Dir), Project name, and Project title (for the html page).

Stipulates any errors in the input, and can only be completed
when these errors have been resolved.
}
