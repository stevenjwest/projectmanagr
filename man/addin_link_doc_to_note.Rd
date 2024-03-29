% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/link-doc-addins.R
\name{addin_link_doc_to_note}
\alias{addin_link_doc_to_note}
\title{Link Project Document GDT to a Project Note}
\usage{
addin_link_doc_to_note()
}
\description{
Generates a Shiny Gadget for linking a Project Doc GDT to a project note.
The Link must be FROM a (Source) Project Docs' Goal/Del/Task TO a Project
Note (Single, SubNote, or Group Header).
}
\details{
The User must select a Task in the Source Project Document. A link will then
be made to a User selected Project Note, which must be open in RStduio.

The ADDIN stipulates any errors in the input, and can only be completed
when these errors have been resolved.
}
