% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-addins.R
\name{addin_extract_todos}
\alias{addin_extract_todos}
\title{Extract TODOs Shiny Gadget (Interactive) - Write to File}
\usage{
addin_extract_todos()
}
\description{
Opens a Shiny gadget where the user can set parameters for \code{\link{extract_todos}},
such as:
\itemize{
\item location (via directory chooser),
\item date (via date input),
\item onlyFirstTodoPerFile (checkbox),
\item sortByFileModTimeDesc (checkbox),
\item priorityFirst (checkbox).
}
The gadget then calls \code{extract_todos()}, inserts the resulting lines
directly into the currently open source file (on disk) at the cursor line,
and re-opens that file in RStudio.
}
