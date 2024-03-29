% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document-functions.R
\name{get_file_list_to_project_notes}
\alias{get_file_list_to_project_notes}
\title{get file list down to project notes}
\usage{
get_file_list_to_project_notes(
  fileList,
  dl,
  settings,
  fileExtensions = list("Rmd")
)
}
\arguments{
\item{fileList}{List of files recursively retrieved by this function.}

\item{dl}{DirsList - a list of directory paths.}

\item{settings}{projectmanagr settings list.}

\item{fileExtensions}{File extensions of files to list.}
}
\description{
Traverses all directory tree in \code{dl} but only
get fileList recursively but only down to project note parent dir level
}
