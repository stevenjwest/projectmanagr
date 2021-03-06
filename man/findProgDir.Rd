% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filesystem-functions.R
\name{findProgDir}
\alias{findProgDir}
\title{Find Programme Dir}
\usage{
findProgDir(fileSystemPath)
}
\description{
Searches fileSystemPath's parent directories to identify
a Programme directory.  This is identified by
finding a 'PROJECTS/' directory and a 'SOP' directory.
}
\details{
If a Programme path is identified, it is returned, otherwise
the function returns a BLANK string "".
}
