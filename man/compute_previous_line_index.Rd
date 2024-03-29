% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document-functions.R
\name{compute_previous_line_index}
\alias{compute_previous_line_index}
\title{Compute Previous Line Index}
\usage{
compute_previous_line_index(lineIndex, contents)
}
\arguments{
\item{lineIndex}{the index to start looking back from.}

\item{contents}{a vector of strings, the index of the first instance of line in this vector is returned.}
}
\description{
Returns the index of the Previous Line in contents that contains any content ("\link{A-z,0-9}"), looking back
from lineIndex to 1.
}
