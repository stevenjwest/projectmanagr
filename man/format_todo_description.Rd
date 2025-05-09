% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-functions.R
\name{format_todo_description}
\alias{format_todo_description}
\title{Format a multi-line TODO description into Markdown bullets}
\usage{
format_todo_description(txt)
}
\arguments{
\item{txt}{A multiline string from the parsed TODO block.}
}
\value{
A character string with bullet indentation.
}
\description{
Splits the text by newline. The first line is prefixed with \verb{* },
subsequent lines each get \verb{   +}, with a blank line after each bullet.
}
\keyword{internal}
