% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document-functions.R
\name{get_content_title}
\alias{get_content_title}
\title{Get Content Title}
\usage{
get_content_title(contentDeclContents, settings, orgPath)
}
\arguments{
\item{contentDeclContents}{The Content Declaration Contents - character vector that
includes all lines between content separators.}

\item{settings}{ProjectManagr organisation settings.yml file}

\item{orgPath}{The path to the root of the organisation}
}
\value{
the Content Title value as string
}
\description{
Extract content title from the content Contents, using the ContentTitleField
parameter declared in settings.
}
