% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document-functions.R
\name{get_content_declaration}
\alias{get_content_declaration}
\title{Get Content Declaration from Project Note}
\usage{
get_content_declaration(sourceNoteRmdPath, selectionSource, settings, orgPath)
}
\arguments{
\item{sourceNoteRmdPath}{Path to source project note Rmd}

\item{selectionSource}{The selection made within source project note, should
be made on a content declaration.}

\item{settings}{ProjectManagr organisation settings.yml file}

\item{orgPath}{The path to the root of the organisation}
}
\value{
List of content declaration containing named parameters: "contentTitle",
"contentDescription", "contentSource", "contentStartLine", "contentEndLine",
"projectNotePath", "projectNoteContentHeader"
}
\description{
Declarations of content identified as existing between \code{settings[["ContentSep"]]}
delimiters.  This function gets all declared content from \code{sourceNoteRmdPath},
and then filters these to identify the one contentDeclaration that is within
the selection made in \code{selectionSource}.
}
