% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document-functions.R
\name{create_hyperlink_section}
\alias{create_hyperlink_section}
\title{Create Hyperlink Section}
\usage{
create_hyperlink_section(toFileName, toFileSection, toFilePath, fromFilePath)
}
\arguments{
\item{toFileName}{Character. A short name for the file you're linking to.}

\item{toFileSection}{Character. The Markdown header (section) in \code{toFilePath}.}

\item{toFilePath}{Character. Path to the target Rmd file.}

\item{fromFilePath}{Character. Path to the Rmd file from which the link is created.}
}
\value{
A character string containing the relative link suitable for R Markdown.
}
\description{
Creates a string for hyperlink in Rmd, using \code{toFileName : toFileSection}
as the hyperlink text, and generating a RELATIVE LINK to
\code{toFilePath#toFileSection} from \code{fromFilePath}.
}
\details{
Internally, the heading portion of the link (the anchor after \verb{#}) is generated
in a way similar to how pandoc creates HTML anchors for headings: all characters
are lowercased, punctuation replaced with a dash, etc.
}
\examples{
\dontrun{
  create_hyperlink_section(
    "myDoc.Rmd",
    "Section Title!",
    "path/to/myDoc.Rmd",
    "path/to/currentDoc.Rmd"
  )
}
}
