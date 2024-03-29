% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insert-functions.R
\name{insert_content}
\alias{insert_content}
\title{Insert Content from Source Content File into a Project Note}
\usage{
insert_content(selectionSource, selectionDestination)
}
\arguments{
\item{selectionSource}{Selection object from Project Note file containing
the Protocol to be inserted.  The selection must be on the FIRST PROTOCOL
DELIMITER that indicates the start of the Protocol declaration. Use
\code{projectmanagr::cursor_selection()} or \code{projectmanagr::user_selection()} to
create this object.}

\item{selectionDestination}{Selection object from the Project Note file where
the protocol is to be inserted.  Use \code{projectmanagr::cursor_selection()} or
\code{projectmanagr::user_selection()} to create this object.}
}
\description{
This Function adds Insertable Content in to a Project Note. Insertable Content
are text files that contain templated content for use in project notes -
created with \code{create_content()} function.
}
\details{
\itemize{
\item The declared parameter in the Content \code{{{INSERTABLE_CONTENT_LINK}}} is replaced
with a relative link from Destination to the Source Project Note.
\item All links in the Content are UPDATED to work from the destination Project
Note.
}

All graphics files included in \code{knitr::include_graphics()} r calls are
linked to the new Project Note appropriately.
}
