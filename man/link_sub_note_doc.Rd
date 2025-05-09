% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-functions.R
\name{link_sub_note_doc}
\alias{link_sub_note_doc}
\title{Link Sub Note to Project Doc GDT}
\usage{
link_sub_note_doc(
  selection,
  settings,
  subNoteRmdPath,
  subNoteContents,
  headerNoteRmdPath,
  headerNoteRmdContents,
  projNoteLinkContents,
  projNoteLinkSummaryContents,
  projNoteSummaryContents,
  subNoteSummaryContents,
  linkNoteRmdPath,
  linkNoteRmdContents,
  orgPath
)
}
\arguments{
\item{selection}{A list of parameters for the selected Rmd file. Expected
elements include \code{rmdType}, \code{filePath}, and optionally
\code{headerNoteLink} and \code{addingSubNote}.}

\item{settings}{A list of configuration settings for template processing,
parameter substitution, and marker definitions.}

\item{subNoteRmdPath}{A string specifying the full file path of the sub note
Rmd file.}

\item{subNoteContents}{A character vector containing the sub note's content.}

\item{headerNoteRmdPath}{A string specifying the full file path of the header
note Rmd file.}

\item{headerNoteRmdContents}{A character vector containing the header note's
content.}

\item{projNoteLinkContents}{A character vector holding the project note link
template.}

\item{projNoteLinkSummaryContents}{A character vector holding the project note
link summary template.}

\item{projNoteSummaryContents}{A character vector with the project note summary
template.}

\item{subNoteSummaryContents}{A character vector with the sub note summary
template.}

\item{linkNoteRmdPath}{A string specifying the full file path of the link
note Rmd file.}

\item{linkNoteRmdContents}{A character vector containing the link note's
content.}

\item{orgPath}{A string specifying the root directory path of the
organisation.}
}
\description{
This function links a new sub note to its associated project document
based on Goal-Deliverable-Task (GDT) information. It extracts GDT data
from the link note contents and inserts corresponding links into the sub
note and project document.
}
\details{
This internal function integrates a new sub note into an existing
project note group. It performs the following tasks:
\enumerate{
\item Extracts GDT details from the link note using
\code{extract_note_obj_doc_link_GDT_summ()}.
\item Iterates over each GDT to compute hyperlink data via
\code{compute_doc_GDT_link()}.
\item Replaces placeholders in link templates with actual values using
\code{sub_note_link_params()} and \code{sub_template_param()}.
\item Determines insertion points in the sub note using header and footer
markers with \code{match_line_index()} and
\code{grep_line_index_from()}.
\item Inserts the generated links into the sub note and updates the
project document.
}
}
\note{
This function assumes that the link note and sub note belong to the
same project note group and that required markers and templates are
present. Missing markers or templates may lead to incorrect link insertion.
}
\examples{
\dontrun{
# Example: Linking a new sub note to a project document.
selection <- list(rmdType = "SUB", filePath = "/path/to/subnote.Rmd")
settings <- get_settings_yml("/path/to/org")
subNoteRmdPath <- "/path/to/project/subnotes/subNote1.Rmd"
subNoteContents <- read_file(subNoteRmdPath)
headerNoteRmdPath <- "/path/to/project/header_note.Rmd"
headerNoteRmdContents <- read_file(headerNoteRmdPath)
projNoteLinkContents <- read_file("Project-Note-Link-Template.Rmd")
projNoteLinkSummaryContents <- read_file(
  "Project-Note-Link-Summary-Template.Rmd")
projNoteSummaryContents <- read_file("Project-Note-Summary-Template.Rmd")
subNoteSummaryContents <- read_file(
  "Project-Sub-Note-Summary-Template.Rmd")
linkNoteRmdPath <- "/path/to/project/link_note.Rmd"
linkNoteRmdContents <- read_file(linkNoteRmdPath)
orgPath <- "/path/to/org"

# Link the sub note with GDT-based links.
link_sub_note_doc(selection, settings, subNoteRmdPath, subNoteContents,
                  headerNoteRmdPath, headerNoteRmdContents,
                  projNoteLinkContents, projNoteLinkSummaryContents,
                  projNoteSummaryContents,
                  subNoteSummaryContents, linkNoteRmdPath,
                  linkNoteRmdContents, orgPath)
}

}
\seealso{
extract_note_obj_doc_link_GDT_summ, compute_doc_GDT_link,
sub_note_link_params, sub_template_param, insert_at_indices,
load_param_vector, match_line_index, grep_line_index_from, write_file,
create_hyperlink
}
