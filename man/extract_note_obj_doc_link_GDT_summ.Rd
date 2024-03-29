% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document-functions.R
\name{extract_note_obj_doc_link_GDT_summ}
\alias{extract_note_obj_doc_link_GDT_summ}
\title{Extract Project Note Doc Links GDT & Summary}
\usage{
extract_note_obj_doc_link_GDT_summ(
  linkNoteRmdContents,
  linkNoteRmdPath,
  settings,
  orgPath
)
}
\description{
Returns a list containing string vectors with projectDocFilePath, goal,
deliverable, task for all objectives, and summary & todo for single &
subnotes (not header notes).
}
\details{
If no objectives are identified, a BLANK LIST \code{list()} is returned.
}
