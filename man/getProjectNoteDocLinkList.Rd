% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/document-functions.R
\name{getProjectNoteDocLinkList}
\alias{getProjectNoteDocLinkList}
\title{Get Project Note Doc Link List}
\usage{
getProjectNoteDocLinkList(projectNoteContents, projectNotePath)
}
\description{
Returns a list of VECTORS:
}
\details{
list[[i]] : A VECTOR that includes the Project Doc ABSOLUTE Link, GOAL Num, DEL Num, TASK Num.

 list[[i]][1] : Project Doc ABSOLUTE Link

 list[[i]][2] : Project Doc GOAL - number plus title

 list[[i]][3] : Project Doc DELIVERABLE - number plus title

 list[[i]][4] : Project Doc TASK - number plus title

 list[[i]][[5]] : LIST containing all summary information in Project Note for this
                  Project Doc GOAL/DEL/TASK

 length( list ) returns the number of Project Doc links in the returned list.

 @param projectNoteContents Character vector containing the contents of a Project Note, which includes links to
 at least one ProjectDoc, separated by "----", and ended with "------".

 @param projectNotePath the FULL PATH to the project note, from which the contents is derived.
}
