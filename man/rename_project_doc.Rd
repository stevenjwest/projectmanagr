% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rename-functions.R
\name{rename_project_doc}
\alias{rename_project_doc}
\title{Rename a Project Document and Update References}
\usage{
rename_project_doc(
  projectDocPath,
  newProjectDocName,
  newProjectDocTitle = "",
  replaceLinksFileExtensions = list("Rmd")
)
}
\arguments{
\item{projectDocPath}{Character string. The full path to the project document
file to be renamed.}

\item{newProjectDocName}{Character string. The new name for the project document
file. Must not contain spaces.}

\item{newProjectDocTitle}{Character string. The new title for the project document.
If not provided, it will be derived from \code{newProjectDocName}. Defaults to
an empty string.}

\item{replaceLinksFileExtensions}{List. A list of file extensions to search for
references to the project document. Defaults to \code{list("Rmd")}.}
}
\value{
This function does not return a value. It modifies files in place by:
\itemize{
\item Renaming the project document file.
\item Updating the title in the renamed document.
\item Updating references in other files across the organization.
}
}
\description{
Renames a project document to a new name and optionally updates the title
within its content. Additionally, updates references to the renamed document
across the organization structure for specified file types.
}
\details{
This function performs the following tasks:
\enumerate{
\item Validates the provided inputs, ensuring no spaces in \code{newProjectDocName}.
\item Renames the specified project document to \code{newProjectDocName}.
\item Updates the document's title in its content to \code{newProjectDocTitle}, if
provided.
\item Updates references to the renamed document in files across the organization,
limited to the extensions specified in \code{replaceLinksFileExtensions}.
}

The function assumes the project document resides within an organized directory
structure. It automatically identifies the root directory of the organization
and other configuration settings.
}
\note{
\itemize{
\item The \code{newProjectDocName} parameter must not contain spaces.
\item This function modifies files directly on disk. Ensure backups are created before
running.
\item Only files with extensions specified in \code{replaceLinksFileExtensions} are
updated for references.
}
}
\examples{
# Rename a project document and update references in Rmd files
rename_project_doc(
  projectDocPath = "/path/to/project_doc.Rmd",
  newProjectDocName = "new_doc_name",
  newProjectDocTitle = "New Document Title",
  replaceLinksFileExtensions = list("Rmd")
)

}
\seealso{
\itemize{
\item \code{\link{update_links_filenames}} for updating references to renamed files.
\item \code{\link{find_org_directory}} for locating the root directory of the organization.
\item \code{\link{get_settings_yml}} for loading configuration settings.
}
}
