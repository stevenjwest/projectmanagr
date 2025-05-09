% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatables-write-rmd-addins.R
\name{addin_datatable_import_export}
\alias{addin_datatable_import_export}
\title{Import/Export Existing Samples and Reps from Datatables in Source Project Notes}
\usage{
addin_datatable_import_export()
}
\value{
This function does not return a value but modifies the current and source project notes
by creating or updating import/export datatables.
}
\description{
Generates a Shiny Gadget for importing existing samples and reps from datatables
that exist in source Project Note(s) into the currently active destination
Project Note, and exports these samples from the source Project Note(s).
}
\details{
This function allows the user to:
\itemize{
\item Select the line in the current Project Note where the IMPORT will be written.
\item Choose a directory tree (dirTree) from the current ORGANISATION: All project notes
are read recursively for existing samples/reps in datatables.
\item View and filter a summary of existing samples/reps in a datatable.
\item Select samples by clicking on the rows in the datatable.
\item Select reps by filling in the rep number in the left-hand column of selected sample rows.
\item Finalize the operation by clicking "DONE", which:
\itemize{
\item EXPORTED the selected samples/reps from the source Project Note: The source datatable
is updated, pointing to the destination Project Note.
\item IMPORTED the selected samples/reps into the destination Project Note: An import
datatable is created with the selected samples/reps, pointing to the source Project Note.
}
}
}
\examples{
# Example usage:
# Run this function as a Shiny Gadget to interactively import/export samples/reps.
addin_datatable_import_export()

# During usage:
# - Select the desired rows in the datatable.
# - Choose the "DONE" option to export and import the samples/reps accordingly.

}
