% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samples-functions.R
\name{sample_create}
\alias{sample_create}
\title{Create Sample Tibble}
\usage{
sample_create(ID, overwrite = TRUE, writeToDisk = TRUE)
}
\arguments{
\item{ID}{A Character Vector that uniquely identifies each Sample.}

\item{overwrite}{Set to TRUE to overwrite the contents of a previously saved samples CSV.
Otherwise, this method will fail when trying to create new samples.  Can set to FALSE if
failing to overwrite is required.}

\item{writeToDisk}{Boolean to indicate whether the newly formed samples DataFrames is saved
to disk.  TRUE by default - set to FALSE to test run this function.}
}
\value{
A newly created and saved TIBBLE : saved to the Project Note DIR, and with the "path"
attribute set to '__samples.csv'.
}
\description{
Create a new Tibble to store samples data in.  User must pass
in a character vector that contains each Sample ID: A unique
string to identify each sample.
}
\details{
Assumes this function is run from inside RStudio R code chunk,
inside a Project Note from ProjectManagr:  The data is automatically
saved to the associated DATA_DIR of the projectmanagr Project Note -
defined as the Directory named with the PREFIX of the Project Note
(the string before the ~_ separator).

If the DATA_DIR does not exist, this function will fail - therefore this
function will only work with ProjectManagr Project Notes (or Rmd files
with a similar naming convention and layout).

The samples data is saved to CSV in the DATA_DIR using a standard filename,
"__samples.csv".  This method will fail if this file already exists in
the DATA_DIR
}
