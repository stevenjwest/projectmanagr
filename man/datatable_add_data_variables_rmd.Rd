% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatables-write-rmd-functions.R
\name{datatable_add_data_variables_rmd}
\alias{datatable_add_data_variables_rmd}
\title{Generate Variables ADD_DATA Data Table and insert into Rmd}
\usage{
datatable_add_data_variables_rmd(
  rmd_path,
  rmd_line,
  datatable_name,
  var_names,
  group_names,
  default_data_vals = list(),
  dt_length = 100
)
}
\arguments{
\item{rmd_path}{path to Rmd file.}

\item{rmd_line}{line in Rmd file to insert table.}

\item{datatable_name}{String of data table name - MUST exist in rmd_path!}

\item{var_names}{Names of variables to be added to first column.}

\item{group_names}{Vector of sample IDs or GROUP NAMES from a group set, which will
constitute the remaining column headers.  These will typically be group
names.}

\item{default_data_vals}{List of default data values to add to data cols &
IDs.}

\item{dt_length}{Int of data table max length in characters - default 100.}
}
\description{
Creates a new VARIABLES DataTable in specified Rmd file at specified line.
}
\details{
The Variables DataTable will contain an initial \code{variables} column containing
the \code{var_names}, and a set of extra data columns as specified in the
\code{group_names} vector. These must be the group names from an existing group
column from the existing datatable titled \code{datatable_name}.
If the table exceeds dt_length characters (default 100), then the table is
split into multiple tables, with IDs as first col, and subsequent \code{data_cols}
given in subsequent tables.
}
