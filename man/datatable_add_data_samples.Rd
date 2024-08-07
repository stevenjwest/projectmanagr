% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatables-functions.R
\name{datatable_add_data_samples}
\alias{datatable_add_data_samples}
\title{Add new Samples Data to Data Table}
\usage{
datatable_add_data_samples(
  contents,
  data_cols,
  datatable_name,
  ids_vector = "",
  default_data_vals = list(),
  dt_length = 100,
  summarise_reps = FALSE
)
}
\arguments{
\item{contents}{character vector containing current document with
existing data tables.}

\item{data_cols}{Character vector of data table column titles to add.  CANNOT
BE BLANK!}

\item{datatable_name}{String of data table name: This data table MUST ALREADY
EXIST/BE DECLARED BEFORE rmd_line in rmd_path.}

\item{ids_vector}{A vector containing the IDs to add: Can be "ALL", a set of GROUP
IDs (which must already be declared in datatable name under a 'group-' data
column), or (a subset of) IDs from datatable.}

\item{default_data_vals}{List of default data values to add to data cols.}

\item{dt_length}{Int of data table max length in characters - default 100.}

\item{summarise_reps}{Boolean to indicate whether reps should be summarised in
the datatable.  If FALSE each ID/rep is on a separate line in the new datatable,
otherwise if TRUE, all reps are summarised using r vector index syntax on
one line in the new datatable.  i.e. each ID is listed ONCE and the reps are
indicated as: 1:3,5,6:10,12,14:25 etc.  Default to FALSE.}
}
\description{
Adds new Sample data to an EXISTING Data Table in specified Rmd file at
specified line.  The Sample DataTable will contain an initial ID column
containing all the EXISTING IDs (IDs that have been resampled or exported
will automatically be EXCLUDED), and an optional set of extra data columns as
specified in the data_cols vector.  If the table exceeds dt_length characters
(default 100), then the table is split into multiple tables, with IDs as
first col, and subsequent data_cols given in subsequent tables.
}
\details{
The function assumes all EXISTING REPS of the current samples are going to
be resampled: if more than one rep exists for any of the samples, this table
will add the first rep column and fill it with all the EXISTING rep numbers.
}
