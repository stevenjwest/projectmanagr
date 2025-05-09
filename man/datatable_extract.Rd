% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatables-read-functions.R
\name{datatable_extract}
\alias{datatable_extract}
\title{Extract datatable from character vector to LIST}
\usage{
datatable_extract(dt_vector)
}
\arguments{
\item{dt_vector}{Character vector containing a projectmanagr datatable.}
}
\description{
This extracts the data as presented in Rmd datatable format into a LIST,
where each element of the list is a Character Vector.  Each Character Vector
has FIRST ELEMENT as the column Header, then all subsequent elements are the
data values from the datatable.
}
\details{
Any multi-observation data values are put into the SAME ELEMENT, using a
space to separate them.

This list is then ready to be put into a new tibble: Each vector will be a
column in the tibble.
}
