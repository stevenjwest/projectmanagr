% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatables-gen-functions.R
\name{define_ids_reps_explicit}
\alias{define_ids_reps_explicit}
\title{Define IDs and REPs explicitly - based on current datatable contents}
\usage{
define_ids_reps_explicit(
  ids_vector,
  reps_vector,
  datatable,
  summarise_reps,
  sep = ".-."
)
}
\value{
named list containing IDs, REPs, summarise_reps, rep_exists
}
\description{
This function fails to parse ALL or <GROUP_NAMES>, and only allows explicit
subsets of IDs & REPs to be defined in the \code{ids_vector} & \code{reps_vector}.
}
\details{
This function is only called when reps_vector has been set - so only need to
confirm validity of ids_vector and reps_vector as valid IDs and REPs.
}
