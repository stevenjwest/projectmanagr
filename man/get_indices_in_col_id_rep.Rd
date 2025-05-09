% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datatables-read-functions.R
\name{get_indices_in_col_id_rep}
\alias{get_indices_in_col_id_rep}
\title{Get row indices from ID & REP values}
\usage{
get_indices_in_col_id_rep(dt, id_names, rep_names, sep = ".-.")
}
\description{
Return all indices of all matches of CONCAT of (\code{id_names} & \code{rep_names}) in
CONCAT of (\verb{dt[["ID]]} & \code{dt[["rep"]]}), in the order they exist in
(\code{id_names} & \code{rep_names}).
}
\details{
\code{sep} is used to create a unique separator between ID and rep, to avoid any
errors where ID ends with numbers, which could create overlappiny ID-rep pairs.
\code{sep} is ".-." by default, an unlikely string to find in an ID.
}
