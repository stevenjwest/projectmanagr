% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filesystem-functions.R
\name{get_datetime}
\alias{get_datetime}
\title{internal function to get datetime in YYYY/MM/DD:hh:mm format}
\usage{
get_datetime(timezone = "UTC", split = "-", splitTime = ":")
}
\arguments{
\item{timezone}{Timezone code to use. Default "UTC".}

\item{split}{Chracter to split YYYY & MM & DD. Default "/".}

\item{splitTime}{Chracter to split DD & hh & mm. Default ":".}
}
\description{
internal function to get datetime in YYYY/MM/DD:hh:mm format
}
