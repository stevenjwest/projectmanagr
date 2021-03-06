% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samples-functions.R
\name{sample_find_2}
\alias{sample_find_2}
\title{Find Samples Recursively inside a Directory}
\usage{
sample_find_2(subProgDir)
}
\arguments{
\item{subProgDir}{a directory that is inside a Programme, which will be searched recursively
to find all files that contain the string glob `__sample*`}
}
\description{
This will identify all samples CSVs inside a Direcotry recursively, presenting
a summary Tibble containing each EXISTING sample - data including:
}
\details{
* ID - sample ID
* SAMPLE: Composite of all subsampling
* COUNT: Number of reps of the sample (sections)
* EXP: The experiment PREFIX ID
* PATH: The absolute path to the Expeirment DIR
}
