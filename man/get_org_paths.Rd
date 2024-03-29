% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-functions.R
\name{get_org_paths}
\alias{get_org_paths}
\title{Get existing Org paths & update the existing org configs to be aware of newOrgPath}
\usage{
get_org_paths(newOrgPath)
}
\arguments{
\item{newOrgPath}{defines the new orgs directory.}
}
\description{
This function ensures all new Orgs are aware of existing ones, and existing Orgs are aware
of new ones.  Organisations can only be aware of eachother if they are in the same parent
directory.
}
