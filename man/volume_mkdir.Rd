% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/volume-functions.R
\name{volume_mkdir}
\alias{volume_mkdir}
\title{Make Volume Directory}
\usage{
volume_mkdir(dirName, projectNoteDir, volName)
}
\arguments{
\item{dirName}{Name for the DIR - sub-DIR in projectNoteDir on volume, and SYMLINK in
local projectNoteDir.  NO SPACES.}

\item{projectNoteDir}{The ABSOLUTE PATH to the projectNoteDir where the SYMLINK will
reside.  NO SPACES.}

\item{volName}{The NAME of the volume in volumes/ - Must EXIST and be VISIBLE
(i.e mounted). NO SPACES.}
}
\description{
This first creates the new sub-DIR on the selected volume - including the full path
from the ORGANISATION ROOT down through PROGRAMME > PROJECT DOC > DIR PROJECT NOTE DIR >
to the sub-DIR.
}
\details{
Then it forms a SYMLINK from this new sub-DIR on selected volume to a symlink in
projectNoteDir.
}
