% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/volume-functions.R
\name{volume_add_dir}
\alias{volume_add_dir}
\title{Add directory to Volume}
\usage{
volume_add_dir(dirName, volName, use_full_path = TRUE)
}
\arguments{
\item{dirName}{Name of dir to form in Project Note DIR.}

\item{volName}{Name of volume to store data on.  The volume must EXIST and be MOUNTED.}

\item{use_full_path}{If true, will write full path from Org root through to Project Note
DIR (prog-dir/proj-doc-dir/proj-note-dir/ path).  If false, the project note DIR will be
directly written to volume.  Direct writing of project note minimises DIR tree, whereas
writing full path eliminates possibility of name clash when writing multiple Project Notes
from different Programmes/Projects to the same volume.}
}
\description{
This function adds dirName to the selected volume (volName), under the current
Project Notes' DIR.  First, this method will generate a DIR the same name as
the Project Notes' DIR on volume - either directly (use_full_path is FALSE) or
with the full path to the Project Note DIR in the Organisation (if use_full_path
is TRUE).
}
\details{
Next, the DIR dirName is created in the new Project Note DIR on volName.

Finally, a Symlink is formed from the new DIR, dirName, on the volume, to inside the
Project Note DIR on the local filesystem.

It is probably also wise to write to the Project Note, somewhere in its header,
the symlink path:  This could facilitate automated mounting of volumes if desired, and
at least serve as a record of where the data for this Project Note is.

The default is to write the current Project Notes' DIR name directly to the volume.
However, if use_full_path is set to true, the full path from the ORGANISATION Root
through to the Project Notes' DIR will be written to the volume.

This allows a Volume to be used for storing just one set of Project Notes data
directly into the volume location with a minimal directory tree, or to store a
mixture of different Project Notes data from different Project Note sets, Project
Docs, and Programmes, without any issues of name clash.
}
