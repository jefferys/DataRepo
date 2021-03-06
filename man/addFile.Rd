% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addFile.R
\name{addFile}
\alias{addFile}
\title{Add a file to a data repo.}
\usage{
addFile(file, repo = getwd(), entry = NULL, asFile = basename(file),
  writeGroup = NULL, fileSize = NULL, checksum = NULL,
  checksumFunc = "tools::md5sum")
}
\arguments{
\item{file}{The file to copy into the repo. May be relative or absolute, but
may not be a symlink or a directory. File copies will be created with permissions
"664" (read by all, write by group and owner, execute by none) and with
group \code{writeGroup} if that was specified.}

\item{repo}{The base directory for the repo. Must exist on the file system
and should have a restrictive set of ownerships and permissions that
includes restrictions on parent directories. By default will use the
current directory if not specified.}

\item{entry}{A subdirectory or subdirectory tree in the repo where the file
should be put. These will be created (recursively) if they don't exist with
the specified group owner and permissions 2775. This translates to sticky
directories with full permissions except other-write. Due to the way
\code{repoDir} is expected to be set up, this is NOT world readable.
Leading and trailing path separators are ignored.}

\item{asFile}{The new name for the file in the repository. Must be a plain
base name, not a directory (extensions are fine.) By default this is the
base name of \code{file}.}

\item{writeGroup}{The file system group that all created directories and the
file copy will be assigned, if set. The default group is used if this if
left \code{NULL} (the default).}

\item{fileSize}{The expected size of the source file, in bytes. By default
this is \code{NULL}, meaning file size will not be checked. If a positive
value is supplied it will be checked against the size of the source file on
the file system. If it does not match then failure is reported and the
checksum is not checked.}

\item{checksum}{The expected checksum of the source file. By default this is
\code{NULL}, meaning no checksum is generated. If given it will be checked
agains the value provided by the \code{checksumFunc}.}

\item{checksumFunc}{The function or function name (as a string) that will be
used when calculating checksums. The calculated source file checksum will
be verified against any provided checksums before copying and against a
recalculated checksum after copying. String function names may be qualified
with a \code{SomePackage::} prefix as the function named is retrieved using
\code{\link{getSomewhere}}. By default the function used is
'\code{'tools::md5sum'}. The specified function should have one parameter,
a file path. The returned checksum value should be an (atomic) vector type
but can not be \code{NULL} or a missing value. When \code{checksumFunc} is
called, \code{path} has already been verified and is known to exist on the
file system as a real file (not a directory or link).}
}
\value{
A named vector of validation results. Each element is named for a
  check performed, and will be the empty string if the check succeeded, a
  failure string if the check failed, and a missing value if the check was
  not performed (i.e. was not wanted or previous checks failed making further
  checking irrelevant.)
}
\description{
Copies the specified \code{file=} into the \code{repo=}, or to the current
directory if no repository is specified. It will be renamed if \code{asFile=}
is specified, and will be put into a subdirectory of the repo if
\code{entry=} is specified, possibly as a multi-directory path. This will be
created if needed. Assuming all optional elements are specified, the path of
the file created in the repository will be \code{<repo>\\< entry>\\<asFile>}.
To validate the file before copy, expected \code{fileSize=} and
\code{checksum=} can be given. If inspection of \code{file=} does not match,
an error-containing validation vector will be returned. Regardless of initial
validation, the copy will be validated using the actual file size and file
checksum after copy. Successful copy is reported as a vector of empty
strings. Problems are reported with one or more non-empty elements.
}
\examples{
\dontrun{
checks <- addFile( sourcefile, repoDir, entry= 'proj_3/set_A', asFile= 's1.dat'
   writeGroup= 'repoWrite', fileSize= 1234567890, checksum= sourceFileMd5 )

report <- checkSummary(checks)
if (report != '') { stop(report) }
}

}

