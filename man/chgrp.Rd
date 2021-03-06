% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsUtils.R
\name{chgrp}
\alias{chgrp}
\title{Change a file's group on a Unix-like system}
\usage{
chgrp(group, file)
}
\arguments{
\item{group}{The new group name for the file or directory.}

\item{file}{The file or directory getting a new group. Must exist.}
}
\value{
The old group name of the file or directory, silently.
}
\description{
This wraps a call to the unix-based \code{chgrp} system command. It validates
input for the \code{group=} and \code{filename=} arguments, so it should be
safe to process user values for these arguments
}
\section{Possible Errors}{


\describe{
   \item{\command{"No such file: <file>"}}{
     The specified \code{file=} argument must exist.
   }
   \item{\command{"No such group: <group> in {<group1>, <group2>, ...}"}}{
      The specified \code{group=} argument must be one of the allowed groups
      for the user as returned by \code{\link{groups}}.
   }
   \item{\command{"Can't change group to '<group>'. Error was: ..."}}{
      If changing the group fails, the resulting error is reported. E.g. if
      you don't have permission to change the group of the file or directory.
   }
   \item{\command{
"System command failed with status <status>
 Command was: chgrp <group> <file>
 The system-level error message (if any) was: ..."}}{
      Some failures may not be caught or reported cleanly. This is a last-
      ditch effort to provide information on such failures.
   }
   \item{\command{
   	"chgrp() is only implemented for unix systems"
   }}{
      Will die with error if .Platform$OS.type != 'unix'.
   }
}
}
\examples{

\dontrun{
chgrp( aGroup, someFile )
}

}
\seealso{
\code{\link{groups}}
}

