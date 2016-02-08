#==============================================================================
# groups()
#==============================================================================
#' Get a user's groups
#'
#' @param user The name of the user to check groups for. By default this is
#'   \code{NULL} and the groups will be provided for the user calling the
#'   function. Providing this parameter can be unsafe if you don't trust the
#'   source of the user name as much as you trust the source of your R code. It
#'   will be ignored (with a warning) unless \code{unsafe= TRUE} is set.
#'
#' @param unsafe Set \code{TRUE} to allow use of the \code{user=} parameter.
#'   Doing so without knowing that the user name is valid \emph{is} unsafe.
#'
#' @return The vector of group names to which the user belongs.
#'
#' @section Possible Errors:
#'
#' \describe{
#'    \item{\command{
#' "System command failed with status <status>
#'  Command was: groups <user>
#'  The system-level error message (if any) was: ..."}}{
#'       Failure of the system command is caught and reported. E.g. if there
#'       \emph{is} no system level \code{groups} command.
#'    }
#'    \item{\command{
#'    	"groups() is only implemented for unix systems"
#'    }}{
#'       Will die with error if .Platform$OS.type != 'unix'.
#'    }
#'}
#'
#' @section Possible Warnings:
#'
#' \describe{
#'    \item{\command{"User '<user>' ignored; potentially unsafe."}}{
#'       As described above, you must explicitly allow use of the user parameter
#'       by agreeing to run this is an \code{unsafe= TRUE} way.
#'    }
#' }
#'
#' @examples
#' groups()
#' groups( user='root' )  # user ignored, caller's user name used.
#' groups( user='root', unsafe=TRUE )
#'
#' # Example of unsafe use.
#' groups( user=';ls', unsafe=TRUE )
#' # Imagine if instead of ';ls' an evil user force-deleted, recursive on /
#' # and R itself ran as root (as it might be poorly configured to do to
#' # access some secure files...)
#'
#' @export
#==============================================================================
groups <- function( user=NULL, unsafe=FALSE ) {
   if (.Platform$OS.type != 'unix' ) {
      stop( 'groups() is only implemented for unix systems' )
   }
   if ( ! unsafe && ! is.null(user) ) {
      warning( 'User "', user, '" ignored; potentially unsafe.')
      user= ""
   }

   # Errors if "groups" is not a valid command
   # Only warns if command returns not zero status
   groups <- system2("groups", c(user), stdout= TRUE)

   # Die if non-zero status (or errorMsg, which should be a subset of all
   # non-zero status results)
   status <- 0
   if (! is.null(attr(groups, 'status'))) {
      status <- attr(groups, 'status')
   }
   errmsg <- ""
   if (! is.null(attr(groups, 'errmsg'))) {
      errmsg <- attr(groups, 'errmsg')
   }

   if (! status == 0 || ! errmsg == '') {
      stop( "System command failed with status ", status, "\n",
            "Command was: groups ", user, "\n",
            "The system-level error message (if any) was: ", errmsg, "\n"
      )
   }

   return(strsplit(groups, ' ', fixed=TRUE)[[1]])
}

#==============================================================================
# chgrp()
#==============================================================================
#' Change a file's group on a Unix-like system
#'
#' This wraps a call to the unix-based \code{chgrp} system command. It validates
#' input for the \code{group=} and \code{filename=} arguments, so it should be
#' safe to process user values for these arguments
#'
#' @param group The new group name for the file or directory.
#'
#' @param file The file or directory getting a new group. Must exist.
#'
#' @return The old group name of the file or directory, silently.
#'
#' @section Possible Errors:
#'
#' \describe{
#'    \item{\command{"No such file: <file>"}}{
#'      The specified \code{file=} argument must exist.
#'    }
#'    \item{\command{"No such group: <group> in {<group1>, <group2>, ...}"}}{
#'       The specified \code{group=} argument must be one of the allowed groups
#'       for the user as returned by \code{\link{groups}}.
#'    }
#'    \item{\command{"Can't change group to '<group>'. Error was: ..."}}{
#'       If changing the group fails, the resulting error is reported. E.g. if
#'       you don't have permission to change the group of the file or directory.
#'    }
#'    \item{\command{
#' "System command failed with status <status>
#'  Command was: chgrp <group> <file>
#'  The system-level error message (if any) was: ..."}}{
#'       Some failures may not be caught or reported cleanly. This is a last-
#'       ditch effort to provide information on such failures.
#'    }
#'    \item{\command{
#'    	"chgrp() is only implemented for unix systems"
#'    }}{
#'       Will die with error if .Platform$OS.type != 'unix'.
#'    }
#'}
#'
#' @seealso \code{\link{groups}}
#'
#' @examples
#'
#' \dontrun{
#' chgrp( aGroup, someFile )
#' }
#'
#' @export
#==============================================================================
chgrp <- function( group, file ) {

   if (.Platform$OS.type != 'unix' ) {
      stop( 'chgrp() is only implemented for unix systems' )
   }

   if (! file.exists( file )) {
      stop("No such file: ", file)
   }
   myGroups <- groups()
   if (! group %in% myGroups) {
      stop( "No such group: ", group, " in {", paste(myGroups, collapse=', ' ), '}')
   }

   oldGroup <- file.info(file)[1, 'grname']

   # Errors if "groups" is not a valid command
   # Only warns if command returns not zero status
   stdErrMsg <- system2("chgrp", c(group, file), stderr= TRUE, stdout= TRUE)

   if (! is.null(stdErrMsg) && length(stdErrMsg) > 0) {
      stop( "Can't change group to '",
            group,
            "'. Error was: ",
            stdErrMsg)
   }

   # Die if non-zero status (or errorMsg, which should be a subset of all
   # non-zero status results)
   status <- 0
   if (! is.null(attr(stdErrMsg, 'status'))) {
      status <- attr(stdErrMsg, 'status')
   }
   errmsg <- ""
   if (! is.null(attr(stdErrMsg, 'errmsg'))) {
      errmsg <- attr(stdErrMsg, 'errmsg')
   }

   if (! status == 0 || ! errmsg == '') {
      stop( "System command failed with status ", status, "\n",
            "Command was: chgrp ", group, " ", file, "\n",
            "The system-level error message (if any) was: ", errmsg, "\n"
      )
   }

   invisible(oldGroup)
}
