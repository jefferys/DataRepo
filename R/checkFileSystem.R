#' Check functions for file system objects
#'
#' A check function test something about its first (required) parameter and
#' returns either an empty string if the check succeeds or a non-empty string
#' if the check fails, even when a test fails due to an error. The check
#' functions described here test file-system related properties.
#'
#' Since file systems change asynchronously with respect to running R code,
#' success or failure of a test does not guarantee that even the next line of
#' code will work if that code depends on the previously checked file system
#' state. This is called a race condition. However, checking up front will in
#' practice be highly correlated with the state of the file system for some
#' reasonable time frame, and it is simpler to handle errors caught early when
#' there is a lot less code in progress that needs to be unwound. As a
#' pre-flight check, these are very useful. Just don't count on the results once
#' the plane is in the air.
#'
#' @return All functions return either an empty string if the check succeeds or
#'   a non-empty string if the check fails (including if checking causes an
#'   error.)
#'
#' @name checkFileSystem
NULL

#' @describeIn checkFileSystem Checks if the path exists on the file system.
#'   This is a simple wrapper for \code{\link{file.exists}}. Will follow links
#'   with the result based on the final target. Returns one of:
#'
#'   \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{path} exists on the file system.}
#'    \item{\emph{"No such path."}\verb{ }
#'       Check failed as \code{path} was not found on the file system.}
#'    \item{\emph{"Checking for a path failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @param path A file system path to check.
#'
#' @examples
#' # Create file system objects for examples.
#' emptyFile  <- tempfile()
#' emptyDir   <- tempfile()
#' noSuchFile <- tempfile()
#' file.create( emptyFile )
#' dir.create(  emptyDir  )
#'
#' linkToEmptyFile <- tempfile()
#' linkToEmptyDir  <- tempfile()
#' linkToNowhere   <- tempfile()
#' linkToLinkToEmptyFile <- tempfile()
#' linkToLinkToNowhere   <- tempfile()
#' okLink <- file.symlink( emptyFile,       linkToEmptyFile       ) &&
#'           file.symlink( emptyDir,        linkToEmptyDir        ) &&
#'           file.symlink( noSuchFile,      linkToNowhere         ) &&
#'           file.symlink( linkToEmptyFile, linkToLinkToEmptyFile ) &&
#'           file.symlink( linkToNowhere,   linkToLinkToNowhere   )
#'
#' checkIsPath( emptyFile )
#' #=> [1] ""
#' checkIsPath( emptyDir )
#' #=> [1] ""
#' checkIsPath( noSuchFile )
#' #=> [1] "No such path."
#' checkIsPath( NA )    # file.exists(NA) is an error
#' #=> [1] "Checking for a path failed with the following error:
#' #=> invalid 'file' argument"
#'
#' if (okLink) {
#'    checkIsPath( linkToEmptyFile )
#'    #=> [1] ""
#'    checkIsPath( linkToEmptyDir )
#'    #=> [1] ""
#'    checkIsPath( linkToNowhere )
#'    #=> [1] "No such path."
#'
#'    checkIsPath( linkToLinkToEmptyFile )
#'    #=> [1] ""
#'    checkIsPath( linkToLinkToNowhere )
#'    #=> [1] "No such path."
#' }
#'
#' @export
checkIsPath <- function( path ) {
   tryCatch({
         if (file.exists(path)) {
            check <- ""
         }
         else {
            check <- "No such path."
         }
      },
      error= function (e) {
         check <<- paste0(
            "Checking for a path failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return(check)
}

#' @describeIn checkFileSystem Checks that the path does not exist on the file
#'   system. This is a simple wrapper for \code{\link{file.exists}}. Will
#'   follow links with the result based on the final target.
#'
#'   \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as no such \code{path} exists on the file system.}
#'    \item{\emph{"No such path."}\verb{ }
#'       Check failed as \code{path} was found on the file system.}
#'    \item{\emph{"Checking for a path failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @export
#'
#' @examples
#' checkIsNotPath( emptyFile )
#' #=> [1] "Path exists."
#' checkIsNotPath( emptyDir )
#' #=> [1] "Path exists."
#' checkIsNotPath( noSuchFile )
#' #=> [1] ""
#' checkIsNotPath( NA )    # file.exists(NA) is an error
#' #=> [1] "Checking for a path failed with the following error:
#' #=> invalid 'file' argument"
#'
#' if (okLink) {
#'    checkIsNotPath( linkToEmptyFile )
#'    #=> [1] "Path exists."
#'    checkIsNotPath( linkToEmptyDir )
#'    #=> [1] "Path exists."
#'    checkIsNotPath( linkToNowhere )
#'    #=> [1] ""
#'
#'    checkIsNotPath( linkToLinkToEmptyFile )
#'    #=> [1] "Path exists."
#'    checkIsNotPath( linkToLinkToNowhere )
#'    #=> [1] ""
#' }
#'
checkIsNotPath <- function( path ) {
   tryCatch({
         if (! file.exists(path)) {
            check <- ""
         }
         else {
            check <- "Path exists."
         }
      },
      error= function (e) {
         check <<- paste0(
            "Checking for a path failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return(check)
}

#' @describeIn checkFileSystem Checks that the path exists and that it is a file
#'   or a link to a file. If the path fails to exist, is a directory, or is a
#'   link to a directory, this fails. Will follow links with the result based on
#'   the final target.
#'
#'   \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{path} exists on the file system and is a file
#'       or a link to a file (not a directory).}
#'    \item{\emph{"No such path."}\verb{ }
#'       Check failed as \code{path} was not found on the file system.}
#'    \item{\emph{"Not a file."}\verb{ }
#'       Check failed as \code{path} was found on the file system but was
#'       neither a file nor a link to file. It probably was a directory.}
#'    \item{\emph{"Checking for a file failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @param path.info The data frame returned by the call \code{file.info(path)}.
#'   By default this is \code{NULL} and will be looked up during execution.
#'   However, this file system operation can be slow; when doing multiple checks
#'   on the same file, might want to do this once and pass the result to each
#'   check that needs it. As the point of providing this is speed, little
#'   validation is done, so if this is not an info data frame for the correct
#'   path, weird and unexpected behavior may result.
#'
#' @examples
#' checkIsFile( emptyFile )
#' #=> [1] ""
#' checkIsFile( emptyDir )
#' #=> [1] "Not a file."
#' checkIsFile( noSuchFile )
#' #=> [1] "No such path."
#' checkIsFile( NA )    # file.exists(NA) is an error
#' #=> [1] "Checking for a file failed with the following error:
#' #=> invalid 'file' argument"
#'
#' checkIsFile( emptyFile, path.info= file.info( emptyFile ))
#' #=> [1] ""
#' checkIsFile( emptyFile, path.info= file.info( emptyDir ))  # Oops.
#' #=> [1] "Not a file." # Not true!
#'
#' if (okLink) {
#'    checkIsFile( linkToEmptyFile )
#'    #=> [1] ""
#'    checkIsFile( linkToEmptyDir )
#'    #=> [1] "Not a file."
#'    checkIsFile( linkToNowhere )
#'    #=> [1] "No such path."
#'
#'    checkIsFile( linkToLinkToEmptyFile )
#'    #=> [1] ""
#'    checkIsFile( linkToLinkToNowhere )
#'    #=> [1] "No such path."
#' }
#'
#' @export
checkIsFile <- function( path, path.info= NULL ) {
   tryCatch({
         if ( ! file.exists(path)) {
            return( "No such path." )
         }

         if (is.null(path.info)) {
            path.info <- file.info( path, extra_cols = FALSE )
         }

         if( path.info$isdir ) {
            check <- 'Not a file.'
         }
         else {
            check <- ''
         }
      },
      error= function (e) {
         check <<- paste0(
            "Checking for a file failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return( check )
}

#' @describeIn checkFileSystem Checks that the path exists and if so ensures it is
#'   neither a file nor a link to a file. If the path fails to exist, is a file,
#'   or is a link to a file, this fails. Will follow links with the result based
#'   on the final target.
#'
#'   \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{path} exists on the file system but was
#'       neither a file nor a link to a file (it was probably a directory).}
#'    \item{\emph{"No such path."}\verb{ }
#'       Check failed as \code{path} was not found on the file system.}
#'    \item{\emph{"Is a file."}\verb{ }
#'       Check failed as \code{path} was found on the file system and was either
#'       a file or a link to file.}
#'    \item{\emph{"Checking for a file failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @examples
#' checkIsNotFile( emptyFile )
#' #=> [1] "Is a file."
#' checkIsNotFile( emptyDir )
#' #=> [1] ""
#' checkIsNotFile( noSuchFile )
#' #=> [1] "No such path."
#' checkIsNotFile( NA )    # file.exists(NA) is an error
#' #=> [1] "Checking for a file failed with the following error:
#' #=> invalid 'file' argument"
#'
#' checkIsNotFile( emptyFile, path.info= file.info( emptyFile ))
#' #=> [1] "Is a file."
#' checkIsNotFile( emptyFile, path.info= file.info( emptyDir ))  # Oops.
#' #=> [1] "" # Not true!
#'
#' if (okLink) {
#'    checkIsNotFile( linkToEmptyFile )
#'    #=> [1] "Is a file."
#'    checkIsNotFile( linkToEmptyDir )
#'    #=> [1] ""
#'    checkIsNotFile( linkToNowhere )
#'    #=> [1] "No such path."
#'
#'    checkIsNotFile( linkToLinkToEmptyFile )
#'    #=> [1] "Is a file."
#'    checkIsNotFile( linkToLinkToNowhere )
#'    #=> [1] "No such path."
#' }
#'
#' @export
checkIsNotFile <- function( path, path.info= NULL ) {
   tryCatch({
         if ( ! file.exists(path)) {
            return( "No such path." )
         }

         if (is.null(path.info)) {
            path.info <- file.info( path, extra_cols = FALSE )
         }

         if( path.info$isdir ) {
            check <- ''
         }
         else {
            check <- 'Is a file.'
         }
      },
      error= function (e) {
         check <<- paste0(
            "Checking for a file failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return( check )
}

#' @describeIn checkFileSystem Checks that the path exists and that it is a
#'   directory or a link to a directory If the path fails to exist, is a file,
#'   or is a link to a file, this fails. Will follow links with the result based
#'   on the final target.
#'
#'   \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{path} exists on the file system and was either
#'       a directory or a link to a directory.}
#'    \item{\emph{"No such path."}\verb{ }
#'       Check failed as \code{path} was not found on the file system.}
#'    \item{\emph{"Not a directory."}\verb{ }
#'       Check failed as \code{path} was found on the file system but was
#'       neither a directory nor a link to directory. (it was probably a file).}
#'    \item{\emph{"Checking for a directory failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @examples
#' checkIsDir( emptyFile )
#' #=> [1] "Not a directory."
#' checkIsDir( emptyDir )
#' #=> [1] ""
#' checkIsDir( noSuchFile )
#' #=> [1] "No such path."
#' checkIsDir( NA )    # file.exists(NA) is an error
#' #=> [1] "Checking for a directory failed with the following error:
#' #=> invalid 'file' argument"
#'
#' checkIsDir( emptyFile, path.info= file.info( emptyFile ))
#' #=> [1] "Not a directory."
#' checkIsDir( emptyFile, path.info= file.info( emptyDir ))  # Oops.
#' #=> [1] "" # Not true!
#'
#' if (okLink) {
#'    checkIsDir( linkToEmptyFile )
#'    #=> [1] "Not a directory."
#'    checkIsDir( linkToEmptyDir )
#'    #=> [1] ""
#'    checkIsDir( linkToNowhere )
#'    #=> [1] "No such path."
#'
#'    checkIsDir( linkToLinkToEmptyFile )
#'    #=> [1] "Not a directory."
#'    checkIsDir( linkToLinkToNowhere )
#'    #=> [1] "No such path."
#' }
#'
#' @export
checkIsDir <- function( path, path.info= NULL ) {
   tryCatch({
         if ( ! file.exists(path)) {
            return( "No such path." )
         }

         if (is.null(path.info)) {
            path.info <- file.info( path, extra_cols = FALSE )
         }

         if( path.info$isdir ) {
            check <- ''
         }
         else {
            check <- 'Not a directory.'
         }
      },
      error= function (e) {
         check <<- paste0(
            "Checking for a directory failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return( check )
}

#' @describeIn checkFileSystem Checks that the path exists and if so ensures it is
#'   neither a directory nor a link to a directory If the path fails to exist,
#'   is a file, or is a link to a file, this fails. Will follow links with the
#'   result based on the final target.
#'
#'   \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{path} exists on the file system but was neither
#'       a directory nor a link to a directory. It was probably a file.}
#'    \item{\emph{"No such path."}\verb{ }
#'       Check failed as \code{path} was not found on the file system.}
#'    \item{\emph{"Is a directory."}\verb{ }
#'       Check failed as \code{path} was found on the file system but was
#'       either a directory or a link to directory.}
#'    \item{\emph{"Checking for a directory failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @examples
#' checkIsNotDir( emptyFile )
#' #=> [1] ""
#' checkIsNotDir( emptyDir )
#' #=> [1] "Is a directory."
#' checkIsNotDir( noSuchFile )
#' #=> [1] "No such path."
#' checkIsNotDir( NA )    # file.exists(NA) is an error
#' #=> [1] "Checking for a directory failed with the following error:
#' #=> invalid 'file' argument"
#'
#' checkIsNotDir( emptyFile, path.info= file.info( emptyFile ))
#' #=> [1] ""
#' checkIsNotDir( emptyFile, path.info= file.info( emptyDir ))  # Oops.
#' #=> [1] "Is a directory." # Not true!
#'
#' if (okLink) {
#'    checkIsNotDir( linkToEmptyFile )
#'    #=> [1] ""
#'    checkIsNotDir( linkToEmptyDir )
#'    #=> [1] "Is a directory."
#'    checkIsNotDir( linkToNowhere )
#'    #=> [1] "No such path."
#'
#'    checkIsNotDir( linkToLinkToEmptyFile )
#'    #=> [1] ""
#'    checkIsNotDir( linkToLinkToNowhere )
#'    #=> [1] "No such path."
#' }
#'
#' @export
checkIsNotDir <- function( path, path.info= NULL ) {
   tryCatch({
         if ( ! file.exists(path)) {
            return( "No such path." )
         }
         if (is.null(path.info)) {
            path.info <- file.info( path, extra_cols = FALSE )
         }

         if( path.info$isdir ) {
            check <- 'Is a directory.'
         }
         else {
            check <- ''
         }
      },
      error= function (e) {
         check <<- paste0(
            "Checking for a directory failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
}

#' @describeIn checkFileSystem Checks a path to see if it is a symlink (and
#'   hence exists). By default, also follows link to ensure it points to a real
#'   file or directory, unless \code{okBadLink} is set \code{TRUE}.
#'
#'   \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{path} exists on the file system and was a
#'       link. If okBadLink is FALSE, it must also point, eventually, to
#'       a real file or directory.}
#'    \item{\emph{"No such path."}\verb{ }
#'       Check failed as the \code{path} was not found on the file system.}
#'    \item{\emph{"Not a link."}\verb{ }
#'       Check failed as \code{path} was found on the file system but was
#'       not a link. (It is probably a real file or directory.)}
#'    \item{\emph{"Bad link."}\verb{ }
#'       Check failed as \code{path} was found on the file system and it was
#'       a link, but \code{okBadLink = FALSE}, and the link (eventually) ends
#'       with a link to nowhere.}
#'    \item{\emph{"Unspecified error occurred checking if existing path was a link."}\verb{ }
#'       A weird condition occurred where the path exists but the check for a
#'       link with \code{\link{Sys.readlink}} returned \code{NA}.}
#'    \item{\emph{"Checking for a link failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @param okBadLink By default this is \code{FALSE} and links with missing
#'   targets will result in a failure message. Set \code{TRUE} to allow bad
#'   links.
#'
#' @export
#'
#' @examples
#' checkIsLink( emptyFile )
#' #=> [1] "Not a link"
#' checkIsLink( emptyDir )
#' #=> [1] "Not a link"
#' checkIsLink( noSuchFile )
#' #=> [1] "No such path."
#' checkIsLink( NA )    # file.exists(NA) is an error
#' #=> [1] "Checking for a link failed with the following error:
#' #=> invalid 'file' argument"
#'
#' if (okLink) {
#'    checkIsLink( linkToEmptyFile )
#'    #=> [1] ""
#'    checkIsLink( linkToEmptyDir )
#'    #=> [1] ""
#'    checkIsLink( linkToNowhere )
#'    #=> [1] "Bad link."
#'    checkIsLink( linkToNowhere, okBadLink= TRUE )
#'    #=> [1] ""
#'
#'    checkIsLink( linkToLinkToEmptyFile )
#'    #=> [1] ""
#'    checkIsLink( linkToLinkToNowhere )
#'    #=> [1] "Bad link."
#'    checkIsLink( linkToLinkToNowhere, okBadLink= TRUE )
#'    #=> [1] ""
#' }
#'
checkIsLink <- function( path, okBadLink = FALSE ) {
   tryCatch({
         linkStat <- Sys.readlink( path )
         if (is.na(linkStat)) {
            if ( ! file.exists(path)) {
               check <- 'No such path.'
            }
            else {
               # This may not be possible...
               check <- 'Unspecified error occurred checking if existing path was a link.'
            }
         }
         else if (linkStat == "") {
            check <- 'Not a link.'
         }
         else {
            if (okBadLink | file.exists(path) ) {
               check <- ''
            }
            else {
               check <- 'Bad link.'
            }
         }
      },
      error= function(e) {
         check <<- paste0(
            "Checking for a link failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return( check )
}

#' @describeIn checkFileSystem Ensures that the path exists and is a real file or
#'   directory. If it is a symbolic link, this fails. The status of a link
#'   target is not checked, as links with or without targets fail.
#'
#'   \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{path} exists on the file system and is a real
#'       file or directory (not a link).}
#'    \item{\emph{"No such path."}\verb{ }
#'       Check failed as the \code{path} was not found on the file system.}
#'    \item{\emph{"Not a link."}\verb{ }
#'       Check failed as \code{path} was found on the file system but it was
#'       a symbolic link.}
#'    \item{\emph{"Unspecified error occurred checking if existing path was a link."}\verb{ }
#'       A weird condition occurred where the path exists but the check for a
#'       link with \code{\link{Sys.readlink}} returned \code{NA}.}
#'    \item{\emph{"Checking for a link failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @examples
#' checkIsNotLink( emptyFile )
#' #=> [1] ""
#' checkIsNotLink( emptyDir )
#' #=> [1] ""
#' checkIsNotLink( noSuchFile )
#' #=> [1] "No such path."
#' checkIsNotLink( NA )    # file.exists(NA) is an error
#' #=> [1] "Checking for a link failed with the following error:
#' #=> invalid 'file' argument"
#'
#' if (okLink) {
#'    checkIsNotLink( linkToEmptyFile )
#'    #=> [1] "Is a link."
#'    checkIsNotLink( linkToEmptyDir )
#'    #=> [1] "Is a link."
#'    checkIsNotLink( linkToNowhere )
#'    #=> [1] "No such path"
#'
#'    checkIsNotLink( linkToLinkToEmptyFile )
#'    #=> [1] "Is a link."
#'    checkIsNotLink( linkToLinkToNowhere )
#'    #=> [1] "No such path."
#' }
#'
#' # cleanup
#' unlink( emptyFile )
#' unlink ( emptyDir, recursive= TRUE )
#' unlink( linkToEmptyFile )
#' unlink( linkToEmptyDir )
#' unlink( linkToNowhere )
#' unlink( linkToLinkToEmptyFile )
#' unlink( linkToLinkToNowhere )

#' @export
checkIsNotLink <- function( path ) {
   tryCatch({
         linkStat <- Sys.readlink( path )
         if (is.na(linkStat)) {
            if ( ! file.exists(path)) {
               check <- 'No such path.'
            }
            else {
               # This may not be possible...
               check <- 'Unspecified error occurred checking if existing path was a link.'
            }
         }
         else if (linkStat == "") {
            check <- ''
         }
         else {
            check <- 'Is a link.'
         }
      },
      error= function(e) {
         check <<- paste0(
            "Checking for a link failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return( check )
}
