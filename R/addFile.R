#' Add a file to a data repo.
#'
#' Copies the specified \code{file} into the \code{repo}, or to the current
#' directory if no repository is specified. It will be renamed if \code{asFile}
#' is specified, and will be put into a subdirectory of the repo if \code{entry}
#' is specified, possibly a multi-directory path. This will be created if
#' needed. Assuming all optional elements are specified, the path of the file
#' created in the repository will be \code{<repo>\\< entry>\\<asFile>}. To
#' validate the file before copy, expected file size and file checksums can be
#' given. If \code{file} does not match, an error-containing validation vector
#' will be returned. Regardless of initial validation, the copy will be
#' validated using the actual file size and file checksum after copy. Any error
#' will result in an error containing validation vector.
#'
#' @param file The file to copy into the repo. May be relative or absolute, but
#'   may not be a symlink or a directory.
#'
#' @param repo The base directory for the repo. Must exist on the file system
#'   and should have a restrictive set of ownerships and permissions that
#'   includes restrictions on parent directories. By default will use the
#'   current directory if not specified.
#'
#' @param entry A subdirectory or subdirectory tree in the repo where the file
#'   should be put. These will be created (recursively) if they don't exist with
#'   the specified group owner and permissions 2775. This translates to sticky
#'   directories with full permissions except other-write. Due to the way
#'   \code{repoDir} is expected to be set up, this is NOT world readable.
#'   Leading and trailing path separators are ignored.
#'
#' @param asFile The new name for the file in the repository. Must be a plain
#'   base name, not a directory (extensions are fine.) By default this is the
#'   base name of \code{file}.
#'
#' @param writeGroup The file system group that all created directories and the
#'   file copy will be assigned, if set. The default group is used if this if
#'   left \code{NULL} (the default).
#'
#' @param fileSize The expected size of the source file, in bytes. By default
#'   this is \code{NULL}, meaning file size will not be checked. If a positive
#'   value is supplied it will be checked against the size of the source file on
#'   the file system. If it does not match then failure is reported and the
#'   checksum is not checked.
#'
#' @param checksum The expected checksum of the source file. By default this is
#'   \code{NULL}, meaning no checksum is generated. If given it will be checked
#'   agains the value provided by the \code{checksumFunc}.
#'
#' @param checksumFunc The function object (not the string name) to use when
#'   calculating checksums. By default this is \code{tools::md5sum}. The
#'   specified function will be called with one parameter, \code{path}. This
#'   returned value should be an (atomic) vector type but can not be \code{NULL}
#'   or a missing value.  When \code{checksumFunc} is called, \code{path} has
#'   already been verified and is known to exist on the file system as a real
#'   file (not a directory or link). The function object passed may not be
#'   \code{NULL} when \code{checksum} is provided, but is not used
#'   \code{checksum} is \code{NULL}. Note, this same function is used to
#'   calculate checksums for validating the copy, so it called even when no
#'   source checksum was provided.
#'
#' @return A named vector of validation results. Each element is named for a
#'   check performed, and will be the empty string if the check succeeded, a
#'   failure string if the check failed, and a missing value if the check was
#'   not performed (i.e. was not wanted or previous checks failed making further
#'   checking irrelevant.)
#'
#' @examples
#' \dontrun{
#' checks <- addFile( sourcefile, repoDir, entry= 'proj_3/set_A', asFile= 's1.dat'
#'    writeGroup= 'repoWrite', fileSize= 1234567890, checksum= sourceFileMd5 )
#'
#' report <- checkSummary(checks)
#' if (report != '') { stop(report) }
#' }
#'
#' @export
addFile <- function( file, repo= getwd(), entry= NULL, asFile= basename(file),
                     writeGroup= NULL, fileSize= NULL, checksum= NULL,
                     checksumFunc= tools::md5sum ) {
   .checkParam_repo  <- function() {
      force(repo)
      check <- checkIsSingle( repo, mode='character' )
      if (check == '') {
         check <- checkCharacterCount( repo )
      }
      if (check == '') {
         check <- checkIsDir(repo)
      }
      return(check)
   }
   .checkParam_entry  <- function() {
      force(entry)
      if (is.null(entry)) {
         return("")
      }
      else {
         check <- checkIsSingle( entry, mode='character' )
         if (check == '') {
            check <- checkCharacterCount( entry )
         }
         return(check)
      }
   }
   .checkParam_asFile <- function() {
      force(asFile)
      check <- checkIsSingle( asFile, mode='character' )
      if (check == '') {
         check <- checkCharacterCount( asFile )
      }
      if (check == '' && grepl( .Platform$file.sep, asFile, fixed= TRUE)) {
         check <- 'Path elements not ok.'
      }
      check
   }
   .checkParam_writeGroup <- function() {
      force(writeGroup)
      if (is.null(writeGroup)) {
         return("")
      }
      else {
         check <- checkIsSingle( writeGroup, mode='character' )
         if (check == '') {
            check <- checkCharacterCount( writeGroup )
         }
         if (check == '') {
            grpList <- groups() # Separate line allows mocking groups().
            check <- checkIsIn( writeGroup, grpList )
            if (check != '') {
               check <- "Not one of user's groups."
            }
         }
      }
      return(check)
   }
   .checkParam_checksumFunc <- function() {
      force(checksumFunc)
      check <- checkIsNotNull( checksumFunc )
      if (check == '' && ! is.function( checksumFunc )) {
         check <- 'Not a function object.'
      }
      return(check)
   }
   .checkValidSource <- function(naOk) {
      check <- validateFile( file, fileSize= fileSize, checksum= checksum,
                          checksumFunc= checksumFunc )
      return(checkSummary( check, naOk= naOk ))
   }
   .doCopy <- function(target) {
      copyOk <- file.copy( from= file, to= target )
      if (is.null(copyOk) || is.na(copyOk) || ! copyOk) {
         return("Copy failed.")
      }
      if (! is.null(writeGroup)) {
         ok <- chgrp(writeGroup, target)
         if (is.null(ok) || ! is.character(ok) || is.na(ok) || length(ok) != 1 || nchar(ok) < 1) {
            return( paste0( "Setting copy write group failed." ))
         }
      }
      if (! Sys.chmod(target, mode = "2775", use_umask = FALSE)) {
         return(paste0( "Setting copy permissions failed." ))
      }
      return("")
   }
   .makeTargetDir <- function( repo, entry ) {
      targetDir <- repo
      if (! is.null(entry)) {
         targetDirs <- strsplit(entry, .Platform$file.sep, fixed=TRUE)[[1]]
         for (dir in targetDirs) {
            if (dir == "") { next }
            targetDir <- file.path(targetDir, dir)
            if (! file.exists( targetDir )) {
               if ( ! dir.create( targetDir )) {
                  stop( makeCheckError(paste0( 'Failure trying to create directory: ', targetDir )))
               }
               if (! is.null(writeGroup)) {
                  ok <- chgrp(writeGroup, targetDir)
                  if (is.null(ok) || ! is.character(ok) || is.na(ok) || length(ok) != 1 || nchar(ok) < 1) {
                     stop( makeCheckError(paste0( 'Failure trying to set write group of directory: ', targetDir )))
                  }
               }
               if (! Sys.chmod(targetDir, mode = "2775", use_umask = FALSE)) {
                  stop( makeCheckError(paste0( 'Failure trying to set permissions of directory: ', targetDir )))
               }
            }
            else {
               if (! dir.exists( targetDir )) {
                  stop( makeCheckError(paste0( 'File blocking directory creation: ', targetDir )))
               }
            }
         }
      }
      return(targetDir)
   }

   # Set up default return with each check a missing values.
   tests <- c('checkParam_repo', 'checkParam_entry', 'checkParam_asFile',
              'checkParam_writeGroup', 'checkParam_checksumFunc',
              'checkValidSource', 'makeTargetDir', 'doCopy', 'checkCopyTarget',
              'unexpectedError')
   result <- rep(NA_character_, length(tests))
   names(result) <- tests

   result <- tryCatch({
         # Vector of check names that can be NA, due to missing validation input
         naOk <- NULL
         if (is.null(fileSize)) {
            naOk <- c(naOk, 'checkFileSizeMatches')
         }
         if (is.null(checksum)) {
            naOk <- c(naOk, 'checkChecksumMatches')
         }

         result['checkParam_repo'] <- .checkParam_repo()
         if (result['checkParam_repo'] != '') { return(result) }

         result['checkParam_entry'] <- .checkParam_entry()
         if (result['checkParam_entry'] != '') { return(result) }

         result['checkParam_asFile'] <- .checkParam_asFile()
         if (result['checkParam_asFile'] != '') { return(result) }

         result['checkParam_writeGroup'] <- .checkParam_writeGroup()
         if (result['checkParam_writeGroup'] != '') { return(result) }

         result['checkParam_checksumFunc'] <- .checkParam_checksumFunc()
         if (result['checkParam_checksumFunc'] != '') { return(result) }

         result['checkValidSource'] <- .checkValidSource(naOk = naOk)
         if (result['checkValidSource'] != '') { return(result) }

         # Need source file size and checksum for copy validation, if not
         # provided. (If they were provided, they were checked already against
         # the source.)
         if (is.null(fileSize)) {
            fileSize <- file.info(file, extra_cols= FALSE)[1, 'size']
         }
         if (is.null(checksum)) {
            checksum <- do.call( checksumFunc, list( file ))
         }

         result['makeTargetDir'] <- tryCatch({
               targetDir <- .makeTargetDir( repo, entry )
               ""
            },
            check= checkMessage,
            error= errorMessage
         )
         if (result['makeTargetDir'] != '') { return(result) }

         target <- file.path( targetDir, asFile)
         result['doCopy'] <- checkIsNotPath(target)
         # Don't want to copy or error out if target exists.
         if (result['doCopy'] == '') {
            result['doCopy'] <- .doCopy(target = target)
            if (result['doCopy'] != '') { return(result) }
         }
         result['checkCopyTarget'] <- checkSummary(
            validateFile( target, fileSize= fileSize, checksum= checksum,
                          checksumFunc= checksumFunc
            ), naOk= naOk
         )

         if (result['checkCopyTarget'] != '') { return(result) }

         # If didn't copy, but got here anyway, know copy and source match, and
         # that there were no unexpected errors.
         result['doCopy'] <- ""
         result['unexpectedError'] <- ""

         result
   }, error= function(e) {
      result['unexpectedError'] <- errorMessage(e)
      return(result)
   })
   return(result)
}

errorMessage <- function (e) {
   return( paste0( "Unexpected error: ", conditionMessage( e )))
}
checkMessage <- function (e) {
   return( conditionMessage( e ))
}
makeCheckError <- function (message) {
   e <- simpleError(message)
   class(e) <- c('check', class(e))
   return(e)
}
