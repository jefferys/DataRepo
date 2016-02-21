#' Create a symbolic link to a repository element.
#'
#' Creates a symbolic link pointing to a target in the repository. The target
#' must exist, and if any validation information is included, the target must
#' match. What to do if the link to be created has the same name as an existing
#' file system object depends on \code{duplicateOk} and \code{onExists}. By
#' default failure will be returned unless the file system object is already a
#' link to the target, or if it is an exact copy of the duplicate. Successful
#' linking is reported as a vector of empty strings. Problems are reported with
#' one or more non-empty elements.
#'
#' @param file The symbolic link to create. Assuming all optional elements are
#'   specified, the target of this link in the repository will be
#'   \code{<repo>\\<entry>\\<asFile>}. If a file object matching the link name
#'   already exists, \code{duplicateOk} and \code{onExists} control what happens.
#'
#' @param repo The base directory for the file to be linked to. By default will
#'   use the current directory if not specified.
#'
#' @param entry A subdirectory or subdirectory tree in the repo where the target
#'   file is expected to be found. By default this is NULL, indicating the root
#'   \code{repo} directory is used.
#'
#' @param asFile The base name of the target file in the repository. By default
#'   this is the base name of \code{file}. This target file must exist and be
#'   a real file, not a link.
#'
#' @param fileSize The expected size of the target file, in bytes. By default
#'   this is \code{NULL}, meaning file size will not be checked. If a positive
#'   value is supplied it will be checked against the size of the source file on
#'   the file system. If it does not match then failure is reported and the
#'   checksum is not checked.
#'
#' @param checksum The expected checksum of the target file. By default this is
#'   \code{NULL}, meaning no checksum is generated. If given it will be checked
#'   agains the value provided by the \code{checksumFunc}.
#'
#' @param checksumFunc The function or function name (as a string) that will be
#'   used when calculating checksums. The calculated target file checksum will
#'   be verified against any provided checksums before linking, and this may
#'   also be used if \code{onExists} requires it. String function names may be
#'   qualified with a \code{SomePackage::} prefix as the function named is
#'   retrieved using \code{\link{getSomewhere}}. By default the function used is
#'   '\code{'tools::md5sum'}. The specified function should have one parameter,
#'   a file path. The returned checksum value should be an (atomic) vector type
#'   but can not be \code{NULL} or a missing value. When \code{checksumFunc} is
#'   called, \code{path} has already been verified and is known to exist on the
#'   file system as a real file (not a directory or link).
#'
#' @param onExists What to do if the link name already exists on the file
#'   system. May not be applied if the existing file is a duplicate of the
#'   target, as defined by \code{duplicateOk}.
#' \code{itemized}{
#'    \code{item}{\code{"error"} - Return failure result.}
#'    \code{item}{\code{"replaceFile"} - Delete existing file and replace with
#'    link. Generates a warning. If this is a link or a directory, this will
#'    fail with error.}
#'    \code{item}{\code{"replaceAny"} - Delete existing file system entry and
#'    replace with a link. Deletes recursively if the existing object is a
#'    directory. You probably don't want to do this. Generates a warning. }
#'    \code{item}{\code{"backupFile"} - Renames an existing file adding
#'    \code{.bak}, generating a warning. Multiple \code{.bak} may be added if
#'    needed to create unique filenames. Replaces the original with the link. If
#'    the original was a directory or a link, returns with error status. }
#'    \code{item}{\code{"backupAny"} - Renames the existing file system entry
#'    adding \code{.bak}, generating a warning. Multiple \code{.bak} may be
#'    added if needed to create unique filenames. Replaces the original with the
#'    link. This is done even if the original was a directory or a link.}
#'    \code{item}{\code{"skip"} - Leave as is. Generates a warning.}
#' }
#'
#' @param duplicateOk By default \code{onExists} applies only if the file
#'   exists and is not a duplicate. Set \code{FALSE} if \code{onExists} should
#'   always be applied. Duplicate means is a real file and matches the target by
#'   size and checksum, or is a link to the target. Links to an identical file
#'   that is not in the repository is \emph{not} a duplicate. Real duplicate
#'   files will be deleted and replaced with links. If the existing file is a
#'   links to the target, nothing needs to be done.
#'
#' @return A named vector of validation results. Each element is named for a
#'   check performed, and will be the empty string if the check succeeded, a
#'   failure string if the check failed, and a missing value if the check was
#'   not performed (i.e. was not wanted or previous checks failed making further
#'   checking irrelevant.) Can be summarized with \code{\link{checkSummary}}.
#'   The elements are named \code{'checkParam_file', 'checkParam_repo',
#'   'checkParam_entry', 'checkParam_asFile', 'checkParam_checksumFunc',
#'   'checkParam_onExists', 'checkParam_duplicateOk', 'checkTarget',
#'   'checkExists', 'createLink', 'unexpectedError'}, in order.
#'
#' @export
linkTo <- function(
   file, repo= getwd(), entry= NULL, asFile= basename(file),
   fileSize= NULL, checksum= NULL, checksumFunc= 'tools::md5sum',
   onExists='error', duplicateOk = TRUE
) {

   .checkParam_file  <- function() {
      force(file)
      check <- checkIsSingle( file, mode='character' )
      if (check == '') {
         check <- checkCharacterCount( file )
      }
      # Path testing depends on onExists
      return(check)
   }
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
         # Target path testing depends on repo
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
      # Target file testing depends on repo and entry
      return(check)
   }
   # fileSize and checksum validated if provided by checkTarget.
   .checkParam_checksumFunc <- function() {
      force(checksumFunc)
      check <- checkIsNotNull( checksumFunc )
      if ( check == "" && ! is.function( checksumFunc )) {
         check <- checkIsSingle(checksumFunc, 'character')
         if ( check == "" ) {
            check <- checkCharacterCount( checksumFunc, minimumCharacterCount= 1 )
         }
      }
      return(check)
   }
   .checkParam_onExists <- function() {
      check <- checkIsSingle(onExists, "character")
      if (check == "" ) {
         check <- checkIsIn( onExists, c("error", "replaceFile", "replaceAny",
                                     "backupFile", "backupAny", "skip"))
      }
      return(check)
   }
   .checkParam_duplicateOk <- function() {
      check <- checkIsSingle( duplicateOk, mode="logical")
      if (check == '' && is.na(duplicateOk)) {
         check <- 'NA not ok.'
      }
      return(check)
   }
   .doLinkCreate <- function(linkName, target) {
      if (file.symlink( target, linkName )) {
         return("")
      }
      else {
         return("Symlink not created.")
      }
   }
   .doDelete <- function(x, fileOnly= TRUE, warn= TRUE) {
      if (fileOnly) {
         if (dir.exists(x)) {
            return("Deletion of pre-existing directory not attempted.")
         }
         else if (Sys.readlink(x) != '') {
            return("Deletion of pre-existing link not attempted.")
         }
      }
      if ( unlink(x, recursive= TRUE) == 0) {
         if (warn) {
            warning(paste0("Removed existing file: ", x))
         }
         return("")
      }
      else {
         if (dir.exists(x)) {
            return("Deletion of pre-existing directory failed.")
         }
         else if (Sys.readlink(x) != '') {
            return("Deletion of pre-existing link failed.")
         }
         else {
            return("Deletion of pre-existing file failed.")
         }
      }
   }
   .doBackup <- function(x, fileOnly= TRUE) {
      if (fileOnly) {
         if (dir.exists(x)) {
            return("Rename of pre-existing directory not attempted.")
         }
         else if (Sys.readlink(x) != '') {
            return("Rename of pre-existing link not attempted.")
         }
      }
      toFile <- paste0( x, ".bak" )
      while (file.exists(toFile)) {
         toFile <- paste0( toFile, ".bak" )
      }
      if (! file.rename(x, toFile)) {
         if (dir.exists(x)) {
            return("Rename of pre-existing directory failed.")
         }
         else if (Sys.readlink(x) != '') {
            return("Rename of pre-existing link failed.")
         }
         else {
            return("Rename of pre-existing file failed.")
         }
      }
      warning(paste0("Renamed existing file ", x, " to ", toFile))
      return("")
   }

   # Set up default return with each check a missing values.
   tests <- c('checkParam_file', 'checkParam_repo', 'checkParam_entry',
              'checkParam_asFile', 'checkParam_checksumFunc',
              'checkParam_onExists', 'checkParam_duplicateOk',
              'checkTarget', 'checkExists', 'createLink', 'unexpectedError')
   result <- rep(NA_character_, length(tests))
   names(result) <- tests

   # Default is none, only set if actually have uncaught error.
   result['unexpectedError'] <- ""

   result <- tryCatch({
      result['checkParam_file'] <- .checkParam_file()
      if (result['checkParam_file'] != '') { return(result) }

      result['checkParam_repo'] <- .checkParam_repo()
      if (result['checkParam_repo'] != '') { return(result) }

      result['checkParam_entry'] <- .checkParam_entry()
      if (result['checkParam_entry'] != '') { return(result) }

      result['checkParam_asFile'] <- .checkParam_asFile()
      if (result['checkParam_asFile'] != '') { return(result) }

      result['checkParam_checksumFunc'] <- .checkParam_checksumFunc()
      if (result['checkParam_checksumFunc'] != '') { return(result) }

      if (is.character(checksumFunc)) {
         checksumFunc <- getSomewhere(checksumFunc)
      }

      result['checkParam_onExists'] <- .checkParam_onExists()
      if (result['checkParam_onExists'] != '') { return(result) }

      result['checkParam_duplicateOk'] <- .checkParam_duplicateOk()
      if (result['checkParam_duplicateOk'] != '') { return(result) }

      # Validate target, including checksum and size if given.
      target <- targetName( repo, entry, asFile )
      naOk <- NULL
      if (is.null(fileSize)) {
         naOk <- c(naOk, 'checkFileSizeMatches')
      }
      if (is.null(checksum)) {
         naOk <- c(naOk, 'checkChecksumMatches')
      }
      result['checkTarget'] <- checkSummary(
         validateFile( target,
            fileSize= fileSize, checksum= checksum, checksumFunc= checksumFunc
         ), naOk= naOk
      )
      if (result['checkTarget'] != '') { return(result) }

      if (! file.exists(file)) {
         result['checkExists'] <- ""
         result['createLink'] <- .doLinkCreate(file, target)
         return(result)
      }

      # If duplicateOk and is duplicate,  Done, otherwise continue
      if (duplicateOk) {
         asLink <- Sys.readlink(file)
         if (asLink != "") {
            # link to anything not target is not a duplicate, even if it is
            # a link to a *different* identical file.
            if (normalizePath(asLink) == normalizePath(target)) {
               result['checkExists'] <- ""
               result['createLink']  <- ""
               return(result)
            }
         }

         # Is it a file? Check if duplicate of target.
         else if (! dir.exists(file)) {
            exitingFileSize <- file.info(file, extra_cols= FALSE)[1, 'size']
            exitingChecksum <- do.call( checksumFunc, list( file ))
            dupeCheck <- checkSummary( validateFile(
               target, fileSize= exitingFileSize, checksum= exitingChecksum,
               checksumFunc= checksumFunc
            ))
            # If is duplicate, replace with link (without warning) or record error.
            if (dupeCheck == "") {
               result['checkExists'] <- .doDelete(file, fileOnly= TRUE, warn= FALSE)
               if (result['checkExists'] != '') { return(result) }
               result['createLink']  <- .doLinkCreate(file, target)
               if (result['createLink'] != '') {
                  warning(paste0("Deleted ", file, " but could not create link to ", target))
               }
               return(result)
            }
         }
      }

      # Either duplicateOk was not set, or it is not a duplicate, so
      # onExists controls what to do.
      if ( onExists == "skip" ) {
         result['checkExists'] <- ""
         warning(paste0("Skipped linking due to existing file: ", file))
         return(result)
      }
      else if (onExists == "replaceFile") {
         result['checkExists'] <- .doDelete(file, fileOnly= TRUE)
         if (result['checkExists'] != '') { return(result) }
         result['createLink']  <- .doLinkCreate(file, target)
         if (result['createLink'] != '') {
            warning(paste0("Deleted ", file, " but could not create link to ", target))
         }
         return(result)
      }
      else if (onExists == "replaceAny") {
         result['checkExists'] <- .doDelete(file, fileOnly= FALSE)
         if (result['checkExists'] != '') { return(result) }
         result['createLink']  <- .doLinkCreate(file, target)
         if (result['createLink'] != '') {
            warning(paste0("Deleted ", file, " but could not create link to ", target))
         }
         return(result)
      }
      else if (onExists == "backupFile") {
         result['checkExists'] <- .doBackup(file, fileOnly= TRUE)
         if (result['checkExists'] != '') { return(result) }
         result['createLink']  <- .doLinkCreate(file, target)
         if (result['createLink'] != '') {
            warning(paste0("Backed up ", file, " but could not create link to ", target))
         }
         return(result)
      }
      else if (onExists == "backupAny") {
         result['checkExists'] <- .doBackup(file, fileOnly= FALSE)
         if (result['checkExists'] != '') { return(result) }
         result['createLink']  <- .doLinkCreate(file, target)
         if (result['createLink'] != '') {
            warning(paste0("Backed up ", file, " but could not create link to ", target))
         }
         return(result)
      }
      # Should only get here if onExists == "error", but if get here for
      # any other reason, still exits as error.
      else {
         result['checkExists'] <- "File exists."
         return(result)
      }
   }, error= function(e) {
      result['unexpectedError'] <- conditionMessage(e)
      return(result)
   })
   return(result)
}


targetName <- function( root, dir=NULL, file ) {
   if (is.null(dir)) {
      return( file.path( root, file ))
   }
   else {
      return( file.path( root, dir, file ))
   }
}
