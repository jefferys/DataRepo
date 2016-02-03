#' Validate source files
#'
#' Applies several checks to validate a file. Outputs a named character vector
#' of strings giving the results of the checks, where the names are the tests
#' and the value is the check result. Empty string are reported for checks that
#' succeed and error message for checks that fail. Missing values
#' \code{NA_character_} are reported when a test id not performed. This means
#' either a test was not supposed to run, or that a previous test failed.
#' Generally, the first failing test causes the result vector to be returned
#' with all following check results reported as missing.
#'
#' @param path The path to the file to check. Should exist and be a real file.
#'   If not, failure will be reported.
#'
#' @param fileSize The expected file of the size, in bytes. May be \code{NULL}
#'   and if so, file size will not be checked. If a positive value, the size of
#'   the file will be looked up on the file system and compared. If it does not
#'   match then failure is reported and the checksum is not checked.
#'
#' @param checksum The checksum the expected file will have. May be \code{NULL}
#'   and if so, file size will not be checked. If given the checksum of the file
#'   specified will be calculated using checksumFUNC, by default using the md5
#'   algorithm as provided by \code{tools::md5sum}.
#'
#' @param checksumFunc The function object (not the string name) to use when
#'   calculating the checksum of a file. Will be called with one parameter,
#'   \code{path}. The value returned from this function call is tested against
#'   the \code{checksum} parameters value. That value may not be \code{NULL} or
#'   a missing value, but may be of any (atomic) vector type. When called, the
#'   path has already been verified to exists on the file system as a real file
#'   (not a directory or link). The function object passed may not be
#'   \code{NULL} if a checksum is provided \code{NULL}, but is not used if the
#'   checksum is \code{NULL}
#'
#' @return A named character vector of test results. The names are the tests
#'   performed and the values are the results of those tests as a string. Empty
#'   strings indicate success, non-empty strings are failure messages indicating
#'   what went wrong. May contain NA's, which either mean something went wrong
#'   earlier so this test was not run, or that the test was selected not to be
#'   run
#'
#'   Tests are:
#'   \itemize{
#'    \item{"checkParam_path" - Is the \code{path} parameter ok? }
#'    \item{""checkParam_checksum" - Is the \code{checksum} parameter ok? }
#'    \item{""checkParam_checksumFunc" - Is the \code{checksumFunc} parameter
#'    ok? }
#'    \item{""checkParam_fileSize" - Is the \code{fileSize} parameter ok? }
#'    \item{""checkParam_checksum_checksumFunc" - Is the combination of the
#'       \code{checksum} and \code{checksumFunc} parameters ok? }
#'    \item{""checkIsFile" - Is \code{path} an existing file system file object?
#'    }
#'    \item{""checkIsNotLink" - Is \code{path} a real file system object (not a
#'    link)? Should always succeed on systems that don't support links, when
#'    checkIsFile passed. }
#'    \item{"checkFileSizeMatches" - Validate that the size of \code{path} in
#'    bytes matches that provided as \code{fileSize}. }
#'    \item{"checkChecksumMatches" - Validate that the checksum of \code{path}
#'    as calculated by the function object provided as \code{checksumFunc}
#'    matches that provided as \code{checksum}. The default is to check md5
#'    sums. }
#' }
#'
#' @export
#'
#' @examples
#' # Setup for examples
#' noSuchFile  <- tempfile( 'noSuchFile'  )
#' emptyFile <- tempfile( 'emptyFile' )
#' md5EmptyFile <- 'd41d8cd98f00b204e9800998ecf8427e'
#' file.create( emptyFile )
#' binFile <- tempfile( 'binFile', fileext= '.bin' )
#' writeBin( as.raw(c(1,2,3)), binFile)
#'
#' check <- validateSourceFile( emptyFile, fileSize= 0, checksum= md5EmptyFile )
#' names(check)
#' #=> [1] "checkParam_path"                   "checkParam_checksum"
#' #=> [3] "checkParam_checksumFunc"           "checkParam_fileSize"
#' #=> [5] "checkParam_checksum_checksumFunc"          "checkIsFile"
#' #=> [7] "checkIsNotLink"                   "checkFileSizeMatches"
#' #=> [9] "checkChecksumMatches"
#' check['checkFileSizeMatches']
#' #=> checkFileSizeMatches
#' #=>                   ""
#' # Remove names to make things simple
#' names(check) <- NULL
#' check
#' #=> [1] "" "" "" "" "" "" "" "" ""
#'
#' # Not checking file contents
#' check <- validateSourceFile( emptyFile, fileSize= NULL, checksum= NULL )
#' names(check) <- NULL
#' #=> [1] "" "" "" "" "" "" "" NA NA
#'
#' # Bad checksum
#' check <- validateSourceFile( emptyFile, fileSize= 0, checksum= 'abc123' )
#' names(check) <- NULL
#' #=> [1] "" "" "" "" "" "" "" ""
#' #=> [9] "Checksum mismatch. Found d41d8cd98f00b204e9800998ecf8427e wanted abc123.
#'
#' # Bad file size. No need to check the checksum if file size doesn't match
#' check <- validateSourceFile( emptyFile, fileSize= 1, checksum= 'BAD' )
#' names(check) <- NULL
#' #=> [1] "" "" "" "" "" "" ""
#' #=> [8] "File size mismatch. Found 0 wanted 1."
#' #=> [9] NA
#'
#' # Bad file path, no need to check anything else.
#' check <- validateSourceFile( noSuchFile, fileSize= 0, checksum= md5EmptyFile )
#' names(check) <- NULL
#' #=> [1] ""  ""  ""  ""  ""  "No such path."
#' #=> [7] NA  NA  NA
#'
#' # Using your own checksum function object
#' SumIt <- function (path) { sum( as.numeric(
#'     readBin( path, what = 'raw', n= file.info(path)[['size']] + 1 )
#' ))}
#' check <- validateSourceFile( binFile, fileSize= 3, checksum= 6, checksumFunc= SumIt )
#' names(check) <- NULL
#' check
#' #=> [1] "" "" "" "" "" "" "" "" ""
#'
#' # Cleanup
#' file.remove( emptyFile )
#' file.remove( binFile )
validateSourceFile <- function ( path, checksum= NULL, checksumFunc= tools::md5sum,
                                 fileSize= NULL ) {
   .checkParam_path         <- function(path) {
      tryCatch ({
         force(path)
         check <- checkIsSingle( path, mode='character' )
         if (check == '') {
            check <- checkCharacterCount( path )
         }
         check
      }, error= function (e) {
          return( paste0( "Unexpected error: ", conditionMessage( e )))
      })
   }
   .checkParam_checksum     <- function(checksum) {
      tryCatch ({
         force(checksum)
         if (is.null(checksum)) {
            ''
         }
         else {
            check <- checkIsSingle(checksum)
            if (check == "" && is.na(checksum)) {
               check <- "Is NA."
            }
            check
         }
      }, error= function (e) {
         return( paste0( "Unexpected error: ", conditionMessage( e )))
      })
   }
   .checkParam_checksumFunc <- function(checksumFunc) {
      tryCatch ({
         force(checksumFunc)
         if (is.null(checksumFunc)) {
            ''
         }
         else if (is.function(checksumFunc)) {
            ''
         }
         else {
            'Not a function object.'
         }
      }, error= function (e) {
         return( paste0( "Unexpected error: ", conditionMessage( e )))
      })
   }
   .checkParam_fileSize     <- function(fileSize) {
      tryCatch ({
         force(fileSize)
         if (is.null(fileSize)) {
            ''
         }
         else {
            check <- checkIsSingle(fileSize, mode='numeric')
            if (check == '' && is.nan(fileSize)) {
               check <- 'Is NaN.'
            }
            if (check == '' && is.na(fileSize)) {
               check <- 'Is NA.'
            }
            if (check == '' && fileSize < 0) {
               check <- 'Is negative.'
            }
            check
         }
      }, error= function (e) {
         return( paste0( "Unexpected error: ", conditionMessage( e )))
      })
   }
   .checkParam_checksum_checksumFunc <- function(checksum, checksumFunc) {
      tryCatch ({
         if ( is.null(checksumFunc) && ! is.null(checksum) ) {
            'Checksum but no checksum function.'
         }
         else {
            ''
         }
         # nocov start - Any reliable way to trigger this error?
      }, error= function (e) {
         return( paste0( "Unexpected error: ", conditionMessage( e )))
      })
      # nocov end
   }

   # Set up default validation result, names and values.
   tests <- c('checkParam_path', 'checkParam_checksum', 'checkParam_checksumFunc',
              'checkParam_fileSize', 'checkParam_checksum_checksumFunc',
              'checkIsFile', 'checkIsNotLink', 'checkFileSizeMatches',
              'checkChecksumMatches')
   result <- rep(NA_character_, length(tests))
   names(result) <- tests

   # Validate parameters for this function. Done if fail.
   result['checkParam_path']         <- .checkParam_path( path )
   if (result['checkParam_path'] != '') { return(result) }

   result['checkParam_checksum']     <- .checkParam_checksum( checksum )
   if (result['checkParam_checksum'] != '') { return(result) }

   result['checkParam_checksumFunc'] <- .checkParam_checksumFunc( checksumFunc )
   if (result['checkParam_checksumFunc'] != '') { return(result) }

   result['checkParam_fileSize']     <- .checkParam_fileSize( fileSize )
   if (result['checkParam_fileSize'] != '') { return(result) }

   result['checkParam_checksum_checksumFunc'] <-
      .checkParam_checksum_checksumFunc( checksum, checksumFunc )
   if (result['checkParam_checksum_checksumFunc'] != '') { return(result) }

   # Parameters good, so validate an actual file is available to check. Done if fail.
   result['checkIsFile']    <- checkIsFile(path)
   if (result['checkIsFile'] != '') { return(result) }

   result['checkIsNotLink'] <- checkIsNotLink(path)
   if (result['checkIsNotLink'] != '') { return(result) }

   # Actual file available, so check size if desired. Done if fail.
   # Default is NA, but assigned agian to be make explicit.
   if (! is.null(fileSize)) {
      result['checkFileSizeMatches'] <- .checkFileSizeMatches(path, fileSize)
      if (result['checkFileSizeMatches'] != '') {
         return(result)
      }
   }
   else {
      # Default is NA, but assigned agian to be make explicit.
      result['checkFileSizeMatches'] <- NA_character_
   }

   # Actual file available and size, if available matched, so check
   # checksum, if desired
   if (! is.null(checksum)) {
      result['checkChecksumMatches'] <- .checkChecksumMatches(path, checksum, checksumFunc)
   }
   else {
      # Default is NA, but assigned agian to be make explicit.
      result['checkChecksumMatches'] <- NA_character_
   }

   # Previous was last check, so just return whatever
   return(result)
}

.checkFileSizeMatches <- function (path, size) {
   tryCatch ({
      check <- ''
      fileStat <- file.info(path, extra_cols = FALSE)
      if (! is.data.frame(fileStat)) {
         check <- "Can't stat file."
      }
      if (check == '' ) {
         gotSize <- fileStat[['size']]
         if (is.null(gotSize)) {
            check <-"Read file size as null."
         }
         if (check == '' ) {
            check <- checkIsSingle(gotSize, mode='numeric')
         }
         if (check == '' && is.nan(gotSize)) {
            check <- 'Read file size as NaN.'
         }
         if (check == '' && is.na(gotSize)) {
            check <- 'Read file size as NA.'
         }
         if (check == '' && gotSize < 0) {
            check <- 'Read file size as negative.'
         }
         if (check == '' && gotSize != size) {
            check <- paste0("File size mismatch. Found ", gotSize, " wanted ", size, ".")
         }
      }
      check
   }, error= function (e) {
      return( paste0( "Unexpected error: ", conditionMessage( e )))
   })
}

.checkChecksumMatches <- function (path, checksum, checksumFunc) {
   tryCatch ({
      check <- ''
      gotChecksum <- do.call( checksumFunc, list( path ))
      if (is.null(gotChecksum)) {
         check <- "Calculated checksum was null."
      }
      if (check == '' && is.na(gotChecksum)) {
         check <- 'Calculated checksum was NA.'
      }
      if (check == '' && gotChecksum != checksum) {
         check <- paste0("Checksum mismatch. Found ", gotChecksum, " wanted ", checksum, ".")
      }
      check
   }, error= function (e) {
      return( paste0( "Unexpected error: ", conditionMessage( e )))
   # nocov start - no idea how to trigger this in a test.
   }, interrupt= function (e) {
      return( paste0( "Keyboard interrupt: ", conditionMessage( e )))
   })
   # nocov end

}
