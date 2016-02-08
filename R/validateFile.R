#' Validate files
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
#' @param fileSize The expected size of the file, in bytes. By default this is
#'   \code{NULL}, meaning file size will not be checked. If a positive value is
#'   supplied it will be checked against the size of the source file on the file
#'   system. If it does not match then failure is reported (and \code{checksum}
#'   is not checked).
#'
#' @param checksum The expected checksum of the file.By default this is
#'   \code{NULL}, meaning no checksum is generated. If given it will be checked
#'   agains the value provided by the \code{checksumFunc}.
#'
#'
#' @param checksumFunc The function object (not the string name) to use when
#'   calculating the checksum of the file. By default this is
#'   \code{tools::md5sum}. The specified function will be called with one
#'   parameter, \code{path}. The value returned is then tested against the
#'   provided \code{checksum}. This returned value should be an (atomic) vector
#'   type but can not be \code{NULL} or a missing value.  When
#'   \code{checksumFunc} is called, \code{path} has already been verified and is
#'   known to exist on the file system as a real file (not a directory or link).
#'   The function object passed may not be \code{NULL} when \code{checksum} is
#'   provided, but is not used \code{checksum} is \code{NULL}.
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
#' check <- validateFile( emptyFile, fileSize= 0, checksum= md5EmptyFile )
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
#' check <- validateFile( emptyFile, fileSize= NULL, checksum= NULL )
#' names(check) <- NULL
#' #=> [1] "" "" "" "" "" "" "" NA NA
#'
#' # Bad checksum
#' check <- validateFile( emptyFile, fileSize= 0, checksum= 'abc123' )
#' names(check) <- NULL
#' #=> [1] "" "" "" "" "" "" "" ""
#' #=> [9] "Checksum mismatch. Found d41d8cd98f00b204e9800998ecf8427e wanted abc123.
#'
#' # Bad file size. No need to check the checksum if file size doesn't match
#' check <- validateFile( emptyFile, fileSize= 1, checksum= 'BAD' )
#' names(check) <- NULL
#' #=> [1] "" "" "" "" "" "" ""
#' #=> [8] "File size mismatch. Found 0 wanted 1."
#' #=> [9] NA
#'
#' # Bad file path, no need to check anything else.
#' check <- validateFile( noSuchFile, fileSize= 0, checksum= md5EmptyFile )
#' names(check) <- NULL
#' #=> [1] ""  ""  ""  ""  ""  "No such path."
#' #=> [7] NA  NA  NA
#'
#' # Using your own checksum function object
#' SumIt <- function (path) { sum( as.numeric(
#'     readBin( path, what = 'raw', n= file.info(path)[['size']] + 1 )
#' ))}
#' check <- validateFile( binFile, fileSize= 3, checksum= 6, checksumFunc= SumIt )
#' names(check) <- NULL
#' check
#' #=> [1] "" "" "" "" "" "" "" "" ""
#'
#' # Cleanup
#' file.remove( emptyFile )
#' file.remove( binFile )
validateFile <- function ( path, checksum= NULL, checksumFunc= tools::md5sum,
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

#' Summarize a vector of check results
#'
#' One check test results in an empty string, an NA, or error message. Multiple
#' test checks can be merged into a vector with each element named for a test
#' and giving its outcome. This can then be summarized into a single check
#' result by merging error strings if any, reporting an empty string if
#' everything passed, and handling NA's as either errors or ignoring them.
#'
#' @param result A vector of check results, with element values being an empty
#'   string (success), a failure string, or \code{NA_character_}. Elements are
#'   named for the test they represent.
#'
#' @param naOk A vector of the names of elements for which \code{NA} is ok. If
#'   \code{NA} is ok for every element, then set this to \code{names(result)}.
#'   By default, this is \code{NULL} and missing values are converted to
#'   failures for every test.
#'
#' @param skip A vector of the names of elements to be left out of the
#'   summary, regardless of their value. By default all elements are considered
#'   and used.
#'
#' @return As other check functions, this returns a single character string
#'   which is empty if the check succeeds and contains a failure message if the
#'   the check fails. However, here success means all tests not skipped succeed.
#'   Missing values are by default considered failures. A failure message is
#'   reported if any element failed.
#'
#'   The failure message is formated like "<testName> = <error message>; ..."
#'   where each element with a failing message, from first to last, are reported
#'   and joined by '; '. If an element has a missing value, it will first be
#'   converted to the failure string \code{"NA not ok."} before summarizing,
#'   unless its test name is provided in a string vector as \code{naOk} (or as
#'   \code{skip}, except then its value will be ignored even if it failed.)
#'
#' @examples
#' fauxChecks <- c('', '', '')
#' names(fauxChecks) <- c('checkA', 'checkB', 'checkC')
#' checkSummary(fauxChecks)
#' #=> [1] ""
#' fauxChecks['checkC'] <- NA_character_
#' checkSummary(fauxChecks)
#' #=> [1] "checkC = NA not ok."
#' checkSummary(fauxChecks, naOk= c('checkC'))
#' #=> [1] ""
#' fauxChecks['checkB'] <- 'I am broken.'
#' checkSummary(fauxChecks)
#' #=> [1] "checkB = I am broken.; checkC = NA not ok."
#' checkSummary(fauxChecks, naOk= c('checkB', 'checkC'))
#' #=> [1] "checkB = I am broken."
#' checkSummary(fauxChecks, skip= c('checkB', 'checkC'))
#' #=> [1] ""
#' @export
checkSummary <- function ( result, naOk= NULL, skip= NULL ) {

   # Parameter check functions
   .checkParam_result <- function( result ) {
      check <- checkIsVector(result, mode= 'character')
      if (check != '' ) return(check)
      check <- checkLength(result, minimumLength = 1)
      if (check != '' ) return(check)
      testNames <- names(result)
      if (is.null(testNames)) {
         return('No names attribute.')
      }
      return( '' )
   }
   .checkParam_naOk <- function( naOk ) {
      if (is.null(naOk)) {
         return('')
      } else {
         check <- checkIsVector(naOk, mode= 'character')
         if (check != '' ) return(check)
         check <- checkLength(naOk, minimumLength = 1)
         return(check)
      }
   }
   .checkParam_skip <- function( skip ) {
      if (is.null(skip)) {
         return('')
      } else {
         check <- checkIsVector(skip, mode= 'character')
         if (check != '' ) return(check)
         check <- checkLength(skip, minimumLength = 1)
         return(check)
      }
   }
   .checkParam_result_naOk <- function( result, naOk ) {
      if (is.null(naOk)) {
         return('')
      } else {
         return(checkIsIn(naOk, names(result)))
      }
   }
   .checkParam_result_skip <- function( result, skip ) {
      if (is.null(skip)) {
         return('')
      } else {
         return(checkIsIn(skip, names(result)))
      }
   }

   ok <- .checkParam_result( result )
   if (ok != '') {
      return( paste0( "checkParam_result = ", ok ))
   }
   ok <- .checkParam_naOk( naOk )
   if (ok != '') {
      return( paste0( "checkParam_naOk = ", ok ))
   }
   ok  <- .checkParam_skip( skip )
   if (ok != '') {
      return( paste0( "checkParam_skip = ", ok ))
   }
   ok <- .checkParam_result_naOk( result, naOk )
   if (ok != '') {
      return( paste0( "checkParam_result_naOk = ", ok ))
   }
   ok <- .checkParam_result_skip( result, skip )
   if (ok != '') {
      return( paste0( "checkParam_result_skip = ", ok ))
   }

   if (! is.null(naOk)) {
      result[intersect( naOk, names( result[is.na( result )] ))] <- ''
   }
   result[is.na(result)] <- 'NA not ok.'

   if (! is.null(skip)) {
      result[skip] <- ""
   }

   check <- ''
   if ( any( result != '' )) {
      badTests <- result[which( result != '' )]
      check <- paste( names( badTests ), badTests, sep= ' = ', collapse= '; ')
   }
   return(check)
}
