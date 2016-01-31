#' Check functions for vector values
#'
#' A check function test something about its first (required) parameter and
#' returns either an empty string if the check succeeds or a non-empty string if
#' the check fails. Even when a test fails due to an error, the error is caught
#' and a message will be returned. The check functions described here test
#' various properties of vectors.
#'
#' Many of the these functions are simple wrappers around existing R functions.
#' Note, calling any of these functions with an unused parameters will cause an
#' actual error as that is thrown before the called function begins executing
#' and hence can not be trapped and converted to a message by the called
#' function.
#'
#' @name checkVector
NULL

#' @param vector A vector value to check.
#'
#' @return All functions return either an empty string if the check succeeds or
#'   a non-empty string if the check fails (including if checking causes an
#'   error.)
#'
#' @describeIn checkVector Use to test that a \code{vector} is an atomic vector
#'   possibly also testing it's specific type (\code{\link{mode}}). The default
#'   \code{mode}, \code{'any'}, will match any vector type, but will not match a
#'   list (unlike \code{\link{is.vector}}). If \code{vector} has any attributes
#'   other than names, it is not a vector. Returns one of:
#'
#'  \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{vector} was an atomic vector, and if a mode
#'       other than \code{any} was specified, it was that type of vector.}
#'    \item{\emph{"Not a vector of mode \code{mode}."}\verb{ }
#'       Check failed as \code{vector} was either not an atomic vector (e.g. was
#'       a list or had some attributes), or it was a vector of a different
#'       \code{mode} (numeric when integer wanted, etc.) }
#'    \item{\emph{"Checking for a vector failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @param mode The mode to test, one of \code{"logical"}, \code{"integer"},
#'   \code{"numeric"}, \code{"double"}, \code{"complex"}, \code{"character"},
#'   \code{"raw"}, \code{"expression"}, or (the default) \code{"any"}. Other
#'   strings are allowed, but will always result in a failure message, including
#'   \code{"list"}.
#'
#' @export
#'
#' @examples
#' # Testing for an atomic vector
#' checkIsVector(NULL)
#' #=> [1] "Not a vector of mode any."
#' checkIsVector(c(1,2))
#' #=> [1] ''
#' checkIsVector(c(1,2), mode='numeric')
#' #=> [1] ''
#' checkIsVector(c(1,2), mode='integer')
#' #=> [1] "Not a vector of mode integer."
#' checkIsVector(c(1,2), mode='xyzzy')
#' #=> [1] "Not a vector of mode xyzzy"
#' checkIsVector(c(1,2), mode=42)
#' #=> [1] "Checking for a vector failed with the following error:
#' #=>  invalid 'mode' argument"
#' checkIsVector( list(A=1) )
#' #=> [1] "Not a vector of mode any."
#' checkIsVector( list(A=1), mode='list' )
#' #=> [1] "Not a vector of mode list."
#'
#' # Compared to is.vector
#' is.vector( list(A=1) )
#' #=> [1] TRUE
#' is.vector( list(A=1), mode='list' )
#' #=> [1] TRUE
#'
checkIsVector <- function (vector, mode= 'any') {
   tryCatch({
      if (is.vector(vector, mode= mode) && ! is.list(vector)) {
         ''
      }
      else {
         paste0( 'Not a vector of mode ', mode, '.')
      }
   }, error= function (e) {
      return( paste0(
         "Checking for a vector failed with the following error: ",
         conditionMessage(e)
      ))
   })
}

#' @describeIn checkVector Use as a convenient method to check if a vector is a
#'   single valued vector. Combines calls to \code{\link{checkIsVector}} and
#'   \code{\link{checkLength}} with min and max lengths of 1. Returns one of:
#'
#'  \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{vector} was an atomic vector of length one,
#'       and if a mode other than \code{any} was specified, it was that type.}
#'    \item{\emph{"Not a vector of mode \code{mode}."}\verb{ }
#'       Check failed as \code{vector} was either not an atomic vector (e.g. was
#'       a list or had some attributes), or it was a vector of a different
#'       \code{mode} (numeric when integer wanted, etc.) }
#'    \item{\emph{"Length is not 1."}\verb{ }
#'       Check failed as \code{vector} was a vector (of the correct mode), but
#'       was either empty (had length 0), or had more than one element.}
#'    \item{\emph{"Checking for a single valued vector failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @examples
#' # Testing for an atomic vector of length 1.
#' checkIsSingle( 1 )
#' #=> [1] ""
#' checkIsSingle( 1, "string" )
#' #=> [1] "Not a vector of mode string"
#' checkIsSingle( c('One string to rule them all.'), "string" )
#' #=> [1] ""
#' checkIsSingle( c(1, 2) )
#' #=> [1] "Length is not 1."
#' checkIsSingle( character(0) )
#' #=> [1] "Length is not 1."
#' checkIsSingle( character(0), "integer" )
#' #=> [1] "Not a vector of mode integer."
#' checkIsSingle( list(A=1) )
#' #=> [1] "Not a vector of mode any"
#' checkIsSingle( NULL )
#' #=> [1] "Not a vector of mode any"
#'
#' @export
checkIsSingle <- function (vector, mode= 'any') {
   tryCatch({
      check <- checkIsVector(vector, mode= mode)
      if (check != '' ) {
         check
      }
      else {
         checkLength(vector, minimumLength= 1, maximumLength= 1)
      }
   }, error= function (e) {
      return( paste0(
         "Checking for a single valued vector failed with the following error: ",
         conditionMessage(e)
      ))
   })
}

#' @describeIn checkVector Use to test the content of an atomic vector against
#' a list of allowed values. If any element of the vector is not in the
#' checklist, this fails. Returns one of:
#'
#'  \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{vector} was an atomic vector of the same
#'       \code{mode} as the \code{checklist} and it contained only elements that
#'       were also in the \code{checklist}.}
#'    \item{\emph{"Some element is not in the checklist."}\verb{ }
#'       Check failed as \code{vector} contained one or more elements that are
#'       not in the \code{checklist}. }
#'    \item{\emph{"Bad parameter 'checklist'. ..."}\verb{ }
#'       Check failed as \code{checklist} was not an atomic vector as tested by
#'       \code{\link{checkIsVector}}. }
#'    \item{\emph{"Bad parameter 'vector'. ..."}\verb{ }
#'       Check failed as \code{vector} was either not an atomic vector or it was
#'       a vector of a different \code{mode} than \code{checklist}, as tested
#'       by \code{checkIsVector(vector, mode(checklist))}.}
#'    \item{\emph{"Checking for a vector failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @param checklist A vector to compare the contents of \code{vector} to.
#'   \code{vector} must be a vector with the same mode as this \code{checklist}.
#'
#' @export
#'
#' @examples
#' # Checking a vector's contents for unregistered values
#' checkIsIn(1, checklist= 1)
#' #=> [1] ''
#' checkIsIn(c(1,2), c(1,2,3))
#' #=> [1] ''
#' checkIsIn( c( "white", "gray" ), c( "black", "white" ))
#' #=> 'Some element is not in the checklist.'
#' checkIsIn( character(0), character(0))
#' #=> [1] ''
#' checkIsIn( c(1,2) )
#' #=> [1] "Bad parameter 'checklist'. Not a vector of mode any."
#' checkIsIn( NULL, character(0) )
#' #=> [1] "Bad parameter 'vector'. Not a vector of mode character."
#' checkIsIn( c("x", NULL), c("x") )
#' #=> ''
#' checkIsIn( NA, c("a", "b", NA) )
#' #=> [1] "Bad parameter 'vector'. Not a vector of mode character."
#'
checkIsIn <- function (vector, checklist=NULL) {
   tryCatch({
      check <- checkIsVector(checklist)
      if (check != '' ) {
         paste0("Bad parameter 'checklist'. ", check)
      }
      else {
         check <- checkIsVector(vector, mode= mode(checklist))
         if (check != '' ) {
            paste0("Bad parameter 'vector'. ", check)
         }
         else if ( all( vector %in% checklist )) {
            ''
         }
         else {
            'Some element is not in the checklist.'
         }
      }
   }, error= function (e) {
      return( paste0(
         "Checking against checklist failed with the following error: ",
         conditionMessage(e)
      ))
   })
}

#' @describeIn checkVector Use to test the content of an atomic vector against a
#'   list of poison values. If any element of the vector is also found in the
#'   checklist, this fails. Returns one of:
#'
#'  \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{vector} was an atomic vector of the same
#'       \code{mode} as the \code{checklist} and it contained no elements that
#'       were also in the \code{checklist}.}
#'    \item{\emph{"Some element is not in the checklist."}\verb{ }
#'       Check failed as \code{vector} contained one or more elements that were
#'       in the \code{checklist}. }
#'    \item{\emph{"Bad parameter 'checklist'. ..."}\verb{ }
#'       Check failed as \code{checklist} was not an atomic vector as tested by
#'       \code{\link{checkIsVector}}. }
#'    \item{\emph{"Bad parameter 'vector'. ..."}\verb{ }
#'       Check failed as \code{vector} was either not an atomic vector or it was
#'       a vector of a different \code{mode} than \code{checklist}, as tested
#'       by \code{checkIsVector(vector, mode(checklist))}.}
#'    \item{\emph{"Checking for a vector failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @export
#'
#' @examples
#' # Checking a vector's contents for banned values
#' checkIsNotIn(1, checklist= 2)
#' #=> [1] ''
#' checkIsNotIn(c(1,2), c(3,4,5))
#' #=> [1] ''
#' checkIsNotIn( 1, c( 1,2,3 ))
#' #=> 'Some element is in the checklist.'
#' checkIsNotIn( c( "white", "gray" ), c( "black", "white" ))
#' #=> 'Some element is in the checklist.'
#' checkIsNotIn( '', character(0) )
#' #=> [1] ''
#' checkIsNotIn( character(0), '' )
#' #=> [1] ''
#' checkIsNotIn( c(1,2) )
#' #=> [1] "Bad parameter 'checklist'. Not a vector of mode any."
#' checkIsNotIn( NULL, character(0) )
#' #=> [1] "Bad parameter 'vector'. Not a vector of mode character."
#' checkIsNotIn( c("x", NULL), c("y", NULL) )
#' #=> ''
#' checkIsNotIn( NA, c("a", "b", NA) )
#' #=> [1] "Bad parameter 'vector'. Not a vector of mode character."
#'
checkIsNotIn <- function (vector, checklist=NULL) {
   tryCatch({
      check <- checkIsVector(checklist)
      if (check != '' ) {
         paste0("Bad parameter 'checklist'. ", check)
      }
      else {
         check <- checkIsVector(vector, mode= mode(checklist))
         if (check != '' ) {
            paste0("Bad parameter 'vector'. ", check)
         }
         else if ( any( vector %in% checklist )) {
            'Some element is in the checklist.'
         }
         else {
            ''
         }
      }
   }, error= function (e) {
      return( paste0(
         "Checking against checklist failed with the following error: ",
         conditionMessage(e)
      ))
   })
}
