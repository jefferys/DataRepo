#' Check functions for values
#'
#' A check function test something about its first (required) parameter and
#' returns either an empty string if the check succeeds or a non-empty string if
#' the check fails. Even when a test fails due to an error, the error is caught
#' and a message will be returned. The check functions described here test
#' various properties of values.
#'
#' Many of the these functions are simple wrappers around existing R functions.
#' Note, calling any of these functions with an unused parameters will cause an
#' actual error as that is thrown before the called function begins executing
#' and hence can not be trapped and converted to a message by the called
#' function.
#'
#' @name checkValue
#'
#' @return All functions return either an empty string if the check succeeds or
#'   a non-empty string if the check fails (including if checking causes an
#'   error.)
NULL

#' @describeIn checkValue Use to test if a value is \code{NULL}. Returns one
#'   of:
#'
#'  \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{value} was \code{NULL}.}
#'    \item{\emph{"Is not null."}\verb{ }
#'       Check failed as \code{value} was something other than \code{NULL}.}
#'    \item{\emph{"Checking for null failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @param value A value to check.
#'
#' @examples
#' # Ensuring a value is NULL
#' checkIsNull( NULL )
#' #=> [1] ""
#' checkIsNull( NA )
#' #=> [1] "Is not null."
#' checkIsNull()
#' #=> [1] "Checking for null failed with the following error:
#' #=> argument \"value\" is missing, with no default"
#'
#' @export
checkIsNull <- function (value) {
   tryCatch(
      {
         if (is.null(value)) {
            check <- ''
         }
         else {
            check <- 'Is not null.'
         }
      },
      error= function (e) {
         check <<- paste0(
            "Checking for null failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return(check)
}

#' @describeIn checkValue Use to test that a value is not \code{NULL}. Returns
#'   one of:
#'
#'  \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{value} was something other than \code{NULL}.}
#'    \item{\emph{"Is null."}\verb{ }
#'       Check failed as \code{value} was \code{NULL}.}
#'    \item{\emph{"Checking for null failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @examples
#' # Testing a value for existence
#' checkIsNotNull( NA )
#' #=> [1] ""
#' checkIsNotNull( NULL )
#' #=> [1] "Is null."
#'
#' checkIsNotNull()
#' #=> [1] "Checking for null failed with the following error:
#' #=> argument \"value\" is missing, with no default"
#'
#' @export
checkIsNotNull <- function (value) {
   tryCatch(
      {
         if (is.null(value)) {
            check <- 'Is null.'
         }
         else {
            check <- ''
         }
      },
      error= function (e) {
         check <<- paste0(
            "Checking for null failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return(check)
}

#' @describeIn checkValue Use to test that a value has a length between
#'   \code{minimumLength} and \code{maximumLength}, inclusive. By default checks
#'   that length is between \code{0} and \code{Inf}, so you need to supply at
#'   least one of these two extra parameters to do anything useful. Returns one
#'   of:
#'
#'  \itemize{
#'    \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{value} had a length >= min and had a length <=
#'       max.}
#'    \item{\emph{"Length is not between \code{min} and \code{max}."}\verb{ }
#'       Check failed as \code{value} had a length > max, or had a length < min
#'       (or both if min and max were chosen poorly!).}
#'    \item{\emph{"Length is not \code{length}."}\verb{ }
#'       Check failed as \code{value} did not have the specified length. This
#'       message is only given when max == min.}
#'    \item{\emph{"Bad parameter 'value'. ..."}\verb{ }
#'       Check failed due to an invalid \code{value} parameter. This probably
#'       means that the provided \code{value} was \code{NULL}.
#'       \code{checkLength(NULL)} results in a failure string, contrary to R's
#'       very tolerant evaluation of \code{length(NULL)} as \code{0}.}
#'    \item{\emph{"Checking for length failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error.}
#'  }
#'
#' @param minimumLength The minimum length \code{value} must have. Shorter than
#'   this and the check will fail. The default is 0. Often abbreviated 'min'.
#'
#' @param maximumLength The maximum length \code{value} must have. Longer than this
#'   and the check will fail. The default is \code{Inf}. Often abbreviated 'max'.
#'
#' @examples
#' # Testing the length of a value.
#' checkLength( character(0) )
#' #=> [1] ""
#' checkLength( c(1, 2), minimumLength= 2 )
#' #=> [1] ""
#' checkLength( c(1, 2), minimumLength= 3 )
#' #=> [1] "Length is not between 3 and Inf."
#' checkLength( c(1, 2), max= 1 )
#' #=> [1] "Length is not between 0 and 1."
#' checkLength( c(1, 2), min= 2, max= 2 )
#' #=> [1] ""
#' checkLength( c(1, 2, 3), min= 2, max= 2 )
#' #=> [1] "Length is not 2."
#' checkLength( NULL )
#' #=> [1] "Bad parameter 'value'. Is null."
#'
#' @export
checkLength <- function (value, minimumLength= 0, maximumLength= Inf) {
   tryCatch({
      check <- checkIsNotNull(value)
      if (check != '' ) {
         return(paste0("Bad parameter 'value'. ", check))
      }
      len <- length(value)
      if (len <= maximumLength && len >= minimumLength) {
         check <- ''
      }
      else if (minimumLength == maximumLength) {
         check <- paste0( "Length is not ", minimumLength, ".")
      }
      else {
         check <- paste0( "Length is not between ",
                          minimumLength, " and ", maximumLength, "." )
      }
   },
   error= function (e) {
      check <<- paste0(
         "Checking for length failed with the following error: ",
         conditionMessage(e)
      )
   }
   )
   return(check)
}

#' @describeIn checkValue Use to check if an expression (\code{value})
#'   evaluates to \code{TRUE}. Returns one of:
#'
#'   \itemize{
#'     \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{value} evaluated to logical \code{TRUE}.}
#'    \item{\emph{"Is false"}\verb{ }
#'       Check failed as \code{value}  evaluated to logical \code{FALSE}.}
#'    \item{\emph{"Is NA."}\verb{ }
#'       Check failed as \code{value}  evaluated to logical \code{NA}.}
#'    \item{\emph{"Result is not a vector of mode logical."}\verb{ }
#'       Check failed as \code{value} evaluated to something other than a
#'       logical vector.}
#'    \item{\emph{"Resulting logical vector is not of length 1."}\verb{ }
#'       Check failed as \code{value} evaluated to logical vector, but either
#'       it was empty (length 0) or it had more than one element.}
#'    \item{\emph{"Checking expression for truth failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error. This is probably a problem with
#'       evaluating the provided expression.}
#'   }
#'
#' @export
#'
#' @examples
#' # Checking an expression for truth
#' checkIsTrue( TRUE )
#' #=> [1] ""
#' checkIsTrue( 1 + 1 == 2 )
#' #=> [1] ""
#' checkIsTrue({
#'    x <- 1
#'    y <- 1
#'    x == y
#' })
#' checkIsTrue( FALSE )
#' #=> [1] "Is false."
#' checkIsTrue( FALSE == NA )
#' #=> [1] "Is NA."
#' checkIsTrue({ x <- c(T,T,F); all(x) })
#' #=> [1] "Is false."
#' checkIsTrue( c(T,T,T) )
#' #=> [1] "Resulting logical vector is not of length 1."
#' checkIsTrue( "true" )
#' #=> [1] "Result is not a vector of mode logical."
#' checkIsTrue( NULL )
#' #=> [1] "Result is not a vector of mode logical."
#' checkIsTrue( stop('Oops.') )
#' #=> [1] ""Checking expression for truth failed with the following error: Oops."
#'
checkIsTrue <- function (value) {
   tryCatch(
      {
         result <- eval(value)
         if (! is.vector(result, mode= 'logical')) {
            check <- "Result is not a vector of mode logical."
         }
         else if (! length(result) == 1) {
            check <- "Resulting logical vector is not of length 1."
         }
         else if ( is.na(result) ) {
            check <- 'Is NA.'
         }
         else if (result) {
            check <- ''
         }
         else {
            check <- 'Is false.'
         }
      },
      error= function (e) {
         check <<- paste0(
            "Checking expression for truth failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return(check)
}

#' @describeIn checkValue Use to check if an expression (\code{value})
#'   evaluates to \code{FALSE}. Returns one of:
#'
#'   \itemize{
#'     \item{\emph{"" (empty string)}\verb{ }
#'       Check succeeded as \code{value} evaluated to logical \code{FALSE}.}
#'    \item{\emph{"Is true."}\verb{ }
#'       Check failed as \code{value}  evaluated to logical \code{TRUE}.}
#'    \item{\emph{"Is NA."}\verb{ }
#'       Check failed as \code{value}  evaluated to logical \code{NA}.}
#'    \item{\emph{"Result is not a vector of mode logical."}\verb{ }
#'       Check failed as \code{value} evaluated to something other than a
#'       logical vector.}
#'    \item{\emph{"Resulting logical vector is not of length 1."}\verb{ }
#'       Check failed as \code{value} evaluated to logical vector, but either
#'       it was empty (length 0) or it had more than one element.}
#'    \item{\emph{"Checking expression for falsehood failed with the following error: ..."}\verb{ }
#'       Check failed unexpectedly with an error. This is probably a problem with
#'       evaluating the provided expression.}
#'   }
#'
#' @export
#'
#' @examples
#' # Checking an expression for falsehood
#' checkIsFalse( TRUE )
#' #=> [1] ""
#' checkIsFalse( 1 + 1 == 3 )
#' #=> [1] ""
#' checkIsFalse({
#'    x <- 1
#'    y <- 2
#'    x == y
#' })
#' checkIsFalse( TRUE )
#' #=> [1] "Is true"
#' checkIsFalse( TRUE == NA )
#' #=> [1] "Is NA."
#' checkIsFalse({ x <- c(T,T,F); any(x) })
#' #=> [1] "Is false."
#' checkIsFalse( c(F,F,F) )
#' #=> [1] "Resulting logical vector is not of length 1."
#' checkIsFalse( "false" )
#' #=> [1] "Result is not a vector of mode logical."
#' checkIsFalse( NULL )
#' #=> [1] "Result is not a vector of mode logical."
#' checkIsFalse( stop('Oops.') )
#' #=> [1] "Checking expression for falsehood failed with the following error: Oops."
#'
checkIsFalse <- function (value) {
   tryCatch(
      {
         result <- eval(value)
         if (! is.vector(result, mode= 'logical')) {
            check <- "Result is not a vector of mode logical."
         }
         else if (! length(result) == 1) {
            check <- "Resulting logical vector is not of length 1."
         }
         else if ( is.na(result) ) {
            check <- 'Is NA.'
         }
         else if (result) {
            check <- 'Is true.'
         }
         else {
            check <- ''
         }
      },
      error= function (e) {
         check <<- paste0(
            "Checking expression for falsehood failed with the following error: ",
            conditionMessage(e)
         )
      }
   )
   return(check)
}
