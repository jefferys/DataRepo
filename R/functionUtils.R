#' Get an exported object by name, possibly from unattached packages.
#'
#' A wrapper around \code{\link{get}} that allows using a qualified names like
#' \code{'SomePackage::myFunc'} to retrieve an object from a specific unattached
#' (but loaded) package. Qualified names are recognized, parsed and then
#' \code{\link{getFromNamespace}} is called instead of\code{get}.
#'
#' @param x An object name, as a string. May be qualified with a prefixed
#'   \code{package::} identifier, in which case the object must be from that
#'   package. Unexported objects will not be found; looking for \code{:::}
#'   qualified objects is an error.
#'
#' @param ... Extra parameters (\code{pos, envir, mode, inherits}) to pass on to
#'   \code{\link{get}}). These are ignored if \code{x} is a qualified name,
#'   Other parameters will trigger an error, but only if \code{x} is an
#'   unqualified name. Since \code{get} is called from within a wrapper function
#'   in this package, these parameters will behave differently than if you
#'   called \code{get} directly.
#'
#' @return The object named. It is an error if the object is not found.
#'
#' @examples
#' getSomewhere('ls')
#' #=> Returns base::ls, as base package is always loaded and attached.
#'
#' try( getSomewhere('getAnywhere'))
#' #=> ERROR - utils::getAnywhere is not attached (it is loaded)
#' getSomewhere('utils::getAnywhere')
#' #=>
#'
#' @export
#' @importFrom tools md5sum
getSomewhere <- function(x, ...) {
   if ( is.null(x) || is.na(x) ||
        ! is.character(x) || length(x) != 1 || nchar(x) < 1 ) {
      stop("Bad parameter 'x': Non-empty single string name required.")
   }
   if (grepl( ":::", x )) {
      stop("Bad parameter 'x': Unexported objects not supported.")
   }
   if (grepl( "::", x )) {
      parsed <- strsplit(x, '::', fixed=TRUE)[[1]]
      if (length(parsed) != 2 || nchar(parsed[1]) < 1 || nchar(parsed[2]) < 1) {
         stop("Bad parameter 'x': Incorrectly specified qualified name.")
      }
      return( utils::getFromNamespace(parsed[2], ns= parsed[1] ))
   }
   else {
      if (missing(...)) {
         return(get(x, envir=parent.frame()))
      }
      else {
         dots <- list(...)
         if (is.null(dots$pos) && is.null(dots$envir)) {
            dots$envir <- parent.frame()
         }
         return( do.call(get, c(list(x=x), dots)))
      }
   }
}
