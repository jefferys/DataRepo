#============================================================================
#' Load a tsv file (repository actions).
#'
#' Essentially this just reads any tab-separated value (tsv) file with headers
#' into a data frame. The file must be formatted correctly to read in, but no
#' other content is checked - that is the role of \code{validateActions()}.
#'
#' @param file The file to load.
#'
#'   Must be a tab-separated file with headers.
#'   Headers may be transformed by replacing spaces, etc. with \code{.} to
#'   allow them to be R variables. Blank lines and lines beginning with \code{#}
#'   are ignored. Any string containing a literal tab must be escaped inside
#'   "double quotes". Use \code{NA} for missing values.
#'
#' @return Returns a data frame corresponding to the file read.
#'
#' @section Errors: The following errors can occur:
#' \tabular{ll}{
#'   \emph{\code{No such file to load: "fileName"}} \tab
#'       The given file name can not be found. Try an absolute path or
#'       check permissions.
#'   \cr
#'   \emph{\code{Reading file "fileName" failed with the following error:}} \tab
#'       There was an error reported by \code{\link{read.delim}} while reading
#'       and parsing the specified tsv file.
#'    \cr
#' }
#'
#' @examples
#' # Create a temporary file to read in.
#' file.tsv <- tempfile()
#' file.create(file.tsv)
#' content <- c("col_a\tcol_b\tcol_c", "one\t1\tTRUE",
#'              "two\t2\tFALSE", "# COMMENT")
#' writeLines( content, file.tsv )
#'
#' # Read in a tsv file
#' if (file.exists(file.tsv)) {
#'    df <- loadActions( file.tsv )
#' }
#' df
#'
#' # Cleanup.
#' file.remove(file.tsv)
#'
#' @export
#============================================================================
loadActions <- function( file ) {
   if (! file.exists( file )) {
      message <- paste0( 'No such file to load: "', file, '"' )
      stop(message)
   }
   tryCatch(
      df <- read.delim(file, comment.char= '#', stringsAsFactors= FALSE ),
      error= function (e) {
         message <- paste0(
            'Reading file "', file, '" failed with the following error:\n\t',
            conditionMessage(e)
         )
         stop(message, call.= FALSE)
      }
   )
   return(df)
}
