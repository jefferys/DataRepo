#' Link a bunch of files to a data repository
#'
#' This just wraps \code{\link{linkTo}} so that a data frame of arguments can
#' be provided; \code{linkTo} will be called iteratively with each one. The
#' vector of check results from each \code{linkTo} call is compressed with
#' \code{\link{checkSummary}} into a single string, merging the result from all
#' attempted \code{linkTo} operations into a character vector with one element
#' for each \code{file} in the provided data frame (named with the \code{file}
#' name) and a value string indicating success (empty) or describing failure.
#' If a failure occurs with any file, a warning is given specifying the
#' number of files with problems.
#'
#' @param df A data frame of arguments to use in a call to \code{linkTo}, one
#'   column per argument. Column names are used to match arguments, not
#'   positions. Extra columns are ignored. Must have a column named \code{file},
#'   and that column should not contain duplicate files. Any \code{linkTo}
#'   arguments for which no columns are provided will use the normal
#'   \code{linkTo} defaults.
#'
#' @return A character vector of problems, one element (named) for each
#'   \code{file} in the input data frame. The value of the element is an empty
#'   string if the file was linked to without error, a missing value if the
#'   status of the link is unknown, and a string describing the error if
#'   anything went wrong.
#'
#' @export
linkAllTo <- function (df) {
   if (! is.data.frame(df)) {
      stop( "checkParam_df= Must be a data frame." )
   }
   if ( is.null(df$file))   {
      stop( "checkParam_df= Must have a 'file' column." )
   }
   if ( anyDuplicated(df$file) ) {
      stop( "checkParam_df= Must not have duplicated 'file' values" )
   }

   linkAllToResult <- rep( NA_character_, nrow(df))
   names(linkAllToResult) <- df$file

   df <- df[, names(df) %in% names(formals(linkTo)), drop= FALSE]

   for (i in 1:nrow(df)) {
      # Once start processing, don't want errors to exit but instead want them
      # to be reported. No return value to track as this updates the internals
      # of addAllFilesResult, including a <<- updated from the error function.
      tryCatch(
         {
            rowOfArgs <- df[i, , drop= FALSE]
            file <- rowOfArgs$file
            onExists <- rowOfArgs$onExists # onExists column may not exist
            naOk <- NULL
            if (! is.null(onExists) && onExists == 'skip') {
               naOk <- c('createLink')
            }
            linkAllToResult[file] <-
               checkSummary( do.call( linkTo, args= rowOfArgs ), naOk = naOk)
         },

         error= function(e) {
            linkAllToResult[file] <<-
               paste0( "Unexpected error: ", conditionMessage( e ))
         }
      )
   }

   if (any(linkAllToResult != '', na.rm = TRUE)) {
      isOk = (is.na(linkAllToResult) | linkAllToResult == '')
      count <- sum( ! isOk )
      if (count == 1) {
         warning( paste0( "Failed linking ", count, " file."))
      }
      else {
         warning( paste0( "Failed linking ", count, " files."))
      }
   }

   return(linkAllToResult)
}
