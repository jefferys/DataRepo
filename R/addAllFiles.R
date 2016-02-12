#' Add a bunch of files to a data repository
#'
#' This just wraps \code{\link{addFile}} so that a data frame of arguments can
#' be provided; \code{addFile} will be called iteratively with each one. The
#' vector of check results from each \code{addFile} call is compressed with
#' \code{\link{checkSummary}} into a single string, merging the result from all
#' attempted \code{addFile} operations into a character vector with one element
#' for each \code{file} in the provided data frame (named with the \code{file}
#' name) and a value string indicating success (empty) or describing failure.
#'
#' @param df A data frame of arguments to use in a call to \code{addFile}, one
#'   column per argument. Column names are used to match arguments, not
#'   positions. Extra columns ignored. Must have a column named \code{file}, and
#'   that column may not contain duplicate files. Any \code{addFile} arguments
#'   for which no columns are provided will use the normal \code{addFile}
#'   defaults.
#'
#' @return A character vector of problems, one element (named) for each
#'   \code{file} in the input data frame. The value of the element is an empty
#'   string if the file was copied and verified to be copied without error, a
#'   missing value if the status of the copy is unknown, and a string describing
#'   the error if anything else went wrong.
#'
#' @export
addAllFiles <- function (df) {
   if (! is.data.frame(df)) {
      stop( "checkParam_df= Must be a data frame." )
   }
   if ( is.null(df$file))   {
      stop( "checkParam_df= Must have a 'file' column." )
   }
   if ( anyDuplicated(df$file) ) {
      stop( "checkParam_df= Must not have duplicated 'file' values" )
   }

   addAllFilesResult <- rep( NA_character_, nrow(df))
   names(addAllFilesResult) <- df$file

   df <- df[, names(df) %in% names(formals(addFile)), drop= FALSE]

   for (i in 1:nrow(df)) {
      # Once start processing, don't want errors to exit but instead want them
      # to be reported. No return value to track as this updates the internals
      # of addAllFilesResult, including a <<- updated from the error function.
      tryCatch(
         {
            rowOfArgs <- df[i, , drop= FALSE]
            file <- rowOfArgs$file
            addAllFilesResult[file] <-
               checkSummary( do.call( addFile, args= rowOfArgs ))
         },

         error= function(e) {
            addAllFilesResult[file] <<-
               paste0( "Unexpected error: ", conditionMessage( e ))
         }
      )
   }

   if (any(addAllFilesResult != '', na.rm = TRUE)) {
      isOk = (is.na(addAllFilesResult) | addAllFilesResult == '')
      count <- sum( ! isOk )
      if (count == 1) {
         warning( paste0( "Failed copying ", count, " file."))
      }
      else {
         warning( paste0( "Failed copying ", count, " files."))
      }
   }

   return(addAllFilesResult)
}
