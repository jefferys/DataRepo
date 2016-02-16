#===
context("Set up for testing addAllFiles.")
#===

# The repo dir
repoDir <- tempfile( 'repo' )
dir.create(  repoDir  )

# Group info
defaultGroup <- file.info(repoDir, extra_cols = TRUE)$grname
groupList <- groups()
groupList <- groupList[groupList != defaultGroup]
otherGroup <- groupList[1]

# A source file to copy
sourceFile <- tempfile( 'sourceFile' )
writeLines(c("A", "three line", "file of text."), sourceFile)
sourceFileSize <- 27
sourceFileMd5 <- "2c77982c7ee2279e119c585859d92a7b"

# Bad file to mock bad copy with
truncFile <- tempfile( 'truncFile' )
writeLines(c("A", "three line", "file."), truncFile)
truncFileSize <- 19
truncFileMd5 <- "dabedc6723adeb9e9fb269c2a82d2ba7"

# Bad file with same size to mock bad copy with.
mutFile <- tempfile( 'mutFile' )
writeLines(c("A", "three 1ine", "file of text."), mutFile)
mutFileSize <- 27
mutFileMd5 <- "0356f6d108bbf0f3e4a8331ec3829f5f"

allParamDF <- data.frame(
   file= c(sourceFile, truncFile, mutFile),
   repo= rep(repoDir, 3),
   entry= c( 'deleteMe', file.path('deleteMe', 'entry'), file.path('deleteMe', 'entry', 'path') ),
   asFile= c( 'source.txt', 'trunc.txt', 'mut.txt'),
   writeGroup= rep(otherGroup, 3),
   fileSize= c(sourceFileSize, truncFileSize, mutFileSize),
   checksum= c(sourceFileMd5, truncFileMd5, mutFileMd5),
   checksumFunc= c('tools::md5sum', 'tools::md5sum', 'tools::md5sum'),
   stringsAsFactors= FALSE
)

# Expected post-copy fs entities
targets <- file.path( allParamDF$repo, allParamDF$entry, allParamDF$asFile )
entryDirs <- file.path( allParamDF$repo, allParamDF$entry )

# Remove copied objects
tearDown <- function() {
   toDelete <- file.path( repoDir, 'deleteMe' )
   unlink( toDelete, recursive= TRUE )
}

describe( "Set up.", {
   describe( "The data frames of test data are reasonable.", {
      it ( "Has the expected all-parameter data frame", {
         expect_equal( nrow( allParamDF ), 3 )
         expect_equal( ncol( allParamDF ), 8 )
         expect_equal( names( allParamDF ), names(formals(addFile)))
      })
   })
   describe( "The group test data are reasonable.", {
      it ( "Has groups with the expected properties", {
         expect_true( otherGroup %in% groups() )
         expect_true( otherGroup != file.info(sourceFile, extra_cols= TRUE)$grname )
      })
   })
   describe( "The file-system: what objects should exist.", {
      it( "Has a repo dir.", {
         expect_true( all( dir.exists( allParamDF$repo )))
      })
      it( "Has files to copy.", {
         expect_true( all( file.exists( allParamDF$file )))
      })
   })
   describe( "The file-system: what objects should not exist.", {
      it( "Has no entry dirs yet", {
         expect_false( all( dir.exists( entryDirs )))
      })
      it( "Has no moved files yet")
      expect_false( all( file.exists( targets )))
   })
})

context( "addAllFiles()" )
describe( "Normal use cases create expected files and return all successes", {
   describe( "Input df has all addFile parameters.", {
      it( "has no other columns", {
         want <- c("", "", "")
         names(want) <- allParamDF$file
         got <- addAllFiles(allParamDF)
         expect_equal(got, want)
         expect_true( all( file.exists( targets )))
         tearDown()
      })
      it( "has extra columns", {
         want <- c("", "", "")
         names(want) <- allParamDF$file
         allParamDF <- data.frame(front= c(1,2,3), allParamDF, back= c('AAA', 'BBB', 'CCC'))
         got <- addAllFiles(allParamDF)
         expect_equal(got, want)
         expect_true( all( file.exists( targets )))
         tearDown()
      })
   })
   describe( "Input df has just 'file' parameter.", {
      it( "and nothing else.", {
         targets <- file.path( getwd(), basename(allParamDF$file) )
         expect_false( any( file.exists( targets )))

         want <- c("", "", "")
         names(want) <- allParamDF$file
         fileOnlyDF <- data.frame(
            file= allParamDF$file, stringsAsFactors= FALSE
         )

         got <- addAllFiles(fileOnlyDF)
         expect_equal(got, want)
         expect_true( all( file.exists( targets )))
         file.remove( targets )
      })
      it( "with extra columns", {
         targets <- file.path( getwd(), basename(allParamDF$file) )
         expect_false( any( file.exists( targets )))

         want <- c("", "", "")
         names(want) <- allParamDF$file
         fileOnlyDF <- data.frame(
            file= allParamDF$file, stringsAsFactors= FALSE
         )
         fileOnlyDF <- data.frame( A=c(1,2,3), B=c(TRUE,FALSE,TRUE), fileOnlyDF)
         got <- addAllFiles(fileOnlyDF)
         expect_equal(got, want)
         expect_true( all( file.exists( targets )))
         file.remove( targets )
      })
   })
   describe( "Input df has some parameters.", {
      it( "No checksum or fileSize content pre-check data", {
         someDF <- allParamDF[, ! names(allParamDF) %in% c("checksum", "fileSize")]
         expect_false( any( file.exists( targets )))

         want <- c("", "", "")
         names(want) <- allParamDF$file
         got <- addAllFiles(someDF)
         expect_equal(got, want)
         expect_true( all( file.exists( targets )))
         tearDown()
      })
      it( "No checksum or fileSize content pre-check data, but has extra column", {
         someDF <- allParamDF[, !names(allParamDF) %in% c("checksum", "fileSize")]
         someDF[,'extra'] <- c(1,2,3)
         expect_false( any( file.exists( targets )))

         want <- c("", "", "")
         names(want) <- allParamDF$file
         got <- addAllFiles(someDF)
         expect_equal(got, want)
         expect_true( all( file.exists( targets )))
         tearDown()
      })
   })
})
describe( "Handling problems.", {
   describe( "Before copy iteration, aborts if an expected error occurs.", {
      it( "Requires a data frame parameter", {
         wantErrorRE <- "checkParam_df= Must be a data frame."
         expect_error( got<- addAllFiles( 'Oops.' ), wantErrorRE)
      })
      it( "Requires a 'file' column in the data frame", {
         badDF <- data.frame(x=c(1,2,3), y=c(TRUE,FALSE,TRUE))
         wantErrorRE <- "checkParam_df= Must have a 'file' column."
         expect_error( got<- addAllFiles( badDF ), wantErrorRE)
      })
      it( "Requires unique 'file' column values", {
         badDF <- data.frame( file= c("Bob", "Ann", "Bob", "Mary"),
                              stringsAsFactors = FALSE)
         wantErrorRE <- "checkParam_df= Must not have duplicated 'file' values"
         expect_error( got<- addAllFiles( badDF ), wantErrorRE)
      })
   })
   describe( "During copy iteration, reports expected errors and continues.", {
      it( "if fails 1, completes others and reports errors, including a warning", {
         allParamDF[2, 'file'] <- 'noSuchFile.txt'
         wantWarningRE <- "Failed copying 1 file."
         expect_warning( got <- addAllFiles(allParamDF), wantWarningRE )
         errorMessage <- paste0(
            "checkValidSource = checkIsFile = No such path.; ",
            "checkIsNotLink = NA not ok.; ",
            "checkFileSizeMatches = NA not ok.; ",
            "checkChecksumMatches = NA not ok.; ",
            "makeTargetDir = NA not ok.; ",
            "doCopy = NA not ok.; ",
            "checkCopyTarget = NA not ok.; ",
            "unexpectedError = NA not ok."
         )
         want <- c("", errorMessage, "")
         names(want) <- allParamDF$file
         expect_equal(got, want)
         expect_true( file.exists( targets[1] ))
         expect_false( file.exists( targets[2] ))
         expect_true( file.exists( targets[3] ))
         tearDown()
      })
      it( "if fails all, reports errors, including a warning", {
         allParamDF[, 'file'] <- c( 'noSuchFile_1.txt', 'noSuchFile_2.txt', 'noSuchFile_3.txt' )
         wantWarningRE <- "Failed copying 3 files."
         expect_warning( got <- addAllFiles(allParamDF), wantWarningRE )
         errorMessage <- paste0(
            "checkValidSource = checkIsFile = No such path.; ",
            "checkIsNotLink = NA not ok.; ",
            "checkFileSizeMatches = NA not ok.; ",
            "checkChecksumMatches = NA not ok.; ",
            "makeTargetDir = NA not ok.; ",
            "doCopy = NA not ok.; ",
            "checkCopyTarget = NA not ok.; ",
            "unexpectedError = NA not ok."
         )
         want <- c(errorMessage, errorMessage, errorMessage)
         names(want) <- allParamDF$file
         expect_equal(got, want)
         expect_false( any(file.exists( targets )))
         tearDown()
      })
   })
   describe( "During copy iteration, reports unexpected errors and continues.", {
      it( "Reports unexpected errors.", {
         wantWarningRE <- "Failed copying 3 files."

         with_mock(
            `DataRepo::checkSummary`= function (...) {
               stop('Bad checkSummary.')
            },
            expect_warning( got <- addAllFiles(allParamDF), wantWarningRE )
         )
         errorMessage <- "Unexpected error: Bad checkSummary."
         want <- c(errorMessage, errorMessage, errorMessage)
         names(want) <- allParamDF$file
         expect_equal(got, want)
         expect_false( any(file.exists( targets )))
         tearDown()

      })
   })
})
