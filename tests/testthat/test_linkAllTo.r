#===
# Set up for testing linkAllTo
#===

# The repo dir
repoDir <- tempfile( 'repo' )
dir.create(  repoDir  )

# A source file to copy
sourceFile <- tempfile( 'sourceFile' )
writeLines(c("A", "three line", "file of text."), sourceFile)
sourceFileSize <- 27
sourceFileMd5 <- "2c77982c7ee2279e119c585859d92a7b"

# A targetFile to link to (copy sourceFile in repo)
entryDir= "abc123"
newBaseName <- basename(tempfile( 'newBaseName' ))
addFile( sourceFile, repo= repoDir, entry=entryDir, asFile= newBaseName,
         fileSize= sourceFileSize, checksum= sourceFileMd5)
targetFile <- file.path(repoDir, entryDir, newBaseName)

# Create files needed for testing system collisions.
fsSetUp <- function() {
   someDir <- tempfile( 'someDir' )
   fs<-list(
      noSuchFile= tempfile( 'noSuchFile' ),
      goodLink=   tempfile( 'goodLink'   ),
      badLink=    tempfile( 'badLink'    ),
      goodFile=   tempfile( 'goodFile'   ),
      badFile=    tempfile( 'emptyFile'  ),

      someDir=    someDir,
      oneFile=    file.path( someDir, 'file1.txt' ),
      twoFile=    file.path( someDir, 'file2.txt' )
   )

   file.symlink( targetFile, fs$goodLink )
   file.symlink( sourceFile, fs$badLink  )
   file.copy(    sourceFile, fs$goodFile )
   file.create( fs$badFile  )
   dir.create(  fs$someDir )
   writeLines(c("One file."), fs$oneFile)
   writeLines(c("Two file."), fs$twoFile)

   return(fs)
}

check_linkAllTo <- function(df) {

   gotWarnings= character(0)
   withCallingHandlers({
      linkAllTo(df)
      }, warning= function(e) {
         gotWarnings <<- c(gotWarnings, conditionMessage(e))
         invokeRestart("muffleWarning")
   })
}

# Delete files set up for testing system collision.
fsTearDown <- function(fs) {
   file.remove( fs$goodLink )
   file.remove( fs$badLink )
   file.remove( fs$goodFile )
   file.remove( fs$badFile )

   # Unlink as remove gives warnings if file path has (deleted) directory.
   unlink( fs$oneFile )
   unlink( fs$twoFile )
   unlink(fs$someDir, recursive = TRUE )
}

#===
context("linkAllTo() testing fixtures.")
#===

describe( "fsSetUp() and fsTearDown()", {
   fs <- fsSetUp()
   describe( "fsSetUp()" , {
      fsCount <- 0
      it( "doesn't create missing file", {
         expect_false( file.exists( fs$noSuchFile ))
         fsCount <<- fsCount + 1
      })
      it( "creates needed links", {
         expect_true( file.exists( fs$goodLink ))
         expect_true( Sys.readlink( fs$goodLink ) == targetFile )
         fsCount <<- fsCount + 1
         expect_true( file.exists( fs$badLink ))
         expect_true( Sys.readlink( fs$badLink ) == sourceFile )
         fsCount <<- fsCount + 1
      })
      it( "creates needed files", {
         expect_true( file.exists( fs$goodFile ))
         fsCount <<- fsCount + 1
         expect_true( file.exists( fs$badFile ))
         fsCount <<- fsCount + 1
         expect_true( file.exists( fs$oneFile ))
         fsCount <<- fsCount + 1
         expect_true( file.exists( fs$twoFile ))
         fsCount <<- fsCount + 1
      })
      it( "creates needed directories and content", {
         expect_true( dir.exists( fs$someDir ))
         fsCount <<- fsCount + 1
         expect_equal(
            sort( list.files(fs$someDir, all.files= TRUE, no..= TRUE,
                             full.names= TRUE)),
            sort( c(fs$oneFile, fs$twoFile) )
         )
      })
      it( "Returns has no untested elements", {
         expect_equal( length(fs), fsCount)
      })
   })

   fsTearDown(fs)
   describe( "fsTearDown", {
      fsCount <- length(fs)
      it( "Missing file still missing", {
         expect_false( file.exists( fs$noSuchFile ))
         fsCount <<- fsCount - 1
      })
      it( "deletes created links", {
         expect_false( file.exists( fs$goodLink ))
         expect_true( is.na(Sys.readlink( fs$goodLink )))
         fsCount <<- fsCount - 1
         expect_false( file.exists( fs$badLink))
         expect_true( is.na(Sys.readlink( fs$badLink )))
         fsCount <<- fsCount - 1
      })
      it( "deletes created files", {
         expect_false( file.exists( fs$goodFile ))
         fsCount <<- fsCount - 1
         expect_false( file.exists( fs$badFile ))
         fsCount <<- fsCount - 1
         expect_false( file.exists( fs$oneFile ))
         fsCount <<- fsCount - 1
         expect_false( file.exists( fs$twoFile ))
         fsCount <<- fsCount - 1
      })
      it( "deletes created directory", {
         expect_false( dir.exists( fs$someDir ))
         expect_false( file.exists( fs$someDir ))
         fsCount <<- fsCount - 1
      })
      it( "Deletes all created elements", {
         expect_equal( fsCount, 0)
      })
   })
})

#===
context("linkAllTo() testing, all valid input data")
#===

describe( "Input df has all linkTo parameters specified", {
   describe( "with no extra columns and duplicateOK= false", {
      fs <- fsSetUp()

      df <- data.frame(
         file= c(fs$noSuchFile, fs$goodLink, fs$badLink, fs$goodFile, fs$badFile, fs$someDir),
         repo= rep(repoDir, 6),
         entry= rep(entryDir, 6),
         asFile= rep(newBaseName, 6),
         fileSize= c(sourceFileSize, NULL, sourceFileSize, NULL, sourceFileSize, NULL),
         checksum= c(NULL, NULL, sourceFileMd5, sourceFileMd5, NULL, NULL),
         checksumFunc= rep('tools::md5sum', 6),
         duplicateOk= c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE),
         onExists= c('error', 'replaceAny', 'skip', 'backupFile', 'replaceFile', 'backupAny'),
         stringsAsFactors= FALSE
      )

      gotWarnings= character(0)
      withCallingHandlers({
         got <- linkAllTo(df)
      }, warning= function(e) {
         gotWarnings <<- c(gotWarnings, conditionMessage(e))
         invokeRestart("muffleWarning")
      })

      it( "Has expected warnings", {

         expect_equal(length(gotWarnings), 5)
         # noSuchFile + error
         # warnRE = NULL

         # goodLink + replaceAny
         warnRE <- paste0( "Removed existing file: ", fs$goodLink)
         expect_true( any(grepl(warnRE, gotWarnings)))

         # badLink + skip
         warnRE <- paste0( "Skipped linking due to existing file: ", fs$badLink)
         expect_true( any(grepl(warnRE, gotWarnings)))

         # goodFile + backupFile
         warnRE <- paste0( "Renamed existing file ", fs$goodFile)
         expect_true( any(grepl(warnRE, gotWarnings)))

         # badFile + replaceFile
         warnRE <- paste0( "Removed existing file: ", fs$badFile)
         expect_true( any(grepl(warnRE, gotWarnings)))

         # someDir + backupAny
         warnRE <- paste0( "Renamed existing file ", fs$someDir)
         expect_true( any(grepl(warnRE, gotWarnings)))

      })

      it( "Returns expected results", {
         expect_equal(names(got), df$file)
         want <-  rep("", length(df$file))
         names(want) <- df$file
         expect_equal( got, want)
      })

      it( "Creates expected links", {
         expect_equal( Sys.readlink(fs$noSuchFile), targetFile )
         expect_equal( Sys.readlink(fs$goodLink), targetFile )
         expect_equal( Sys.readlink(fs$goodFile), targetFile )
         expect_equal( Sys.readlink(fs$badFile), targetFile )
         expect_equal( Sys.readlink(fs$someDir), targetFile )

         # Not created, but skipped, so has original value
         expect_equal( Sys.readlink(fs$badLink), sourceFile )
      })

      fsTearDown(fs)
   })
   describe( "with no extra columns and duplicateOK= true", {
      fs <- fsSetUp()

      df <- data.frame(
         file= c(fs$noSuchFile, fs$goodLink, fs$badLink, fs$goodFile, fs$badFile, fs$someDir),
         repo= rep(repoDir, 6),
         entry= rep(entryDir, 6),
         asFile= rep(newBaseName, 6),
         fileSize= c(sourceFileSize, NULL, sourceFileSize, NULL, sourceFileSize, NULL),
         checksum= c(NULL, NULL, sourceFileMd5, sourceFileMd5, NULL, NULL),
         checksumFunc= rep('tools::md5sum', 6),
         duplicateOk= c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
         onExists= c('error', 'replaceAny', 'skip', 'backupFile', 'replaceFile', 'backupAny'),
         stringsAsFactors= FALSE
      )

      gotWarnings= character(0)
      withCallingHandlers({
         got <- linkAllTo(df)
      }, warning= function(e) {
         gotWarnings <<- c(gotWarnings, conditionMessage(e))
         invokeRestart("muffleWarning")
      })

      it( "Has expected warnings", {

         expect_equal(length(gotWarnings), 3)
         # noSuchFile + error
         # warnRE = NULL

         # goodLink + replaceAny
         # warnRE = NULL

         # badLink + skip
         warnRE <- paste0( "Skipped linking due to existing file: ", fs$badLink)
         expect_true( any(grepl(warnRE, gotWarnings)))

         # goodFile + backupFile
         # warnRE = NULL

         # badFile + replaceFile
         warnRE <- paste0( "Removed existing file: ", fs$badFile)
         expect_true( any(grepl(warnRE, gotWarnings)))

         # someDir + backupAny
         warnRE <- paste0( "Renamed existing file ", fs$someDir)
         expect_true( any(grepl(warnRE, gotWarnings)))

      })

      it( "Returns expected results", {
         expect_equal(names(got), df$file)
         want <-  rep("", length(df$file))
         names(want) <- df$file
         expect_equal( got, want)
      })

      it( "Creates expected links", {
         expect_equal( Sys.readlink(fs$noSuchFile), targetFile )
         expect_equal( Sys.readlink(fs$goodLink), targetFile )
         expect_equal( Sys.readlink(fs$goodFile), targetFile )
         expect_equal( Sys.readlink(fs$badFile), targetFile )
         expect_equal( Sys.readlink(fs$someDir), targetFile )

         # Not created, but skipped, so has original value
         expect_equal( Sys.readlink(fs$badLink), sourceFile )
      })

      fsTearDown(fs)
   })
   describe( "with extra columns and duplicateOK= true", {
      fs <- fsSetUp()

      df <- data.frame(
         extraColumn1= c(11,22,33,44,55,66),
         file= c(fs$noSuchFile, fs$goodLink, fs$badLink, fs$goodFile, fs$badFile, fs$someDir),
         repo= rep(repoDir, 6),
         entry= rep(entryDir, 6),
         asFile= rep(newBaseName, 6),
         fileSize= c(sourceFileSize, NULL, sourceFileSize, NULL, sourceFileSize, NULL),
         checksum= c(NULL, NULL, sourceFileMd5, sourceFileMd5, NULL, NULL),
         checksumFunc= rep('tools::md5sum', 6),
         duplicateOk= c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),
         onExists= c('error', 'replaceAny', 'skip', 'backupFile', 'replaceFile', 'backupAny'),
         extraColumn2= c("A", "B", "C", "D", "E", "F"),
         stringsAsFactors= FALSE
      )

      gotWarnings= character(0)
      withCallingHandlers({
         got <- linkAllTo(df)
      }, warning= function(e) {
         gotWarnings <<- c(gotWarnings, conditionMessage(e))
         invokeRestart("muffleWarning")
      })

      it( "Has expected warnings", {

         expect_equal(length(gotWarnings), 3)
         # noSuchFile + error
         # warnRE = NULL

         # goodLink + replaceAny
         # warnRE = NULL

         # badLink + skip
         warnRE <- paste0( "Skipped linking due to existing file: ", fs$badLink)
         expect_true( any(grepl(warnRE, gotWarnings)))

         # goodFile + backupFile
         # warnRE = NULL

         # badFile + replaceFile
         warnRE <- paste0( "Removed existing file: ", fs$badFile)
         expect_true( any(grepl(warnRE, gotWarnings)))

         # someDir + backupAny
         warnRE <- paste0( "Renamed existing file ", fs$someDir)
         expect_true( any(grepl(warnRE, gotWarnings)))

      })

      it( "Returns expected results", {
         expect_equal(names(got), df$file)
         want <-  rep("", length(df$file))
         names(want) <- df$file
         expect_equal( got, want)
      })

      it( "Creates expected links", {
         expect_equal( Sys.readlink(fs$noSuchFile), targetFile )
         expect_equal( Sys.readlink(fs$goodLink), targetFile )
         expect_equal( Sys.readlink(fs$goodFile), targetFile )
         expect_equal( Sys.readlink(fs$badFile), targetFile )
         expect_equal( Sys.readlink(fs$someDir), targetFile )

         # Not created, but skipped, so has original value
         expect_equal( Sys.readlink(fs$badLink), sourceFile )
      })

      fsTearDown(fs)
   })
})

describe( "Input df has just some parameters specified", {
   describe( "with no extra columns", {
      fs <- fsSetUp()

      df <- data.frame(
         file= c(fs$noSuchFile, fs$goodLink, fs$goodFile),
         repo= rep(repoDir, 3),
         entry= rep(entryDir, 3),
         asFile= rep(newBaseName, 3),
         stringsAsFactors= FALSE
      )

      it( "File system before:", {
         expect_false( file.exists( fs$noSuchFile ))
         expect_equal( Sys.readlink(fs$goodLink), targetFile )
         expect_equal( Sys.readlink(fs$goodFile), "" )
      })

      expect_silent(got <- linkAllTo(df))

      it( "Returns expected results", {
         expect_equal(names(got), df$file)
         want <-  rep("", length(df$file))
         names(want) <- df$file
         expect_equal( got, want)
      })

      it( "Creates expected links", {
         expect_equal( Sys.readlink(fs$noSuchFile), targetFile )
         expect_equal( Sys.readlink(fs$goodLink), targetFile )
         expect_equal( Sys.readlink(fs$goodFile), targetFile )
      })

      fsTearDown(fs)
   })
   describe( "with extra columns", {
      fs <- fsSetUp()

      df <- data.frame(
         front <- c("just some", NA_character_, "data"),
         file= c(fs$noSuchFile, fs$goodLink, fs$goodFile),
         repo= rep(repoDir, 3),
         entry= rep(entryDir, 3),
         middle <- c(1.1, NaN, -Inf),
         asFile= rep(newBaseName, 3),
         end <- c(TRUE, NA, FALSE),
         stringsAsFactors= FALSE
      )

      it( "File system before:", {
         expect_false( file.exists( fs$noSuchFile ))
         expect_equal( Sys.readlink(fs$goodLink), targetFile )
         expect_equal( Sys.readlink(fs$goodFile), "" )
      })

      expect_silent(got <- linkAllTo(df))

      it( "Returns expected results", {
         expect_equal(names(got), df$file)
         want <-  rep("", length(df$file))
         names(want) <- df$file
         expect_equal( got, want)
      })

      it( "Creates expected links", {
         expect_equal( Sys.readlink(fs$noSuchFile), targetFile )
         expect_equal( Sys.readlink(fs$goodLink), targetFile )
         expect_equal( Sys.readlink(fs$goodFile), targetFile )
      })

      fsTearDown(fs)
   })
})

describe( "Handling problems.", {
   describe( "Before linking iteration, aborts if an expected error occurs.", {
      it( "Requires a data frame parameter", {
         wantErrorRE <- "checkParam_df= Must be a data frame."
         expect_error( got<- linkAllTo( 'Oops.' ), wantErrorRE)
      })
      it( "Requires a 'file' column in the data frame", {
         badDF <- data.frame(x=c(1,2,3), y=c(TRUE,FALSE,TRUE))
         wantErrorRE <- "checkParam_df= Must have a 'file' column."
         expect_error( got<- linkAllTo( badDF ), wantErrorRE)
      })
      it( "Requires unique 'file' column values", {
         badDF <- data.frame( file= c("Bob", "Ann", "Bob", "Mary"),
                              stringsAsFactors = FALSE)
         wantErrorRE <- "checkParam_df= Must not have duplicated 'file' values"
         expect_error( got<- linkAllTo( badDF ), wantErrorRE)
      })
   })
   describe( "During linking iteration, reports 1 expected error.", {
      fs <- fsSetUp()

      df <- data.frame(
         file= c(fs$noSuchFile, fs$someDir, fs$goodFile),
         repo= rep(repoDir, 3),
         entry= rep(entryDir, 3),
         asFile= rep(newBaseName, 3),
         stringsAsFactors= FALSE
      )

      it( "File system before:", {
         expect_false( file.exists( fs$noSuchFile ))
         expect_true( dir.exists(fs$someDir))
         expect_equal( Sys.readlink(fs$goodFile), "" )
      })

      gotWarnings= character(0)
      withCallingHandlers({
         got <- linkAllTo(df)
      }, warning= function(e) {
         gotWarnings <<- c(gotWarnings, conditionMessage(e))
         invokeRestart("muffleWarning")
      })

      it( "Has expected warnings", {

         expect_equal(length(gotWarnings), 1)
         warnRE <- "Failed linking 1 file."
         expect_true( any(grepl(warnRE, gotWarnings)))
      })

      it( "Returns expected results", {
         expect_equal(names(got), df$file)
         want <-  c("", "checkExists = File exists.; createLink = NA not ok.", "")
         names(want) <- df$file
         expect_equal( got, want)
      })

      it( "Creates expected links", {
         expect_equal( Sys.readlink(fs$noSuchFile), targetFile )
         expect_equal( Sys.readlink(fs$goodFile), targetFile )

         # Not replaced
         expect_true( dir.exists(fs$someDir))

      })

      fsTearDown(fs)
   })
   describe( "During linking iteration, reports each expected error.", {
      fs <- fsSetUp()

      df <- data.frame(
         file= c(fs$badFile, fs$someDir, fs$goodFile),
         repo= rep(repoDir, 3),
         entry= rep(entryDir, 3),
         asFile= rep(newBaseName, 3),
         stringsAsFactors= FALSE
      )

      it( "File system before:", {
         expect_true( file.exists( fs$badFile ))
         expect_true( dir.exists(fs$someDir))
         expect_equal( Sys.readlink(fs$goodFile), "" )
      })

      gotWarnings= character(0)
      withCallingHandlers({
         got <- linkAllTo(df)
      }, warning= function(e) {
         gotWarnings <<- c(gotWarnings, conditionMessage(e))
         invokeRestart("muffleWarning")
      })

      it( "Has expected warnings", {

         expect_equal(length(gotWarnings), 1)
         warnRE <- "Failed linking 2 files."
         expect_true( any(grepl(warnRE, gotWarnings)))
      })

      it( "Returns expected results", {
         expect_equal(names(got), df$file)
         want <-  c("checkExists = File exists.; createLink = NA not ok.",
                    "checkExists = File exists.; createLink = NA not ok.",
                    "")
         names(want) <- df$file
         expect_equal( got, want)
      })

      it( "Creates expected links", {
         expect_equal( Sys.readlink(fs$goodFile), targetFile )

         # Not replaced
         expect_true( dir.exists(fs$someDir))
         expect_true( file.exists(fs$badFile))

      })

      fsTearDown(fs)
   })
   describe( "During linking iteration, reports each unexpected error.", {
      fs <- fsSetUp()

      df <- data.frame(
         file= c(fs$noSuchFile, fs$goodLink, fs$goodFile),
         repo= rep(repoDir, 3),
         entry= rep(entryDir, 3),
         asFile= rep(newBaseName, 3),
         stringsAsFactors= FALSE
      )

      it( "File system before:", {
         expect_false( file.exists( fs$noSuchFile ))
         expect_equal( Sys.readlink(fs$goodLink), targetFile )
         expect_equal( Sys.readlink(fs$goodFile), "" )
      })

      it("Catches warnings and returns correct messages ", {
         wantWarningRE <- "Failed linking 3 files."

         with_mock(
            `DataRepo::checkSummary`= function (...) {
               stop('Bad checkSummary.')
            },
            expect_warning( got <- linkAllTo(df), wantWarningRE )
         )

         errorMessage <- "Unexpected error: Bad checkSummary."
         want <- c(errorMessage, errorMessage, errorMessage)
         names(want) <- df$file
         expect_equal(got, want)
      })

      it( 'File system after', {
         expect_false( file.exists( fs$noSuchFile ))
         expect_equal( Sys.readlink(fs$goodLink), targetFile )
         expect_equal( Sys.readlink(fs$goodFile), "" )
      })

      fsTearDown(fs)
   })

})
