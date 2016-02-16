# Set up basic file system objects for testing
# The repo dir
repoDir <- tempfile( 'repo' )
dir.create(  repoDir  )
defaultGroup <- file.info(repoDir, extra_cols = TRUE)$grname
groupList <- groups()
groupList <- groupList[groupList != defaultGroup]
otherGroup <- groupList[1]

# A source file to copy
sourceFile <- tempfile( 'sourceFile' )
writeLines(c("A", "three line", "file of text."), sourceFile)
sourceFileSize <- 27
sourceFileMd5 <- "2c77982c7ee2279e119c585859d92a7b"
newBaseName <- basename(tempfile( 'newBaseName' ))

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

context( "Setup for testing addFile()" )
describe( "File system as set up for addFile testing", {
   it( "has source file and repo dir", {
      expect_true( dir.exists( repoDir ))
      expect_true( file.exists( sourceFile ))
   })
   it( "has valid source file", {
      ok <- validateFile(sourceFile, checksum= sourceFileMd5, fileSize= sourceFileSize)
      expect_true( all( ok == '' ))
   })
   it( "has more than one group", {
      expect_false(defaultGroup == otherGroup)
   })
})

# Helper test functions and data
checked <- c('checkParam_repo', 'checkValidSource', 'checkCopy')

context( "addFile() to repo" )
describe( "Succesfully adding a new file to the repo (all checks)", {
   want <- c( checkParam_repo= '', checkParam_entry= '', checkParam_asFile= '',
              checkParam_writeGroup= '', checkParam_checksumFunc= "",
              checkValidSource= '', makeTargetDir= "", doCopy= '',
              checkCopyTarget= '', unexpectedError='' )
   describe( "repo= path with defaults", {
      it( "Adds full-path file to the current dir (relative path repo)", {
         targetFile <- file.path( getwd(), basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))

         got <- addFile( sourceFile, fileSize= sourceFileSize,
                         checksum= sourceFileMd5 )
         expect_equal(got, want)

         expect_true( file.exists( targetFile ))
         expect_true( file.remove( targetFile ))
         expect_false( file.exists( targetFile ))
      })
      it( "Adds full-path file to full-path repo", {
         targetFile <- file.path( repoDir, basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))

         got <- addFile( sourceFile, repo= repoDir, fileSize= sourceFileSize,
                         checksum= sourceFileMd5 )
         expect_equal(got, want)

         expect_true( file.exists( targetFile ))

         expect_true( file.remove( targetFile ))
         expect_false( file.exists( targetFile ))
      })
      it( "Adds relative-path file to full-path repo", {
         newHomeDir <- dirname( sourceFile )
         oldHomeDir <- getwd()
         on.exit(setwd(oldHomeDir), add= TRUE)
         setwd( newHomeDir )
         localFile  <-  basename( sourceFile )
         targetFile <- file.path( repoDir, localFile)
         expect_false( file.exists( targetFile ))
         on.exit( unlink(targetFile), add= TRUE)

         got <- addFile( localFile, repo= repoDir, fileSize= sourceFileSize,
                         checksum= sourceFileMd5 )
         expect_equal(got, want)

         expect_true( file.exists( targetFile ))
         expect_true( file.remove( targetFile ))
         expect_false( file.exists( targetFile ))

         unlink( targetFile )
         setwd( oldHomeDir )
      })
   })
   describe( "repo= path with new name", {
      it( "Adds /path file to the current dir (./path repo) with new name", {
         targetFile <- file.path( getwd(), newBaseName)
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))

         got <- addFile( sourceFile, asFile= newBaseName,
                         fileSize= sourceFileSize, checksum= sourceFileMd5 )
         expect_equal(got, want)

         expect_true( file.exists( targetFile ))

         expect_true( file.remove( targetFile ))
         expect_false( file.exists( targetFile ))
      })
      it( "Adds full-path file to full-path repo with new name", {
         targetFile <- file.path( repoDir, newBaseName)
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))

         got <- addFile( sourceFile, repo= repoDir, asFile= newBaseName,
                         fileSize= sourceFileSize, checksum= sourceFileMd5 )
         expect_equal(got, want)

         expect_true( file.exists( targetFile ))

         expect_true( file.remove( targetFile ))
         expect_false( file.exists( targetFile ))
      })
      it( "Adds relative-path file to full-path repo with new name", {
         newHomeDir <- dirname( sourceFile )
         oldHomeDir <- getwd()
         on.exit(setwd(oldHomeDir), add= TRUE)
         setwd( newHomeDir )
         localFile  <-  basename( sourceFile )
         targetFile <- file.path( repoDir, newBaseName)
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))

         got <- addFile( localFile, repo= repoDir, asFile= newBaseName,
                         fileSize= sourceFileSize, checksum= sourceFileMd5 )
         expect_equal(got, want)

         expect_true( file.exists( targetFile ))
         expect_true( file.remove( targetFile ))
         expect_false( file.exists( targetFile ))

         unlink( targetFile )
         setwd( oldHomeDir )
      })
   })
   describe( "repo= path with partial checks", {
      it( "works with checksum but not fileSize (/path file, ./path repo)", {
         targetFile <- file.path( getwd(), basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))

         got <- addFile( sourceFile, fileSize= NULL, checksum= sourceFileMd5 )
         expect_equal(got, want)

         expect_true( file.exists( targetFile ))

         expect_true( file.remove( targetFile ))
         expect_false( file.exists( targetFile ))
      })
      it( "works with file size but not checksum (/path file, /path repo)", {
         targetFile <- file.path( getwd(), basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))

         got <- addFile( sourceFile, fileSize= sourceFileSize, checksum= NULL )
         expect_equal(got, want)

         expect_true( file.exists( targetFile ))

         expect_true( file.remove( targetFile ))
         expect_false( file.exists( targetFile ))

      })
      it( "works without file size or checksum (./path file, /path repo)", {
         newHomeDir <- dirname( sourceFile )
         oldHomeDir <- getwd()
         on.exit(setwd(oldHomeDir), add= TRUE)
         setwd( newHomeDir )
         localFile  <-  basename( sourceFile )
         targetFile <- file.path( repoDir, localFile)
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))

         got <- addFile( localFile, repo= repoDir, fileSize= NULL,
                         checksum= NULL )
         expect_equal(got, want)

         expect_true( file.exists( targetFile ))
         expect_true( file.remove( targetFile ))
         expect_false( file.exists( targetFile ))

         unlink( targetFile )
         setwd( oldHomeDir )
      })
   })
   describe( "repo= path, when creating a new entry.", {
      it( "Adds full-path file to the current dir (relative path repo)", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( getwd(), "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( getwd(), entryPath, basename( sourceFile ))
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            got <- addFile( sourceFile, entry= entryPath,
                            fileSize= sourceFileSize, checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
         }
      })
      it( "Adds full-path file to full-path repo", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( repoDir, entryPath, basename( sourceFile ))
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            got <- addFile( sourceFile, repo= repoDir, entry= entryPath,
                            fileSize= sourceFileSize, checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
            unlink(dirToRemove, recursive= TRUE)
            expect_false( file.exists( targetFile ))
            expect_false( dir.exists( dirToRemove ))
         }
      })
      it( "Adds relative-path file to full-path repo", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            newHomeDir <- dirname( sourceFile )
            oldHomeDir <- getwd()
            on.exit(setwd(oldHomeDir), add= TRUE)
            setwd( newHomeDir )
            localFile  <-  basename( sourceFile )
            targetFile <- file.path( repoDir, entryPath, localFile)
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)

            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            got <- addFile( localFile, repo= repoDir, entry= entryPath,
                            fileSize= sourceFileSize, checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
            unlink(dirToRemove, recursive= TRUE)
            expect_false( file.exists( targetFile ))
            expect_false( dir.exists( dirToRemove ))

            setwd( oldHomeDir )
         }
      })
      it( "Adds full-path file to full-path repo with new name", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( repoDir, entryPath, newBaseName)
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            got <- addFile( sourceFile, repo= repoDir, entry= entryPath,
                            asFile= newBaseName, fileSize= sourceFileSize,
                            checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
            unlink(dirToRemove, recursive= TRUE)
            expect_false( file.exists( targetFile ))
            expect_false( dir.exists( dirToRemove ))
         }
      })
      it( "Adds ignores leading and trailing slashes.", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( repoDir, entryPath, basename( sourceFile ))
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            got <- addFile( sourceFile, repo= repoDir,
               entry= paste0(
                  .Platform$file.sep, entryPath, .Platform$file.sep ),
               fileSize= sourceFileSize, checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
            unlink(dirToRemove, recursive= TRUE)
            expect_false( file.exists( targetFile ))
            expect_false( dir.exists( dirToRemove ))
         }
      })
   })
   describe( "repo= path, when creating a new entry with new group.", {
      it( "Adds full-path file to the current dir (relative path repo)", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( getwd(), "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( getwd(), entryPath, basename( sourceFile ))
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            got <- addFile( sourceFile, entry= entryPath,
                            writeGroup= otherGroup, fileSize= sourceFileSize,
                            checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_equal( file.info( targetFile, extra_cols = TRUE )$grname,
                          otherGroup )
            expect_true( dir.exists( dirToRemove ))
            expect_equal( file.info( dirToRemove, extra_cols = TRUE )$grname,
                          otherGroup )
            unlink(dirToRemove, recursive= TRUE)
            expect_false( file.exists( targetFile ))
            expect_false( dir.exists( dirToRemove ))
         }
      })
      it( "Adds full-path file to full-path repo", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( repoDir, entryPath, basename( sourceFile ))
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            got <- addFile( sourceFile, repo= repoDir, entry= entryPath,
                            writeGroup= otherGroup, fileSize= sourceFileSize,
                            checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_equal( file.info( targetFile, extra_cols = TRUE )$grname,
                          otherGroup )
            expect_true( dir.exists( dirToRemove ))
            expect_equal( file.info( dirToRemove, extra_cols = TRUE )$grname,
                          otherGroup )

            unlink(dirToRemove, recursive= TRUE)
            expect_false( file.exists( targetFile ))
            expect_false( dir.exists( dirToRemove ))
         }
      })
      it( "Adds relative-path file to full-path repo", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            newHomeDir <- dirname( sourceFile )
            oldHomeDir <- getwd()
            on.exit(setwd(oldHomeDir), add= TRUE)
            setwd( newHomeDir )
            localFile  <-  basename( sourceFile )
            targetFile <- file.path( repoDir, entryPath, localFile)
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)

            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            got <- addFile( localFile, repo= repoDir, entry= entryPath,
                            writeGroup= otherGroup, fileSize= sourceFileSize,
                            checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_equal( file.info( targetFile, extra_cols = TRUE )$grname,
                          otherGroup )
            expect_true( dir.exists( dirToRemove ))
            expect_equal( file.info( dirToRemove, extra_cols = TRUE )$grname,
                          otherGroup )
            unlink(dirToRemove, recursive= TRUE)
            expect_false( file.exists( targetFile ))
            expect_false( dir.exists( dirToRemove ))

            setwd( oldHomeDir )
         }
      })
      it( "Adds full-path file to full-path repo with entry and new name", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( repoDir, entryPath, newBaseName)
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            got <- addFile( sourceFile, repo= repoDir, entry= entryPath,
                            writeGroup= otherGroup, asFile= newBaseName,
                            fileSize= sourceFileSize, checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_equal( file.info( targetFile, extra_cols = TRUE )$grname,
                          otherGroup )
            expect_equal( as.character(file.info( targetFile, extra_cols = TRUE )$mode),
                          "664" )
            expect_true( dir.exists( dirToRemove ))
            expect_equal( file.info( dirToRemove, extra_cols = TRUE )$grname,
                          otherGroup )
            expect_equal( as.character(file.info( dirToRemove, extra_cols = TRUE )$mode),
                          "2775" )
            unlink(dirToRemove, recursive= TRUE)
            expect_false( file.exists( targetFile ))
            expect_false( dir.exists( dirToRemove ))
         }
      })
   })
   describe( "repo= path, when have an existing file or entry", {
      it( "succeeds if existing file is same as source", {
         targetFile <- file.path( repoDir, basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         file.copy( sourceFile, targetFile )
         expect_true( file.exists( targetFile ))

         got <- addFile( sourceFile, repo= repoDir, fileSize= sourceFileSize,
                         checksum= sourceFileMd5 )
         expect_equal(got, want)
         expect_true( file.exists( targetFile ))
      })
      it( "succeeds if existing path + file same as entry + source", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( getwd(), "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( getwd(), entryPath, basename( sourceFile ))
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            dir.create(file.path( getwd(), entryPath), recursive= TRUE)
            file.copy(sourceFile, targetFile)
            expect_true( dir.exists( dirToRemove ))
            expect_true( file.exists( targetFile ))

            got <- addFile( sourceFile, entry= entryPath,
                            fileSize= sourceFileSize, checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
         }
      })
      it( "succeeds if existing path same as entry", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( getwd(), "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( getwd(), entryPath, basename( sourceFile ))
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            dir.create(file.path( getwd(), entryPath), recursive= TRUE)
            expect_true( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            got <- addFile( sourceFile, entry= entryPath,
                            fileSize= sourceFileSize, checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
         }
      })
      it( "succeeds if existing partial path same as part of entry", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( getwd(), "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( getwd(), entryPath, basename( sourceFile ))
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            dir.create( dirToRemove )
            expect_true( dir.exists( dirToRemove ))
            expect_false( dir.exists( file.path( getwd(), entryPath )))
            expect_false( file.exists( targetFile ))

            got <- addFile( sourceFile, entry= entryPath,
                            fileSize= sourceFileSize, checksum= sourceFileMd5 )
            expect_equal(got, want)

            expect_true( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
         }
      })
   })
})

describe( "Reports problems", {
   describe( "with bad parameters", {
      it( "catches bad path= parameters", {
         wantRE <- "checkParam_path = Not a vector of mode character."
         got <- addFile( file= NULL, repo= repoDir, asFile= newBaseName,
                         fileSize= NULL, checksum= NULL )
         expect_match(got['checkValidSource'], wantRE)
      })
      it( "catches bad repo= parameters", {
         want <- c(checkParam_repo= "Not a vector of mode character.")

         got <- addFile( file= sourceFile, repo= NULL )
         expect_equal(got['checkParam_repo'], want)

         got <- addFile( file= sourceFile, repo= list() )
         expect_equal(got['checkParam_repo'], want)

         got <- addFile( file= sourceFile, repo= NA )
         expect_equal(got['checkParam_repo'], want)

         want <- c(checkParam_repo= "Length is not 1.")
         got <- addFile( file= sourceFile, repo= c( 'a', 'b' ))
         expect_equal(got['checkParam_repo'], want)

         want <- c(checkParam_repo= "Character count is not between 1 and Inf.")
         got <- addFile( file= sourceFile, repo= "" )
         expect_equal(got['checkParam_repo'], want)

         noSuchFile <- tempfile("noSuchFile")
         expect_false(file.exists(noSuchFile))
         want <- c(checkParam_repo= "No such path.")
         got <- addFile( file= sourceFile, repo= noSuchFile )
         expect_equal(got['checkParam_repo'], want)

         want <- c(checkParam_repo= "Not a directory.")
         got <- addFile( file= sourceFile, repo= sourceFile )
         expect_equal(got['checkParam_repo'], want)

      })
      it( "catches bad entry= parameters", {
         want <- c(checkParam_entry= "Not a vector of mode character.")

         got <- addFile( file= sourceFile, repo= repoDir, entry= list() )
         expect_equal(got['checkParam_entry'], want)

         got <- addFile( file= sourceFile, repo= repoDir, entry= NA )
         expect_equal(got['checkParam_entry'], want)

         want <- c(checkParam_entry= "Length is not 1.")
         got <- addFile( file= sourceFile, repo= repoDir, entry= c('a', 'b') )
         expect_equal(got['checkParam_entry'], want)

         want <- c(checkParam_entry= "Character count is not between 1 and Inf.")
         got <- addFile( file= sourceFile, repo= repoDir, entry= "" )
         expect_equal(got['checkParam_entry'], want)
      })
      it( "catches bad writeGroup= parameters", {
         want <- c(checkParam_writeGroup= "Not a vector of mode character.")

         got <- addFile( file= sourceFile, repo= repoDir, writeGroup= list() )
         expect_equal(got['checkParam_writeGroup'], want)

         got <- addFile( file= sourceFile, repo= repoDir, writeGroup= NA )
         expect_equal(got['checkParam_writeGroup'], want)

         want <- c(checkParam_writeGroup= "Length is not 1.")
         got <- addFile( file= sourceFile, repo= repoDir, writeGroup= c('a', 'b') )
         expect_equal(got['checkParam_writeGroup'], want)

         want <- c(checkParam_writeGroup= "Character count is not between 1 and Inf.")
         got <- addFile( file= sourceFile, repo= repoDir, writeGroup= "" )
         expect_equal(got['checkParam_writeGroup'], want)

         grp <- paste(letters[trunc(runif(15,1,27))], collapse = '')
         want <- c(checkParam_writeGroup= "Not one of user's groups.")
         got <- addFile( file= sourceFile, repo= repoDir, writeGroup= grp )
         expect_equal(got['checkParam_writeGroup'], want)

         want <- c(unexpectedError= "Unexpected error: bad groups.")
         with_mock(
            `groups`= function(...) {stop('bad groups.')},
            got <- addFile( file= sourceFile, repo= repoDir, writeGroup= grp )
         )
         expect_equal(got['unexpectedError'], want)

      })
      it( "catches bad checksumFunc= parameters", {
         want <- c(checkParam_checksumFunc= "Is null.")
         got <- addFile( file= sourceFile, repoDir, checksumFunc= NULL )
         expect_equal(got['checkParam_checksumFunc'], want)

         want <- c(checkParam_checksumFunc= "Character count is not between 1 and Inf.")
         got <- addFile( file= sourceFile, repo= repoDir,
                         checksumFunc= "" )
         expect_equal(got['checkParam_checksumFunc'], want)

         want <- c(checkParam_checksumFunc= "Not a vector of mode character.")
         got <- addFile( file= sourceFile, repo= repoDir, checksumFunc= NA )
         expect_equal(got['checkParam_checksumFunc'], want)
      })
      it( "catches bad fileAs= parameters", {
          want <- c(checkParam_asFile= "Not a vector of mode character.")

          got <- addFile( file= sourceFile, asFile= NULL )
          expect_equal(got['checkParam_asFile'], want)

          got <- addFile( file= sourceFile, asFile= list() )
          expect_equal(got['checkParam_asFile'], want)

          got <- addFile( file= sourceFile, asFile= NA )
          expect_equal(got['checkParam_asFile'], want)

          want <- c(checkParam_asFile= "Length is not 1.")
          got <- addFile( file= sourceFile, asFile= c('a', 'b') )
          expect_equal(got['checkParam_asFile'], want)

          want <- c(checkParam_asFile= "Character count is not between 1 and Inf.")
          got <- addFile( file= sourceFile, asFile= "" )
          expect_equal(got['checkParam_asFile'], want)

          noSuchFile <- tempfile("noSuchFile")
          expect_false(file.exists(noSuchFile))
          want <- c(checkParam_asFile= "Path elements not ok.")
          got <- addFile( file= sourceFile, asFile= noSuchFile )
          expect_equal(got['checkParam_asFile'], want)
      })
      it( "catches bad fileSize= parameters", {
         wantRE <- "checkParam_fileSize = Not a vector of mode numeric."
         got <- addFile( sourceFile, repo= repoDir, fileSize= 'bob' )
         expect_match(got['checkValidSource'], wantRE)
      })
      it( "catches bad checksum= parameters", {
         wantRE <- "checkParam_checksum = Length is not 1."
         got <- addFile( sourceFile, repo= repoDir, checksum= c(1,2) )
         expect_match(got['checkValidSource'], wantRE)
      })
   })
   describe( "with invalid source file", {
      want <- c( checkParam_repo= '', checkParam_entry= '', checkParam_asFile= '',
                 checkParam_writeGroup= "", checkParam_checksumFunc= "",
                 checkValidSource= "*****", makeTargetDir= NA, doCopy= NA,
                 checkCopyTarget= NA, unexpectedError= NA )
      it('catches missing path', {
         noSuchPath <- tempfile( 'noSuchFile' )
         expect_false( file.exists( noSuchPath ))

         want['checkValidSource'] <-
            "checkIsFile = No such path.; checkIsNotLink = NA not ok."
         got <- addFile( noSuchPath, repo= repoDir )
         expect_equal(got, want)
      })
      it('catches dir paths', {
         expect_true( dir.exists( repoDir ))

         want['checkValidSource'] <-
            "checkIsFile = Not a file.; checkIsNotLink = NA not ok."
         got <- addFile( repoDir, repo= repoDir )
         expect_equal(got, want)
      })
      it('catches bad size files, with and without checksums', {
         want['checkValidSource'] <-paste0(
               "checkFileSizeMatches = File size mismatch. Found ",
               sourceFileSize, ' wanted ', sourceFileSize + 100,
               ".; checkChecksumMatches = NA not ok." )
         got <- addFile( sourceFile, repo= repoDir,
                         fileSize= sourceFileSize + 100, checksum= 'MyBad' )
         expect_equal(got, want)

         want['checkValidSource'] <- paste0(
               "checkFileSizeMatches = File size mismatch. Found ",
               sourceFileSize, ' wanted ', sourceFileSize + 100, "." )
         got <- addFile( sourceFile, repo= repoDir,
                         fileSize= sourceFileSize + 100, checksum= NULL )
         expect_equal(got, want)
      })
      it('catches bad checksum files, with and without good file size checks', {
         want['checkValidSource'] <- paste0(
               "checkChecksumMatches = Checksum mismatch. Found ",
               sourceFileMd5, " wanted MyBad." )
         got <- addFile( sourceFile, repo= repoDir, fileSize= sourceFileSize,
                         checksum= 'MyBad' )
         expect_equal(got, want)

         want['checkValidSource'] <- paste0(
               "checkChecksumMatches = Checksum mismatch. Found ",
               sourceFileMd5, " wanted MyBad.")
         got <- addFile( sourceFile, repo= repoDir, checksum= 'MyBad' )
         expect_equal(got, want)
      })
   })
   describe( "with bad copy", {
      want <- c( checkParam_repo= "", checkParam_entry= '',
                 checkParam_asFile= '', checkParam_writeGroup= "",
                 checkParam_checksumFunc= "", checkValidSource= "",
                 makeTargetDir= "", doCopy= '******', checkCopyTarget= NA,
                 unexpectedError= NA )
      it('catches reported file copy failure', {
         want['doCopy'] <- 'Copy failed.'
         with_mock(
            `base::file.copy`= function(...) {return(FALSE)},
            got <- addFile( sourceFile, repo= repoDir, fileSize= sourceFileSize,
                            checksum= sourceFileMd5 )
         )
         expect_equal(got, want)
      })
      it('catches file copy exiting with error', {
         want['unexpectedError'] <- 'Unexpected error: Bad file.copy.'
         want['doCopy'] <- ''
         with_mock(
            `base::file.copy`= function(...) {stop("Bad file.copy.")},
            got <- addFile( sourceFile, repo= repoDir, fileSize= sourceFileSize,
                            checksum= sourceFileMd5 )
         )
         expect_equal(got, want)
      })
      it('catches failure to change group of file after copy', {
         want['doCopy'] <- 'Setting copy write group failed.'
         with_mock(
            `DataRepo::chgrp`= function(...) {return(NA)},
            got <- addFile( sourceFile, repo= repoDir, writeGroup= otherGroup,
                            fileSize= sourceFileSize, checksum= sourceFileMd5 )
         )
         expect_equal(got, want)
         file.remove( file.path( repoDir, basename( sourceFile )))
      })
      it('catches failure to change permission of file after copy', {
         want['doCopy'] <- 'Setting copy permissions failed.'
         with_mock(
            `base::Sys.chmod`= function(...) {return(FALSE)},
            got <- addFile( sourceFile, repo= repoDir, writeGroup= otherGroup,
                            fileSize= sourceFileSize, checksum= sourceFileMd5 )
         )
         expect_equal(got, want)
         file.remove( file.path( repoDir, basename( sourceFile )))
      })
   })
   describe( "with copy failing due to bad copy/different existing file", {
      want <- c( checkParam_repo= "", checkParam_entry= '',
                 checkParam_asFile= '', checkParam_writeGroup= "",
                 checkParam_checksumFunc= "", checkValidSource= "",
                 makeTargetDir= "", doCopy= 'Path exists.',
                 checkCopyTarget= "****", unexpectedError= NA )

      it( 'catches truncated file, size check; checksum check', {
         targetFile <- file.path( repoDir, basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))
         file.copy(truncFile, targetFile)

         want['checkCopyTarget'] <- paste0(
              "checkFileSizeMatches = File size mismatch. Found ",
              truncFileSize, ' wanted ', sourceFileSize,
              ".; checkChecksumMatches = NA not ok." )
         with_mock(
            `base::file.copy` = function(...) {TRUE},
            got <- addFile( sourceFile, repo= repoDir, fileSize= sourceFileSize,
                            checksum= sourceFileMd5 )
         )
         expect_equal(got, want)
         expect_true( file.exists( targetFile ))
         file.remove(targetFile)
         expect_false( file.exists( targetFile ))
      })
      it('catches truncated file, size check;  no checksum check', {
         targetFile <- file.path( repoDir, basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))
         file.copy(truncFile, targetFile)

         want['checkCopyTarget'] <- paste0(
           "checkFileSizeMatches = File size mismatch. Found ", truncFileSize,
           ' wanted ', sourceFileSize, "." )
         with_mock(
            `base::file.copy` = function(...) {TRUE},
            got <- addFile( sourceFile, repo= repoDir,
                            fileSize= sourceFileSize )
         )
         expect_equal(got, want)
         expect_true( file.exists( targetFile ))
         file.remove(targetFile)
         expect_false( file.exists( targetFile ))
      })
      it('catches truncated file, no size check;  checksum check', {
         targetFile <- file.path( repoDir, basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))
         file.copy(truncFile, targetFile)

         want['checkCopyTarget'] <- paste0(
           "checkFileSizeMatches = File size mismatch. Found ", truncFileSize,
           ' wanted ', sourceFileSize, ".; checkChecksumMatches = NA not ok." )
         with_mock(
            `base::file.copy` = function(...) {TRUE},
            got <- addFile( sourceFile, repo= repoDir, fileSize= NULL,
                            checksum= sourceFileMd5 )
         )
         expect_equal(got, want)
         expect_true( file.exists( targetFile ))
         file.remove(targetFile)
         expect_false( file.exists( targetFile ))
      })
      it('catches truncated file, no size check;  no checksum check', {
         targetFile <- file.path( repoDir, basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))
         file.copy(truncFile, targetFile)

         want['checkCopyTarget'] <- paste0(
           "checkFileSizeMatches = File size mismatch. Found ", truncFileSize,
           ' wanted ', sourceFileSize, "." )
         with_mock(
            `base::file.copy` = function(...) {TRUE},
            got <- addFile( sourceFile, repo= repoDir )
         )
         expect_equal(got, want)
         expect_true( file.exists( targetFile ))
         file.remove(targetFile)
         expect_false( file.exists( targetFile ))
      })
      it('catches mutated file, size check; checksum check', {
         targetFile <- file.path( repoDir, basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))
         file.copy(mutFile, targetFile)

         want['checkCopyTarget'] <- paste0(
           "checkChecksumMatches = Checksum mismatch. Found ", mutFileMd5,
           ' wanted ', sourceFileMd5, "." )
         with_mock(
            `base::file.copy` = function(...) {TRUE},
            got <- addFile( sourceFile, repo= repoDir,
                            fileSize= sourceFileSize, checksum= sourceFileMd5 )
         )
         expect_equal(got, want)
         expect_true( file.exists( targetFile ))
         file.remove(targetFile)
         expect_false( file.exists( targetFile ))
      })
      it('catches mutated file, size check;  no checksum check', {
         targetFile <- file.path( repoDir, basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))
         file.copy(mutFile, targetFile)

         want['checkCopyTarget'] <- paste0(
           "checkChecksumMatches = Checksum mismatch. Found ", mutFileMd5,
           ' wanted ', sourceFileMd5, "." )
         with_mock(
            `base::file.copy` = function(...) {TRUE},
            got <- addFile( sourceFile, repo= repoDir, fileSize= sourceFileSize )
         )
         expect_equal(got, want)
         expect_true( file.exists( targetFile ))
         file.remove(targetFile)
         expect_false( file.exists( targetFile ))
      })
      it('catches mutated file, no size check;  checksum check', {
         targetFile <- file.path( repoDir, basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))
         file.copy(mutFile, targetFile)

         want['checkCopyTarget'] <- paste0(
           "checkChecksumMatches = Checksum mismatch. Found ", mutFileMd5,
           ' wanted ', sourceFileMd5, "." )
         with_mock(
            `base::file.copy` = function(...) {TRUE},
            got <- addFile( sourceFile, repo= repoDir, checksum= sourceFileMd5 )
         )
         expect_equal(got, want)
         expect_true( file.exists( targetFile ))
         file.remove(targetFile)
         expect_false( file.exists( targetFile ))
      })
      it('catches mutated file, no size check;  no checksum check', {
         targetFile <- file.path( repoDir, basename( sourceFile ))
         on.exit( unlink(targetFile), add= TRUE)
         expect_false( file.exists( targetFile ))
         file.copy(mutFile, targetFile)

         want['checkCopyTarget'] <- paste0(
           "checkChecksumMatches = Checksum mismatch. Found ", mutFileMd5,
           ' wanted ', sourceFileMd5, "." )
         with_mock(
            `base::file.copy` = function(...) {TRUE},
            got <- addFile( sourceFile, repo= repoDir )
         )
         expect_equal(got, want)
         expect_true( file.exists( targetFile ))
         file.remove(targetFile)
         expect_false( file.exists( targetFile ))
      })
   })
   describe( "with entry directory or entry path problems", {
      want <- c( checkParam_repo= '', checkParam_entry= '',
                 checkParam_asFile= '', checkParam_writeGroup= "",
                 checkParam_checksumFunc= "", checkValidSource= '',
                 makeTargetDir= '*****', doCopy= NA, checkCopyTarget= NA,
                 unexpectedError= NA )
      it( "fails if file exists with same name as path dir", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         fileToRemove <- file.path( repoDir, "an")
         file.create(fileToRemove)
         expect_true( file.exists( fileToRemove ))
         expect_false( dir.exists( fileToRemove ))

         on.exit( unlink(fileToRemove), add= TRUE)

         want['makeTargetDir'] <-
            paste0( 'File blocking directory creation: ', fileToRemove )
         got <- addFile( sourceFile, repo= repoDir, entry= entryPath,
                         asFile= newBaseName, fileSize= sourceFileSize,
                         checksum= sourceFileMd5 )
         expect_equal(got, want)

         file.remove(fileToRemove)
         expect_false( file.exists( fileToRemove ))

      })
      it( "fails if dir.create returns false.", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( repoDir, entryPath, newBaseName)
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            want['makeTargetDir'] <- paste0(
               'Failure trying to create directory: ', dirToRemove )
            with_mock(
               `base::dir.create`= function(...) {FALSE},
               got <- addFile( sourceFile, repo= repoDir, entry= entryPath,
                               asFile= newBaseName, fileSize= sourceFileSize,
                               checksum= sourceFileMd5 )
            )
            expect_equal(got, want)

            expect_false( file.exists( targetFile ))
            expect_false( dir.exists( dirToRemove ))
         }
      })
      it( "fails if dir.create fails for any other reason.", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( repoDir, entryPath, newBaseName)
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            want['makeTargetDir'] <- 'Unexpected error: bad dir.create.'
            with_mock(
               `base::dir.create`= function(...) { stop('bad dir.create.') },
               got <- addFile( sourceFile, repo= repoDir, entry= entryPath,
                               asFile= newBaseName, fileSize= sourceFileSize,
                               checksum= sourceFileMd5 )
            )
            expect_equal(got, want)

            expect_false( file.exists( targetFile ))
            expect_false( dir.exists( dirToRemove ))
         }
      })
      it( "fails if changing group after create fails.", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( repoDir, entryPath, newBaseName)
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            want['makeTargetDir'] <- paste0(
               "Failure trying to set write group of directory: ", dirToRemove )
            with_mock(
               `DataRepo::chgrp`= function(...) { return(NA) },
               got <- addFile( sourceFile, repo= repoDir, entry= entryPath,
                               asFile= newBaseName, writeGroup= otherGroup,
                               fileSize= sourceFileSize,
                               checksum= sourceFileMd5 )
            )
            expect_equal(got, want)

            expect_false( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
         }
      })
      it( "fails if changing permissions after create fails.", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( repoDir, entryPath, newBaseName)
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            want['makeTargetDir'] <- paste0(
               "Failure trying to set permissions of directory: ", dirToRemove )
            with_mock(
               `base::Sys.chmod`= function(...) { return(FALSE) },
               got <- addFile( sourceFile, repo= repoDir, entry= entryPath,
                               asFile= newBaseName, writeGroup= otherGroup,
                               fileSize= sourceFileSize,
                               checksum= sourceFileMd5 )
            )
            expect_equal(got, want)

            expect_false( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
         }
      })
      it( "fails if has unexpected error changing group.", {
         entryPath <- file.path( 'an', 'entry', 'path' )
         dirToRemove <- file.path( repoDir, "an")
         if (file.exists(dirToRemove)) {
            stop(paste0("Not deleting ", dirToRemove))
         }
         else {
            targetFile <- file.path( repoDir, entryPath, newBaseName)
            on.exit( unlink(dirToRemove, recursive= TRUE), add= TRUE)
            expect_false( dir.exists( dirToRemove ))
            expect_false( file.exists( targetFile ))

            want['makeTargetDir'] <- "Unexpected error: bad chgrp."
            with_mock(
               `DataRepo::chgrp`= function(...) { stop('bad chgrp.') },
               got <- addFile( sourceFile, repo= repoDir, entry= entryPath,
                               asFile= newBaseName, writeGroup= otherGroup,
                               fileSize= sourceFileSize,
                               checksum= sourceFileMd5 )
            )
            expect_equal(got, want)

            expect_false( file.exists( targetFile ))
            expect_true( dir.exists( dirToRemove ))
         }
      })
   })
   describe( "unexpected error handling.", {
      want <- c( checkParam_repo= '', checkParam_entry= '', checkParam_asFile= '',
                 checkParam_writeGroup= "", checkParam_checksumFunc= "", checkValidSource= '',
                 makeTargetDir= NA, doCopy= NA, checkCopyTarget= NA,
                 unexpectedError= "Unexpected error: Bad do.call." )

      it( "catches other uncaught errors.", {
         targetFile <- file.path( repoDir, basename(sourceFile))
         on.exit( unlink(targetFile), add= TRUE)

         with_mock(
            `base::do.call`= function(...) {stop("Bad do.call.")},
            got <- addFile( sourceFile )
         )
         expect_equal(got, want)
      })
   })
})
