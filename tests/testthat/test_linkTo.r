# Set up basic file system objects for testing
# The repo dir
repoDir <- tempfile( 'repo' )
dir.create(  repoDir  )

defaultGroup <- file.info(repoDir, extra_cols = TRUE)$grname
groupList <- groups()
groupList <- groupList[groupList != defaultGroup]
otherGroup <- groupList[1]

# Non-existing file
noSuchFile <- tempfile( 'noSuchFile' )

# Source file
sourceFile <- tempfile( 'sourceFile' )
writeLines(c("A", "three line", "file of text."), sourceFile)
sourceFileSize <- 27
sourceFileMd5 <- "2c77982c7ee2279e119c585859d92a7b"

# Repo file for source
entryDir= "abc123"
newBaseName <- basename(tempfile( 'newBaseName' ))
addFile( sourceFile, repo= repoDir, entry=entryDir, asFile= newBaseName,
        fileSize= sourceFileSize, checksum= sourceFileMd5)
targetFile <- file.path(repoDir, entryDir, newBaseName)

expect_link <- function(linkName, targetFile) {
   expect_true(file.exists(linkName))
   expect_equal(checkIsLink(linkName), "")
   expect_equal(Sys.readlink( linkName ), targetFile)
}

expect_noLink <- function(linkName) {
   expect_false(file.exists(linkName))
   expect_true( is.na( Sys.readlink( linkName )))
}

makeLink <- function(targetFile) {
   linkName <- tempfile('linkToRepo')
   expect_noLink(linkName)
   file.symlink(targetFile, linkName)
   expect_link(linkName, targetFile)
   return(linkName)
}

wantLinkToOk <- c(
   'checkParam_file'= "", 'checkParam_repo'= "", 'checkParam_entry'= "",
   'checkParam_asFile'= "", 'checkParam_checksumFunc'= "",
   'checkParam_onExists'= "", 'checkParam_duplicateOk'= "",
   'checkTarget'= "", 'checkExists'= "", 'createLink'= "",
   'unexpectedError' = ""
)

expect_LinkTo <- function( got, linkName, targetName,
   checkParam_file= "", checkParam_repo= "", checkParam_entry= "",
   checkParam_asFile= "", checkParam_checksumFunc= "",
   checkParam_onExists= "", checkParam_duplicateOk= "",
   checkTarget= "", checkExists= "", createLink= "", unexpectedError = "",
checkLink= TRUE
) {
   want <- c(
      'checkParam_file'= checkParam_file, 'checkParam_repo'= checkParam_repo,
      'checkParam_entry'= checkParam_entry,
      'checkParam_asFile'= checkParam_asFile,
      'checkParam_checksumFunc'= checkParam_checksumFunc,
      'checkParam_onExists'= checkParam_onExists,
      'checkParam_duplicateOk'= checkParam_duplicateOk,
      'checkTarget'= checkTarget, 'checkExists'= checkExists,
      'createLink'= createLink, 'unexpectedError' = unexpectedError
   )
   expect_equal( got, want )
   if (checkLink) {
      expect_link(linkName, targetName)
   }
}

context( "Setup for testing linkTo()" )
describe( "File system as set up for linkTo testing", {
   it( "has repo dir", {
      expect_true( dir.exists( repoDir ))
   })
   it( "has non-existing path", {
      expect_false( file.exists( noSuchFile ))
   })
   it( "has pre-existing source file", {
      expect_true( file.exists( sourceFile ))
      ok <- validateFile(sourceFile, checksum= sourceFileMd5, fileSize= sourceFileSize)
      expect_true( all( ok == '' ))
   })
   it( "has more than one group", {
      expect_false(defaultGroup == otherGroup)
   })
   it( "has target in repo", {
      expect_true(file.exists(targetFile))
   })
})

context( "linkTo() - no existing file" )
# Whitebox testing - this is handled without regard to duplicateOk or onExists,,
# so these are left at default values while testing file = a new, non-existing
# filename
describe( "linkTo() - file= does not exist", {
   file <- tempfile( 'noSuchFile' )   # Does not exist by contract
   setup <- function() {
      expect_false( file.exists(file) )
   }
   teardown <- function( rmLink ) {
      if (rmLink) {
         expect_equal( unlink(file), 0 )
      }
   }

   describe( "with ", {
      duplicateOk <- TRUE

      describe("duplicateOk= TRUE; onExists= 'error'", {
         duplicateOk <- TRUE; onExists <- 'error'

         it( "works: fileSize= NULL; checksum <- NULL", {
            makesLink <- TRUE; fileSize <- NULL; checksum <- NULL
            setup()
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists = onExists,
               fileSize= fileSize, checksum= checksum )
            )
            expect_LinkTo( got, file, targetFile, checkLink= makesLink )
            teardown( rmLink= makesLink )
         })
         it( "works: fileSize= NULL; checksum <- sourceFileMd5", {
            makesLink <- TRUE; fileSize <- NULL; checksum <- sourceFileMd5
            setup()
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists = onExists,
               fileSize= fileSize, checksum= checksum )
            )
            expect_LinkTo( got, file, targetFile, checkLink= makesLink)
            teardown(rmLink= makesLink)
         })
         it( "works: fileSize= sourceFileSize; checksum <- NULL", {
            makesLink <- TRUE; fileSize <- sourceFileSize; checksum <- NULL
            setup()
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists = onExists,
               fileSize= fileSize, checksum= checksum )
            )
            expect_LinkTo( got, file, targetFile, checkLink= makesLink )
            teardown( rmLink= makesLink )
         })
         it( "works: fileSize= sourceFileSize; checksum <- sourceFileMd5", {
            makesLink <- TRUE; fileSize <- sourceFileSize; checksum <- sourceFileMd5
            setup()
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists = onExists,
               fileSize= fileSize, checksum= checksum )
            )
            expect_LinkTo( got, file, targetFile, checkLink= makesLink )
            teardown( rmLink= makesLink )
         })
         it( "Reports error if has problem creating symlink", {
            makesLink <- FALSE; fileSize <- NULL; checksum <- NULL;
            setup()
            with_mock(
               `base::file.symlink`= function(...) { FALSE },
               expect_silent(got <- linkTo(
                  file, repo=repoDir, entry= entryDir, asFile= newBaseName,
                  duplicateOk= duplicateOk, onExists = onExists,
                  fileSize= fileSize, checksum= checksum )
               )
            )
            expect_LinkTo( got, file, targetFile, checkLink= makesLink,
               createLink= "Symlink not created." )
            teardown( rmLink= makesLink )
         })
      })
      describe("duplicateOk= FALSE; onExists= 'error'", {
         duplicateOk <- TRUE; onExists <- 'error'

         it( "works: fileSize= NULL; checksum <- NULL", {
            makesLink <- TRUE; fileSize <- NULL; checksum <- NULL
            setup()
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists = onExists,
               fileSize= fileSize, checksum= checksum )
            )
            expect_LinkTo( got, file, targetFile, checkLink= makesLink )
            teardown( rmLink= makesLink )
         })
         it( "works: fileSize= NULL; checksum <- sourceFileMd5", {
            makesLink <- TRUE; fileSize <- NULL; checksum <- sourceFileMd5
            setup()
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists = onExists,
               fileSize= fileSize, checksum= checksum )
            )
            expect_LinkTo( got, file, targetFile, checkLink= makesLink)
            teardown(rmLink= makesLink)
         })
         it( "works: fileSize= sourceFileSize; checksum <- NULL", {
            makesLink <- TRUE; fileSize <- sourceFileSize; checksum <- NULL
            setup()
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists = onExists,
               fileSize= fileSize, checksum= checksum )
            )
            expect_LinkTo( got, file, targetFile, checkLink= makesLink )
            teardown( rmLink= makesLink )
         })
         it( "works: fileSize= sourceFileSize; checksum <- sourceFileMd5", {
            makesLink <- TRUE; fileSize <- sourceFileSize; checksum <- sourceFileMd5
            setup()
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists = onExists,
               fileSize= fileSize, checksum= checksum )
            )
            expect_LinkTo( got, file, targetFile, checkLink= makesLink )
            teardown( rmLink= makesLink )
         })
         it( "Reports error if has problem creating symlink", {
            makesLink <- FALSE; fileSize <- NULL; checksum <- NULL;
            setup()
            with_mock(
               `base::file.symlink`= function(...) { FALSE },
               expect_silent(got <- linkTo(
                  file, repo=repoDir, entry= entryDir, asFile= newBaseName,
                  duplicateOk= duplicateOk, onExists = onExists,
                  fileSize= fileSize, checksum= checksum )
               )
            )
            expect_LinkTo( got, file, targetFile, checkLink= makesLink,
                           createLink= "Symlink not created." )
            teardown( rmLink= makesLink )
         })
      })

   })
})

context( "linkTo() - file= existing correct link" )
describe( "linkTo() - file= existing correct link", {
   file <- tempfile( 'linkToTarget' )
   describe( "duplicateOk= TRUE", {
      duplicateOk <- TRUE
      describe( "with onExists= error", {
         onExists <- 'error'

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile )
         })
      })
      describe( "with onExists= replaceFile", {
         it( "works when  is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile )
         })
      })
      describe( "with onExists= replaceAny", {
         it( "works when  is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny')
            )
            expect_LinkTo( got, file, targetFile )
         })
      })
      describe( "with onExists= backupFile", {
         it( "works when  is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
         })
      })
      describe( "with onExists= backupAny", {
         it( "works when  is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny')
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny')
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny')
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny')
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
         })
      })
      describe( "with onExists= skip", {
         it( "works when  is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'skip')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'skip')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip')
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip')
            )
            expect_LinkTo( got, file, targetFile )
         })
      })
      it( "works without creating new symlink", {
         with_mock(
            `base::file.symlink`= function(...) { stop("Oops") },
            expect_silent(got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny' ))
         )
         expect_LinkTo( got, file, targetFile )
      })
   })
   describe( "with duplicateOk= FALSE", {
      duplicateOk <- FALSE
      describe( "with onExists= error, returns failure messages...", {
         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "File exists.", createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "File exists.", createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "File exists.", createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "File exists.", createLink= NA_character_ )
         })
      })
      describe( "with onExists= replaceFile, returns failure messages...", {
         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "Deletion of pre-existing link not attempted.",
               createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "Deletion of pre-existing link not attempted.", createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "Deletion of pre-existing link not attempted.", createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "Deletion of pre-existing link not attempted.", createLink= NA_character_ )
         })
      })
      describe( "with onExists= replaceAny, ok with warning message...", {
         wantWarningRE <- paste0("Removed existing file: ", file)

         it( "works when is no target checking.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target checksum.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
         })
         it( "works when checking target checksum and size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
         })
      })
      describe( "with onExists= backupFile, returns failure messages...", {
         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "Rename of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "Rename of pre-existing link not attempted.", createLink= NA_character_ )
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "Rename of pre-existing link not attempted.", createLink= NA_character_ )
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile,
               checkExists= "Rename of pre-existing link not attempted.", createLink= NA_character_ )
            expect_false(file.exists(paste0(file, '.bak')))
         })
      })
      describe( "with onExists= backupAny, ok with warning message...", {
         backupFile <- paste0( file, '.bak')
         wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
         on.exit( unlink( backupFile, recursive= TRUE ))

         it( "works when is no target checking.", {
            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupAny, multiple bak files, ok with warning message...", {
         backupFile <- paste0( file, '.bak', '.bak')
         alreadyExistsFile <- paste0( file, '.bak')
         expect_true(file.create( alreadyExistsFile ))
         wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
         on.exit( unlink( backupFile, recursive= TRUE ))
         on.exit( unlink( alreadyExistsFile, recursive= TRUE ))

         it( "works when is no target checking.", {
            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
         })
         expect_equal(unlink( alreadyExistsFile, recursive= TRUE), 0)
      })
      describe( "with onExists= skip, ok with warning message...", {
         wantWarningRE <- paste0("Skipped linking due to existing file: ", file)

         it( "works when is no target checking.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'skip'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'skip'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, createLink= NA_character_ )
         })
      })
      it( "Reports error if problem creating symlink", {
         wantWarningRE <- paste0( "Deleted ", file, " but could not create link to ", targetFile)
         with_mock(
            `base::file.symlink`= function(...) { FALSE },
            expect_warning(got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
               ), wantWarningRE
            )
         )
         expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                        createLink= "Symlink not created." )
      })
   })

   unlink(file)
})

context( "linkTo() - existing link - incorrect" )
describe( "linkTo() - existing link - incorrect", {
   describe( "with duplicateOk= TRUE", {
      duplicateOk <- TRUE
      describe( "with onExists= error, returns failure messages...", {

         file <- makeLink(sourceFile)
         expect_true(Sys.readlink(file) != targetFile) # Link different
         on.exit( unlink(file), add= TRUE)

         it( "works when is no target checking.", {
            got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'error')

            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile) # Link not replaced
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })

         expect_equal(unlink(file), 0)
      })
      describe( "with onExists= replaceFile, returns failure messages...", {
         file <- makeLink(sourceFile)
         expect_true(Sys.readlink(file) != targetFile) # Link different
         on.exit( unlink(file), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing link not attempted.",
                           createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing link not attempted.",
                           createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing link not attempted.",
                           createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         expect_equal(unlink(file), 0)
      })
      describe( "with onExists= replaceAny, ok with warning message...", {
         it( "works when is no target checking.", {
            file <- makeLink(sourceFile)
            expect_true(Sys.readlink(file) != targetFile) # Link different
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            file <- makeLink(sourceFile)
            expect_true(Sys.readlink(file) != targetFile) # Link different
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            file <- makeLink(sourceFile)
            expect_true(Sys.readlink(file) != targetFile) # Link different
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            file <- makeLink(sourceFile)
            expect_true(Sys.readlink(file) != targetFile) # Link different
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= backupFile, returns failure messages...", {
         file <- makeLink(sourceFile)
         expect_true(Sys.readlink(file) != targetFile) # Link different
         on.exit( unlink(file), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })

         expect_equal(unlink(file), 0)
      })
      describe( "with onExists= backupAny, ok with warning message...", {
         it( "works when is no target checking.", {
            file <- makeLink(sourceFile)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            file <- makeLink(sourceFile)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            file <- makeLink(sourceFile)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            file <- makeLink(sourceFile)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupAny, multiple bak files, ok with warning message...", {
         it( "works when is no target checking.", {
            file <- makeLink(sourceFile)
            alreadyExistsFile <- paste0( file, '.bak')
            alreadyExistsFileBack <- paste0( file, '.bak', '.bak')
            expect_true(file.create( alreadyExistsFile ))
            on.exit( unlink( alreadyExistsFile, recursive= TRUE ))
            on.exit( unlink( alreadyExistsFileBack, recursive= TRUE ))
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFile, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFileBack, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            file <- makeLink(sourceFile)
            alreadyExistsFile <- paste0( file, '.bak')
            alreadyExistsFileBack <- paste0( file, '.bak', '.bak')
            expect_true(file.create( alreadyExistsFile ))
            on.exit( unlink( alreadyExistsFile, recursive= TRUE ))
            on.exit( unlink( alreadyExistsFileBack, recursive= TRUE ))
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFile, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFileBack, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            file <- makeLink(sourceFile)
            alreadyExistsFile <- paste0( file, '.bak')
            alreadyExistsFileBack <- paste0( file, '.bak', '.bak')
            expect_true(file.create( alreadyExistsFile ))
            on.exit( unlink( alreadyExistsFile, recursive= TRUE ))
            on.exit( unlink( alreadyExistsFileBack, recursive= TRUE ))
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFile, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFileBack, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            file <- makeLink(sourceFile)
            alreadyExistsFile <- paste0( file, '.bak')
            alreadyExistsFileBack <- paste0( file, '.bak', '.bak')
            expect_true(file.create( alreadyExistsFile ))
            on.exit( unlink( alreadyExistsFile, recursive= TRUE ))
            on.exit( unlink( alreadyExistsFileBack, recursive= TRUE ))
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFile, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFileBack, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= skip, ok with warning message...", {
         file <- makeLink(sourceFile)
         on.exit( unlink(file), add= TRUE)
         wantWarningRE <- paste0("Skipped linking due to existing file: ", file)

         it( "works when is no target checking.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })

         expect_equal(unlink(file), 0)
      })
      it( "Reports error if problem creating symlink, okExists= 'replaceAny'", {
         file <- makeLink(sourceFile)
         expect_true(Sys.readlink(file) != targetFile) # Link different
         on.exit( unlink(file), add= TRUE)

         wantWarningRE <- paste0( "Deleted ", file, " but could not create link to ", targetFile)
         with_mock(
            `base::file.symlink`= function(...) { FALSE },
            expect_warning(got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
         )
         expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                        createLink= "Symlink not created." )

         expect_equal(unlink(file), 0)
      })

   })
   describe( "with duplicateOk= FALSE", {
      duplicateOk <- FALSE

      describe( "with onExists= error, returns failure messages...", {
         file <- makeLink(sourceFile)
         on.exit( unlink(file), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "File exists.", createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "File exists.", createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "File exists.", createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "File exists.", createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })

         expect_equal(unlink(file), 0)
      })
      describe( "with onExists= replaceFile, returns failure messages...", {
         file <- makeLink(sourceFile)
         on.exit( unlink(file), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
         })

         expect_equal(unlink(file), 0)
      })
      describe( "with onExists= replaceAny, ok with warning message...", {
         it( "works when is no target checking.", {
            file <- makeLink(sourceFile)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            file <- makeLink(sourceFile)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            file <- makeLink(sourceFile)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            file <- makeLink(sourceFile)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= backupFile, returns failure messages...", {
         file <- makeLink(sourceFile)
         on.exit( unlink(file), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
            expect_false(file.exists(paste0(file, '.bak')))
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing link not attempted.",
               createLink= NA_character_ )
            expect_true(Sys.readlink(file) == sourceFile)
            expect_false(file.exists(paste0(file, '.bak')))
         })

         expect_equal(unlink(file), 0)
      })
      describe( "with onExists= backupAny, ok with warning message...", {
         it( "works when is no target checking.", {
            file <- makeLink(sourceFile)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            file <- makeLink(sourceFile)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            file <- makeLink(sourceFile)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            file <- makeLink(sourceFile)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupAny, multiple bak files, ok with warning message...", {
         it( "works when is no target checking.", {
            file <- makeLink(sourceFile)
            alreadyExistsFile <- paste0( file, '.bak')
            alreadyExistsFileBack <- paste0( file, '.bak', '.bak')
            expect_true(file.create( alreadyExistsFile ))
            on.exit( unlink( alreadyExistsFile, recursive= TRUE ))
            on.exit( unlink( alreadyExistsFileBack, recursive= TRUE ))
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFile, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFileBack, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            file <- makeLink(sourceFile)
            alreadyExistsFile <- paste0( file, '.bak')
            alreadyExistsFileBack <- paste0( file, '.bak', '.bak')
            expect_true(file.create( alreadyExistsFile ))
            on.exit( unlink( alreadyExistsFile, recursive= TRUE ))
            on.exit( unlink( alreadyExistsFileBack, recursive= TRUE ))
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFile, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFileBack, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            file <- makeLink(sourceFile)
            alreadyExistsFile <- paste0( file, '.bak')
            alreadyExistsFileBack <- paste0( file, '.bak', '.bak')
            expect_true(file.create( alreadyExistsFile ))
            on.exit( unlink( alreadyExistsFile, recursive= TRUE ))
            on.exit( unlink( alreadyExistsFileBack, recursive= TRUE ))
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFile, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFileBack, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            file <- makeLink(sourceFile)
            alreadyExistsFile <- paste0( file, '.bak')
            alreadyExistsFileBack <- paste0( file, '.bak', '.bak')
            expect_true(file.create( alreadyExistsFile ))
            on.exit( unlink( alreadyExistsFile, recursive= TRUE ))
            on.exit( unlink( alreadyExistsFileBack, recursive= TRUE ))
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFile, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsFileBack, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= skip, ok with warning message...", {
         file <- makeLink(sourceFile)
         on.exit( unlink(file), add= TRUE)
         wantWarningRE <- paste0("Skipped linking due to existing file: ", file)

         it( "works when is no target checking.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })

         expect_equal(unlink(file), 0)
      })
      it( "Reports error if problem creating symlink, okExists= 'replaceAny'", {
         file <- makeLink(sourceFile)
         expect_true(Sys.readlink(file) != targetFile) # Link different
         on.exit( unlink(file), add= TRUE)

         wantWarningRE <- paste0( "Deleted ", file, " but could not create link to ", targetFile)
         with_mock(
            `base::file.symlink`= function(...) { FALSE },
            expect_warning(got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
         )
         expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                        createLink= "Symlink not created." )

         expect_equal(unlink(file), 0)
      })
   })
})

context( "linkTo() - existing file - duplicate" )
describe( "linkTo() - existing file - duplicate", {
   file <- tempfile('copyOfSource')

   describe( "with duplicateOk= TRUE", {
      duplicateOk <- TRUE
      describe( "with onExists= error", {
         onExists <- 'error'
         it( "works when is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= replaceFile", {
         onExists <- 'replaceFile'
         it( "works when  is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= replaceAny", {
         onExists <- 'replaceAny'
         it( "works when  is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= backupFile", {
         onExists <- 'backupFile'
         it( "works when  is no target checking.", {
            expect_true(file.copy(sourceFile, file))

            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_false(file.exists(paste0(file, '.bak')))
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= backupAny", {
         onExists <- 'backupAny'
         it( "works when  is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= skip", {
         onExists <- 'skip'
         it( "works when is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      it( "Reports error if problem creating symlink, okExists= 'replaceFile'", {
         expect_true(file.copy(sourceFile, file))
         on.exit( unlink(file), add= TRUE)

         wantWarningRE <- paste0( "Deleted ", file, " but could not create link to ", targetFile)
         with_mock(
            `base::file.symlink`= function(...) { FALSE },
            expect_warning(got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile'
            ), wantWarningRE
            )
         )
         expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                        createLink= "Symlink not created." )

         expect_equal(unlink(file), 0)
      })
   })
   describe( "with duplicateOk= FALSE", {
      duplicateOk <- FALSE
      describe( "with onExists= error, returns failure messages...", {
         onExists <- 'error'
         expect_true(file.copy(sourceFile, file))
         on.exit( unlink(file), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists)
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })

         expect_equal(unlink(file), 0)
      })
      describe( "with onExists= replaceFile, ok with warning message...", {
         onExists <- 'replaceFile'
         it( "works when is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= replaceAny, ok with warning message...", {
         onExists <- 'replaceAny'
         it( "works when is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= backupFile, ok with warning message...", {
         onExists <- 'backupFile'
         it( "works when is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupFile, multiple bak files, ok with warning message...", {
         onExists <- 'backupFile'
         alreadyExistsFile <- paste0( file, '.bak')
         expect_true(file.create( alreadyExistsFile ))
         on.exit( unlink( alreadyExistsFile, recursive= TRUE ))

         it( "works when is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })

         expect_equal(unlink( alreadyExistsFile ), 0)
      })
      describe( "with onExists= backupAny, ok with warning message...", {
         onExists <- 'backupAny'
         it( "works when is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupAny, multiple bak files, ok with warning message...", {
         onExists <- 'backupAny'
         alreadyExistsFile <- paste0( file, '.bak')
         expect_true(file.create( alreadyExistsFile ))
         on.exit( unlink( alreadyExistsFile, recursive= TRUE ))

         it( "works when is no target checking.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.copy(sourceFile, file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })

         expect_equal(unlink( alreadyExistsFile ), 0)
      })
      describe( "with onExists= skip, ok with warning message...", {
         onExists <- 'skip'
         expect_true(file.copy(sourceFile, file))
         on.exit( unlink(file), add= TRUE)
         wantWarningRE <- paste0("Skipped linking due to existing file: ", file)

         it( "works when is no target checking.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= onExists
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })

         expect_equal(unlink(file), 0)
      })
      it( "Reports error if problem creating symlink, okExists= 'replaceFile'", {
         expect_true(file.copy(sourceFile, file))
         on.exit( unlink(file), add= TRUE)

         wantWarningRE <- paste0( "Deleted ", file, " but could not create link to ", targetFile)
         with_mock(
            `base::file.symlink`= function(...) { FALSE },
            expect_warning(got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile'
            ), wantWarningRE
            )
         )
         expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                        createLink= "Symlink not created." )

         expect_equal(unlink(file), 0)
      })

   })
})

context( "linkTo() - existing file - different" )
describe( "linkTo() - existing file - different", {
   file <- tempfile('notDuplicateFile')

   describe( "with duplicateOk= TRUE", {
      duplicateOk <- TRUE
      describe( "with onExists= error, returns failure messages...", {
         expect_true(file.create(file))
         on.exit( unlink(file), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })

         expect_equal(unlink(file), 0)
      })
      describe( "with onExists= replaceFile, ok with warning message...", {
         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= replaceAny, ok with warning message...", {
         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= backupFile, ok with warning message...", {
         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupFile, multiple bak files, ok with warning message...", {
         alreadyExistsFile <- paste0( file, '.bak')
         expect_true(file.create( alreadyExistsFile ))
         on.exit( unlink( alreadyExistsFile, recursive= TRUE ))

         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         expect_equal(unlink( alreadyExistsFile ), 0)
      })
      describe( "with onExists= backupAny, ok with warning message...", {
         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupAny, multiple bak files, ok with warning message...", {
         alreadyExistsFile <- paste0( file, '.bak')
         expect_true(file.create( alreadyExistsFile ))
         on.exit( unlink( alreadyExistsFile, recursive= TRUE ))

         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })

         expect_equal(unlink( alreadyExistsFile ), 0)
      })
      describe( "with onExists= skip, ok with warning message...", {
         expect_true(file.create(file))
         on.exit( unlink(file), add= TRUE)
         wantWarningRE <- paste0("Skipped linking due to existing file: ", file)

         it( "works when is no target checking.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })

         expect_equal(unlink(file), 0)
      })
      it( "Reports error if problem creating symlink, okExists= 'backupFile'", {
         expect_true(file.create(file))
         backupFile <- paste0( file, '.bak')
         on.exit( unlink(file), add= TRUE)

         wantWarningRE <- paste0( "Backed up ", file, " but could not create link to ", targetFile)
         with_mock(
            `base::file.symlink`= function(...) { FALSE },
            expect_warning(got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
         )
         expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                        createLink= "Symlink not created." )

         expect_true(file.exists( backupFile ))
         expect_equal(unlink( backupFile, recursive= TRUE), 0)
         expect_equal(unlink(file), 0)
      })
   })
   describe( "with duplicateOk= FALSE", {
      duplicateOk <- FALSE
      describe( "with onExists= error, returns failure messages...", {
         expect_true(file.create(file))
         on.exit( unlink(file), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
            expect_equal(Sys.readlink(file), "")
         })

         expect_equal(unlink(file), 0)
      })
      describe( "with onExists= replaceFile, ok with warning message...", {
         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= replaceAny, ok with warning message...", {
         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= backupFile, ok with warning message...", {
         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupFile, multiple bak files, ok with warning message...", {
         alreadyExistsFile <- paste0( file, '.bak')
         expect_true(file.create( alreadyExistsFile ))
         on.exit( unlink( alreadyExistsFile, recursive= TRUE ))

         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         expect_equal(unlink( alreadyExistsFile ), 0)
      })
      describe( "with onExists= backupAny, ok with warning message...", {
         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupAny, multiple bak files, ok with warning message...", {
         alreadyExistsFile <- paste0( file, '.bak')
         expect_true(file.create( alreadyExistsFile ))
         on.exit( unlink( alreadyExistsFile, recursive= TRUE ))

         it( "works when is no target checking.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(file.create(file))
            on.exit( unlink(file), add= TRUE)
            backupFile <- paste0( file, '.bak', '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })

         expect_equal(unlink( alreadyExistsFile ), 0)
      })
      describe( "with onExists= skip, ok with warning message...", {
         expect_true(file.create(file))
         on.exit( unlink(file), add= TRUE)
         wantWarningRE <- paste0("Skipped linking due to existing file: ", file)

         it( "works when is no target checking.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })

         expect_equal(unlink(file), 0)
      })
      it( "Reports error if problem creating symlink, okExists= 'backupFile'", {
         expect_true(file.create(file))
         on.exit( unlink(file), add= TRUE)
         backupFile <- paste0( file, '.bak')

         wantWarningRE <- paste0( "Backed up ", file, " but could not create link to ", targetFile)
         with_mock(
            `base::file.symlink`= function(...) { FALSE },
            expect_warning(got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
         )
         expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                        createLink= "Symlink not created." )

         expect_equal(unlink(file), 0)
         expect_true(file.exists( backupFile ))
         expect_equal(unlink( backupFile, recursive= TRUE), 0)
      })
   })
})

context( "linkTo() - existing dir" )
describe( "linkTo() - existing dir", {
   file <- tempfile('bad_dir')
   describe( "with duplicateOk= TRUE", {
      duplicateOk <- TRUE
      describe( "with onExists= error, returns failure messages...", {
         dir.create(file)
         on.exit( unlink(file, recursive= TRUE), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
         })

         expect_equal(unlink(file, recursive= TRUE), 0)
      })
      describe( "with onExists= replaceFile, returns failure messages...", {
         dir.create(file)
         on.exit( unlink(file, recursive= TRUE), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })

         expect_equal(unlink(file, recursive= TRUE), 0)
      })
      describe( "with onExists= replaceAny, ok with warning message...", {

         it( "works when is no target checking.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= backupFile, returns failure messages...", {
         dir.create(file)
         on.exit( unlink(file, recursive= TRUE), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })

         expect_equal(unlink(file, recursive= TRUE), 0)
      })
      describe( "with onExists= backupAny, ok with warning message...", {

         it( "works when is no target checking.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupAny, multiple bak files, ok with warning message...", {

         it( "works when is no target checking.", {
            expect_true(dir.create( file ))
            on.exit( unlink(file, recursive= TRUE), add= TRUE)

            alreadyExistsDir <- paste0( file, '.bak')
            expect_true(dir.create( alreadyExistsDir ))
            on.exit( unlink( alreadyExistsDir, recursive= TRUE ))

            backupFile <- paste0( file, '.bak', '.bak')
            expect_true(! file.exists( backupFile ))
            on.exit( unlink( backupFile, recursive= TRUE ))

            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)

            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsDir, recursive= TRUE ), 0)
         })
         it( "works when checking target size.", {
            expect_true(dir.create( file ))
            on.exit( unlink(file, recursive= TRUE), add= TRUE)

            alreadyExistsDir <- paste0( file, '.bak')
            expect_true(dir.create( alreadyExistsDir ))
            on.exit( unlink( alreadyExistsDir, recursive= TRUE ))

            backupFile <- paste0( file, '.bak', '.bak')
            expect_true(! file.exists( backupFile ))
            on.exit( unlink( backupFile, recursive= TRUE ))

            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)

            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsDir, recursive= TRUE ), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(dir.create( file ))
            on.exit( unlink(file, recursive= TRUE), add= TRUE)

            alreadyExistsDir <- paste0( file, '.bak')
            expect_true(dir.create( alreadyExistsDir ))
            on.exit( unlink( alreadyExistsDir, recursive= TRUE ))

            backupFile <- paste0( file, '.bak', '.bak')
            expect_true(! file.exists( backupFile ))
            on.exit( unlink( backupFile, recursive= TRUE ))

            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)

            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsDir, recursive= TRUE ), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(dir.create( file ))
            on.exit( unlink(file, recursive= TRUE), add= TRUE)

            alreadyExistsDir <- paste0( file, '.bak')
            expect_true(dir.create( alreadyExistsDir ))
            on.exit( unlink( alreadyExistsDir, recursive= TRUE ))

            backupFile <- paste0( file, '.bak', '.bak')
            expect_true(! file.exists( backupFile ))
            on.exit( unlink( backupFile, recursive= TRUE ))

            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)

            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsDir, recursive= TRUE ), 0)
         })
      })
      describe( "with onExists= skip, ok with warning message...", {
         dir.create(file)
         on.exit( unlink(file, recursive= TRUE), add= TRUE)
         wantWarningRE <- paste0("Skipped linking due to existing file: ", file)

         it( "works when is no target checking.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })

         expect_equal(unlink(file, recursive= TRUE), 0)
      })
      it( "Reports error if problem creating symlink, okExists= 'backupAny'", {
         dir.create(file)
         on.exit( unlink(file, recursive= TRUE), add= TRUE)
         backupFile <- paste0( file, '.bak')
         on.exit( unlink( backupFile, recursive= TRUE ))

         wantWarningRE <- paste0( "Backed up ", file, " but could not create link to ", targetFile)
         with_mock(
            `base::file.symlink`= function(...) { FALSE },
            expect_warning(got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
         )
         expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                        createLink= "Symlink not created." )

         expect_equal(unlink(file, recursive= TRUE), 0)
         expect_true(dir.exists( backupFile ))
         expect_equal(unlink( backupFile, recursive= TRUE), 0)
      })

   })
   describe( "with duplicateOk= FALSE", {
      duplicateOk <- FALSE
      describe( "with onExists= error, returns failure messages...", {
         dir.create(file)
         on.exit( unlink(file, recursive= TRUE), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'error')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "File exists.", createLink= NA_character_ )
         })

         expect_equal(unlink(file, recursive= TRUE), 0)
      })
      describe( "with onExists= replaceFile, returns failure messages...", {
         dir.create(file)
         on.exit( unlink(file, recursive= TRUE), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing directory not attempted.",
               createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing directory not attempted.",
               createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing directory not attempted.",
               createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing directory not attempted.",
               createLink= NA_character_ )
         })

         expect_equal(unlink(file, recursive= TRUE), 0)
      })
      describe( "with onExists= replaceAny, ok with warning message...", {

         it( "works when is no target checking.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target size.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
              ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
         it( "works when checking target checksum and size.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            wantWarningRE <- paste0("Removed existing file: ", file)
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_equal(unlink(file), 0)
         })
      })
      describe( "with onExists= backupFile, returns failure messages...", {
         dir.create(file)
         on.exit( unlink(file, recursive= TRUE), add= TRUE)

         it( "works when is no target checking.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing directory not attempted.",
               createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing directory not attempted.",
               createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing directory not attempted.",
               createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile')
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing directory not attempted.",
               createLink= NA_character_ )
         })

         expect_equal(unlink(file, recursive= TRUE), 0)
      })
      describe( "with onExists= backupAny, ok with warning message...", {

         it( "works when is no target checking.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target size.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
         it( "works when checking target checksum and size.", {
            dir.create(file)
            on.exit( unlink(file, recursive= TRUE), add= TRUE)
            backupFile <- paste0( file, '.bak')
            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)
            on.exit( unlink( backupFile, recursive= TRUE ))

            expect_true(! file.exists( backupFile ))
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
         })
      })
      describe( "with onExists= backupAny, multiple bak files, ok with warning message...", {

         it( "works when is no target checking.", {
            expect_true(dir.create( file ))
            on.exit( unlink(file, recursive= TRUE), add= TRUE)

            alreadyExistsDir <- paste0( file, '.bak')
            expect_true(dir.create( alreadyExistsDir ))
            on.exit( unlink( alreadyExistsDir, recursive= TRUE ))

            backupFile <- paste0( file, '.bak', '.bak')
            expect_true(! file.exists( backupFile ))
            on.exit( unlink( backupFile, recursive= TRUE ))

            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)

            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsDir, recursive= TRUE ), 0)
         })
         it( "works when checking target size.", {
            expect_true(dir.create( file ))
            on.exit( unlink(file, recursive= TRUE), add= TRUE)

            alreadyExistsDir <- paste0( file, '.bak')
            expect_true(dir.create( alreadyExistsDir ))
            on.exit( unlink( alreadyExistsDir, recursive= TRUE ))

            backupFile <- paste0( file, '.bak', '.bak')
            expect_true(! file.exists( backupFile ))
            on.exit( unlink( backupFile, recursive= TRUE ))

            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)

            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsDir, recursive= TRUE ), 0)
         })
         it( "works when checking target checksum.", {
            expect_true(dir.create( file ))
            on.exit( unlink(file, recursive= TRUE), add= TRUE)

            alreadyExistsDir <- paste0( file, '.bak')
            expect_true(dir.create( alreadyExistsDir ))
            on.exit( unlink( alreadyExistsDir, recursive= TRUE ))

            backupFile <- paste0( file, '.bak', '.bak')
            expect_true(! file.exists( backupFile ))
            on.exit( unlink( backupFile, recursive= TRUE ))

            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)

            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsDir, recursive= TRUE ), 0)
         })
         it( "works when checking target checksum and size.", {
            expect_true(dir.create( file ))
            on.exit( unlink(file, recursive= TRUE), add= TRUE)

            alreadyExistsDir <- paste0( file, '.bak')
            expect_true(dir.create( alreadyExistsDir ))
            on.exit( unlink( alreadyExistsDir, recursive= TRUE ))

            backupFile <- paste0( file, '.bak', '.bak')
            expect_true(! file.exists( backupFile ))
            on.exit( unlink( backupFile, recursive= TRUE ))

            wantWarningRE <- paste0("Renamed existing file ", file, " to ", backupFile)

            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile )
            expect_true(file.exists( backupFile ))
            expect_equal(unlink( backupFile, recursive= TRUE), 0)
            expect_equal(unlink( file, recursive= TRUE), 0)
            expect_equal(unlink( alreadyExistsDir, recursive= TRUE ), 0)
         })
      })
      describe( "with onExists= skip, ok with warning message...", {
         dir.create(file)
         on.exit( unlink(file, recursive= TRUE), add= TRUE)
         wantWarningRE <- paste0("Skipped linking due to existing file: ", file)

         it( "works when is no target checking.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })
         it( "works when checking target checksum and size.", {
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'skip'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           createLink= NA_character_ )
         })

         expect_equal(unlink(file, recursive= TRUE), 0)
      })
      it( "Reports error if problem creating symlink, okExists= 'backupAny'", {
         dir.create(file)
         on.exit( unlink(file, recursive= TRUE), add= TRUE)
         backupFile <- paste0( file, '.bak')
         on.exit( unlink( backupFile, recursive= TRUE ))

         wantWarningRE <- paste0( "Backed up ", file, " but could not create link to ", targetFile)
         with_mock(
            `base::file.symlink`= function(...) { FALSE },
            expect_warning(got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
         )
         expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                        createLink= "Symlink not created." )

         expect_equal(unlink(file, recursive= TRUE), 0)
         expect_true(dir.exists( backupFile ))
         expect_equal(unlink( backupFile, recursive= TRUE), 0)
      })
   })
})

context( "linkTo() reports errors" )
describe( "Bad parameters", {
   it( "catches bad file= parameters", {

      want <- c(checkParam_file= "Not a vector of mode character.")

      got <- linkTo( file= NULL )
      expect_equal(got['checkParam_file'], want)

      got <- linkTo( file= list() )
      expect_equal(got['checkParam_file'], want)

      got <- linkTo( file= NA )
      expect_equal(got['checkParam_file'], want)

      want <- c(checkParam_file= "Length is not 1.")
      got <- linkTo( file= c( 'a', 'b' ))
      expect_equal(got['checkParam_file'], want)

      want <- c(checkParam_file= "Character count is not between 1 and Inf.")
      got <- linkTo( file= "" )
      expect_equal(got['checkParam_file'], want)
   })
   it( "catches bad repo= parameters", {
      want <- c(checkParam_repo= "Not a vector of mode character.")

      got <- linkTo( file= noSuchFile, repo= NULL )
      expect_equal(got['checkParam_repo'], want)

      got <- linkTo( file= noSuchFile, repo= list() )
      expect_equal(got['checkParam_repo'], want)

      got <- linkTo( file= noSuchFile, repo= NA )
      expect_equal(got['checkParam_repo'], want)

      want <- c(checkParam_repo= "Length is not 1.")
      got <- linkTo( file= noSuchFile, repo= c( 'a', 'b' ))
      expect_equal(got['checkParam_repo'], want)

      want <- c(checkParam_repo= "Character count is not between 1 and Inf.")
      got <- linkTo( file= noSuchFile, repo= "" )
      expect_equal(got['checkParam_repo'], want)

      want <- c(checkParam_repo= "No such path.")
      got <- linkTo( file= noSuchFile, repo= noSuchFile )
      expect_equal(got['checkParam_repo'], want)

      want <- c(checkParam_repo= "Not a directory.")
      got <- linkTo( file= noSuchFile, repo= sourceFile )
      expect_equal(got['checkParam_repo'], want)

   })
   it( "catches bad entry= parameters", {
      want <- c(checkParam_entry= "Not a vector of mode character.")

      got <- linkTo( file= noSuchFile, repo= repoDir, entry= list() )
      expect_equal(got['checkParam_entry'], want)

      got <- linkTo( file= noSuchFile, repo= repoDir, entry= NA )
      expect_equal(got['checkParam_entry'], want)

      want <- c(checkParam_entry= "Length is not 1.")
      got <- linkTo( file= noSuchFile, repo= repoDir, entry= c('a', 'b') )
      expect_equal(got['checkParam_entry'], want)

      want <- c(checkParam_entry= "Character count is not between 1 and Inf.")
      got <- linkTo( file= noSuchFile, repo= repoDir, entry= "" )
      expect_equal(got['checkParam_entry'], want)
   })
   it( "catches bad asFile= parameters", {
      want <- c(checkParam_asFile= "Not a vector of mode character.")

      got <- linkTo( file= noSuchFile, asFile= NULL )
      expect_equal(got['checkParam_asFile'], want)

      got <- linkTo( file= noSuchFile, repo= repoDir, asFile= list() )
      expect_equal(got['checkParam_asFile'], want)

      got <- linkTo( file= noSuchFile, asFile= NA )
      expect_equal(got['checkParam_asFile'], want)

      want <- c(checkParam_asFile= "Length is not 1.")
      got <- linkTo( file= noSuchFile, repo= repoDir, asFile= c('a', 'b') )
      expect_equal(got['checkParam_asFile'], want)

      want <- c(checkParam_asFile= "Character count is not between 1 and Inf.")
      got <- linkTo( file= noSuchFile, asFile= "" )
      expect_equal(got['checkParam_asFile'], want)

      want <- c(checkParam_asFile= "Path elements not ok.")
      got <- linkTo( file= noSuchFile, repo= repoDir, asFile= noSuchFile )
      expect_equal(got['checkParam_asFile'], want)
   })
   it( "catches bad checksumFunc= parameters", {
      want <- c(checkParam_checksumFunc= "Is null.")
      got <- linkTo( file= sourceFile, repoDir, checksumFunc= NULL )
      expect_equal(got['checkParam_checksumFunc'], want)

      want <- c(checkParam_checksumFunc= "Character count is not between 1 and Inf.")
      got <- linkTo( file= sourceFile, repo= repoDir,
                      checksumFunc= "" )
      expect_equal(got['checkParam_checksumFunc'], want)

      want <- c(checkParam_checksumFunc= "Not a vector of mode character.")
      got <- linkTo( file= sourceFile, repo= repoDir, checksumFunc= NA )
      expect_equal(got['checkParam_checksumFunc'], want)
   })
   it( "catches bad onExists= parameters", {
      want <- c(checkParam_onExists= "Not a vector of mode character.")

      got <- linkTo( file= noSuchFile, onExists= NULL )
      expect_equal(got['checkParam_onExists'], want)

      got <- linkTo( file= noSuchFile, onExists= list() )
      expect_equal(got['checkParam_onExists'], want)

      got <- linkTo( file= noSuchFile, onExists= NA )
      expect_equal(got['checkParam_onExists'], want)

      want <- c(checkParam_onExists= "Length is not 1.")
      got <- linkTo( file= noSuchFile, onExists= c( 'a', 'b' ))
      expect_equal(got['checkParam_onExists'], want)

      want <- c(checkParam_onExists= "Some element is not in the checklist.")

      got <- linkTo( file= noSuchFile, onExists= "" )
      expect_equal(got['checkParam_onExists'], want)

      got <- linkTo( file= noSuchFile, onExists= "NA" )
      expect_equal(got['checkParam_onExists'], want)
   })
   it( "catches bad duplicateOk= parameters", {
      want <- c(checkParam_duplicateOk= "Not a vector of mode logical.")

      got <- linkTo( file= noSuchFile, duplicateOk= NULL )
      expect_equal(got['checkParam_duplicateOk'], want)

      got <- linkTo( file= noSuchFile, duplicateOk= list() )
      expect_equal(got['checkParam_duplicateOk'], want)

      got <- linkTo( file= noSuchFile, duplicateOk= "NA" )
      expect_equal(got['checkParam_duplicateOk'], want)

      want <- c(checkParam_duplicateOk= "Length is not 1.")
      got <- linkTo( file= noSuchFile, duplicateOk= c( TRUE, F ))
      expect_equal(got['checkParam_duplicateOk'], want)

      want <- c(checkParam_duplicateOk= "NA not ok.")
      got <- linkTo( file= noSuchFile, duplicateOk= NA)
      expect_equal(got['checkParam_duplicateOk'], want)
   })
})
describe( "Bad target", {
   describe( "Without initial source file", {
      it('catches missing path', {
         want <- c( checkTarget=
            "checkIsFile = No such path.; checkIsNotLink = NA not ok." )
         got <- linkTo( noSuchFile, repo= dirname(noSuchFile) )
         expect_equal(got['checkTarget'], want)
      })
      it('catches bad size targets, with and without checksums', {
         want <- c( checkTarget= paste0(
            "checkFileSizeMatches = File size mismatch. Found ",
            sourceFileSize, ' wanted ', sourceFileSize + 100,
            ".; checkChecksumMatches = NA not ok." ))
         got <- linkTo( noSuchFile, repo= repoDir, entry= entryDir, asFile= newBaseName,
                         fileSize= sourceFileSize + 100, checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)

         want <- c( checkTarget= paste0(
            "checkFileSizeMatches = File size mismatch. Found ",
            sourceFileSize, ' wanted ', sourceFileSize + 100, "." ))
         got <- linkTo( noSuchFile, repo= repoDir, entry= entryDir, asFile= newBaseName,
                         fileSize= sourceFileSize + 100, checksum= NULL )
         expect_equal(got['checkTarget'], want)
      })
      it('catches bad checksum files, with and without good file size checks', {
         want <- c( checkTarget= paste0(
            "checkChecksumMatches = Checksum mismatch. Found ",
            sourceFileMd5, " wanted MyBad." ))
         got <- linkTo( noSuchFile, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize, checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)

         want <- c( checkTarget= paste0(
            "checkChecksumMatches = Checksum mismatch. Found ",
            sourceFileMd5, " wanted MyBad."))
         got <- linkTo( noSuchFile, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)
      })
   })
   describe( "With initial (correct) source link", {
      file <- makeLink(targetFile)
      expect_true(file.exists(file))
      expect_equal(Sys.readlink(file), targetFile)
      on.exit(unlink(file), add= TRUE)

      it('catches missing path', {
         want <- c( checkTarget=
                       "checkIsFile = No such path.; checkIsNotLink = NA not ok." )
         got <- linkTo( file, repo= dirname(file), asFile='noSuchFile.deleteMe' )
         expect_equal(got['checkTarget'], want)
      })
      it('catches bad size targets, with and without checksums', {
         want <- c( checkTarget= paste0(
            "checkFileSizeMatches = File size mismatch. Found ",
            sourceFileSize, ' wanted ', sourceFileSize + 100,
            ".; checkChecksumMatches = NA not ok." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize + 100, checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)

         want <- c( checkTarget= paste0(
            "checkFileSizeMatches = File size mismatch. Found ",
            sourceFileSize, ' wanted ', sourceFileSize + 100, "." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize + 100, checksum= NULL )
         expect_equal(got['checkTarget'], want)
      })
      it('catches bad checksum files, with and without good file size checks', {
         want <- c( checkTarget= paste0(
            "checkChecksumMatches = Checksum mismatch. Found ",
            sourceFileMd5, " wanted MyBad." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize, checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)

         want <- c( checkTarget= paste0(
            "checkChecksumMatches = Checksum mismatch. Found ",
            sourceFileMd5, " wanted MyBad."))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)
      })

      expect_equal(unlink(file), 0)
   })
   describe( "With initial (correct) file", {
      file <- tempfile("copyOfSource")
      expect_true(file.copy(sourceFile, file))
      on.exit(unlink(file), add= TRUE)

      it('catches missing path', {
         want <- c( checkTarget=
                       "checkIsFile = No such path.; checkIsNotLink = NA not ok." )
         got <- linkTo( file, repo= dirname(file), asFile='noSuchFile.deleteMe' )
         expect_equal(got['checkTarget'], want)
      })
      it('catches bad size targets, with and without checksums', {
         want <- c( checkTarget= paste0(
            "checkFileSizeMatches = File size mismatch. Found ",
            sourceFileSize, ' wanted ', sourceFileSize + 100,
            ".; checkChecksumMatches = NA not ok." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize + 100, checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)

         want <- c( checkTarget= paste0(
            "checkFileSizeMatches = File size mismatch. Found ",
            sourceFileSize, ' wanted ', sourceFileSize + 100, "." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize + 100, checksum= NULL )
         expect_equal(got['checkTarget'], want)
      })
      it('catches bad checksum files, with and without good file size checks', {
         want <- c( checkTarget= paste0(
            "checkChecksumMatches = Checksum mismatch. Found ",
            sourceFileMd5, " wanted MyBad." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize, checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)

         want <- c( checkTarget= paste0(
            "checkChecksumMatches = Checksum mismatch. Found ",
            sourceFileMd5, " wanted MyBad."))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)
      })

      expect_equal(unlink(file), 0)
   })
   describe( "With initial (bad) source link", {
      emptyFile <- tempfile('emptyFile')
      expect_true(file.create(emptyFile))
      on.exit(unlink(emptyFile), add= TRUE)

      file <- makeLink(emptyFile)
      on.exit(unlink(file), add= TRUE)
      expect_true(file.exists(file))
      expect_equal(Sys.readlink(file), emptyFile)

      it('catches missing path', {
         want <- c( checkTarget=
                       "checkIsFile = No such path.; checkIsNotLink = NA not ok." )
         got <- linkTo( file, repo= dirname(file), asFile='noSuchFile.deleteMe' )
         expect_equal(got['checkTarget'], want)
      })
      it('catches bad size targets, with and without checksums', {
         want <- c( checkTarget= paste0(
            "checkFileSizeMatches = File size mismatch. Found ",
            sourceFileSize, ' wanted ', sourceFileSize + 100,
            ".; checkChecksumMatches = NA not ok." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize + 100, checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)

         want <- c( checkTarget= paste0(
            "checkFileSizeMatches = File size mismatch. Found ",
            sourceFileSize, ' wanted ', sourceFileSize + 100, "." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize + 100, checksum= NULL )
         expect_equal(got['checkTarget'], want)
      })
      it('catches bad checksum files, with and without good file size checks', {
         want <- c( checkTarget= paste0(
            "checkChecksumMatches = Checksum mismatch. Found ",
            sourceFileMd5, " wanted MyBad." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize, checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)

         want <- c( checkTarget= paste0(
            "checkChecksumMatches = Checksum mismatch. Found ",
            sourceFileMd5, " wanted MyBad."))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)
      })

      expect_equal(unlink(file), 0)
      expect_equal(unlink(emptyFile), 0)
   })
   describe( "With initial (bad) file", {
      file <- tempfile('emptyFile')
      expect_true(file.create(file))
      on.exit(unlink(file), add= TRUE)

      it('catches missing path', {
         want <- c( checkTarget=
                       "checkIsFile = No such path.; checkIsNotLink = NA not ok." )
         got <- linkTo( file, repo= dirname(file), asFile='noSuchFile.deleteMe' )
         expect_equal(got['checkTarget'], want)
      })
      it('catches bad size targets, with and without checksums', {
         want <- c( checkTarget= paste0(
            "checkFileSizeMatches = File size mismatch. Found ",
            sourceFileSize, ' wanted ', sourceFileSize + 100,
            ".; checkChecksumMatches = NA not ok." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize + 100, checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)

         want <- c( checkTarget= paste0(
            "checkFileSizeMatches = File size mismatch. Found ",
            sourceFileSize, ' wanted ', sourceFileSize + 100, "." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize + 100, checksum= NULL )
         expect_equal(got['checkTarget'], want)
      })
      it('catches bad checksum files, with and without good file size checks', {
         want <- c( checkTarget= paste0(
            "checkChecksumMatches = Checksum mismatch. Found ",
            sourceFileMd5, " wanted MyBad." ))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        fileSize= sourceFileSize, checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)

         want <- c( checkTarget= paste0(
            "checkChecksumMatches = Checksum mismatch. Found ",
            sourceFileMd5, " wanted MyBad."))
         got <- linkTo( file, repo= repoDir, entry= entryDir, asFile= newBaseName,
                        checksum= 'MyBad' )
         expect_equal(got['checkTarget'], want)
      })

      expect_equal(unlink(file), 0)
   })
})
describe( "Problems deleting file objects", {
   describe( "un-deleteable duplicate file", {
      noWriteDir <-  tempfile('noWriteDir')
      expect_true(dir.create(noWriteDir))
      file <- file.path(noWriteDir, 'copyOfSource.txt')
      expect_true(file.copy(sourceFile, file))
      dirMode <- file.info(noWriteDir)$mode
      expect_true(Sys.chmod(noWriteDir, mode = "555", use_umask = FALSE))
      on.exit(Sys.chmod(noWriteDir, mode = dirMode, use_umask = FALSE), add= TRUE)
      on.exit(unlink(file), add= TRUE)

      describe( "Errors and warnings if duplicateOk= TRUE", {
         duplicateOk <- TRUE
         it( "Fails with onExists= replaceFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing file failed.",
               createLink= NA_character_ )
         })
         it( "Fails with onExists= replaceAny.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing file failed.",
               createLink= NA_character_ )
         })

         # As duplicate is true, delete rather than backup attempted.
         it( "Fails with onExists= backupFile", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing file failed.",
               createLink= NA_character_ )
         })
         it( "Fails with onExists= backupAny", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing file failed.",
               createLink= NA_character_ )
         })
      })
      describe( "Errors and warnings if duplicateOk= FALSE", {
         duplicateOk <- FALSE
         it( "Fails with onExists= replaceFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing file failed.",
               createLink= NA_character_ )
         })
         it( "Fails with onExists= replaceAny.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Deletion of pre-existing file failed.",
               createLink= NA_character_ )
         })
         it( "Fails with onExists= backupFile.", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing file failed.",
               createLink= NA_character_ )
         })
         it( "Fails with onExists= backupAny.", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing file failed.",
               createLink= NA_character_ )
         })
      })

      Sys.chmod(noWriteDir, mode = dirMode, use_umask = FALSE)
      expect_equal(unlink(file), 0)
   })
   describe( "un-deleteable different file", {
      noWriteDir <-  tempfile('noWriteDir')
      expect_true(dir.create(noWriteDir))
      file <- file.path(noWriteDir, 'noSuchFile.txt')
      expect_true(file.create(file))
      dirMode <- file.info(noWriteDir)$mode
      expect_true(Sys.chmod(noWriteDir, mode = "555", use_umask = FALSE))
      on.exit(Sys.chmod(noWriteDir, mode = dirMode, use_umask = FALSE), add= TRUE)
      on.exit(unlink(file), add= TRUE)

      describe( "Errors and warnings if duplicateOk= TRUE", {
         duplicateOk <- TRUE
         it( "Fails with onExists= replaceFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing file failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= replaceAny.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing file failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupFile", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing file failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupAny", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing file failed.",
                           createLink= NA_character_ )
         })
      })
      describe( "Errors and warnings if duplicateOk= FALSE", {
         duplicateOk <- FALSE
         it( "Fails with onExists= replaceFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing file failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= replaceAny.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing file failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupFile.", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing file failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupAny.", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing file failed.",
                           createLink= NA_character_ )
         })
      })

      Sys.chmod(noWriteDir, mode = dirMode, use_umask = FALSE)
      expect_equal(unlink(file), 0)
   })
   describe( "un-deleteable correct link", {
      noWriteDir <- tempfile('noWriteDir')
      expect_true( dir.create( noWriteDir ))
      file <- file.path(noWriteDir, 'correctLink')
      expect_true(file.symlink(targetFile, file))
      dirMode <- file.info(noWriteDir)$mode
      expect_true(Sys.chmod(noWriteDir, mode = "555", use_umask = FALSE))
      on.exit(Sys.chmod(noWriteDir, mode = dirMode, use_umask = FALSE), add= TRUE)
      on.exit(unlink(file), add= TRUE)

      describe( "Works with duplicateOk= TRUE", {
         duplicateOk <- TRUE
         it( "Works with onExists= replaceFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile'))
            expect_LinkTo( got, file, targetFile)
         })
         it( "Fails with onExists= replaceAny.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'))
            expect_LinkTo( got, file, targetFile)
         })

         # As duplicate is true, delete rather than backup attempted.
         it( "Fails with onExists= backupFile", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'))
            expect_LinkTo( got, file, targetFile)
         })
         it( "Fails with onExists= backupAny", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'))
            expect_LinkTo( got, file, targetFile)
         })
      })
      describe( "Errors and warnings if duplicateOk= FALSE", {
         duplicateOk <- FALSE
         it( "Fails with onExists= replaceFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing link not attempted.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= replaceAny.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing link failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing link not attempted.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupAny.", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing link failed.",
                           createLink= NA_character_ )
         })
      })

      Sys.chmod(noWriteDir, mode = dirMode, use_umask = FALSE)
      expect_equal(unlink(file), 0)
   })
   describe( "un-deleteable incorrect link", {
      noWriteDir <- tempfile('noWriteDir')
      expect_true( dir.create( noWriteDir ))
      file <- file.path(noWriteDir, 'correctLink')
      expect_true(file.symlink(sourceFile, file))
      dirMode <- file.info(noWriteDir)$mode
      expect_true(Sys.chmod(noWriteDir, mode = "555", use_umask = FALSE))
      on.exit(Sys.chmod(noWriteDir, mode = dirMode, use_umask = FALSE), add= TRUE)
      on.exit(unlink(file), add= TRUE)

      describe( "Errors and warnings if duplicateOk= TRUE", {
         duplicateOk <- TRUE
         it( "Fails with onExists= replaceFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing link not attempted.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= replaceAny.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing link failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing link not attempted.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupAny.", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing link failed.",
                           createLink= NA_character_ )
         })
      })
      describe( "Errors and warnings if onDuplicate= duplicateOk", {
         duplicateOk <- FALSE
         it( "Fails with onExists= replaceFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing link not attempted.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= replaceAny.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing link failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing link not attempted.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupAny.", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
            ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing link failed.",
                           createLink= NA_character_ )
         })
      })

      Sys.chmod(noWriteDir, mode = dirMode, use_umask = FALSE)
      expect_equal(unlink(file), 0)
   })
   describe( "un-deleteable directory", {
      noWriteDir <- tempfile('noWriteDir')
      expect_true(dir.create(noWriteDir))
      file <- file.path(noWriteDir, 'plainDir')
      expect_true(dir.create(file))
      dirMode <- file.info(noWriteDir)$mode
      expect_true(Sys.chmod(noWriteDir, mode = "555", use_umask = FALSE))
      on.exit(Sys.chmod(noWriteDir, mode = dirMode, use_umask = FALSE), add= TRUE)
      on.exit(unlink(file), add= TRUE)

      describe( "Errors and warnings if duplicateOk= TRUE", {
         duplicateOk <- TRUE
         it( "Fails with onExists= replaceFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'replaceFile'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= replaceAny.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'replaceAny'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing directory failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupFile", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupAny", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'backupAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
               checkExists= "Rename of pre-existing directory failed.",
               createLink= NA_character_ )
         })
      })
      describe( "Errors and warnings if onDuplicate= FALSE", {
         duplicateOk <- FALSE
         it( "Fails with onExists= replaceFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize, checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceFile'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= replaceAny.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               checksum= sourceFileMd5,
               duplicateOk= duplicateOk, onExists= 'replaceAny'))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Deletion of pre-existing directory failed.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupFile.", {
            expect_silent( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               fileSize = sourceFileSize,
               duplicateOk= duplicateOk, onExists= 'backupFile'
            ))
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing directory not attempted.",
                           createLink= NA_character_ )
         })
         it( "Fails with onExists= backupAny.", {
            wantWarningRE <- "cannot rename file .* reason .Permission denied."
            expect_warning( got <- linkTo(
               file, repo=repoDir, entry= entryDir, asFile= newBaseName,
               duplicateOk= duplicateOk, onExists= 'backupAny'
               ), wantWarningRE
            )
            expect_LinkTo( got, file, targetFile, checkLink= FALSE,
                           checkExists= "Rename of pre-existing directory failed.",
                           createLink= NA_character_ )
         })
      })

      Sys.chmod(noWriteDir, mode = dirMode, use_umask = FALSE)
      expect_equal( unlink( file, recursive= TRUE ), 0)
   })
})
describe( "Catching other errors", {
   file <- tempfile('notDuplicateFile')
   expect_true(file.create(file))

   it ("Reports errors if anything is not caught.", {
      with_mock(
         `DataRepo::checkSummary`= function(...) { stop("Bad checkSummary.") },
         expect_silent( got <- linkTo(
            file, repo=repoDir, entry= entryDir, asFile= newBaseName,
            duplicateOk= TRUE, onExists= 'error'
         ))
      )
      expect_LinkTo( got, file, targetFile, checkLink= FALSE,
         checkTarget= NA_character_, checkExists= NA_character_,
         createLink= NA_character_, unexpectedError= "Bad checkSummary." )
   })

   expect_equal(unlink(file), 0)
})
