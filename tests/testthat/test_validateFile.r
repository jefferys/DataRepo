# Set up basic file system objects for testing
badPath    <- tempfile( 'badPath'    )
emptyFile <- tempfile( 'emptyFile' )
sourceDir  <- tempfile( 'sourceDir' )
binFile <- tempfile( 'binFile', fileext= '.bin' )

file.create( emptyFile )
dir.create(  sourceDir  )
writeBin( as.raw(c(1,2,3)), binFile)

# Set up simple file system symlinks for testing
fileLink <- tempfile( 'fileLink' )
dirLink  <- tempfile( 'dirLink'  )
badLink  <- tempfile( 'badLink'  )

hasFileLinks <- file.symlink( emptyFile, fileLink )
hasDirLinks  <- file.symlink( sourceDir,  dirLink  )
hasBadLinks  <- file.symlink( badPath,    badLink  )

# Set up transitive file system symlinks for testing
fileLinkLink <- tempfile( 'fileLinkLink' )
dirLinkLink  <- tempfile( 'dirLinkLink' )
badLinkLink  <- tempfile( 'badLinkLink'  )

hasFileLinkLinks <- file.symlink( fileLink, fileLinkLink )
hasDirLinkLinks  <- file.symlink( dirLink,  dirLinkLink  )
hasBadLinkLinks  <- file.symlink( badLink,  badLinkLink  )

SumIt <- function (path) { sum( as.numeric(
   readBin( path, what = 'raw', n= file.info(path)[['size']] + 1 )
))}

# Set up global data for tests
checked <- c('checkParam_path', 'checkParam_checksum',
              'checkParam_checksumFunc', 'checkParam_fileSize',
              'checkParam_checksum_checksumFunc', 'checkIsFile',
              'checkIsNotLink', 'checkFileSizeMatches', 'checkChecksumMatches' )

md5EmptyFile= 'd41d8cd98f00b204e9800998ecf8427e'
md5BinFile= '5289df737df57326fcdd22597afb1fac'
binFileSize= 3
emptyFileSize= 0
SumItBinFile= 6

# Set up test helpers

# Test that the returned value from validateFile is correctly structured.
expect_resultFormat_validateFile <- function (got, checked) {
   expect_true( is.vector( got, mode = 'character' ))
   expect_equal( length( got ), length( checked ))
   expect_equal( names( got ), checked )
}

# Test an element validateFile result (one check function).
expect_testResult_validateFile <- function (got, check, result, useMatch= FALSE) {
   names(result) <- check
   if (useMatch) {
      expect_match( got[check], result )
   }
   else {
      expect_equal( got[check], result )
   }
}

context( "Setup for testing validateFile()" )
describe( "File system resources for validateFile function tests", {
   it( "includes an existing empty file.", {
      expect_true( file.exists( emptyFile ))
      expect_false( dir.exists( emptyFile ))
   })
   it( "includes an existing binary file.", {
      expect_true( file.exists( binFile ))
      expect_false( dir.exists( binFile ))
      expect_equal( readBin( binFile, what = 'raw', n=3 ), as.raw( c( 1, 2, 3 )))
   })
   it( "includes an existing direcctory.", {
      expect_true( dir.exists( sourceDir ))
   })
   it( "includes a non-existing path.", {
      expect_false( file.exists( badPath ))
   })
   it( "includes a symlink to a file", {
      if (! hasFileLinks) skip("Symlinks to files are not supported on this os.")
      expect_true( file.exists( fileLink ))
      expect_equal( Sys.readlink(fileLink), emptyFile )
   })
   it( "includes a symlink to a dir", {
      if (! hasDirLinks) skip("Symlinks to directories are not supported on this os.")
      expect_true( dir.exists( dirLink ))
      expect_equal( Sys.readlink(dirLink), sourceDir )
   })
   it( "includes bad symlinks", {
      if (! hasFileLinks) skip("Symlinks to files are not supported on this os.")
      if (! hasBadLinks) skip("Symlinks without existing targets are not allowed on this os.")
      expect_false( file.exists( badLink ))
      expect_equal( Sys.readlink(badLink), badPath )
   })
   it( "includes transitive file symlinks", {
      if (! hasFileLinks) skip("Symlinks to files are not supported on this os.")
      if (! hasFileLinkLinks) skip("Symlinks to symlinks to files are not supported on this os.")
      expect_true( file.exists( fileLinkLink ))
      expect_equal( Sys.readlink(fileLinkLink), fileLink )
      expect_equal( Sys.readlink(Sys.readlink(fileLinkLink)), emptyFile )
   })
   it( "includes transitive dir symlinks", {
      if (! hasDirLinks) skip("Symlinks to directories are not supported on this os.")
      if (! hasDirLinkLinks) skip("Symlinks to symlinks to directories are not supported on this os.")
      expect_true( dir.exists( dirLinkLink ))
      expect_equal( Sys.readlink(dirLinkLink), dirLink )
      expect_equal( Sys.readlink(Sys.readlink(dirLinkLink)), sourceDir )
   })
   it( "includes bad  transitive symlinks", {
      if (! hasFileLinks) skip("Symlinks to files are not supported on this os.")
      if (! hasBadLinks) skip("Symlinks without existing targets are not allowed on this os.")
      if (! hasBadLinkLinks) skip("Symlinks to symlinks without existing targets are not allowed on this os.")
      expect_false( file.exists( badLinkLink ))
      expect_equal( Sys.readlink(badLinkLink), badLink )
      expect_equal( Sys.readlink(Sys.readlink(badLinkLink)), badPath )
   })
})

context( "validateFile() All pass" )
describe( "validateFile() succeeds", {
   it( "Succeeds for a real file, fileSize and checkSum are NULL", {
      want <- rep("", length(checked))
      names(want) <- checked
      want['checkFileSizeMatches'] <- NA_character_
      want['checkChecksumMatches'] <- NA_character_

      got <- validateFile( path= emptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_equal( got, want )
      got <- validateFile( path= binFile )
      expect_resultFormat_validateFile(got, checked)
      expect_equal( got, want )
   })
   it( "Succeeds for a real file, with fileSize test but checkSum = NULL", {
      want <- rep("", length(checked))
      names(want) <- checked
      want['checkFileSizeMatches'] <- ''
      want['checkChecksumMatches'] <- NA_character_

      got <- validateFile( path= emptyFile, fileSize= emptyFileSize )
      expect_resultFormat_validateFile(got, checked)
      expect_equal( got, want )

      got <- validateFile( path= binFile, fileSize= binFileSize )
      expect_resultFormat_validateFile(got, checked)
      expect_equal( got, want )
   })
   it( "Succeeds for a real file, with fileSize= NULL but checkSum= correct", {
      want <- rep("", length(checked))
      names(want) <- checked
      want['checkFileSizeMatches'] <- NA_character_
      want['checkChecksumMatches'] <- ''

      got <- validateFile( path= emptyFile, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_equal( got, want )

      got <- validateFile( path= binFile, checksum= md5BinFile )
      expect_resultFormat_validateFile(got, checked)
      expect_equal( got, want )
   })
   it( "Succeeds for a real file, with fileSize test and checkSum= correct", {
      want <- rep("", length(checked))
      names(want) <- checked
      want['checkFileSizeMatches'] <- ''
      want['checkChecksumMatches'] <- ''

      got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_equal( got, want )

      got <- validateFile( path= binFile, fileSize= binFileSize, checksum= md5BinFile )
      expect_resultFormat_validateFile(got, checked)
      expect_equal( got, want )
   })
})

context( "validateFile() parameters tests" )
describe( "Failing 'path=' parameter", {
   it( "is NULL", {
      got <- validateFile( path= NULL )
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a vector of mode character."
      expect_testResult_validateFile(got, 'checkParam_path', want)
   })
   it( "is an object but not a vector", {
      got <- validateFile( path= list("ok") )
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a vector of mode character."
      expect_testResult_validateFile(got, 'checkParam_path', want)
   })
   it( "is a vector but not a character vector", {
      got <- validateFile( path= 1 )
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a vector of mode character."
      expect_testResult_validateFile(got, 'checkParam_path', want)
   })
   it( "is a character vector, but is empty", {
      got <- validateFile( path= character(0) )
      expect_resultFormat_validateFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateFile(got, 'checkParam_path', want)
   })
   it( "is a character vector, but has multiple element", {
      got <- validateFile( path= c('A', 'B') )
      expect_resultFormat_validateFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateFile(got, 'checkParam_path', want)
   })
   # String = 1 element character vector
   it( "is a string, but it is empty", {
      got <- validateFile( path= '' )
      expect_resultFormat_validateFile(got, checked)
      want <- "Character count is not between 1 and Inf."
      expect_testResult_validateFile(got, 'checkParam_path', want)
   })
   it( "is a string, but it is missing", {
      got <- validateFile( path= NA_character_ )
      expect_resultFormat_validateFile(got, checked)
      want <- "Contains NA."
      expect_testResult_validateFile(got, 'checkParam_path', want)
   })
   it( "it can have unexpected failures.", {
      expect_false( exists( 'noSuchObjectExists' ))
      got <- validateFile( path= noSuchObjectExists )
      expect_resultFormat_validateFile(got, checked)
      want <- "Unexpected error: object 'noSuchObjectExists' not found"
      expect_testResult_validateFile(got, 'checkParam_path', want )
   })
})
describe( "Failing 'checksum=' parameter", {
   # Null is ok.
   it( "is an object other than a vector", {
      got <- validateFile( emptyFile, checksum= list('A'))
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a vector of mode any."
      expect_testResult_validateFile(got, 'checkParam_checksum', want)
   })
   # Any vector object is ok.
   it( "is an empty vector.", {
      got <- validateFile( emptyFile, checksum= character(0) )
      expect_resultFormat_validateFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateFile(got, 'checkParam_checksum', want)
   })
   it( "is a multiple element vector.", {
      got <- validateFile( path= emptyFile, checksum= c(1, 2) )
      expect_resultFormat_validateFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateFile(got, 'checkParam_checksum', want)
   })
   it( "is a single element vector, but that element is missing.", {
      got <- validateFile( path= emptyFile, checksum= NA )
      expect_resultFormat_validateFile(got, checked)
      want <- "Is NA."
      expect_testResult_validateFile(got, 'checkParam_checksum', want)
   })
   it( "it can have unexpected failures.", {
      expect_false( exists( 'noSuchObjectExists' ))
      got <- validateFile( path= emptyFile, checksum= noSuchObjectExists )
      expect_resultFormat_validateFile(got, checked)
      want <- "Unexpected error: object 'noSuchObjectExists' not found"
      expect_testResult_validateFile(got, 'checkParam_checksum', want )
   })
})
describe( "Failing 'checksumFunc=' parameter", {
   # Null is ok, unless checksum != NULL, but that is a cross-parameter test
   it( "is an object other than a function", {
      got <- validateFile( emptyFile, checksumFunc= 'nchar')
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a function object."
      expect_testResult_validateFile(got, 'checkParam_checksumFunc', want)
   })
   it( "is an object other than a function", {
      got <- validateFile( emptyFile, checksumFunc= NA)
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a function object."
      expect_testResult_validateFile(got, 'checkParam_checksumFunc', want)
   })
   it( "it can have unexpected failures.", {
      expect_false(exists('noSuchObjectExists'))
       got <- validateFile( path= emptyFile, checksumFunc= noSuchObjectExists )
       expect_resultFormat_validateFile(got, checked)
       want <- "Unexpected error: object 'noSuchObjectExists' not found"
       expect_testResult_validateFile(got, 'checkParam_checksumFunc', want )
    })
})
describe( "Failing 'fileSize=' parameter", {
   # Null ok
   it( "is an object but not a vector", {
      got <- validateFile( path= emptyFile, fileSize= list(fileSize=0) )
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a vector of mode numeric."
      expect_testResult_validateFile(got, 'checkParam_fileSize', want)
   })
   it( "is a vector but not a numeric vector", {
      got <- validateFile( path= emptyFile, fileSize= '123K' )
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a vector of mode numeric."
      expect_testResult_validateFile(got, 'checkParam_fileSize', want)
   })
   it( "is a numeric vector, but is empty", {
      got <- validateFile( path= emptyFile, fileSize= integer(0) )
      expect_resultFormat_validateFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateFile(got, 'checkParam_fileSize', want)
   })
   it( "is a numeric vector, but has multiple element", {
      got <- validateFile( path= emptyFile, fileSize= c(100, 1000) )
      expect_resultFormat_validateFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateFile(got, 'checkParam_fileSize', want)
   })
   # A number means a 1 element numeric vector.
   it( "is a number, but it is NaN", {
      got <- validateFile(  emptyFile, fileSize= NaN )
      expect_resultFormat_validateFile(got, checked)
      want <- "Is NaN."
      expect_testResult_validateFile(got, 'checkParam_fileSize', want)
   })
   it( "is a number, but it is missing", {
      got <- validateFile( emptyFile, fileSize= NA_real_ )
      expect_resultFormat_validateFile(got, checked)
      want <- "Is NA."
      expect_testResult_validateFile(got, 'checkParam_fileSize', want)

   })
   it( "is a number, but it is negative", {
      got <- validateFile( emptyFile, fileSize= -100 )
      expect_resultFormat_validateFile(got, checked)
      want <- "Is negative."
      expect_testResult_validateFile(got, 'checkParam_fileSize', want)
   })
   it( "it can have unexpected failures.", {
      expect_false(exists('noSuchObjectExists'))
      got <- validateFile( path= emptyFile, fileSize= noSuchObjectExists )
      expect_resultFormat_validateFile(got, checked)
      want <- "Unexpected error: object 'noSuchObjectExists' not found"
      expect_testResult_validateFile(got, 'checkParam_fileSize', want )
   })
})
describe( "cross-parameters tests", {
   describe( "'checksumFunc=' validity depends on 'checksum='", {
      it("may not be null if checksum is not NULL.", {
        got <- validateFile( emptyFile, checksum= '1', checksumFunc = NULL)
        expect_resultFormat_validateFile(got, checked)
        want <- 'Checksum but no checksum function.'
        expect_testResult_validateFile(got, 'checkParam_checksum_checksumFunc', want)
      })
      it( "it can have unexpected failures.", {
         skip("No way to trigger this error is apparant - nocovr used in code")
      })
   })
})

context( "validateFile() checkIsFile test" )
describe( "checkIsFile, default params with various paths", {
   it( "Succeeds if path is an existing file on the filesystem", {
      want <- ""
      got <- validateFile( path= emptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsFile", want)
   })
   it( "Fails if path does not exist on the filesystem", {
      want <- "No such path."
      got <- validateFile( path= badPath )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsFile", want)
   })
   it( "Fails if path is a dir", {
      got <- validateFile( path= sourceDir )
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a file."
      expect_testResult_validateFile(got, "checkIsFile", want)
   })
   it( "Succeeds if path is link to a file", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      want <- ""
      got <- validateFile( path= fileLink )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsFile", want)
   })
   it( "Fails if path is a bad (no-target) link", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      else if (! hasBadLinks) {
         skip("Symlinks without existing targets are not allowed on this os.")
      }
      got <- validateFile( path= badPath )
      expect_resultFormat_validateFile(got, checked)
      want <- "No such path."
      expect_testResult_validateFile(got, "checkIsFile", want)
   })
   it( "Fails if path is a link to a directory", {
      if (! hasDirLinks) {
         skip("Symlinks to directories are not supported on this os.")
      }
      got <- validateFile( path= dirLink )
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a file."
      expect_testResult_validateFile(got, "checkIsFile", want)
   })
   it( "Succeeds if path is link to a link to a file", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      else if (! hasFileLinkLinks) {
         skip("Transitive symlinks to files are not supported on this os.")
      }
      want <- ""
      got <- validateFile( path= fileLinkLink )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsFile", want)
   })
   it( "Fails if path is a link to a bad (no-target) link", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      else if (! hasBadLinks) {
         skip("Symlinks without existing targets are not allowed on this os.")
      }
      else if (! hasFileLinkLinks) {
         skip("Transitive symlinks to files are not supported on this os.")
      }
      else if (! hasBadLinks) {
         skip("Transitive symlinks without existing targets are not allowed on this os.")
      }
      got <- validateFile( path= badLinkLink )
      expect_resultFormat_validateFile(got, checked)
      want <- "No such path."
      expect_testResult_validateFile(got, "checkIsFile", want)
   })
   it( "Fails if path is a link to a link to a directory", {
      if (! hasDirLinks) {
         skip("Symlinks to directories are not supported on this os.")
      }
      if (! hasDirLinkLinks) {
         skip("Transitive symlinks to directories are not supported on this os.")
      }
      got <- validateFile( path= dirLinkLink )
      expect_resultFormat_validateFile(got, checked)
      want <- "Not a file."
      expect_testResult_validateFile(got, "checkIsFile", want)
   })
})

context( "validateFile() checkIsLink test" )
describe( "checkIsLink, default params with various paths", {
   it( "Succeeds if path is an existing file on the filesystem", {
      want <- ""
      got <- validateFile( path= emptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if file does not exist on the filesystem", {
      want <- NA_character_
      got <- validateFile( path= badPath )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if path is a dir", {
      want <- NA_character_
      got <- validateFile( path= sourceDir )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsNotLink", want)
   })
   it( "Fails if path is link to a file", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      want <- "Is a link."
      got <- validateFile( path= fileLink )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if path is a bad (no-target) link", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      else if (! hasBadLinks) {
         skip("Symlinks without existing targets are not allowed on this os.")
      }
      want <- NA_character_
      got <- validateFile( path= badPath )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if path is a link to a directory", {
      if (! hasDirLinks) {
         skip("Symlinks to directories are not supported on this os.")
      }
      want <- NA_character_
      got <- validateFile( path= dirLink )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsNotLink", want)
   })
   it( "Fails if path is link to a link to a file", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      else if (! hasFileLinkLinks) {
         skip("Transitive symlinks to files are not supported on this os.")
      }
      want <- "Is a link."
      got <- validateFile( path= fileLinkLink )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if path is a link to a bad (no-target) link", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      else if (! hasBadLinks) {
         skip("Symlinks without existing targets are not allowed on this os.")
      }
      else if (! hasFileLinkLinks) {
         skip("Transitive symlinks to files are not supported on this os.")
      }
      else if (! hasBadLinks) {
         skip("Transitive symlinks without existing targets are not allowed on this os.")
      }
      want <- NA_character_
      got <- validateFile( path= badLinkLink )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if path is a link to a link to a directory", {
      if (! hasDirLinks) {
         skip("Symlinks to directories are not supported on this os.")
      }
      if (! hasDirLinkLinks) {
         skip("Transitive symlinks to directories are not supported on this os.")
      }
      want <- NA_character_
      got <- validateFile( path= dirLinkLink )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkIsNotLink", want)
   })
})

context( "validateFile() checkFileSizeMatches test" )
describe( "Not running a size check test (default)", {
   it("Returns NA when fileSize is NULL and checksum (good) is given", {
      want <- NA_character_
      got <- validateFile( path= emptyFile, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it("Returns NA when fileSize is NULL and checksum (bad) is given", {
      want <- NA_character_
      got <- validateFile( path= emptyFile, fileSize=NULL, checksum= 0 )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it("Returns NA when fileSize is NULL and checksum is NULL", {
      want <- NA_character_
      got <- validateFile( path= emptyFile, fileSize=NULL, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
})
describe( "size check succeeds if size correct (fileSize= parameter)", {
   it("succeeds for a file if checksum (bad) is given", {
      want <- ""
      got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= 'A' )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it("succeeds for a file if checksum (good) is given", {
      want <- ""
      got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it("succeeds for a file if checksum is NULL", {
      want <- ""
      got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
})
describe( "size check fails if size wrong (fileSize= parameter)", {
   it("fails if checksum (bad) is given", {
      want <- "File size mismatch. Found 0 wanted 1."
      got <- validateFile( path= emptyFile, fileSize= 1, checksum= 'A' )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it("fails if checksum (good) is given", {
      want <- "File size mismatch. Found 0 wanted 1001."
      got <- validateFile( path= emptyFile, fileSize= 1001, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it("fails if checksum is NULL", {
      want <- "File size mismatch. Found 0 wanted 123456789."
      got <- validateFile( path= emptyFile, fileSize= 123456789, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
})
describe( "Internal failures in checkFileSizeMatches tests", {
   it ("fails if stat doesn't return a data frame", {
      want <- "Can't stat file."
      with_mock(
         `base::is.data.frame`= function (...) FALSE,
         got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if stat doesn't return a size value", {
      want <- "Read file size as null."
      stat <- file.info(emptyFile)
      stat$size <- NULL
      with_mock(
         `base::file.info`= function (...) {stat},
         got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if stat doesn't return a numeric size value", {
      want <- "Not a vector of mode numeric."
      stat <- file.info(emptyFile)
      stat$size <- 'Oops'
      with_mock(
         `base::file.info`= function (...) {stat},
         got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if stat returns a NaN size value", {
      want <- "Read file size as NaN."
      stat <- file.info(emptyFile)
      stat$size <- NaN
      with_mock(
         `base::file.info`= function (...) {stat},
         got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if stat returns a NA size value", {
      want <- "Read file size as NA."
      stat <- file.info(emptyFile)
      stat$size <- NA_integer_
      with_mock(
         `base::file.info`= function (...) {stat},
         got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if stat returns a negative size value", {
      want <- "Read file size as negative."
      stat <- file.info(emptyFile)
      stat$size <- -1
      with_mock(
         `base::file.info`= function (...) {stat},
         got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if unexpected error is triggered", {
      want <- "Unexpected error: Bad is.data.frame!"
      with_mock(
         `base::is.data.frame`= function (...) stop('Bad is.data.frame!'),
         got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkFileSizeMatches", want)
   })

})

context( "validateFile() checkChecksumMatches test" )
describe( "Not running a checksum test when checksum= NULL(default)", {
   it("Returns NA when fileSize is NULL", {
      want <- NA_character_

      got <- validateFile( path= emptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= emptyFile, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= emptyFile, fileSize= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= emptyFile, fileSize= NULL, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is bad", {
      want <- NA_character_

      got <- validateFile( path= emptyFile, fileSize= 100 )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= emptyFile, fileSize= 100, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is good", {
      want <- NA_character_

      got <- validateFile( path= emptyFile, fileSize= emptyFileSize )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
})
describe( "Result of checksum tests when checksum= bad value", {
   it("Returns failure when fileSize is NULL", {
      want <- "Checksum mismatch. Found d41d8cd98f00b204e9800998ecf8427e wanted abc123."

      got <- validateFile( path= emptyFile, checksum= 'abc123' )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= emptyFile, checksum= 'abc123', fileSize= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA string when fileSize is bad", {
      want <- NA_character_
      got <- validateFile( path= emptyFile, fileSize= 1, checksum= 'abc123' )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns failure string when fileSize is good", {
      want <- "Checksum mismatch. Found d41d8cd98f00b204e9800998ecf8427e wanted abc123."
      got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= 'abc123' )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
})
describe( "Result of checksum tests when checksum= correct value", {
   it("Returns success when fileSize is NULL", {
      want <- ""

      got <- validateFile( path= emptyFile, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= emptyFile, checksum= md5EmptyFile, fileSize= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is bad", {
      want <- NA_character_
      got <- validateFile( path= emptyFile, fileSize= 1, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns succeess (empty string) when fileSize is good", {
      want <- ""
      got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
})
describe( "Internal failures in checkChecksumMatches tests", {
   it( "Returns failure if checksum was calculated to be NULL.", {
      want <- "Calculated checksum was null."
      badCheckFunc <- function (...) return(NULL)
      got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksumFunc= badCheckFunc, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it( "Returns failure if checksum was calculated to be missing.", {
      want <- "Calculated checksum was NA."
      badCheckFunc <- function (...) return(NA_character_)
      got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksumFunc= badCheckFunc, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it( "Returns failure if checksum signals an error.", {
      want <- "Unexpected error: bad checksum func!"
      badCheckFunc <- function (...) stop( 'bad checksum func!' )
      got <- validateFile( path= emptyFile, fileSize= emptyFileSize, checksumFunc= badCheckFunc, checksum= md5EmptyFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it( "Returns failure if checksum calculation interrupted by an interrupt signal.", {
      skip('No idea how to test this. Hidden from coverage testing in code.')
   })
})

context( "validateFile() checksumFunc=" )
describe( "Checking new checksum function", {
   expect_equal( SumIt(emptyFile), 0 )
   expect_equal( SumIt(binFile), SumItBinFile )
})
describe( "Not running a checksum test when checksum= NULL(default)", {
   it("Returns NA when fileSize is NULL", {
      want <- NA_character_

      got <- validateFile( path= binFile, checksumFunc= SumIt )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= binFile, checksumFunc= SumIt, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= binFile, checksumFunc= SumIt, fileSize= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= binFile, fileSize= NULL, checksumFunc= SumIt, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is bad", {
      want <- NA_character_

      got <- validateFile( path= binFile, checksumFunc= SumIt, fileSize= 100 )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= binFile, checksumFunc= SumIt, fileSize= 100, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is good", {
      want <- NA_character_

      got <- validateFile( path= binFile, checksumFunc= SumIt, fileSize= binFileSize )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= binFile, fileSize= binFileSize, checksumFunc= SumIt, checksum= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
})
describe( "Result of checksum tests when checksum= bad value", {
   it("Returns failure when fileSize is NULL", {
      want <- "Checksum mismatch. Found 6 wanted 5289df737df57326fcdd22597afb1fac."

      got <- validateFile( path= binFile, checksum= md5BinFile, checksumFunc = SumIt )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= binFile, checksum= md5BinFile, checksumFunc = SumIt, fileSize= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA string when fileSize is bad", {
      want <- NA_character_
      got <- validateFile( path= binFile, fileSize= emptyFileSize, checksumFunc= SumIt, checksum= 'abc123' )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns failure string when fileSize is good", {
      want <- "Checksum mismatch. Found 6 wanted abc123."
      got <- validateFile( path= binFile, fileSize= binFileSize, checksumFunc= SumIt, checksum= 'abc123' )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
})
describe( "Result of checksum tests when checksum= correct value", {
   it("Returns success when fileSize is NULL", {
      want <- ""

      got <- validateFile( path= binFile, checksumFunc= SumIt, checksum= SumItBinFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)

      got <- validateFile( path= binFile, checksumFunc= SumIt, checksum= SumItBinFile, fileSize= NULL )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is bad", {
      want <- NA_character_
      got <- validateFile( path= binFile, fileSize= 1, checksumFunc= SumIt, checksum= SumItBinFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
   it("Returns succeess (empty string) when fileSize is good", {
      want <- ""
      got <- validateFile( path= binFile, fileSize= binFileSize, checksumFunc= SumIt, checksum= SumItBinFile )
      expect_resultFormat_validateFile(got, checked)
      expect_testResult_validateFile(got, "checkChecksumMatches", want)
   })
})

context("checkSummary()")
describe( "Minimal functionality tests with defaults", {
   it ( 'Returns success string if no error', {
      obj <- c(testA='', testB='', testC='')
      want <- ''
      got <- checkSummary( obj )
      expect_equal(got, want)
   })
   it ( 'Returns failure string if any failure', {
      obj <- c(testA='', testB='', testC='Bad test c.')
      want <- 'testC = Bad test c.'
      got <- checkSummary( obj )
      expect_equal(got, want)
   })
   it ( "Returns failure string for NA's by default", {
      obj <- c(testA='', testB=NA, testC='')
      want <- 'testB = NA not ok.'
      got <- checkSummary( obj )
      expect_equal(got, want)
   })
   it ( 'Returns combination string for multiple failures', {
      obj <- c(testA="", testB=NA, testC="Bad c.", testD="Bad d.", testE="", testF= NA)
      want <- 'testB = NA not ok.; testC = Bad c.; testD = Bad d.; testF = NA not ok.'
      got <- checkSummary( obj )
      expect_equal(got, want)
   })
})
describe( "table testing of skip= and okNa= parameters", {
   describe( "where all results are success (empty) strings", {
      it ( 'works for all combinations of skip and okNo on one element objects', {
         obj <- c (A= "")

         tests <- list(
            list( naOk= NULL,   skip= NULL, want= '' ),
            list( naOk= c('A'), skip= NULL, want= '' ),

            list( naOk= NULL,   skip= c('A'), want= '' ),
            list( naOk= c('A'), skip= c('A'), want= '' )
         )

         lapply( tests, function (x) {
            got <- checkSummary(obj, naOk= x$naOk, skip= x$skip)
            expect_equal(got, x$want)
         })
      })
      it ( 'works for all combinations of skip and okNo on two element objects', {
         obj <- c (A= "", B= "")

         tests <- list(
            list( naOk= NULL,         skip= NULL, want= '' ),
            list( naOk= c('A'),       skip= NULL, want= '' ),
            list( naOk= c('B'),       skip= NULL, want= '' ),
            list( naOk= c('A', 'B' ), skip= NULL, want= '' ),

            list( naOk= NULL,         skip= c('A'), want= '' ),
            list( naOk= c('A'),       skip= c('A'), want= '' ),
            list( naOk= c('B'),       skip= c('A'), want= '' ),
            list( naOk= c('A', 'B' ), skip= c('A'), want= '' ),

            list( naOk= NULL,         skip= c('B'), want= '' ),
            list( naOk= c('A'),       skip= c('B'), want= '' ),
            list( naOk= c('B'),       skip= c('B'), want= '' ),
            list( naOk= c('A', 'B' ), skip= c('B'), want= '' ),

            list( naOk= NULL,         skip= c('A', 'B'), want= '' ),
            list( naOk= c('A'),       skip= c('A', 'B'), want= '' ),
            list( naOk= c('B'),       skip= c('A', 'B'), want= '' ),
            list( naOk= c('A', 'B' ), skip= c('A', 'B'), want=  '')

         )

         lapply( tests, function (x) {
            got <- checkSummary(obj, naOk= x$naOk, skip= x$skip)
            expect_equal(got, x$want)
         })
      })
   })
   describe( "where all results are failure strings", {
      it ( 'works for all combinations of skip and okNo on one element objects', {
         obj <- c(A= "Bad a.")

         tests <- list(
            list( naOk= NULL,   skip= NULL, want= 'A = Bad a.' ),
            list( naOk= c('A'), skip= NULL, want= 'A = Bad a.' ),

            list( naOk= NULL,   skip= c('A'), want= '' ),
            list( naOk= c('A'), skip= c('A'), want= '' )
         )

         lapply( tests, function (x) {
            got <- checkSummary(obj, naOk= x$naOk, skip= x$skip)
            expect_equal(got, x$want)
         })
      })
      it ( 'works for all combinations of skip and okNo on two element objects', {
         obj <- c (A= "Bad a.", B= "Bad b.")

         tests <- list(
            list( naOk= NULL,         skip= NULL, want= 'A = Bad a.; B = Bad b.' ),
            list( naOk= c('A'),       skip= NULL, want= 'A = Bad a.; B = Bad b.' ),
            list( naOk= c('B'),       skip= NULL, want= 'A = Bad a.; B = Bad b.' ),
            list( naOk= c('A', 'B' ), skip= NULL, want= 'A = Bad a.; B = Bad b.' ),

            list( naOk= NULL,         skip= c('A'), want= 'B = Bad b.' ),
            list( naOk= c('A'),       skip= c('A'), want= 'B = Bad b.' ),
            list( naOk= c('B'),       skip= c('A'), want= 'B = Bad b.' ),
            list( naOk= c('A', 'B' ), skip= c('A'), want= 'B = Bad b.' ),

            list( naOk= NULL,         skip= c('B'), want= 'A = Bad a.' ),
            list( naOk= c('A'),       skip= c('B'), want= 'A = Bad a.' ),
            list( naOk= c('B'),       skip= c('B'), want= 'A = Bad a.' ),
            list( naOk= c('A', 'B' ), skip= c('B'), want= 'A = Bad a.' ),

            list( naOk= NULL,         skip= c('A', 'B'), want= '' ),
            list( naOk= c('A'),       skip= c('A', 'B'), want= '' ),
            list( naOk= c('B'),       skip= c('A', 'B'), want= '' ),
            list( naOk= c('A', 'B' ), skip= c('A', 'B'), want=  '')

         )

         lapply( tests, function (x) {
            got <- checkSummary(obj, naOk= x$naOk, skip= x$skip)
            expect_equal(got, x$want)
         })
      })
   })
   describe( "where all results are missing (NA) results", {
      it ( 'works for all combinations of skip and okNo on one element objects', {
         obj <- c (A= NA_character_)

         tests <- list(
            list( naOk= NULL,   skip= NULL, want= 'A = NA not ok.' ),
            list( naOk= c('A'), skip= NULL, want= '' ),

            list( naOk= NULL,   skip= c('A'), want= '' ),
            list( naOk= c('A'), skip= c('A'), want= '' )
         )

         lapply( tests, function (x) {
            got <- checkSummary(obj, naOk= x$naOk, skip= x$skip)
            expect_equal(got, x$want)
         })
      })
      it ( 'works for all combinations of skip and okNo on two element objects', {
         obj <- c (A= NA_character_, B= NA_character_)

         tests <- list(
            list( naOk= NULL,         skip= NULL, want= 'A = NA not ok.; B = NA not ok.' ),
            list( naOk= c('A'),       skip= NULL, want= 'B = NA not ok.' ),
            list( naOk= c('B'),       skip= NULL, want= 'A = NA not ok.' ),
            list( naOk= c('A', 'B' ), skip= NULL, want= '' ),

            list( naOk= NULL,         skip= c('A'), want= 'B = NA not ok.' ),
            list( naOk= c('A'),       skip= c('A'), want= 'B = NA not ok.' ),
            list( naOk= c('B'),       skip= c('A'), want= '' ),
            list( naOk= c('A', 'B' ), skip= c('A'), want= '' ),

            list( naOk= NULL,         skip= c('B'), want= 'A = NA not ok.' ),
            list( naOk= c('A'),       skip= c('B'), want= '' ),
            list( naOk= c('B'),       skip= c('B'), want= 'A = NA not ok.' ),
            list( naOk= c('A', 'B' ), skip= c('B'), want= '' ),

            list( naOk= NULL,         skip= c('A', 'B'), want= '' ),
            list( naOk= c('A'),       skip= c('A', 'B'), want= '' ),
            list( naOk= c('B'),       skip= c('A', 'B'), want= '' ),
            list( naOk= c('A', 'B' ), skip= c('A', 'B'), want=  '')

         )

         lapply( tests, function (x) {
            got <- checkSummary(obj, naOk= x$naOk, skip= x$skip)
            expect_equal(got, x$want)
         })
      })
   })
   describe("where have one each of success, failure, and NA", {
      it ( 'works for all combinations of skip and okNo.', {
         obj <- c (A= "", B= "Bad b.", C= NA)

         tests <- list(
            list( naOk= NULL,             skip= NULL, want= 'B = Bad b.; C = NA not ok.' ),
            list( naOk= c('A'),           skip= NULL, want= 'B = Bad b.; C = NA not ok.' ),
            list( naOk= c('B'),           skip= NULL, want= 'B = Bad b.; C = NA not ok.' ),
            list( naOk= c('C'),           skip= NULL, want= 'B = Bad b.' ),
            list( naOk= c('A', 'B'     ), skip= NULL, want= 'B = Bad b.; C = NA not ok.' ),
            list( naOk= c('A',      'C'), skip= NULL, want= 'B = Bad b.' ),
            list( naOk= c(     'B', 'C'), skip= NULL, want= 'B = Bad b.' ),
            list( naOk= c('A', 'B', 'C'), skip= NULL, want= 'B = Bad b.' ),

            list( naOk= NULL,             skip= c('A'), want= 'B = Bad b.; C = NA not ok.' ),
            list( naOk= c('A'),           skip= c('A'), want= 'B = Bad b.; C = NA not ok.' ),
            list( naOk= c('B'),           skip= c('A'), want= 'B = Bad b.; C = NA not ok.' ),
            list( naOk= c('C'),           skip= c('A'), want= 'B = Bad b.' ),
            list( naOk= c('A', 'B'     ), skip= c('A'), want= 'B = Bad b.; C = NA not ok.' ),
            list( naOk= c('A',      'C'), skip= c('A'), want= 'B = Bad b.' ),
            list( naOk= c(     'B', 'C'), skip= c('A'), want= 'B = Bad b.' ),
            list( naOk= c('A', 'B', 'C'), skip= c('A'), want= 'B = Bad b.' ),

            list( naOk= NULL,             skip= c('B'), want= 'C = NA not ok.' ),
            list( naOk= c('A'),           skip= c('B'), want= 'C = NA not ok.' ),
            list( naOk= c('B'),           skip= c('B'), want= 'C = NA not ok.' ),
            list( naOk= c('C'),           skip= c('B'), want= '' ),
            list( naOk= c('A', 'B'     ), skip= c('B'), want= 'C = NA not ok.' ),
            list( naOk= c('A',      'C'), skip= c('B'), want= '' ),
            list( naOk= c(     'B', 'C'), skip= c('B'), want= '' ),
            list( naOk= c('A', 'B', 'C'), skip= c('B'), want= '' ),

            list( naOk= NULL,             skip= c('C'), want= 'B = Bad b.' ),
            list( naOk= c('A'),           skip= c('C'), want= 'B = Bad b.' ),
            list( naOk= c('B'),           skip= c('C'), want= 'B = Bad b.' ),
            list( naOk= c('C'),           skip= c('C'), want= 'B = Bad b.' ),
            list( naOk= c('A', 'B'     ), skip= c('C'), want= 'B = Bad b.' ),
            list( naOk= c('A',      'C'), skip= c('C'), want= 'B = Bad b.' ),
            list( naOk= c(     'B', 'C'), skip= c('C'), want= 'B = Bad b.' ),
            list( naOk= c('A', 'B', 'C'), skip= c('C'), want= 'B = Bad b.' ),

            list( naOk= NULL,             skip= c('A', 'B'), want= 'C = NA not ok.' ),
            list( naOk= c('A'),           skip= c('A', 'B'), want= 'C = NA not ok.' ),
            list( naOk= c('B'),           skip= c('A', 'B'), want= 'C = NA not ok.' ),
            list( naOk= c('C'),           skip= c('A', 'B'), want= '' ),
            list( naOk= c('A', 'B'     ), skip= c('A', 'B'), want= 'C = NA not ok.' ),
            list( naOk= c('A',      'C'), skip= c('A', 'B'), want= '' ),
            list( naOk= c(     'B', 'C'), skip= c('A', 'B'), want= '' ),
            list( naOk= c('A', 'B', 'C'), skip= c('A', 'B'), want= '' ),

            list( naOk= NULL,             skip= c('A', 'C'), want= 'B = Bad b.' ),
            list( naOk= c('A'),           skip= c('A', 'C'), want= 'B = Bad b.' ),
            list( naOk= c('B'),           skip= c('A', 'C'), want= 'B = Bad b.' ),
            list( naOk= c('C'),           skip= c('A', 'C'), want= 'B = Bad b.' ),
            list( naOk= c('A', 'B'     ), skip= c('A', 'C'), want= 'B = Bad b.' ),
            list( naOk= c('A',      'C'), skip= c('A', 'C'), want= 'B = Bad b.' ),
            list( naOk= c(     'B', 'C'), skip= c('A', 'C'), want= 'B = Bad b.' ),
            list( naOk= c('A', 'B', 'C'), skip= c('A', 'C'), want= 'B = Bad b.' ),

            list( naOk= NULL,             skip= c('B', 'C'), want= '' ),
            list( naOk= c('A'),           skip= c('B', 'C'), want= '' ),
            list( naOk= c('B'),           skip= c('B', 'C'), want= '' ),
            list( naOk= c('C'),           skip= c('B', 'C'), want= '' ),
            list( naOk= c('A', 'B'     ), skip= c('B', 'C'), want= '' ),
            list( naOk= c('A',      'C'), skip= c('B', 'C'), want= '' ),
            list( naOk= c(     'B', 'C'), skip= c('B', 'C'), want= '' ),
            list( naOk= c('A', 'B', 'C'), skip= c('B', 'C'), want= '' ),

            list( naOk= NULL,             skip= c('A', 'B', 'C'), want= '' ),
            list( naOk= c('A'),           skip= c('A', 'B', 'C'), want= '' ),
            list( naOk= c('B'),           skip= c('A', 'B', 'C'), want= '' ),
            list( naOk= c('C'),           skip= c('A', 'B', 'C'), want= '' ),
            list( naOk= c('A', 'B'     ), skip= c('A', 'B', 'C'), want= '' ),
            list( naOk= c('A',      'C'), skip= c('A', 'B', 'C'), want= '' ),
            list( naOk= c(     'B', 'C'), skip= c('A', 'B', 'C'), want= '' ),
            list( naOk= c('A', 'B', 'C'), skip= c('A', 'B', 'C'), want= '' )
         )

         lapply( tests, function (x) {
            got <- checkSummary(obj, naOk= x$naOk, skip= x$skip)
            expect_equal(got, x$want)
         })
      })
   })
})
describe( "table testing of bad parameters", {
   it( "Generates error messages with bad result=", {
      tests <- list(
         list( id= 'null',   result= NULL,         naOk= NULL, skip=NULL, want= "checkParam_result = Not a vector of mode character." ),
         list( id= 'list',   result= list(),       naOk= NULL, skip=NULL, want= "checkParam_result = Not a vector of mode character." ),
         list( id= '1,2',    result= c(1,2),       naOk= NULL, skip=NULL, want= "checkParam_result = Not a vector of mode character." ),
         list( id= 'na',     result= NA,           naOk= NULL, skip=NULL, want= "checkParam_result = Not a vector of mode character." ),
         list( id= 'empty',  result= character(0), naOk= NULL, skip=NULL, want= "checkParam_result = Length is not between 1 and Inf." ),
         list( id= 'noName', result= "bob",        naOk= NULL, skip=NULL, want= "checkParam_result = No names attribute." )
      )
      lapply( tests, function (x) {
         got <- checkSummary(x$result, naOk= x$naOk, skip= x$skip)
         expect_equal(got, x$want, info= x$id)
      })
   })
   it( "Generates error messages with bad naOk=", {
      tests <- list(
         list( id= 'list',    result= c(A=''), naOk= list(),       skip=NULL, want= "checkParam_naOk = Not a vector of mode character." ),
         list( id= '1,2',     result= c(A=''), naOk= c(1,2),       skip=NULL, want= "checkParam_naOk = Not a vector of mode character." ),
         list( id= 'na',      result= c(A=''), naOk= NA,           skip=NULL, want= "checkParam_naOk = Not a vector of mode character." ),
         list( id= 'empty',   result= c(A=''), naOk= character(0), skip=NULL, want= "checkParam_naOk = Length is not between 1 and Inf." ),
         list( id= 'unknown', result= c(A=''), naOk= "unknown",    skip=NULL, want= "checkParam_result_naOk = Some element is not in the checklist." )
      )
      lapply( tests, function (x) {
         got <- checkSummary(x$result, naOk= x$naOk, skip= x$skip)
         expect_equal(got, x$want, info= x$id)
      })
   })
   it( "Generates error messages with bad skip=", {
      tests <- list(
         list( id= 'list',    result= c(A=''), naOk= NULL, skip= list(),       want= "checkParam_skip = Not a vector of mode character." ),
         list( id= '1,2',     result= c(A=''), naOk= NULL, skip= c(1,2),       want= "checkParam_skip = Not a vector of mode character." ),
         list( id= 'na',      result= c(A=''), naOk= NULL, skip=  NA,          want= "checkParam_skip = Not a vector of mode character." ),
         list( id= 'empty',   result= c(A=''), naOk= NULL, skip= character(0), want= "checkParam_skip = Length is not between 1 and Inf." ),
         list( id= 'unknown', result= c(A=''), naOk= NULL, skip= "unknown",    want= "checkParam_result_skip = Some element is not in the checklist." )
      )
      lapply( tests, function (x) {
         got <- checkSummary(x$result, naOk= x$naOk, skip= x$skip)
         expect_equal(got, x$want, info= x$id)
      })
   })
})
