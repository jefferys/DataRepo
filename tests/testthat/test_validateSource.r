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

# Test that the returned value from validateSourceFile is correctly structured.
expect_resultFormat_validateSourceFile <- function (got, checked) {
   expect_true( is.vector( got, mode = 'character' ))
   expect_equal( length( got ), length( checked ))
   expect_equal( names( got ), checked )
}

# Test an element validateSourceFile result (one check function).
expect_testResult_validateSourceFile <- function (got, check, result, useMatch= FALSE) {
   names(result) <- check
   if (useMatch) {
      expect_match( got[check], result )
   }
   else {
      expect_equal( got[check], result )
   }
}

context( "Setup for testing validateFileSystem()" )
describe( "File system resources for validateSource function tests", {
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

context( "validateSourceFile() All pass" )
describe( "validateSourceFile() succeeds", {
   it( "Succeeds for a real file, fileSize and checkSum are NULL", {
      want <- rep("", length(checked))
      names(want) <- checked
      want['checkFileSizeMatches'] <- NA_character_
      want['checkChecksumMatches'] <- NA_character_

      got <- validateSourceFile( path= emptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_equal( got, want )
      got <- validateSourceFile( path= binFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_equal( got, want )
   })
   it( "Succeeds for a real file, with fileSize test but checkSum = NULL", {
      want <- rep("", length(checked))
      names(want) <- checked
      want['checkFileSizeMatches'] <- ''
      want['checkChecksumMatches'] <- NA_character_

      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_equal( got, want )

      got <- validateSourceFile( path= binFile, fileSize= binFileSize )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_equal( got, want )
   })
   it( "Succeeds for a real file, with fileSize= NULL but checkSum= correct", {
      want <- rep("", length(checked))
      names(want) <- checked
      want['checkFileSizeMatches'] <- NA_character_
      want['checkChecksumMatches'] <- ''

      got <- validateSourceFile( path= emptyFile, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_equal( got, want )

      got <- validateSourceFile( path= binFile, checksum= md5BinFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_equal( got, want )
   })
   it( "Succeeds for a real file, with fileSize test and checkSum= correct", {
      want <- rep("", length(checked))
      names(want) <- checked
      want['checkFileSizeMatches'] <- ''
      want['checkChecksumMatches'] <- ''

      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_equal( got, want )

      got <- validateSourceFile( path= binFile, fileSize= binFileSize, checksum= md5BinFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_equal( got, want )
   })
})

context( "validateSourceFile() parameters tests" )
describe( "Failing 'path=' parameter", {
   it( "is NULL", {
      got <- validateSourceFile( path= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a vector of mode character."
      expect_testResult_validateSourceFile(got, 'checkParam_path', want)
   })
   it( "is an object but not a vector", {
      got <- validateSourceFile( path= list("ok") )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a vector of mode character."
      expect_testResult_validateSourceFile(got, 'checkParam_path', want)
   })
   it( "is a vector but not a character vector", {
      got <- validateSourceFile( path= 1 )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a vector of mode character."
      expect_testResult_validateSourceFile(got, 'checkParam_path', want)
   })
   it( "is a character vector, but is empty", {
      got <- validateSourceFile( path= character(0) )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateSourceFile(got, 'checkParam_path', want)
   })
   it( "is a character vector, but has multiple element", {
      got <- validateSourceFile( path= c('A', 'B') )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateSourceFile(got, 'checkParam_path', want)
   })
   # String = 1 element character vector
   it( "is a string, but it is empty", {
      got <- validateSourceFile( path= '' )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Character count is not between 1 and Inf."
      expect_testResult_validateSourceFile(got, 'checkParam_path', want)
   })
   it( "is a string, but it is missing", {
      got <- validateSourceFile( path= NA_character_ )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Contains NA."
      expect_testResult_validateSourceFile(got, 'checkParam_path', want)
   })
   it( "it can have unexpected failures.", {
      expect_false( exists( 'noSuchObjectExists' ))
      got <- validateSourceFile( path= noSuchObjectExists )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Unexpected error: object 'noSuchObjectExists' not found"
      expect_testResult_validateSourceFile(got, 'checkParam_path', want )
   })
})
describe( "Failing 'checksum=' parameter", {
   # Null is ok.
   it( "is an object other than a vector", {
      got <- validateSourceFile( emptyFile, checksum= list('A'))
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a vector of mode any."
      expect_testResult_validateSourceFile(got, 'checkParam_checksum', want)
   })
   # Any vector object is ok.
   it( "is an empty vector.", {
      got <- validateSourceFile( emptyFile, checksum= character(0) )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateSourceFile(got, 'checkParam_checksum', want)
   })
   it( "is a multiple element vector.", {
      got <- validateSourceFile( path= emptyFile, checksum= c(1, 2) )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateSourceFile(got, 'checkParam_checksum', want)
   })
   it( "is a single element vector, but that element is missing.", {
      got <- validateSourceFile( path= emptyFile, checksum= NA )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Is NA."
      expect_testResult_validateSourceFile(got, 'checkParam_checksum', want)
   })
   it( "it can have unexpected failures.", {
      expect_false( exists( 'noSuchObjectExists' ))
      got <- validateSourceFile( path= emptyFile, checksum= noSuchObjectExists )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Unexpected error: object 'noSuchObjectExists' not found"
      expect_testResult_validateSourceFile(got, 'checkParam_checksum', want )
   })
})
describe( "Failing 'checksumFunc=' parameter", {
   # Null is ok, unless checksum != NULL, but that is a cross-parameter test
   it( "is an object other than a function", {
      got <- validateSourceFile( emptyFile, checksumFunc= 'nchar')
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a function object."
      expect_testResult_validateSourceFile(got, 'checkParam_checksumFunc', want)
   })
   it( "is an object other than a function", {
      got <- validateSourceFile( emptyFile, checksumFunc= NA)
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a function object."
      expect_testResult_validateSourceFile(got, 'checkParam_checksumFunc', want)
   })
   it( "it can have unexpected failures.", {
      expect_false(exists('noSuchObjectExists'))
       got <- validateSourceFile( path= emptyFile, checksumFunc= noSuchObjectExists )
       expect_resultFormat_validateSourceFile(got, checked)
       want <- "Unexpected error: object 'noSuchObjectExists' not found"
       expect_testResult_validateSourceFile(got, 'checkParam_checksumFunc', want )
    })
})
describe( "Failing 'fileSize=' parameter", {
   # Null ok
   it( "is an object but not a vector", {
      got <- validateSourceFile( path= emptyFile, fileSize= list(fileSize=0) )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a vector of mode numeric."
      expect_testResult_validateSourceFile(got, 'checkParam_fileSize', want)
   })
   it( "is a vector but not a numeric vector", {
      got <- validateSourceFile( path= emptyFile, fileSize= '123K' )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a vector of mode numeric."
      expect_testResult_validateSourceFile(got, 'checkParam_fileSize', want)
   })
   it( "is a numeric vector, but is empty", {
      got <- validateSourceFile( path= emptyFile, fileSize= integer(0) )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateSourceFile(got, 'checkParam_fileSize', want)
   })
   it( "is a numeric vector, but has multiple element", {
      got <- validateSourceFile( path= emptyFile, fileSize= c(100, 1000) )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Length is not 1."
      expect_testResult_validateSourceFile(got, 'checkParam_fileSize', want)
   })
   # A number means a 1 element numeric vector.
   it( "is a number, but it is NaN", {
      got <- validateSourceFile(  emptyFile, fileSize= NaN )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Is NaN."
      expect_testResult_validateSourceFile(got, 'checkParam_fileSize', want)
   })
   it( "is a number, but it is missing", {
      got <- validateSourceFile( emptyFile, fileSize= NA_real_ )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Is NA."
      expect_testResult_validateSourceFile(got, 'checkParam_fileSize', want)

   })
   it( "is a number, but it is negative", {
      got <- validateSourceFile( emptyFile, fileSize= -100 )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Is negative."
      expect_testResult_validateSourceFile(got, 'checkParam_fileSize', want)
   })
   it( "it can have unexpected failures.", {
      expect_false(exists('noSuchObjectExists'))
      got <- validateSourceFile( path= emptyFile, fileSize= noSuchObjectExists )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Unexpected error: object 'noSuchObjectExists' not found"
      expect_testResult_validateSourceFile(got, 'checkParam_fileSize', want )
   })
})
describe( "cross-parameters tests", {
   describe( "'checksumFunc=' validity depends on 'checksum='", {
      it("may not be null if checksum is not NULL.", {
        got <- validateSourceFile( emptyFile, checksum= '1', checksumFunc = NULL)
        expect_resultFormat_validateSourceFile(got, checked)
        want <- 'Checksum but no checksum function.'
        expect_testResult_validateSourceFile(got, 'checkParam_checksum_checksumFunc', want)
      })
      it( "it can have unexpected failures.", {
         skip("No way to trigger this error is apparant - nocovr used in code")
      })
   })
})

context( "validateSourceFile() checkIsFile test" )
describe( "checkIsFile, default params with various paths", {
   it( "Succeeds if path is an existing file on the filesystem", {
      want <- ""
      got <- validateSourceFile( path= emptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsFile", want)
   })
   it( "Fails if path does not exist on the filesystem", {
      want <- "No such path."
      got <- validateSourceFile( path= badPath )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsFile", want)
   })
   it( "Fails if path is a dir", {
      got <- validateSourceFile( path= sourceDir )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a file."
      expect_testResult_validateSourceFile(got, "checkIsFile", want)
   })
   it( "Succeeds if path is link to a file", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      want <- ""
      got <- validateSourceFile( path= fileLink )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsFile", want)
   })
   it( "Fails if path is a bad (no-target) link", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      else if (! hasBadLinks) {
         skip("Symlinks without existing targets are not allowed on this os.")
      }
      got <- validateSourceFile( path= badPath )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "No such path."
      expect_testResult_validateSourceFile(got, "checkIsFile", want)
   })
   it( "Fails if path is a link to a directory", {
      if (! hasDirLinks) {
         skip("Symlinks to directories are not supported on this os.")
      }
      got <- validateSourceFile( path= dirLink )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a file."
      expect_testResult_validateSourceFile(got, "checkIsFile", want)
   })
   it( "Succeeds if path is link to a link to a file", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      else if (! hasFileLinkLinks) {
         skip("Transitive symlinks to files are not supported on this os.")
      }
      want <- ""
      got <- validateSourceFile( path= fileLinkLink )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsFile", want)
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
      got <- validateSourceFile( path= badLinkLink )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "No such path."
      expect_testResult_validateSourceFile(got, "checkIsFile", want)
   })
   it( "Fails if path is a link to a link to a directory", {
      if (! hasDirLinks) {
         skip("Symlinks to directories are not supported on this os.")
      }
      if (! hasDirLinkLinks) {
         skip("Transitive symlinks to directories are not supported on this os.")
      }
      got <- validateSourceFile( path= dirLinkLink )
      expect_resultFormat_validateSourceFile(got, checked)
      want <- "Not a file."
      expect_testResult_validateSourceFile(got, "checkIsFile", want)
   })
})

context( "validateSourceFile() checkIsLink test" )
describe( "checkIsLink, default params with various paths", {
   it( "Succeeds if path is an existing file on the filesystem", {
      want <- ""
      got <- validateSourceFile( path= emptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if file does not exist on the filesystem", {
      want <- NA_character_
      got <- validateSourceFile( path= badPath )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if path is a dir", {
      want <- NA_character_
      got <- validateSourceFile( path= sourceDir )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsNotLink", want)
   })
   it( "Fails if path is link to a file", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      want <- "Is a link."
      got <- validateSourceFile( path= fileLink )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if path is a bad (no-target) link", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      else if (! hasBadLinks) {
         skip("Symlinks without existing targets are not allowed on this os.")
      }
      want <- NA_character_
      got <- validateSourceFile( path= badPath )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if path is a link to a directory", {
      if (! hasDirLinks) {
         skip("Symlinks to directories are not supported on this os.")
      }
      want <- NA_character_
      got <- validateSourceFile( path= dirLink )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsNotLink", want)
   })
   it( "Fails if path is link to a link to a file", {
      if (! hasFileLinks) {
         skip("Symlinks to files are not supported on this os.")
      }
      else if (! hasFileLinkLinks) {
         skip("Transitive symlinks to files are not supported on this os.")
      }
      want <- "Is a link."
      got <- validateSourceFile( path= fileLinkLink )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsNotLink", want)
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
      got <- validateSourceFile( path= badLinkLink )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsNotLink", want)
   })
   it( "Never tested if path is a link to a link to a directory", {
      if (! hasDirLinks) {
         skip("Symlinks to directories are not supported on this os.")
      }
      if (! hasDirLinkLinks) {
         skip("Transitive symlinks to directories are not supported on this os.")
      }
      want <- NA_character_
      got <- validateSourceFile( path= dirLinkLink )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkIsNotLink", want)
   })
})

context( "validateSourceFile() checkFileSizeMatches test" )
describe( "Not running a size check test (default)", {
   it("Returns NA when fileSize is NULL and checksum (good) is given", {
      want <- NA_character_
      got <- validateSourceFile( path= emptyFile, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it("Returns NA when fileSize is NULL and checksum (bad) is given", {
      want <- NA_character_
      got <- validateSourceFile( path= emptyFile, fileSize=NULL, checksum= 0 )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it("Returns NA when fileSize is NULL and checksum is NULL", {
      want <- NA_character_
      got <- validateSourceFile( path= emptyFile, fileSize=NULL, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
})
describe( "size check succeeds if size correct (fileSize= parameter)", {
   it("succeeds for a file if checksum (bad) is given", {
      want <- ""
      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= 'A' )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it("succeeds for a file if checksum (good) is given", {
      want <- ""
      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it("succeeds for a file if checksum is NULL", {
      want <- ""
      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
})
describe( "size check fails if size wrong (fileSize= parameter)", {
   it("fails if checksum (bad) is given", {
      want <- "File size mismatch. Found 0 wanted 1."
      got <- validateSourceFile( path= emptyFile, fileSize= 1, checksum= 'A' )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it("fails if checksum (good) is given", {
      want <- "File size mismatch. Found 0 wanted 1001."
      got <- validateSourceFile( path= emptyFile, fileSize= 1001, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it("fails if checksum is NULL", {
      want <- "File size mismatch. Found 0 wanted 123456789."
      got <- validateSourceFile( path= emptyFile, fileSize= 123456789, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
})
describe( "Internal failures in checkFileSizeMatches tests", {
   it ("fails if stat doesn't return a data frame", {
      want <- "Can't stat file."
      with_mock(
         `base::is.data.frame`= function (...) FALSE,
         got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if stat doesn't return a size value", {
      want <- "Read file size as null."
      stat <- file.info(emptyFile)
      stat$size <- NULL
      with_mock(
         `base::file.info`= function (...) {stat},
         got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if stat doesn't return a numeric size value", {
      want <- "Not a vector of mode numeric."
      stat <- file.info(emptyFile)
      stat$size <- 'Oops'
      with_mock(
         `base::file.info`= function (...) {stat},
         got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if stat returns a NaN size value", {
      want <- "Read file size as NaN."
      stat <- file.info(emptyFile)
      stat$size <- NaN
      with_mock(
         `base::file.info`= function (...) {stat},
         got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if stat returns a NA size value", {
      want <- "Read file size as NA."
      stat <- file.info(emptyFile)
      stat$size <- NA_integer_
      with_mock(
         `base::file.info`= function (...) {stat},
         got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if stat returns a negative size value", {
      want <- "Read file size as negative."
      stat <- file.info(emptyFile)
      stat$size <- -1
      with_mock(
         `base::file.info`= function (...) {stat},
         got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })
   it ("fails if unexpected error is triggered", {
      want <- "Unexpected error: Bad is.data.frame!"
      with_mock(
         `base::is.data.frame`= function (...) stop('Bad is.data.frame!'),
         got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkFileSizeMatches", want)
   })

})

context( "validateSourceFile() checkChecksumMatches test" )
describe( "Not running a checksum test when checksum= NULL(default)", {
   it("Returns NA when fileSize is NULL", {
      want <- NA_character_

      got <- validateSourceFile( path= emptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= emptyFile, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= emptyFile, fileSize= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= emptyFile, fileSize= NULL, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is bad", {
      want <- NA_character_

      got <- validateSourceFile( path= emptyFile, fileSize= 100 )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= emptyFile, fileSize= 100, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is good", {
      want <- NA_character_

      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
})
describe( "Result of checksum tests when checksum= bad value", {
   it("Returns failure when fileSize is NULL", {
      want <- "Checksum mismatch. Found d41d8cd98f00b204e9800998ecf8427e wanted abc123."

      got <- validateSourceFile( path= emptyFile, checksum= 'abc123' )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= emptyFile, checksum= 'abc123', fileSize= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA string when fileSize is bad", {
      want <- NA_character_
      got <- validateSourceFile( path= emptyFile, fileSize= 1, checksum= 'abc123' )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns failure string when fileSize is good", {
      want <- "Checksum mismatch. Found d41d8cd98f00b204e9800998ecf8427e wanted abc123."
      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= 'abc123' )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
})
describe( "Result of checksum tests when checksum= correct value", {
   it("Returns success when fileSize is NULL", {
      want <- ""

      got <- validateSourceFile( path= emptyFile, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= emptyFile, checksum= md5EmptyFile, fileSize= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is bad", {
      want <- NA_character_
      got <- validateSourceFile( path= emptyFile, fileSize= 1, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns succeess (empty string) when fileSize is good", {
      want <- ""
      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
})
describe( "Internal failures in checkChecksumMatches tests", {
   it( "Returns failure if checksum was calculated to be NULL.", {
      want <- "Calculated checksum was null."
      badCheckFunc <- function (...) return(NULL)
      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksumFunc= badCheckFunc, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it( "Returns failure if checksum was calculated to be missing.", {
      want <- "Calculated checksum was NA."
      badCheckFunc <- function (...) return(NA_character_)
      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksumFunc= badCheckFunc, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it( "Returns failure if checksum signals an error.", {
      want <- "Unexpected error: bad checksum func!"
      badCheckFunc <- function (...) stop( 'bad checksum func!' )
      got <- validateSourceFile( path= emptyFile, fileSize= emptyFileSize, checksumFunc= badCheckFunc, checksum= md5EmptyFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it( "Returns failure if checksum calculation interrupted by an interrupt signal.", {
      skip('No idea how to test this. Hidden from coverage testing in code.')
   })
})

context( "validateSourceFile() checksumFunc=" )
describe( "Checking new checksum function", {
   expect_equal( SumIt(emptyFile), 0 )
   expect_equal( SumIt(binFile), SumItBinFile )
})
describe( "Not running a checksum test when checksum= NULL(default)", {
   it("Returns NA when fileSize is NULL", {
      want <- NA_character_

      got <- validateSourceFile( path= binFile, checksumFunc= SumIt )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= binFile, checksumFunc= SumIt, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= binFile, checksumFunc= SumIt, fileSize= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= binFile, fileSize= NULL, checksumFunc= SumIt, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is bad", {
      want <- NA_character_

      got <- validateSourceFile( path= binFile, checksumFunc= SumIt, fileSize= 100 )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= binFile, checksumFunc= SumIt, fileSize= 100, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is good", {
      want <- NA_character_

      got <- validateSourceFile( path= binFile, checksumFunc= SumIt, fileSize= binFileSize )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= binFile, fileSize= binFileSize, checksumFunc= SumIt, checksum= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
})
describe( "Result of checksum tests when checksum= bad value", {
   it("Returns failure when fileSize is NULL", {
      want <- "Checksum mismatch. Found 6 wanted 5289df737df57326fcdd22597afb1fac."

      got <- validateSourceFile( path= binFile, checksum= md5BinFile, checksumFunc = SumIt )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= binFile, checksum= md5BinFile, checksumFunc = SumIt, fileSize= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA string when fileSize is bad", {
      want <- NA_character_
      got <- validateSourceFile( path= binFile, fileSize= emptyFileSize, checksumFunc= SumIt, checksum= 'abc123' )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns failure string when fileSize is good", {
      want <- "Checksum mismatch. Found 6 wanted abc123."
      got <- validateSourceFile( path= binFile, fileSize= binFileSize, checksumFunc= SumIt, checksum= 'abc123' )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
})
describe( "Result of checksum tests when checksum= correct value", {
   it("Returns success when fileSize is NULL", {
      want <- ""

      got <- validateSourceFile( path= binFile, checksumFunc= SumIt, checksum= SumItBinFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)

      got <- validateSourceFile( path= binFile, checksumFunc= SumIt, checksum= SumItBinFile, fileSize= NULL )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns NA when fileSize is bad", {
      want <- NA_character_
      got <- validateSourceFile( path= binFile, fileSize= 1, checksumFunc= SumIt, checksum= SumItBinFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
   it("Returns succeess (empty string) when fileSize is good", {
      want <- ""
      got <- validateSourceFile( path= binFile, fileSize= binFileSize, checksumFunc= SumIt, checksum= SumItBinFile )
      expect_resultFormat_validateSourceFile(got, checked)
      expect_testResult_validateSourceFile(got, "checkChecksumMatches", want)
   })
})
