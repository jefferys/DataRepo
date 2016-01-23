
context( 'checkFileSystem setup' )
#============================================================================

noSuchPath <- tempfile( 'noSuchPath')

emptyFile <- tempfile( 'emptyFile' )
emptyDir  <- tempfile( 'emptyDir'  )
file.create( emptyFile )
dir.create(  emptyDir  )

linkToEmptyFile <- tempfile( 'linkToEmptyFile' )
linkToEmptyDir  <- tempfile( 'linkToEmptyDir'  )
linkToNowhere   <- tempfile( 'linkToNowhere'   )
noFileLinks <- ! file.symlink( emptyFile,  linkToEmptyFile )
noDirLinks  <- ! file.symlink( emptyDir,   linkToEmptyDir  )
noBadLinks  <- ! file.symlink( noSuchPath, linkToNowhere   )

linkToLinkToEmptyFile <- tempfile( 'linkToLinkToEmptyFile' )
linkToLinkToNowhere   <- tempfile( 'linkToLinkToNowhere'   )
noLinkToLinks    <- ! file.symlink( linkToEmptyFile, linkToLinkToEmptyFile )
noLinkToBadLinks <- ! file.symlink( linkToNowhere,   linkToLinkToNowhere )

describe( "The setup for testing checkIsPath and friends.", {
   it( "Provides a non-existing path.", {
      expect_false( file.exists( noSuchPath ))
   })
   it("Provides a path to and existing file", {
      expect_true( file.exists( emptyFile ))
      expect_false( dir.exists( emptyFile ))
   })
   it("Provides a path to and existing dir", {
      expect_true( file.exists( emptyDir ))
      expect_true( dir.exists( emptyDir ))
   })
})

context( 'checkIsPath()' )
#============================================================================
describe( "checkIsPath() - path=", {
   it( "'path' exists? Return an empty string.", {
      want <- ""
      got <- checkIsPath( emptyFile )
      expect_equal( got, want)
      got <- checkIsPath( emptyDir )
      expect_equal( got, want)
   })
   it( "'path' does not exist? Return a failure string.", {
      want <- "No such path."
      got <- checkIsPath( noSuchPath )
      expect_equal( got, want)
   })
   it( "'path' is a bad value? Return an error string (not an error).", {
      WantRE <- paste0(
         "Checking for a path failed with the following error:",
         " invalid 'file' argument"
      )
      expect_silent( { got <- checkIsPath( NA )} )
      expect_match( got, WantRE)
   })
   it( "Internal error? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a path failed with the following error:",
         " naughty 'file.exists'"
      )
      with_mock(
         `base::file.exists` = function(...) stop("naughty 'file.exists'"),
         expect_silent({ got <- checkIsPath( emptyFile ) })
      )
      expect_match( got, wantRE)
   })
   it( "'path' exists but is a file symlink? Return an empty string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      want <- ""
      got <- checkIsPath( linkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a dir symlink? Return an empty string.", {
      if (noDirLinks) skip("Symlinks to files are not supported on this os.")
      want <- ""
      got <- checkIsPath( linkToEmptyDir )
      expect_equal( got, want)
   })
   it( "'path' exists but is a no-target symlink? Return a failure string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      if (noBadLinks) {skip("Symlinks to missing paths are not allowed on this os.")}
      want <- "No such path."
      got <- checkIsPath( linkToNowhere )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a symlink? Return an empty string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- ""
      got <- checkIsPath( linkToLinkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- "No such path."
      got <- checkIsPath( linkToLinkToNowhere )
      expect_equal( got, want)
   })
})

context( 'checkIsNotPath()' )
#============================================================================
describe( "checkIsNotPath() - path=", {
   it( "'path' does not exist? Return an empty string.", {
      want <- ""
      got <- checkIsNotPath( noSuchPath )
      expect_equal( got, want )
   })
   it( "'path' exists? Return a failure string.", {
      want <- "Path exists."
      got <- checkIsNotPath( emptyFile )
      expect_match( got, want)
   })
   it( "'path' is a bad value? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a path failed with the following error:",
         " invalid 'file' argument"
      )
      expect_silent({ got <- checkIsNotPath( NA ) })
      expect_match( got, wantRE)
   })
   it( "Internal error? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a path failed with the following error:",
         " naughty 'file.exists'"
      )
      with_mock(
         `base::file.exists` = function(...) stop("naughty 'file.exists'"),
         expect_silent({ got <- checkIsNotPath( emptyFile ) })
      )
      expect_match( got, wantRE)
   })
   it( "'path' exists but is a file symlink? Return a failure string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      want <- "Path exists."
      got <- checkIsNotPath( linkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a dir symlink? Return a failure string.", {
      if (noDirLinks) skip("Symlinks to files are not supported on this os.")
      want <- "Path exists."
      got <- checkIsNotPath( linkToEmptyDir )
      expect_equal( got, want)
   })
   it( "'path' exists but is a no-target symlink? Return an empty string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      if (noBadLinks) {skip("Symlinks to missing paths are not allowed on this os.")}
      want <- ""
      got <- checkIsNotPath( linkToNowhere )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a symlink? Return a failure  string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- "Path exists."
      got <- checkIsNotPath( linkToLinkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a no-target symlink? Return an empty string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- ""
      got <- checkIsNotPath( linkToLinkToNowhere )
      expect_equal( got, want)
   })

})

context( 'checkIsFile()' )
#============================================================================
describe( "checkIsFile() - path=", {
   it( "'path' exists and is file? Return an empty string.", {
      want <- ""
      got <- checkIsFile( emptyFile )
      expect_equal( got, want)
   })
   it( "'path' does not exist? Return a failure string.", {
      want <- "No such path."
      got <- checkIsFile( noSuchPath )
      expect_equal( got, want)
   })
   it( "'path' exists but is not a file? Return a failure string.", {
      want <- "Not a file."
      got <- checkIsFile( emptyDir )
      expect_equal( got, want)
   })
   it( "'path' is a bad value? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a file failed with the following error:",
         " invalid 'file' argument"
      )
      expect_silent({ got <- checkIsFile( NA ) })
      expect_match( got, wantRE)
   })
   it( "Internal error? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a file failed with the following error:",
         " naughty 'file.info'"
      )
      with_mock(
         `base::file.info` = function(...) stop("naughty 'file.info'"),
         expect_silent({ got <- checkIsFile( emptyFile ) })
      )
      expect_match( got, wantRE)
   })
   it( "'path' exists but is a file symlink? Return an empty string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      want <- ""
      got <- checkIsFile( linkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a dir symlink? Return a failure string.", {
      if (noDirLinks) skip("Symlinks to files are not supported on this os.")
      want <- "Not a file."
      got <- checkIsFile( linkToEmptyDir )
      expect_equal( got, want)
   })
   it( "'path' exists but is a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      want <- "No such path."
      got <- checkIsFile( linkToNowhere )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a file symlink? Return an empty string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- ""
      got <- checkIsFile( linkToLinkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noLinkToLinks) skip("Symlinks to links are not allowed on this os.")
      want <- "No such path."
      got <- checkIsFile( linkToLinkToNowhere )
      expect_equal( got, want)
   })

})
describe( "checkIsFile() - path.info=", {

   fileInfo    <- file.info(emptyFile)
   dirInfo     <- file.info(emptyDir)
   missingInfo <- file.info(noSuchPath)
   badInfo <- FALSE

   it( "Does not call stat when path.info is provided", {
      want <- ""
      with_mock(
         `base::file.info` = function(...) stop("naughty"),
         expect_silent({ got <- checkIsFile( emptyFile, path.info= fileInfo ) })
      )
      expect_equal( got, want)
   })

   describe( "results with correct path.info", {
      it( "'path' exists and is file? Return an empty string.", {
         want <- ""
         got <- checkIsFile( emptyFile, path.info= fileInfo )
         expect_equal( got, want)
      })
      it( "'path' does not exist? Return a failure string.", {
         want <- "No such path."
         got <- checkIsFile( noSuchPath, path.info= missingInfo )
         expect_equal( got, want)
      })
      it( "'path' exists but is not a file? Return a failure string.", {
         want <- "Not a file."
         got <- checkIsFile( emptyDir, path.info= dirInfo )
         expect_equal( got, want)
      })
      it( "'path' is a bad value? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a file failed with the following error:",
            " invalid 'file' argument"
         )
         expect_silent({ got <- checkIsFile( NA, path.info= fileInfo ) })
         expect_match( got, wantRE)
      })
      it(  "Internal error? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a file failed with the following error:",
            " naughty 'file.exists'"
         )
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsFile( emptyFile, path.info= fileInfo ) })
         )
         expect_match( got, wantRE)
      })
      # Note, file.info on link returns the information about the ultimate target, possibly NA.
      it( "'path' exists but is a file symlink? Return an empty string.", {
         if (noFileLinks) skip("Symlinks to files are not supported on this os.")
         want <- ""
         got <- checkIsFile( linkToEmptyFile, path.info= file.info( linkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a dir symlink? Return a failure string.", {
         if (noDirLinks) skip("Symlinks to files are not supported on this os.")
         want <- "Not a file."
         got <- checkIsFile( linkToEmptyDir, path.info= file.info( linkToEmptyDir ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a no-target symlink? Return a failure string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
         want <- "No such path."
         got <- checkIsFile( linkToNowhere, path.info= file.info( linkToNowhere ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a symlink to a file symlink? Return an empty string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
         want <- ""
         got <- checkIsFile( linkToLinkToEmptyFile, path.info= file.info( linkToLinkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
         if (noLinkToLinks) skip("Symlinks to links are not allowed on this os.")
         want <- "No such path."
         got <- checkIsFile( linkToLinkToNowhere, path.info= file.info( linkToLinkToNowhere ))
         expect_equal( got, want)
      })
   })
   describe( "results with incorrect path.info", {
      it( "'path.info' valid but wrong? Use anyway", {
         want <- "Not a file."
         got <- checkIsFile( emptyFile, path.info= dirInfo )
         expect_equal( got, want)
         got <- checkIsFile( emptyFile, path.info= file.info( linkToEmptyDir ))
         expect_equal( got, want)

         want <- ""
         got <- checkIsFile( emptyDir, path.info= fileInfo ) # dir but with file info
         expect_equal( got, want)
         got <- checkIsFile( emptyDir, path.info= file.info( linkToEmptyFile ))
         expect_equal( got, want)
         got <- checkIsFile( emptyDir, path.info= file.info( linkToLinkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path.info' is a bad value? Return an error string (not an error).", {
         wantRE <- "Checking for a file failed with the following error: .+"

         got <- checkIsFile( emptyFile, path.info = badInfo )
         expect_match( got, wantRE)
         got <- checkIsFile( emptyFile, path.info= missingInfo )
         expect_match( got, wantRE)
         got <- checkIsFile( emptyFile, path.info= file.info( linkToNowhere ))
         expect_match( got, wantRE)
         got <- checkIsFile( emptyFile, path.info= file.info( linkToLinkToNowhere ))
         expect_match( got, wantRE)
      })
      it( "'path.info' ignored if 'path' does not exists.", {
         want <- "No such path."
         got <- checkIsFile( noSuchPath, path.info= fileInfo ) # file but with dir info
         expect_match( got, want)
         got <- checkIsFile( noSuchPath, path.info= badInfo )
         expect_match( got, want)
      })
      it( "'path.info' ignored if 'path' is bad.", {
         wantRE <- paste0(
            "Checking for a file failed with the following error:",
            " invalid 'file' argument"
         )

         got <- checkIsFile( NA, path.info= fileInfo )
         expect_match( got, wantRE)

         got <- checkIsFile( NA, path.info= missingInfo  )
         expect_match( got, wantRE)
      })
      it( "Internal error? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a file failed with the following error:",
            " naughty 'file.exists'"
         )
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsFile( emptyFile, path.info= dirInfo ) })
         )
         expect_match( got, wantRE)
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsFile( emptyFile, path.info= badInfo ) })
         )
         expect_match( got, wantRE)
      })
   })
})

context( 'checkIsNotFile()' )
#============================================================================
describe( "checkIsNotFile() - path=", {
   it( "'path' exists and is not a file? Return an empty string.", {
      want <- ""
      got <- checkIsNotFile( emptyDir )
      expect_equal( got, want)
   })
   it( "'path' does not exist? Return a failure string.", {
      want <- "No such path."
      got <- checkIsNotFile( noSuchPath )
      expect_equal( got, want)
   })
   it( "'path' exists but is a file? Return a failure string.", {
      want <- "Is a file."
      got <- checkIsNotFile( emptyFile )
      expect_equal( got, want)
   })
   it( "'path' is a bad value? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a file failed with the following error:",
         " invalid 'file' argument"
      )
      expect_silent({ got <- checkIsNotFile( NA ) })
      expect_match( got, wantRE)
   })
   it( "Internal error? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a file failed with the following error:",
         " naughty 'file.info'"
      )
      with_mock(
         `base::file.info` = function(...) stop("naughty 'file.info'"),
         expect_silent({ got <- checkIsNotFile( emptyFile ) })
      )
      expect_match( got, wantRE)
   })
   it( "'path' exists but is a file symlink? Return a failure string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      want <- "Is a file."
      got <- checkIsNotFile( linkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a dir symlink? Return an empty string.", {
      if (noDirLinks) skip("Symlinks to files are not supported on this os.")
      want <- ""
      got <- checkIsNotFile( linkToEmptyDir )
      expect_equal( got, want)
   })
   it( "'path' exists but is a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      want <- "No such path."
      got <- checkIsNotFile( linkToNowhere )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a file symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- "Is a file."
      got <- checkIsNotFile( linkToLinkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      if (noLinkToLinks) skip("Symlinks to links are not allowed on this os.")
      want <- "No such path."
      got <- checkIsNotFile( linkToLinkToNowhere )
      expect_equal( got, want)
   })

})
describe( "checkIsNotFile() - path.info=", {

   fileInfo    <- file.info(emptyFile)
   dirInfo     <- file.info(emptyDir)
   missingInfo <- file.info(noSuchPath)
   badInfo <- FALSE

   it( "Does not call stat when path.info is provided", {
      want <- ""
      with_mock(
         `base::file.info` = function(...) stop("naughty"),
         expect_silent({ got <- checkIsNotFile( emptyDir, path.info= dirInfo ) })
      )
      expect_equal( got, want)
   })

   describe( "results with correct path.info", {
      it( "'path' exists and is not file? Return an empty string.", {
         want <- ""
         got <- checkIsNotFile( emptyDir, path.info= dirInfo )
         expect_equal( got, want)
      })
      it( "'path' does not exist? Return a failure string.", {
         want <- "No such path."
         got <- checkIsNotFile( noSuchPath, path.info= missingInfo )
         expect_equal( got, want)
      })
      it( "'path' exists but is a file? Return a failure string.", {
         want <- "Is a file."
         got <- checkIsNotFile( emptyFile, path.info= fileInfo )
         expect_equal( got, want)
      })
      it( "'path' is a bad value? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a file failed with the following error:",
            " invalid 'file' argument"
         )
         expect_silent({ got <- checkIsNotFile( NA, path.info= dirInfo ) })
         expect_match( got, wantRE)
      })
      it(  "Internal error? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a file failed with the following error:",
            " naughty 'file.exists'"
         )
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsNotFile( emptyDir, path.info= dirInfo) })
         )
         expect_match( got, wantRE)
      })
      # Note, file.info on link returns the information about the ultimate target, possibly NA.
      it( "'path' exists but is a file symlink? Return a failure string.", {
         if (noFileLinks) skip("Symlinks to files are not supported on this os.")
         want <- "Is a file."
         got <- checkIsNotFile( linkToEmptyFile, path.info= file.info( linkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a dir symlink? Return an empty string.", {
         if (noDirLinks) skip("Symlinks to files are not supported on this os.")
         want <- ""
         got <- checkIsNotFile( linkToEmptyDir, path.info= file.info( linkToEmptyDir ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a no-target symlink? Return a failure string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
         want <- "No such path."
         got <- checkIsNotFile( linkToNowhere, path.info= file.info( linkToNowhere ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a symlink to a file symlink? Return a failure string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
         want <- "Is a file."
         got <- checkIsNotFile( linkToLinkToEmptyFile, path.info= file.info( linkToLinkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
         if (noLinkToLinks) skip("Symlinks to links are not allowed on this os.")
         want <- "No such path."
         got <- checkIsNotFile( linkToLinkToNowhere, path.info= file.info( linkToLinkToNowhere ))
         expect_equal( got, want)
      })
   })
   describe( "results with incorrect path.info", {
      it( "'path.info' valid but wrong? Use anyway", {
         want <- ""
         got <- checkIsNotFile( emptyFile, path.info= dirInfo ) # file but with dir info
         expect_equal( got, want)
         got <- checkIsNotFile( emptyFile, path.info= file.info( linkToEmptyDir ))
         expect_equal( got, want)

         want <- "Is a file."
         got <- checkIsNotFile( emptyDir, path.info= fileInfo ) # dir but with file info
         expect_equal( got, want)
         got <- checkIsNotFile( emptyDir, path.info= file.info( linkToEmptyFile ))
         expect_equal( got, want)
         got <- checkIsNotFile( emptyDir, path.info= file.info( linkToLinkToEmptyFile ))
         expect_equal( got, want)

      })
      it( "'path.info' is a bad value? Return an error string (not an error).", {
         wantRE <- "Checking for a file failed with the following error: .+"

         got <- checkIsNotFile( emptyDir, path.info = badInfo )
         expect_match( got, wantRE)
         got <- checkIsNotFile( emptyDir, path.info= missingInfo )
         expect_match( got, wantRE)
         got <- checkIsNotFile( emptyDir, path.info= file.info( linkToNowhere ))
         expect_match( got, wantRE)
         got <- checkIsNotFile( emptyDir, path.info= file.info( linkToLinkToNowhere ))
         expect_match( got, wantRE)
      })
      it( "'path.info' ignored if 'path' does not exists.", {
         want <- "No such path."
         got <- checkIsNotFile( noSuchPath, path.info= dirInfo ) # file but with dir info
         expect_match( got, want)
         got <- checkIsNotFile( noSuchPath, path.info= badInfo )
         expect_match( got, want)
      })
      it( "'path.info' ignored if 'path' is bad.", {
         wantRE <- paste0(
            "Checking for a file failed with the following error:",
            " invalid 'file' argument"
         )

         got <- checkIsNotFile( NA, path.info= dirInfo )
         expect_match( got, wantRE)

         got <- checkIsNotFile( NA, path.info= missingInfo  )
         expect_match( got, wantRE)
      })
      it( "Internal error? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a file failed with the following error:",
            " naughty 'file.exists'"
         )
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsNotFile( emptyDir, path.info= fileInfo ) })
         )
         expect_match( got, wantRE)
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsNotFile( emptyDir, path.info= badInfo ) })
         )
         expect_match( got, wantRE)
      })
   })
})

context( "checkIsDir()" )
#============================================================================
describe( "checkIsDir() - path=", {
   it( "'path' exists and is dir? Return an empty string.", {
      want <- ""
      got <- checkIsDir( emptyDir )
      expect_equal( got, want)
   })
   it( "'path' does not exist? Return a failure string.", {
      want <- "No such path."
      got <- checkIsDir( noSuchPath )
      expect_equal( got, want)
   })
   it( "'path' exists but is not a dir? Return a failure string.", {
      want <- "Not a directory."
      got <- checkIsDir( emptyFile )
      expect_equal( got, want)
   })
   it( "'path' is a bad value? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a directory failed with the following error:",
         " invalid 'file' argument"
      )
      expect_silent({ got <- checkIsDir( NA ) })
      expect_match( got, wantRE)
   })
   it( "Internal error? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a directory failed with the following error:",
         " naughty 'file.info'"
      )
      with_mock(
         `base::file.info` = function(...) stop("naughty 'file.info'"),
         expect_silent({ got <- checkIsDir( emptyDir ) })
      )
      expect_match( got, wantRE)
   })
   it( "'path' exists but is a file symlink? Return a failure string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      want <- "Not a directory."
      got <- checkIsDir( linkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a dir symlink? Return an empty string.", {
      if (noDirLinks) skip("Symlinks to files are not supported on this os.")
      want <- ""
      got <- checkIsDir( linkToEmptyDir )
      expect_equal( got, want)
   })
   it( "'path' exists but is a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      want <- "No such path."
      got <- checkIsDir( linkToNowhere )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a file symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- "Not a directory."
      got <- checkIsDir( linkToLinkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      if (noLinkToLinks) skip("Symlinks to links are not allowed on this os.")
      want <- "No such path."
      got <- checkIsDir( linkToLinkToNowhere )
      expect_equal( got, want)
   })

})
describe( "checkIsDir() - path.info=", {

   fileInfo    <- file.info(emptyFile)
   dirInfo     <- file.info(emptyDir)
   missingInfo <- file.info(noSuchPath)
   badInfo <- FALSE

   it( "Does not call stat when path.info is provided", {
      want <- ""
      with_mock(
         `base::file.info` = function(...) stop("naughty"),
         expect_silent({ got <- checkIsDir( emptyDir, path.info= dirInfo ) })
      )
      expect_equal( got, want)
   })

   describe( "results with correct path.info", {
      it( "'path' exists and is dir? Return an empty string.", {
         want <- ""
         got <- checkIsDir( emptyDir, path.info= dirInfo )
         expect_equal( got, want)
      })
      it( "'path' does not exist? Return a failure string.", {
         want <- "No such path."
         got <- checkIsDir( noSuchPath, path.info= missingInfo )
         expect_equal( got, want)
      })
      it( "'path' exists but is not a dir? Return a failure string.", {
         want <- "Not a directory."
         got <- checkIsDir( emptyFile, path.info= fileInfo )
         expect_equal( got, want)
      })
      it( "'path' is a bad value? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a directory failed with the following error:",
            " invalid 'file' argument"
         )
         expect_silent({ got <- checkIsDir( NA, path.info= dirInfo ) })
         expect_match( got, wantRE)
      })
      it(  "Internal error? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a directory failed with the following error:",
            " naughty 'file.exists'"
         )
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsDir( emptyDir, path.info= dirInfo) })
         )
         expect_match( got, wantRE)
      })
      # Note, file.info on link returns the information about the ultimate target, possibly NA.
      it( "'path' exists but is a file symlink? Return a failure string.", {
         if (noFileLinks) skip("Symlinks to files are not supported on this os.")
         want <- "Not a directory."
         got <- checkIsDir( linkToEmptyFile, path.info= file.info( linkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a dir symlink? Return an empty string.", {
         if (noDirLinks) skip("Symlinks to files are not supported on this os.")
         want <- ""
         got <- checkIsDir( linkToEmptyDir, path.info= file.info( linkToEmptyDir ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a no-target symlink? Return a failure string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
         want <- "No such path."
         got <- checkIsDir( linkToNowhere, path.info= file.info( linkToNowhere ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a symlink to a file symlink? Return a failure string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
         want <- "Not a directory."
         got <- checkIsDir( linkToLinkToEmptyFile, path.info= file.info( linkToLinkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
         if (noLinkToLinks) skip("Symlinks to links are not allowed on this os.")
         want <- "No such path."
         got <- checkIsDir( linkToLinkToNowhere, path.info= file.info( linkToLinkToNowhere ))
         expect_equal( got, want)
      })
   })
   describe( "results with incorrect path.info", {
      it( "'path.info' valid but wrong? Use anyway", {
         want <- ""
         got <- checkIsDir( emptyFile, path.info= dirInfo ) # file but with dir info
         expect_equal( got, want)
         got <- checkIsDir( emptyFile, path.info= file.info( linkToEmptyDir ))
         expect_equal( got, want)

         want <- "Not a directory."
         got <- checkIsDir( emptyDir, path.info= fileInfo ) # dir but with file info
         expect_equal( got, want)
         got <- checkIsDir( emptyDir, path.info= file.info( linkToEmptyFile ))
         expect_equal( got, want)
         got <- checkIsDir( emptyDir, path.info= file.info( linkToLinkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path' is a bad value? Return an error string (not an error).", {
         wantRE <- "Checking for a directory failed with the following error: .+"

         got <- checkIsDir( emptyDir, path.info = badInfo )
         expect_match( got, wantRE)
         got <- checkIsDir( emptyDir, path.info= missingInfo )
         expect_match( got, wantRE)
         got <- checkIsDir( emptyDir, path.info= file.info( linkToNowhere ))
         expect_match( got, wantRE)
         got <- checkIsDir( emptyDir, path.info= file.info( linkToLinkToNowhere ))
         expect_match( got, wantRE)

      })
      it( "'path.info' ignored if 'path' does not exists.", {
         want <- "No such path."
         got <- checkIsDir( noSuchPath, path.info= dirInfo ) # file but with dir info
         expect_match( got, want)
         got <- checkIsDir( noSuchPath, path.info= badInfo )
         expect_match( got, want)
      })
      it( "'path.info' ignored if 'path' is bad.", {
         wantRE <- paste0(
            "Checking for a directory failed with the following error:",
            " invalid 'file' argument"
         )

         got <- checkIsDir( NA, path.info= dirInfo )
         expect_match( got, wantRE)

         got <- checkIsDir( NA, path.info= missingInfo  )
         expect_match( got, wantRE)
      })
      it( "Internal error? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a directory failed with the following error:",
            " naughty 'file.exists'"
         )
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsDir( emptyDir, path.info= fileInfo ) })
         )
         expect_match( got, wantRE)
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsDir( emptyDir, path.info= badInfo ) })
         )
         expect_match( got, wantRE)
      })
   })
})

context( "checkIsNotDir()" )
#============================================================================
describe( "checkIsNotDir() - path=", {
   it( "'path' exists and is not directory? Return an empty string.", {
      want <- ""
      got <- checkIsNotDir( emptyFile )
      expect_equal( got, want)
   })
   it( "'path' does not exist? Return a failure string.", {
      want <- "No such path."
      got <- checkIsNotDir( noSuchPath )
      expect_equal( got, want)
   })
   it( "'path' exists but is a directory? Return a failure string.", {
      want <- "Is a directory."
      got <- checkIsNotDir( emptyDir )
      expect_equal( got, want)
   })
   it( "'path' is a bad value? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a directory failed with the following error:",
         " invalid 'file' argument"
      )
      expect_silent({ got <- checkIsNotDir( NA ) })
      expect_match( got, wantRE)
   })
   it( "Internal error? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a directory failed with the following error:",
         " naughty 'file.info'"
      )
      with_mock(
         `base::file.info` = function(...) stop("naughty 'file.info'"),
         expect_silent({ got <- checkIsNotDir( emptyDir ) })
      )
      expect_match( got, wantRE)
   })
   it( "'path' exists but is a file symlink? Return an empty string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      want <- ""
      got <- checkIsNotDir( linkToEmptyFile, path.info= file.info( linkToEmptyFile ))
      expect_equal( got, want)
   })
   it( "'path' exists but is a dir symlink? Return a failure string.", {
      if (noDirLinks) skip("Symlinks to files are not supported on this os.")
      want <- "Is a directory."
      got <- checkIsNotDir( linkToEmptyDir, path.info= file.info( linkToEmptyDir ))
      expect_equal( got, want)
   })
   it( "'path' exists but is a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      want <- "No such path."
      got <- checkIsNotDir( linkToNowhere, path.info= file.info( linkToNowhere ))
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a file symlink? Return an empty string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- ""
      got <- checkIsNotDir( linkToLinkToEmptyFile, path.info= file.info( linkToLinkToEmptyFile ))
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      if (noLinkToLinks) skip("Symlinks to links are not allowed on this os.")
      want <- "No such path."
      got <- checkIsNotDir( linkToLinkToNowhere, path.info= file.info( linkToLinkToNowhere ))
      expect_equal( got, want)
   })

})
describe( "checkIsNotDir() - path.info=", {

   fileInfo    <- file.info(emptyFile)
   dirInfo     <- file.info(emptyDir)
   missingInfo <- file.info(noSuchPath)
   badInfo <- FALSE

   it( "Does not call stat when path.info is provided", {
      want <- ""
      with_mock(
         `base::file.info` = function(...) stop("naughty"),
         expect_silent({ got <- checkIsNotDir( emptyFile, path.info= fileInfo ) })
      )
      expect_equal( got, want)
   })

   describe( "path.info= good values", {
      it( "'path' exists and is not dir? Return an empty string.", {
         want <- ""
         got <- checkIsNotDir( emptyFile, path.info= fileInfo )
         expect_equal( got, want)
      })
      it( "'path' does not exist? Return a failure string.", {
         want <- "No such path."
         got <- checkIsNotDir( noSuchPath, path.info= missingInfo )
         expect_equal( got, want)
      })
      it( "'path' exists but is a dir? Return a failure string.", {
         want <- "Is a directory."
         got <- checkIsNotDir( emptyDir, path.info= dirInfo )
         expect_equal( got, want)
      })
      it( "'path' is a bad value? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a directory failed with the following error:",
            " invalid 'file' argument"
         )
         expect_silent({ got <- checkIsNotDir( NA, path.info= fileInfo ) })
         expect_match( got, wantRE)
      })
      it(  "Internal error? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a directory failed with the following error:",
            " naughty 'file.exists'"
         )
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsNotDir( emptyFile, path.info= fileInfo ) })
         )
         expect_match( got, wantRE)
      })
      # Note, file.info on link returns the information about the ultimate target, possibly NA.
      it( "'path' exists but is a file symlink? Return an empty string.", {
         if (noFileLinks) skip("Symlinks to files are not supported on this os.")
         want <- ""
         got <- checkIsNotDir( linkToEmptyFile, path.info= file.info( linkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a dir symlink? Return a failure string.", {
         if (noDirLinks) skip("Symlinks to files are not supported on this os.")
         want <- "Is a directory."
         got <- checkIsNotDir( linkToEmptyDir, path.info= file.info( linkToEmptyDir ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a no-target symlink? Return a failure string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
         want <- "No such path."
         got <- checkIsNotDir( linkToNowhere, path.info= file.info( linkToNowhere ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a symlink to a file symlink? Return an empty string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
         want <- ""
         got <- checkIsNotDir( linkToLinkToEmptyFile, path.info= file.info( linkToLinkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
         if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
         if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
         if (noLinkToLinks) skip("Symlinks to links are not allowed on this os.")
         want <- "No such path."
         got <- checkIsNotDir( linkToLinkToNowhere, path.info= file.info( linkToLinkToNowhere ))
         expect_equal( got, want)
      })

   })
   describe( "path.info= incorrect or bad values", {
      it( "'path.info' valid but wrong? Use anyway", {
         want <- "Is a directory."
         got <- checkIsNotDir( emptyFile, path.info= dirInfo )
         expect_equal( got, want)
         got <- checkIsNotDir( emptyFile, path.info= file.info( linkToEmptyDir ))
         expect_equal( got, want)

         want <- ""
         got <- checkIsNotDir( emptyDir, path.info= fileInfo ) # dir but with file info
         expect_equal( got, want)
         got <- checkIsNotDir( emptyDir, path.info= file.info( linkToEmptyFile ))
         expect_equal( got, want)
         got <- checkIsNotDir( emptyDir, path.info= file.info( linkToLinkToEmptyFile ))
         expect_equal( got, want)
      })
      it( "'path' is a bad value? Return an error string (not an error).", {
         wantRE <- "Checking for a directory failed with the following error: .+"

         got <- checkIsNotDir( emptyFile, path.info = badInfo )
         expect_match( got, wantRE)
         got <- checkIsNotDir( emptyFile, path.info= missingInfo )
         expect_match( got, wantRE)
         got <- checkIsNotDir( emptyFile, path.info= file.info( linkToNowhere ))
         expect_match( got, wantRE)
         got <- checkIsNotDir( emptyFile, path.info= file.info( linkToLinkToNowhere ))
         expect_match( got, wantRE)
      })
      it( "'path.info' ignored if 'path' does not exists.", {
         want <- "No such path."
         got <- checkIsNotDir( noSuchPath, path.info= fileInfo ) # file but with dir info
         expect_match( got, want)
         got <- checkIsNotDir( noSuchPath, path.info= badInfo )
         expect_match( got, want)
      })
      it( "'path.info' ignored if 'path' is bad.", {
         wantRE <- paste0(
            "Checking for a directory failed with the following error:",
            " invalid 'file' argument"
         )

         got <- checkIsNotDir( NA, path.info= fileInfo )
         expect_match( got, wantRE)

         got <- checkIsNotDir( NA, path.info= missingInfo  )
         expect_match( got, wantRE)
      })
      it( "Internal error? Return an error string (not an error).", {
         wantRE <- paste0(
            "Checking for a directory failed with the following error:",
            " naughty 'file.exists'"
         )
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsNotDir( emptyFile, path.info= dirInfo ) })
         )
         expect_match( got, wantRE)
         with_mock(
            `base::file.exists` = function(...) stop("naughty 'file.exists'"),
            expect_silent({ got <- checkIsNotDir( emptyFile, path.info= badInfo ) })
         )
         expect_match( got, wantRE)
      })
   })
})

context( 'checkIsNotLink()' )
#============================================================================
describe( "checkIsNotLink() - path=", {
   it( "'path' exists and is not a link? Return an empty string.", {
      want <- ""

      got <- checkIsNotLink( emptyFile )
      expect_equal( got, want)
      got <- checkIsNotLink( emptyDir )
      expect_equal( got, want)

   })
   it( "'path' does not exist? Return a failure string.", {
      want <- "No such path."
      got <- checkIsNotLink( noSuchPath )
      expect_equal( got, want)
   })
   it( "'path' is a bad value? Return an error string (not an error).", {
      wantRE <- "Checking for a link failed with the following error: .+"
      expect_silent({ got <- checkIsNotLink( NA ) })
      expect_match( got, wantRE)
   })
   it( "'path' exists but is a file symlink? Return a failure string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      want <- "Is a link."
      got <- checkIsNotLink( linkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a dir symlink? Return a failure string.", {
      if (noDirLinks) skip("Symlinks to files are not supported on this os.")
      want <- "Is a link."
      got <- checkIsNotLink( linkToEmptyDir )
      expect_equal( got, want)
   })
   it( "'path' exists but is a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      want <- "Is a link."
      got <- checkIsNotLink( linkToNowhere )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- "Is a link."
      got <- checkIsNotLink( linkToLinkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- "Is a link."
      got <- checkIsNotLink( linkToLinkToNowhere )
      expect_equal( got, want)
   })
   it( "Sys.read but path exists? Return an error string (not an error).", {
      wantRE <- 'Unspecified error occured checking if existing path was link.'
      with_mock(
         `base::Sys.readlink` = function(...) {return(NA_character_)},
         expect_silent({ got <- checkIsNotLink( emptyDir ) })
      )
      expect_match( got, wantRE)
   })
   it( "Internal error? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a link failed with the following error:",
         " naughty 'Sys.readlink'"
      )
      with_mock(
         `base::Sys.readlink` = function(...) stop("naughty 'Sys.readlink'"),
         expect_silent({ got <- checkIsNotLink( emptyDir ) })
      )
      expect_match( got, wantRE)
   })
})

context( 'checkIsLink()' )
#============================================================================
describe( "checkIsLink() - path=", {
   it( "'path' exists and is not a link? Return a failure string.", {
      want <- "Not a link."
      got <- checkIsLink( emptyFile )
      expect_equal( got, want)
      got <- checkIsLink( emptyDir )
      expect_equal( got, want)
   })
   it( "'path' does not exist? Return a failure string.", {
      want <- "No such path."
      got <- checkIsLink( noSuchPath )
      expect_equal( got, want)
   })
   it( "'path' is a bad value? Return an error string (not an error).", {
      wantRE <- "Checking for a link failed with the following error: .+"
      expect_silent({ got <- checkIsLink( NA ) })
      expect_match( got, wantRE)
   })
   it( "'path' exists but is a file symlink? Return an empty string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      want <- ""
      got <- checkIsLink( linkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a dir symlink? Return an empty string.", {
      if (noDirLinks) skip("Symlinks to files are not supported on this os.")
      want <- ""
      got <- checkIsLink( linkToEmptyDir )
      expect_equal( got, want)
   })
   it( "'path' exists but is a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      want <- "Bad link."
      got <- checkIsLink( linkToNowhere )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a symlink? Return an empty string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- ""
      got <- checkIsLink( linkToLinkToEmptyFile )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a no-target symlink? Return a failure string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- "Bad link."
      got <- checkIsLink( linkToLinkToNowhere )
      expect_equal( got, want)
   })
   it( "Sys.read error but path exists? Return an error string (not an error).", {
      wantRE <- 'Unspecified error occured checking if existing path was link.'
      with_mock(
         `base::Sys.readlink` = function(...) {return(NA_character_)},
         expect_silent({ got <- checkIsLink( emptyDir ) })
      )
      expect_match( got, wantRE)
   })
   it( "Internal error? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a link failed with the following error:",
         " naughty 'Sys.readlink'"
      )
      with_mock(
         `base::Sys.readlink` = function(...) stop("naughty 'Sys.readlink'"),
         expect_silent({ got <- checkIsLink( emptyDir ) })
      )
      expect_match( got, wantRE)
   })
})

describe( "checkIsLink() - okBadLink=", {
   it( "'path' exists and is not a link? Return a failure string.", {
      want <- "Not a link."
      got <- checkIsLink( emptyFile, okBadLink= TRUE )
      expect_equal( got, want)
      got <- checkIsLink( emptyDir, okBadLink= TRUE )
      expect_equal( got, want)
   })
   it( "'path' does not exist? Return a failure string.", {
      want <- "No such path."
      got <- checkIsLink( noSuchPath, okBadLink= TRUE )
      expect_equal( got, want)
   })
   it( "'path' is a bad value? Return an error string (not an error).", {
      wantRE <- "Checking for a link failed with the following error: .+"
      expect_silent({ got <- checkIsLink( NA, okBadLink= TRUE ) })
      expect_match( got, wantRE)
   })
   it( "'path' exists but is a file symlink? Return an empty string.", {
      if (noFileLinks) skip("Symlinks to files are not supported on this os.")
      want <- ""
      got <- checkIsLink( linkToEmptyFile, okBadLink= TRUE )
      expect_equal( got, want)
   })
   it( "'path' exists but is a dir symlink? Return an empty string.", {
      if (noDirLinks) skip("Symlinks to files are not supported on this os.")
      want <- ""
      got <- checkIsLink( linkToEmptyDir, okBadLink= TRUE )
      expect_equal( got, want)
   })
   it( "'path' exists but is a no-target symlink? Return an empty string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      want <- ""
      got <- checkIsLink( linkToNowhere, okBadLink= TRUE )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a symlink? Return an empty string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- ""
      got <- checkIsLink( linkToLinkToEmptyFile, okBadLink= TRUE )
      expect_equal( got, want)
   })
   it( "'path' exists but is a symlink to a no-target symlink? Return an empty string.", {
      if (noFileLinks)   skip("Symlinks to files are not supported on this os.")
      if (noBadLinks)    skip("Symlinks to missing paths are not allowed on this os.")
      if (noLinkToLinks) skip("Symlinks to symlinks are not allowed on this os.")
      want <- ""
      got <- checkIsLink( linkToLinkToNowhere, okBadLink= TRUE )
      expect_equal( got, want)
   })
   it( "Sys.read error but path exists? Return an error string (not an error).", {
      wantRE <- 'Unspecified error occured checking if existing path was link.'
      with_mock(
         `base::Sys.readlink` = function(...) {return(NA_character_)},
         expect_silent({ got <- checkIsLink( emptyDir, okBadLink= TRUE ) })
      )
      expect_match( got, wantRE)
   })
   it( "Internal error? Return an error string (not an error).", {
      wantRE <- paste0(
         "Checking for a link failed with the following error:",
         " naughty 'Sys.readlink'"
      )
      with_mock(
         `base::Sys.readlink` = function(...) stop("naughty 'Sys.readlink'"),
         expect_silent({ got <- checkIsLink( emptyDir, okBadLink= TRUE ) })
      )
      expect_match( got, wantRE)
   })
})
