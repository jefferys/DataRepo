#----------------------------------------------------------------------------
context( 'Setup for testing loadActions() ')
#----------------------------------------------------------------------------

# noSuchFile - A filename that does not exist.
noSuchFile      <- tempfile( "noSuchFile" )

# emptyFile - A filename of a file that exists but is empty.
emptyActionFile <- tempfile( "emptyActionFile" )
file.create(emptyActionFile)

# actionFile - A filename of a file of actions.
actionFile <- tempfile( "actionFile" )
file.create(actionFile)
content <- c("col_a\tcol_b\tcol_c", "one\t1\tTRUE", "two\t2\tFALSE", "# ignore me")
writeLines( content, actionFile )
# paired data frame
actionDF <- data.frame(
   col_a= c("one", "two"),
   col_b= c(1,2),
   col_c= c(T,F),
   stringsAsFactors= FALSE
)

describe( 'Check Setup', {
   expect_false( file.exists( noSuchFile      ))
   expect_true(  file.exists( emptyActionFile ))
   expect_true(  file.exists( actionFile      ))
})

#----------------------------------------------------------------------------
context( 'loadActions()' )
#----------------------------------------------------------------------------

describe( 'Default behavior', {
   it( 'returns the expected data frame', {
      got <- loadActions( actionFile )
      expect_is( got, "data.frame" )
      expect_equal( got, actionDF)
   })
})
describe( 'file param', {
   it("errors if specified file does not exist", {
      errorRE <- 'No such file to load: ".*noSuchFile.*"'
      expect_error( loadActions(noSuchFile), errorRE )
   })
   it("errors if specified file not formatted for reading in.", {
      errorRE <- paste0(
         'Reading file ".*emptyActionFile.*" failed with the following error:\\n',
         '\\tno lines available in input'
      )
      expect_error( loadActions(emptyActionFile), errorRE )
   })
})
