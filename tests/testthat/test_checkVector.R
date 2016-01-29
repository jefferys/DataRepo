context( "Setting up values for checking." )

null <- NULL

na   <- NA
emptyCharacter <- character(0)
emptyString  <- c('')
char         <- c('A')
string       <- c('One string.')
character   <- c('one', 'two two', 'three three three')
weirdCharacter <- c('', NA_character_, 'NA', '∑')
numeric <- c(-2.7, 0, 3e27)
weirdNumeric <- c(Inf, NaN, -Inf, -0, +0, NA_real_)
logical <- c(TRUE, TRUE, FALSE, FALSE)
weirdLogical <- c( T, NA, F )
integer <- as.integer( c(-3, 0, 1) )
weirdInteger <- as.integer( c( -0, +0, NA_integer_ ))

emptyList <- list()
aList <- list(A=c(1,2), B=list(recursive=c(1,2)))

anObject <- structure(aList, class='bogus')
aFunction <- function() {return('a')}

context( "checkIsSingle" )
describe( "The vector= parameter", {
   it( "returns empty string for vector= any vector of length one", {
      want <- ''

      got <- checkIsSingle( 1 )
      expect_equal( got, want )
      got <- checkIsSingle( c(1) )
      expect_equal( got, want )
      got <- checkIsSingle( "foo" )
      expect_equal( got, want )
      got <- checkIsSingle( "One string to rule them all" )
      expect_equal( got, want )
   })
   it( "returns failure message for vector= any vector of length not one", {
      want <- 'Length is not 1.'

      got <- checkIsSingle( c(1, 2) )
      expect_equal( got, want )
      got <- checkIsSingle( character(0) )
      expect_equal( got, want )
   })
   it( "returns failure message for vector= not a vector", {
      want <- "Not a vector of mode any."

      got <- checkIsSingle( list(A=1) )
      expect_equal( got, want )
      got <- checkIsSingle( NULL )
      expect_equal( got, want )
   })
   it( "returns error message (not error) with internal error", {
      wantRE <- "Checking for a single valued vector failed with the following error: .+"
      with_mock(
         `base::checkIsVector` = function(...) stop("naughty 'checkIsVector'"),
         expect_silent({ got <- checkIsSingle( "ok" ) })
      )
      expect_match( got, wantRE)
   })

})
describe( "The mode= parameter", {
   it( "returns empty string for vector of length one when mode matches", {
      want <- ''

      got <- checkIsSingle( 1, 'numeric' )
      expect_equal( got, want )
      got <- checkIsSingle( c(1), mode= 'double' )
      expect_equal( got, want )
      got <- checkIsSingle( "foo", "character" )
      expect_equal( got, want )
      got <- checkIsSingle( "One string to rule them all", mode= "character" )
      expect_equal( got, want )
   })
   it( "returns failure message for vector not of length one when mode matches", {
      want <- 'Length is not 1.'

      got <- checkIsSingle( c(1, 2), 'double' )
      expect_equal( got, want )
      got <- checkIsSingle( character(0), mode= 'character' )
      expect_equal( got, want )
   })
   it( "returns failure message for vector of length if mode mis-matched", {
      want <- 'Not a vector of mode character.'
      got <- checkIsSingle( 1, 'character' )
      expect_equal( got, want )

      want <- 'Not a vector of mode character.'
      got <- checkIsSingle( c(1), mode= 'character' )
      expect_equal( got, want )

      want <- 'Not a vector of mode double.'
      got <- checkIsSingle( "foo", "double" )
      expect_equal( got, want )

      want <- 'Not a vector of mode numeric.'
      got <- checkIsSingle( "One string to rule them all", mode= "numeric" )
      expect_equal( got, want )
   })
   it( "returns failure message when both vector length != one and mode mimatches", {
      want <- 'Not a vector of mode character.'
      got <- checkIsSingle( c(1, 2), 'character' )
      expect_equal( got, want )

      want <- 'Not a vector of mode double.'
      got <- checkIsSingle( character(0), mode= 'double' )
      expect_equal( got, want )
   })
   it( "returns failure message for vector= not a vector", {
      want <- "Not a vector of mode any."

      got <- checkIsSingle( list(A=1) )
      expect_equal( got, want )
      got <- checkIsSingle( NULL )
      expect_equal( got, want )
   })
   it( "returns error message (not error) with internal error", {
      wantRE <- "Checking for a single valued vector failed with the following error: .+"
      with_mock(
         `base::checkIsVector` = function(...) stop("naughty 'checkIsVector'"),
         expect_silent({ got <- checkIsSingle( "ok" ) })
      )
      expect_match( got, wantRE)
   })
})

context( 'checkIsVector()' )
describe( "checkIsVector, vector=", {
   it( "returns empty string '' for vector= vectors", {
      want <- ''

      got <- checkIsVector(na)
      expect_equal( got, want)

      got <- checkIsVector( emptyCharacter )
      expect_equal( got, want )
      got <- checkIsVector( emptyString )
      expect_equal( got, want )
      got <- checkIsVector( string )
      expect_equal( got, want )
      got <- checkIsVector( weirdCharacter )
      expect_equal( got, want )
      got <- checkIsVector( weirdNumeric )
      expect_equal( got, want )
      got <- checkIsVector( weirdLogical )
      expect_equal( got, want )
      got <- checkIsVector( weirdInteger )
      expect_equal( got, want )
   })
   it( "returns a failure string for vector= non-vectors", {
      want = 'Not a vector of mode any.'

      got <- checkIsVector(NULL)
      expect_equal( got, want)
      got <- checkIsVector(emptyList)
      expect_equal( got, want)
      got <- checkIsVector(aList)
      expect_equal( got, want)
      got <- checkIsVector(anObject)
      expect_equal( got, want)
      got <- checkIsVector(aFunction)
      expect_equal( got, want)
   })
   it( "returns a failure string for vector= vector with any attribute", {
      want = 'Not a vector of mode any.'
      vector <- structure( character( 0 ), abcde= '12345' )
      got <- checkIsVector( vector )
      expect_equal( got, want )
   })
   it( "for internal errors it returns an error", {
      wantRE <- "Checking for a vector failed with the following error: naughty 'is.vector'"
      with_mock(
         `base::is.vector` = function(...) stop("naughty 'is.vector'"),
         expect_silent({ got <- checkIsVector( character ) })
      )
      expect_match( got, wantRE)
   })
})
describe( "checkIsVector, mode=", {
   it( "returns an empty string '' when vector matches mode.", {
      want <- ''

      got <- checkIsVector(na, mode='any')
      expect_equal( got, want)
      got <- checkIsVector(na, mode='logical')
      expect_equal( got, want)

      got <- checkIsVector( emptyCharacter, mode='any')
      expect_equal( got, want )
      got <- checkIsVector( emptyCharacter, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( emptyString, mode='any')
      expect_equal( got, want )
      got <- checkIsVector( emptyString, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( string, mode='any')
      expect_equal( got, want )
      got <- checkIsVector( string, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( weirdCharacter, mode='any')
      expect_equal( got, want )
      got <- checkIsVector( weirdCharacter, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( weirdNumeric, mode='any')
      expect_equal( got, want )
      got <- checkIsVector( weirdNumeric, mode='numeric')
      expect_equal( got, want )
      got <- checkIsVector( weirdNumeric, mode='double')
      expect_equal( got, want )
      got <- checkIsVector( weirdLogical, mode='any')
      expect_equal( got, want )
      got <- checkIsVector( weirdLogical, mode='logical')
      expect_equal( got, want )
      got <- checkIsVector( weirdInteger, mode='any')
      expect_equal( got, want )
      got <- checkIsVector( weirdInteger, mode='integer')
      expect_equal( got, want )

   })
   it( "returns a failure string when vector does not match mode.", {
      want <- 'Not a vector of mode character.'

      got <- checkIsVector( na, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( numeric, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( logical, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( integer, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( aList, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( emptyList, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( anObject, mode='character')
      expect_equal( got, want )
      got <- checkIsVector( aFunction, mode='character')
      expect_equal( got, want )

      want <- 'Not a vector of mode numeric.'
      got <- checkIsVector( character, mode='numeric')
      expect_equal( got, want )

      want <- 'Not a vector of mode double.'
      got <- checkIsVector( character, mode='double')
      expect_equal( got, want )

      want <- 'Not a vector of mode ineger.'
      got <- checkIsVector( character, mode='ineger')
      expect_equal( got, want )

      want <- 'Not a vector of mode complex.'
      got <- checkIsVector( character, mode='complex')
      expect_equal( got, want )

      want <- 'Not a vector of mode expression.'
      got <- checkIsVector( character, mode='expression')
      expect_equal( got, want )

      want <- 'Not a vector of mode logical.'
      got <- checkIsVector( character, mode='logical')
      expect_equal( got, want )

      want <- 'Not a vector of mode raw.'
      got <- checkIsVector( character, mode='raw')
      expect_equal( got, want )

   })
   it( "returns a failure string for value= list or mode= list", {
      want <- 'Not a vector of mode list.'
      got <- checkIsVector( na, mode='list')
      expect_equal( got, want )
      got <- checkIsVector( numeric, mode='list')
      expect_equal( got, want )
      got <- checkIsVector( logical, mode='list')
      expect_equal( got, want )
      got <- checkIsVector( integer, mode='list')
      expect_equal( got, want )
      got <- checkIsVector( aList, mode='list')
      expect_equal( got, want )
      got <- checkIsVector( emptyList, mode='list')
      expect_equal( got, want )
      got <- checkIsVector( anObject, mode='list')
      expect_equal( got, want )
      got <- checkIsVector( aFunction, mode='list')
      expect_equal( got, want )
   })
   it( "returns a failure string for unknown (string) mode.", {
      want <- 'Not a vector of mode object.'
      got <- checkIsVector( anObject, mode='object')
      expect_equal( got, want )
      want <- 'Not a vector of mode function.'
      got <- checkIsVector( aFunction, mode='function')
      expect_equal( got, want )
      want <- 'Not a vector of mode foobar.'
      got <- checkIsVector( character, mode='foobar')
      expect_equal( got, want )
   })
   it( "returns a failure string when vector= value has an attribute", {
      want = 'Not a vector of mode character.'
      vector <- structure( character( 0 ), abcde= '12345' )
      got <- checkIsVector( vector, mode='character' )
      expect_equal( got, want )
   })
   it( "returns an error string for invalid mode", {
      wantRE <- "Checking for a vector failed with the following error: .+"
      expect_silent({ got <- checkIsVector( character, mode= 42 ) })
      expect_match( got, wantRE)
   })
   it( "returns an error string for internal errors", {
      wantRE <- "Checking for a vector failed with the following error: naughty 'is.vector'"
      with_mock(
         `base::is.vector` = function(...) stop("naughty 'is.vector'"),
         expect_silent({ got <- checkIsVector( character, mode='character' ) })
      )
      expect_match( got, wantRE)
   })
})

context( 'checkIsIn()' )
describe( "whitelist checking", {
   it( "returns an empty string if all elements in checklist", {
      want <- ''

      got <- checkIsIn(          1,         1  )
      expect_equal(got, want)
      got <- checkIsIn(          2, c(1, 2, 3) )
      expect_equal(got, want)
      got <- checkIsIn(    c(2, 1), c(1, 2, 3) )
      expect_equal(got, want)
      got <- checkIsIn( c(3, 1, 2), c(1, 2, 3) )
      expect_equal(got, want)

      got <- checkIsIn( -Inf,     weirdNumeric )
      expect_equal(got, want)
      got <- checkIsIn( NaN,      weirdNumeric )
      expect_equal(got, want)
      got <- checkIsIn( NA_real_, weirdNumeric )
      expect_equal(got, want)
      got <- checkIsIn( Inf,      weirdNumeric )
      expect_equal(got, want)
      got <- checkIsIn( +0,       weirdNumeric )
      expect_equal(got, want)
      got <- checkIsIn( -0,       weirdNumeric )
      expect_equal(got, want)

      got <- checkIsIn( character(0), character(0) )
      expect_equal(got, want)
      got <- checkIsIn( c("warm", "cold"), c("hot", "warm", "cold", "warm", "hot") )
      expect_equal(got, want)
      got <- checkIsIn(            '', weirdCharacter )
      expect_equal(got, want)
      got <- checkIsIn( NA_character_, weirdCharacter )
      expect_equal(got, want)
      got <- checkIsIn(          'NA', weirdCharacter )
      expect_equal(got, want)
      got <- checkIsIn(           '∑', weirdCharacter )
      expect_equal(got, want)

      got <- checkIsIn( c(T, TRUE, F, FALSE, NA), c(NA, FALSE, T))
      expect_equal(got, want)
      got <- checkIsIn( T, TRUE )
      expect_equal(got, want)
      got <- checkIsIn( TRUE, T )
      expect_equal(got, want)
      got <- checkIsIn( c(NA, NULL), NA )
      expect_equal(got, want)

      got <- checkIsIn( 1,           as.integer( 1, 2, NA_integer_ ))
      expect_equal(got, want)
      got <- checkIsIn( NA_integer_, c( 1, 2, NA_integer_ ))
      expect_equal(got, want)

   })
   it( "returns a failure string if any element not in checklist", {
      want <- 'Some element is not in the checklist.'
      got <- checkIsIn( 1, 2)
      expect_equal(got, want)
      got <- checkIsIn( c( "white", "gray" ), c( "black", "white" ))
      expect_equal(got, want)
      got <- checkIsIn( c(1,2,3,4,5,6,7,8), c(100,200) )
      expect_equal(got, want)

   })
   it( "returns a failure string if checklist is invalid", {
      want <- "Bad parameter 'checklist'. Not a vector of mode any."

      got <- checkIsIn( c(1,2) )
      expect_equal(got, want)
      got <- checkIsIn( c(1,2), checklist= aList )
      expect_equal(got, want)

   })
   it( "returns a failure string if vector is invalid (given checklist)", {
      want <- "Bad parameter 'vector'. Not a vector of mode character."

      got <- checkIsIn( NULL, character(0) )
      expect_equal(got, want)
      got <- checkIsIn( NA, c("a", "b", NA) )
      expect_equal(got, want)

   })
   it( "returns an error string (not error) if internal error occurs", {
      wantRE <- "Checking against checklist failed with the following error: naughty 'checkIsVector'"
      with_mock(
         `checkIsVector` = function(...) stop("naughty 'checkIsVector'"),
         expect_silent({ got <- checkIsIn( 4, 4 ) })
      )
      expect_match( got, wantRE)
   })
})

context( 'checkIsNotIn()' )
describe( "blacklist checking", {
   it( "returns an empty string if no element is in the checklist", {
      want <- ''

      got <- checkIsNotIn(1, checklist= 2)
      expect_equal(got, want)
      got <- checkIsNotIn(c(1,2), c(3,4,5))
      expect_equal(got, want)
      got <- checkIsNotIn( '', character(0) )
      expect_equal(got, want)
      got <- checkIsNotIn( character(0), '' )
      expect_equal(got, want)
      got <- checkIsNotIn( c("x", NULL), c("y", NULL) )
      expect_equal(got, want)
   })
   it( "returns a failure string if any element is in thechecklist", {
      want <- 'Some element is in the checklist.'

      got <- checkIsNotIn( 1, c( 1,2,3 ))
      expect_equal(got, want)
      got <- checkIsNotIn( c(3,2,1), c( 1,2,3 ))
      expect_equal(got, want)
      got <- checkIsNotIn( c( "white", "gray" ), c( "black", "white" ))
      expect_equal(got, want)
   })
   it( "returns a failure string if checklist is invalid", {
      want <- "Bad parameter 'checklist'. Not a vector of mode any."

      got <- checkIsNotIn( c(1,2) )
      expect_equal(got, want)
      got <- checkIsNotIn( 'Foo', checklist= anObject )
      expect_equal(got, want)
   })
   it( "returns a failure string if vector is invalid (given checklist)", {
      want <- "Bad parameter 'vector'. Not a vector of mode character."

      got <- checkIsNotIn( NULL, character(0) )
      expect_equal(got, want)
      got <- checkIsNotIn( NA, c("a", "b", NA) )
      expect_equal(got, want)
      got <- checkIsNotIn( aList, checklist= "Bar" )
      expect_equal(got, want)
   })
   it( "returns an error string (not error) if internal error occurs", {
      wantRE <- "Checking against checklist failed with the following error: naughty 'checkIsVector'"
      with_mock(
         `checkIsVector` = function(...) stop("naughty 'checkIsVector'"),
         expect_silent({ got <- checkIsNotIn( 4, 5 ) })
      )
      expect_match( got, wantRE)
   })
})
