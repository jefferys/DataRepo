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

context( "checkIsNull()" )
describe( "checkIsNull - value=", {
   it( "returns '' if value= NULL", {
      want <- ''
      got <- checkIsNull( null )
      expect_equal( got, want )
   })
   it( "returns 'Not NULL.' if value= anything else", {
      want <- 'Is not null.'
      got <- checkIsNull( na )
      expect_equal( got, want )
      got <- checkIsNull( emptyCharacter )
      expect_equal( got, want )
      got <- checkIsNull( emptyString )
      expect_equal( got, want )
      got <- checkIsNull( char )
      expect_equal( got, want )
      got <- checkIsNull( string )
      expect_equal( got, want )
      got <- checkIsNull( character )
      expect_equal( got, want )
      got <- checkIsNull( weirdCharacter )
      expect_equal( got, want )
      got <- checkIsNull( numeric )
      expect_equal( got, want )
      got <- checkIsNull( weirdNumeric )
      expect_equal( got, want )
      got <- checkIsNull( logical )
      expect_equal( got, want )
      got <- checkIsNull( weirdLogical )
      expect_equal( got, want )
      got <- checkIsNull( integer )
      expect_equal( got, want )
      got <- checkIsNull( weirdInteger )
      expect_equal( got, want )
      got <- checkIsNull( aList )
      expect_equal( got, want )
      got <- checkIsNull( emptyList )
      expect_equal( got, want )
      got <- checkIsNull( anObject )
      expect_equal( got, want )
      got <- checkIsNull( aFunction )
      expect_equal( got, want )
   })
   it( "returns error message (not error) with internal error", {
      wantRE <- "Checking for null failed with the following error: .+"
      got <- checkIsNull()
      expect_match( got, wantRE )
   })
})

context( "checkIsNotNull()" )
describe( "checkIsNotNull - value=", {
   it( "returns 'Is null.' if value= NULL", {
      want <- 'Is null.'
      got <- checkIsNotNull( null )
      expect_equal( got, want )
   })
   it( "returns '' if value= anything else", {
      want <- ''

      got <- checkIsNotNull( na )
      expect_equal( got, want )
      got <- checkIsNotNull( emptyCharacter )
      expect_equal( got, want )
      got <- checkIsNotNull( emptyString )
      expect_equal( got, want )
      got <- checkIsNotNull( char )
      expect_equal( got, want )
      got <- checkIsNotNull( string )
      expect_equal( got, want )
      got <- checkIsNotNull( character )
      expect_equal( got, want )
      got <- checkIsNotNull( weirdCharacter )
      expect_equal( got, want )
      got <- checkIsNotNull( numeric )
      expect_equal( got, want )
      got <- checkIsNotNull( weirdNumeric )
      expect_equal( got, want )
      got <- checkIsNotNull( logical )
      expect_equal( got, want )
      got <- checkIsNotNull( weirdLogical )
      expect_equal( got, want )
      got <- checkIsNotNull( integer )
      expect_equal( got, want )
      got <- checkIsNotNull( weirdInteger )
      expect_equal( got, want )
      got <- checkIsNotNull( aList )
      expect_equal( got, want )
      got <- checkIsNotNull( emptyList )
      expect_equal( got, want )
      got <- checkIsNotNull( anObject )
      expect_equal( got, want )
      got <- checkIsNotNull( aFunction )
      expect_equal( got, want )
   })
   it( "returns error message (not error) with internal error", {
      wantRE <- "Checking for null failed with the following error: .+"
      got <- checkIsNotNull()
      expect_match( got, wantRE )
   })
})

context( "checkLength()" )
describe( "checkLength - value=", {
   it( "returns '' for any value", {
      want <- ''

      got <- checkLength( na )
      expect_equal( got, want )
      got <- checkLength( emptyCharacter )
      expect_equal( got, want )
      got <- checkLength( emptyString )
      expect_equal( got, want )
      got <- checkLength( char )
      expect_equal( got, want )
      got <- checkLength( string )
      expect_equal( got, want )
      got <- checkLength( character )
      expect_equal( got, want )
      got <- checkLength( weirdCharacter )
      expect_equal( got, want )
      got <- checkLength( numeric )
      expect_equal( got, want )
      got <- checkLength( weirdNumeric )
      expect_equal( got, want )
      got <- checkLength( logical )
      expect_equal( got, want )
      got <- checkLength( weirdLogical )
      expect_equal( got, want )
      got <- checkLength( integer )
      expect_equal( got, want )
      got <- checkLength( weirdInteger )
      expect_equal( got, want )
      got <- checkLength( aList )
      expect_equal( got, want )
      got <- checkLength( emptyList )
      expect_equal( got, want )
      got <- checkLength( anObject )
      expect_equal( got, want )
      got <- checkLength( aFunction )
      expect_equal( got, want )

   })
   it( "returns failure message for bad value=", {
      want <- "Bad parameter 'value'. Is null."
      got <- checkLength( NULL )
      expect_match( got, want )
   })
   it( "returns error message (not error) with internal error", {
      wantRE <- "Checking for length failed with the following error: .+"
      with_mock(
         `base::checkIsNotNull` = function(...) stop("naughty 'checkIsNotNull'"),
         expect_silent({ got <- checkLength( "ok" ) })
      )
      expect_match( got, wantRE)
   })
})
describe( "checkLength - maximumLength= only", {
   it( "returns '' for any length <= maximumLength", {
      want <- ''

      got <- checkLength( na, maximumLength= 1 )
      expect_equal( got, want )
      got <- checkLength( na, maximumLength= 2 )
      expect_equal( got, want )
      got <- checkLength( emptyCharacter, maximumLength= 0 )
      expect_equal( got, want )
      got <- checkLength( emptyCharacter, maximumLength= 1 )
      expect_equal( got, want )
      got <- checkLength( emptyString, maximumLength= 1 )
      expect_equal( got, want )
      got <- checkLength( emptyString, maximumLength= 10 )
      expect_equal( got, want )
      got <- checkLength( weirdCharacter, maximumLength= 4 )
      expect_equal( got, want )
      got <- checkLength( weirdCharacter, maximumLength= 5 )
      expect_equal( got, want )
      got <- checkLength( weirdNumeric, maximumLength= 6 )
      expect_equal( got, want )
      got <- checkLength( weirdNumeric, maximumLength= 10e3 )
      expect_equal( got, want )
      got <- checkLength( weirdLogical, maximumLength= 3 )
      expect_equal( got, want )
      got <- checkLength( weirdLogical, maximumLength= 4 )
      expect_equal( got, want )
      got <- checkLength( weirdInteger, maximumLength= 3 )
      expect_equal( got, want )
      got <- checkLength( weirdInteger, maximumLength= 11.2 )
      expect_equal( got, want )
   })
   it( "returns failure string for any length > maximumLength", {

      want <- 'Length is not 0.'
      got <- checkLength( na, maximumLength= 0 )
      expect_equal( got, want )

      want <- 'Length is not between 0 and -1.'
      got <- checkLength( emptyCharacter, maximumLength= -1 )
      expect_equal( got, want )

      want <- 'Length is not 0.'
      got <- checkLength( emptyString, maximumLength= 0 )
      expect_equal( got, want )

      want <- 'Length is not between 0 and 3.'
      got <- checkLength( weirdCharacter, maximumLength= 3 )
      expect_equal( got, want )

      want <- 'Length is not between 0 and 1.'
      got <- checkLength( weirdCharacter, maximumLength= 1 )
      expect_equal( got, want )

      want <- 'Length is not between 0 and 5.'
      got <- checkLength( weirdNumeric, maximumLength= 5 )
      expect_equal( got, want )

      want <- 'Length is not between 0 and 1.'
      got <- checkLength( weirdNumeric, maximumLength= 1 )
      expect_equal( got, want )

      want <- 'Length is not between 0 and 2.'
      got <- checkLength( weirdLogical, maximumLength= 2 )
      expect_equal( got, want )

      want <- 'Length is not between 0 and 1.'
      got <- checkLength( weirdLogical, maximumLength= 1 )
      expect_equal( got, want )

      want <- 'Length is not between 0 and 2.'
      got <- checkLength( weirdInteger, maximumLength= 2 )
      expect_equal( got, want )

      want <- 'Length is not between 0 and 1.'
      got <- checkLength( weirdInteger, maximumLength= 1 )
      expect_equal( got, want )
   })
   it( "returns error message (not error) for bad maximumLength value", {
      wantRE <- "Checking for length failed with the following error: .+"
      got <- checkLength( character, maximumLength=NULL)
      expect_match( got, wantRE )
   })

})
describe( "checkLength - minimumLength= only", {
   it( "returns '' for any length >= min", {
      want <- ''

      got <- checkLength( na, minimumLength= 1 )
      expect_equal( got, want )
      got <- checkLength( na, minimumLength= 0 )
      expect_equal( got, want )
      got <- checkLength( emptyCharacter, minimumLength= 0 )
      expect_equal( got, want )
      got <- checkLength( emptyCharacter, minimumLength= -1 )
      expect_equal( got, want )
      got <- checkLength( emptyString, minimumLength= 0 )
      expect_equal( got, want )
      got <- checkLength( emptyString, minimumLength= -10 )
      expect_equal( got, want )
      got <- checkLength( weirdCharacter, minimumLength= 4 )
      expect_equal( got, want )
      got <- checkLength( weirdCharacter, minimumLength= 3 )
      expect_equal( got, want )
      got <- checkLength( weirdNumeric, minimumLength= 6 )
      expect_equal( got, want )
      got <- checkLength( weirdNumeric, minimumLength= 1 )
      expect_equal( got, want )
      got <- checkLength( weirdLogical, minimumLength= 3 )
      expect_equal( got, want )
      got <- checkLength( weirdLogical, minimumLength= 2 )
      expect_equal( got, want )
      got <- checkLength( weirdInteger, minimumLength= 3 )
      expect_equal( got, want )
      got <- checkLength( weirdInteger, minimumLength= 1 )
      expect_equal( got, want )
   })
   it( "returns failure string for any length < min", {

      want <- 'Length is not between 2 and Inf.'
      got <- checkLength( na, minimumLength= 2 )
      expect_equal( got, want )

      want <- 'Length is not between 1 and Inf.'
      got <- checkLength( emptyCharacter, minimumLength= 1 )
      expect_equal( got, want )

      want <- 'Length is not between 2 and Inf.'
      got <- checkLength( emptyString, minimumLength= 2 )
      expect_equal( got, want )

      want <- 'Length is not between 5 and Inf.'
      got <- checkLength( weirdCharacter, minimumLength= 5 )
      expect_equal( got, want )

      want <- 'Length is not between 500 and Inf.'
      got <- checkLength( weirdCharacter, minimumLength= 500 )
      expect_equal( got, want )

      want <- 'Length is not between 7 and Inf.'
      got <- checkLength( weirdNumeric, minimumLength=7  )
      expect_equal( got, want )

      want <- 'Length is not Inf.'
      got <- checkLength( weirdNumeric, minimumLength=Inf )
      expect_equal( got, want )

      want <- 'Length is not between 4 and Inf.'
      got <- checkLength( weirdLogical, minimumLength= 4 )
      expect_equal( got, want )

      want <- 'Length is not between 5 and Inf.'
      got <- checkLength( weirdLogical, minimumLength= 5 )
      expect_equal( got, want )

      want <- 'Length is not between 4 and Inf.'
      got <- checkLength( weirdInteger, minimumLength= 4 )
      expect_equal( got, want )

      want <- 'Length is not between 400 and Inf.'
      got <- checkLength( weirdInteger, minimumLength= 4e2 )
      expect_equal( got, want )
   })
   it( "returns error message (not error) for bad minimumLength value", {
      wantRE <- "Checking for length failed with the following error: .+"
      got <- checkLength( character, minimumLength=NA_integer_)
      expect_match( got, wantRE )
   })
})
describe( "checkLength - minimumLength= and maximumLength=", {
   it( "returns '' for any minimumLength <= length <= max", {
      want <- ''

      got <- checkLength( emptyCharacter, minimumLength= 0, maximumLength= 0 )
      expect_equal( got, want )
      got <- checkLength( emptyCharacter, minimumLength= 0, maximumLength= 1 )
      expect_equal( got, want )
      got <- checkLength( emptyCharacter, minimumLength= -1, maximumLength= 1 )
      expect_equal( got, want )

      got <- checkLength( emptyString, minimumLength= 1, maximumLength= 1 )
      expect_equal( got, want )
      got <- checkLength( emptyString, minimumLength= 1, maximumLength= 2 )
      expect_equal( got, want )
      got <- checkLength( emptyString, minimumLength= 0, maximumLength= 2 )
      expect_equal( got, want )

      got <- checkLength( weirdNumeric, minimumLength=6, maximumLength=6  )
      expect_equal( got, want )
      got <- checkLength( weirdNumeric, minimumLength=5, maximumLength=6  )
      expect_equal( got, want )
      got <- checkLength( weirdNumeric, minimumLength=5, maximumLength=7  )
      expect_equal( got, want )
      got <- checkLength( weirdNumeric, minimumLength=1, maximumLength=99  )
      expect_equal( got, want )

      got <- checkLength( aList, minimumLength= 2, maximumLength= 2)
      expect_equal( got, want )
      got <- checkLength( emptyList, minimumLength= 0, maximumLength= 0)
      expect_equal( got, want )
      got <- checkLength( anObject, minimumLength= 2, maximumLength= 2)
      expect_equal( got, want )
      got <- checkLength( aFunction, minimumLength= 1, maximumLength= 1)
      expect_equal( got, want )

   })
   it( "returns a failure string for any length outside min..max", {
      want <- 'Length is not between -2 and -1.'
      got <- checkLength( emptyCharacter, minimumLength= -2, maximumLength= -1 )
      expect_equal( got, want )
      want <- 'Length is not between 1 and 2.'
      got <- checkLength( emptyCharacter, minimumLength= 1, maximumLength= 2 )
      expect_equal( got, want )
      want <- 'Length is not between 1 and -1.'
      got <- checkLength( emptyCharacter, minimumLength= 1, maximumLength= -1 )
      expect_equal( got, want )

      want <- 'Length is not 0.'
      got <- checkLength( emptyString, minimumLength= 0, maximumLength= 0 )
      expect_equal( got, want )
      want <- 'Length is not 2.'
      got <- checkLength( emptyString, minimumLength= 2, maximumLength= 2 )
      expect_equal( got, want )

      want <- 'Length is not between 100 and 200.'
      got <- checkLength( weirdNumeric, minimumLength=100, maximumLength=200  )
      expect_equal( got, want )
      want <- 'Length is not between 1 and 3.'
      got <- checkLength( weirdNumeric, minimumLength=1, maximumLength=3  )
      expect_equal( got, want )
      want <- 'Length is not Inf.'
      got <- checkLength( weirdNumeric, minimumLength=Inf, maximumLength=Inf  )
      expect_equal( got, want )

      want <- 'Length is not between 0 and 1.'
      got <- checkLength( aList, minimumLength= 0, maximumLength= 1)
      expect_equal( got, want )
      want <- 'Length is not between 1 and 100.'
      got <- checkLength( emptyList, minimumLength= 1, maximumLength= 100)
      expect_equal( got, want )
      want <- 'Length is not 1.'
      got <- checkLength( anObject, minimumLength= 1, maximumLength= 1)
      expect_equal( got, want )
      want <- 'Length is not 2.'
      got <- checkLength( aFunction, minimumLength= 2, maximumLength= 2)
      expect_equal( got, want )

   })
   it( "returns error message (not error) for bad minimumLength or maximumLength value", {
      wantRE <- "Checking for length failed with the following error: .+"
      got <- checkLength( character, minimumLength=NaN, maximumLength=NA )
      expect_match( got, wantRE )
   })
})

context( 'checkIsTrue()' )
describe( "checkIsTrue", {
   it( "returns empty string for true expressions", {
      want <- ''

      got <- checkIsTrue( TRUE )
      expect_equal( got, want )
      got <- checkIsTrue( c(T) )
      expect_equal( got, want )
      got <- checkIsTrue( 1 + 1 == 2 )
      expect_equal( got, want )
      got <- checkIsTrue({
         x <- 1
         y <- 1
         x == y
      })
      expect_equal( got, want )
      x <- 3
      f <- function () {x == 3}
      got <- checkIsTrue( f() )
      expect_equal( got, want)
   })
   it( "returns failure string for false expressions", {
      want <- "Is false."

      got <- checkIsTrue( FALSE )
      expect_equal( got, want )
      got <- checkIsTrue({ x <- c(T,T,F); all(x)} )
      expect_equal( got, want )
   })
   it( "returns failure string for NA expressions", {
      want <- "Is NA."

      got <- checkIsTrue( NA )
      expect_equal( got, want )
      got <- checkIsTrue( all( T, T, NA ))
      expect_equal( got, want )
   })
   it( "returns failure string for not vector expressions", {
      want <- "Result is not a vector of mode logical."

      got <- checkIsTrue( NULL )
      expect_equal( got, want )
      got <- checkIsTrue( list(A=TRUE) )
      expect_equal( got, want )
   })
   it( "returns failure string for not logical vector expressions", {
      want <- "Result is not a vector of mode logical."

      got <- checkIsTrue( "true" )
      expect_equal( got, want )
      got <- checkIsTrue( c("T", "F") )
      expect_equal( got, want )

   })
   it( "returns failure string for not single logical vector expressions", {
      want <- "Resulting logical vector is not of length 1."

      got <- checkIsTrue( c(T,T,T) )
      expect_equal( got, want )
      got <- checkIsTrue( logical(0) )
      expect_equal( got, want )

   })
   it( "returns error string for failed expression evaluation", {
      want <- "Checking expression for truth failed with the following error: Oops."

      expect_silent(got <- checkIsTrue( stop('Oops.') ))
      expect_equal( got, want )
   })
})

context( 'checkIsFalse()' )
describe( "checkIsFalse", {
   it( "returns empty string for false expressions", {
      want <- ''

      got <- checkIsFalse( FALSE )
      expect_equal( got, want )
      got <- checkIsFalse( c(F) )
      expect_equal( got, want )
      got <- checkIsFalse( 1 + 1 == 3 )
      expect_equal( got, want )
      got <- checkIsFalse({
         x <- 1
         y <- 2
         x == y
      })
      expect_equal( got, want )
      x <- FALSE
      f <- function () {x}
      got <- checkIsFalse( f() )
      expect_equal( got, want)
   })
   it( "returns failure string for false expressions", {
      want <- "Is true."

      got <- checkIsFalse( TRUE )
      expect_equal( got, want )
      got <- checkIsFalse({ x <- c(T,T,F); any(x)} )
      expect_equal( got, want )
   })
   it( "returns failure string for NA expressions", {
      want <- "Is NA."

      got <- checkIsFalse( NA )
      expect_equal( got, want )
      got <- checkIsFalse( all( T, T, NA ))
      expect_equal( got, want )
   })
   it( "returns failure string for not vector expressions", {
      want <- "Result is not a vector of mode logical."

      got <- checkIsFalse( NULL )
      expect_equal( got, want )
      got <- checkIsFalse( list(A=FALSE) )
      expect_equal( got, want )
   })
   it( "returns failure string for not logical vector expressions", {
      want <- "Result is not a vector of mode logical."

      got <- checkIsFalse( "false" )
      expect_equal( got, want )
      got <- checkIsFalse( c("T", "F") )
      expect_equal( got, want )

   })
   it( "returns failure string for not single logical vector expressions", {
      want <- "Resulting logical vector is not of length 1."

      got <- checkIsFalse( c(F,F,F) )
      expect_equal( got, want )
      got <- checkIsFalse( logical(0) )
      expect_equal( got, want )

   })
   it( "returns error string for failed expression evaluation", {
      want <- "Checking expression for falsehood failed with the following error: Oops."

      expect_silent(got <- checkIsFalse( stop('Oops.') ))
      expect_equal( got, want )
   })
})
