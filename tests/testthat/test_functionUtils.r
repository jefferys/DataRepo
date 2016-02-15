context(" Set up functionUtils tests.")
describe( "Set up.", {
   it( "Has needed package/search path", {
      expect_true("package:base" %in% search())
      expect_false("package:tools" %in% search())
   })
})

context( "DataRepo::getSomewhere()" )
describe( "Finds existing fully qualified objects", {
   it('gets fully-qualified functions from attached packages', {
      got <- getSomewhere('base::ls')
      want <- base::ls
      expect_equal(got, want)
   })
   it('gets fully-qualified functions from unattached packages', {
      got <- getSomewhere('tools::md5sum')
      want <- tools::md5sum
      expect_equal(got, want)
   })
   it('ignores extra parameters while doing so', {
      got <- getSomewhere('tools::md5sum', mode='logical')
      want <- tools::md5sum
      expect_equal(got, want)

      got <- getSomewhere('base::ls', xyzzy= 'logical')
      want <- base::ls
      expect_equal(got, want)
   })
})
describe( "Finds existing unqualified objects", {
   it('gets unqualified functions from attached packages', {
      got <- getSomewhere('ls')
      want <- base::ls
      expect_equal(got, want)
   })
   it('uses matching extra parameters', {
      echo <- c(1,1,1)
      f <- function() {
         echo <- function(x,y) {return(x)}

         got <- getSomewhere('echo', mode='function')
         expect_equal(got, echo)
         expect_silent( got <- got("bob") )
         expect_equal( got, "bob" )

         errorRE <- "object 'echo' of mode 'raw' was not found"
         expect_error( getSomewhere('echo', mode='raw'), errorRE)
      }
      f()
   })
})
describe( "Errors when objects are not found", {
   it('fails with non-existing unqualified name.', {
      errorRE <- "object 'xyzzy' not found"
      expect_error( getSomewhere('xyzzy'), errorRE)
   })
   it('fails with non-existing qualified name.', {
      errorRE <- "object 'xyzzy' not found"
      expect_error( getSomewhere('utils::xyzzy'), errorRE)

      # The message from R in this case is kind of odd.
      errorRE <- "there is no package called.*xyzzy"
      expect_error( getSomewhere('xyzzy::print'), errorRE)
   })
   it('fails if no qualified name object but unqualified name object exists', {
      errorRE <- "object 'ls' not found"
      expect_error( getSomewhere('utils::ls'), errorRE)
   })
})
describe( "Errors when parameters bad", {
   it('fails with non-string x.', {
      errorRE <- "Bad parameter 'x': Non-empty single string name required."
      expect_error( getSomewhere(x=NULL), errorRE)

      errorRE <- "Bad parameter 'x': Non-empty single string name required."
      expect_error( getSomewhere(x=list(A="B")), errorRE)

      errorRE <- "Bad parameter 'x': Non-empty single string name required."
      expect_error( getSomewhere(x=NA_character_), errorRE)

      errorRE <- "Bad parameter 'x': Non-empty single string name required."
      expect_error( getSomewhere(x=c(1)), errorRE)

      errorRE <- "Bad parameter 'x': Non-empty single string name required."
      expect_error( getSomewhere(x=c('A', 'B')), errorRE)

      errorRE <- "Bad parameter 'x': Non-empty single string name required."
      expect_error( getSomewhere(x=character(0)), errorRE)

      errorRE <- "Bad parameter 'x': Non-empty single string name required."
      expect_error( getSomewhere(x=""), errorRE)
   })
   it('fails with ::: containing x.', {
      errorRE <- "Bad parameter 'x': Unexported objects not supported."
      expect_error( getSomewhere(x='utils:::insideTrack'), errorRE)
   })
   it('fails with incorrectly formatted :: containing x.', {
      errorRE <- "Bad parameter 'x': Incorrectly specified qualified name."
      expect_error( getSomewhere(x='utils::'), errorRE)

      errorRE <- "Bad parameter 'x': Incorrectly specified qualified name."
      expect_error( getSomewhere(x='::getAnywhere'), errorRE)

      errorRE <- "Bad parameter 'x': Incorrectly specified qualified name."
      expect_error( getSomewhere(x='::'), errorRE)
   })
})
