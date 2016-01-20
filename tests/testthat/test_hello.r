context( 'hello()' )

describe( 'Default behavior', {
   it( 'Outputs and returns expected message when called', {
      outRE <- 'Hello, world!'
      expect_output( got <- hello(), outRE )
      expect_match( got, outRE )
   })
})
