context('test_fsUtils: groups()')
describe( 'groups()', {
   it( 'Calls "groups" internally (mocked syscall)', {
      wantGroupList <- c("A", "B", "C")
      mockReturnedGroupList <- paste(wantGroupList, collapse=' ')
      with_mock(
         `base::system2` = function( cmd, args, ... ) {
            message(cmd)
            return(mockReturnedGroupList)
         },
         expect_message( groups(), "groups", fixed= TRUE )
      )
   })
   it( 'Returns groups with defaut settings (mocked syscall)', {
      wantGroupList <- c("A", "B", "C")
      mockReturnedGroupList <- paste(wantGroupList, collapse=' ')
      with_mock(
         `base::system2` = function( cmd, args, ... ) {
            message(args)
            return(mockReturnedGroupList)
         },
         expect_message( groups(), "", fixed= TRUE ),
         expect_equal( groups(), wantGroupList)
      )
   })
   it( 'Ignores user= without setting unsafe= TRUE  (mocked syscall)', {
      wantGroupList <- c("A", "B", "C")
      mockReturnedGroupList <- paste(wantGroupList, collapse=' ')
      wantIgnoreUserRE <- 'User "ignoreMe" ignored; potentially unsafe\\.'
      with_mock(
         `base::system2` = function( cmd, args, ... ) {
            message(args)
            return(mockReturnedGroupList)
         },
         expect_warning( got <- groups( user= 'ignoreMe' ), wantIgnoreUserRE )
      )
      expect_equal( got, wantGroupList )
   })
   it( 'Uses user when set unsafe= TRUE  (mocked syscall)', {
      wantGroupList <- c("A", "B", "C")
      mockReturnedGroupList <- paste(wantGroupList, collapse=' ')
      wantUser = 'mockUser'
      with_mock(
         `base::system2` = function( cmd, args, ... ) {
            message(args)
            return(mockReturnedGroupList)
         },
         expect_message( groups( user= wantUser, unsafe= TRUE ),
                         wantUser, fixed= TRUE ),
         expect_equal( groups( user= wantUser, unsafe= TRUE ), wantGroupList )
      )
   })
   it( 'Returns error if system2 call returns non-success status', {
      wantUser = 'mockUser'
      wantMessageRE <- paste0( 'System command failed with status 1\\n',
                               'Command was: groups mockUser\\n',
                               'The system-level error message \\(if any\\) was: \\n')
      with_mock(
         `base::system2` = function( cmd, args, ... ) {
            message(args)
            back <- character()
            attr(back, 'status') <- 1
            return(back)
         },
         expect_error( groups( user= wantUser, unsafe= TRUE ),
                       wantMessageRE, perl = TRUE)
      )
   })
   it( 'Returns error if system2 call returns error attr, even if succeds', {
      wantUser = 'mockUser'
      wantMessageRE <- paste0(
         'System command failed with status 0\\n',
         'Command was: groups mockUser\\n',
         'The system-level error message \\(if any\\) was: Oops\\n'
      )
      with_mock(
         `base::system2` = function( cmd, args, ... ) {
            message(args)
            back <- character()
            attr(back, 'errmsg') <- "Oops"
            return(back)
         },
         expect_error( groups( user= wantUser, unsafe= TRUE ),
                       wantMessageRE, perl = TRUE)
      )
   })
   it ('Does not run on non-unix platforms', {
      wantErrorRE <- 'groups\\(\\) is only implemented for unix systems';
      unlockBinding('.Platform', baseenv())
      .Platform$OS.type <<- 'windows'
      expect_error( groups(), wantErrorRE )
      .Platform$OS.type <<- 'unix'
      lockBinding('.Platform', baseenv())
   })
})

# This uses with_mock to mock the system2 call, but unfortunately the groups()
# call within chgrp() also uses system2, and I don't know how to mock one
# but not the other. So this has to make use of knowledge of the internals of
# groups() also, providing a different return from the mocked system2 function
# based on the call that would be made their. It would be nice if from within
# the mock I could state a call stack depth that would be mocked down to, or to
# specify, based on parameters, to just call the unmocked function...
context('test_fsUtils: chgrp()')
describe( 'chgrp()', {
   it( 'Calls "chgrp" internally (mocked syscall)', {
      aFile <- tempfile( 'testFile' )
      file.create(aFile)
      aGroup <- file.info(aFile)[1, 'grname']
      with_mock(
         `base::system2` = function( cmd, args=c(aGroup, aFile), ... ) {
            # Mock groups() return
            if (cmd == 'groups') {
               return( aGroup )
            }
            message(cmd)
            return(NULL)
         },
         expect_message( chgrp( aGroup, aFile ), "chgrp", fixed= TRUE )
      )
   })
   it( 'Returns group with defaut settings (mocked syscall)', {
      aFile <- tempfile( 'testFile' )
      file.create(aFile)
      aGroup <- file.info(aFile)[1, 'grname']
      with_mock(
         `base::system2` = function( cmd, args=c(aGroup, aFile), ... ) {
            # Mock groups() return
            if (cmd == 'groups') {
               return( aGroup )
            }
            message(paste(args,collapse=' '))
            return(NULL)
         },
         expect_message( chgrp( aGroup, aFile ),
                         paste( c( aGroup, aFile ), collapse=' '), fixed= TRUE ),
         expect_equal( chgrp( aGroup, aFile ), aGroup)
      )
   })
   it( 'Error if file does not exist', {
      aFile <- tempfile( 'testFile' )
      file.create(aFile)
      aGroup <- file.info(aFile)[1, 'grname']
      badFile <- 'NoSuchFile'
      expect_false(file.exists(badFile))
      wantErrorRE <- paste0('No such file: ', badFile)
      expect_error( chgrp(aGroup, badFile), wantErrorRE, perl= TRUE)
   })
   it( 'Error if group does not exist', {
      aFile <- tempfile( 'testFile' )
      file.create(aFile)
      groups <- groups()
      expect_false( 'noSuchGroup' %in% groups )
      wantErrorRE <- 'No such group: noSuchGroup'
      expect_error( chgrp('noSuchGroup', aFile), wantErrorRE, perl= TRUE)
   })
   it( 'Returns error if system2 call returns anything', {
      aFile <- tempfile( 'testFile' )
      file.create(aFile)
      aGroup <- file.info(aFile)[1, 'grname']
      wantMessageRE <- paste0( "Can't change group to '", aGroup, "'. Error was: Also oops")
      with_mock(
         `base::system2` = function( cmd, args=c(aGroup, aFile), ... ) {
            # Mock groups() return
            if (cmd == 'groups') {
               return( aGroup )
            }
            return('Also oops')
         },
         expect_error( chgrp(aGroup, aFile), wantMessageRE, perl = TRUE)
      )
   })
   it( 'Returns error if system2 call returns non-success status', {
      aFile <- tempfile( 'testFile' )
      file.create(aFile)
      aGroup <- file.info(aFile)[1, 'grname']
      wantMessageRE <- paste0( 'System command failed with status 1\\n',
                               'Command was: chgrp ', aGroup, ' ', aFile, '\\n',
                               'The system-level error message \\(if any\\) was: \\n')
      with_mock(
         `base::system2` = function( cmd, args=c(aGroup, aFile), ... ) {
            # Mock groups() return
            if (cmd == 'groups') {
               return( aGroup )
            }
            back <- character()
            attr(back, 'status') <- 1
            return(back)
         },
         expect_error( chgrp(aGroup, aFile), wantMessageRE, perl = TRUE)
      )
   })
   it( 'Returns error if system2 call returns error attr, even if succeds', {
      aFile <- tempfile( 'testFile' )
      file.create(aFile)
      aGroup <- file.info(aFile)[1, 'grname']
      wantMessageRE <- paste0(
         'System command failed with status 0\\n',
         'Command was: chgrp ', aGroup, ' ', aFile, '\\n',
         'The system-level error message \\(if any\\) was: Oops\\n'
      )
      with_mock(
         `base::system2` = function( cmd, args=c(aGroup, aFile), ... ) {
            # Mock groups() return
            if (cmd == 'groups') {
               return( aGroup )
            }
            back <- character()
            attr(back, 'errmsg') <- "Oops"
            return(back)
         },
         expect_error( chgrp(aGroup, aFile), wantMessageRE, perl = TRUE)
      )
   })
   it ('Does not run on non-unix platforms', {
      wantErrorRE <- 'chgrp\\(\\) is only implemented for unix systems';
      on.exit({
         .Platform$OS.type <<- 'unix'
         lockBinding('.Platform', baseenv())
         }, add = TRUE
      )
      unlockBinding('.Platform', baseenv())
      .Platform$OS.type <<- 'windows'
      aFile <- tempfile( 'testFile' )
      file.create(aFile)
      aGroup <- file.info(aFile)[1, 'grname']
      expect_error( chgrp(aGroup, aFile), wantErrorRE )
   })
})
