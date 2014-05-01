C MODULE ARGVER
C  ============================================================================
C  pgm: ARGVER .. Output version/date info and exit if first arg is "-version"
C
C  use:     CALL ARGVER()
C
C   in: (arg) ....... (optional) if the first command line argument is
C   in:               "-version" or "-VERSION" then output the program's
C   in:               name, version number, and date as defined bu variables
C   in:               in common block "upvrsx"
C  out: (stdout) .... if the above option is given, output the version/date
C  out:               information to standard out and stop the program
C
C  rqd: subroutines - getarg (intrinsic)
C
C  ver: 20040218
C  ============================================================================
      SUBROUTINE ARGVER()
 
      INCLUDE 'upvrsx'

      CHARACTER*8   ARGCMD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/argver.f,v $
     . $',                                                             '
     .$Id: argver.f,v 1.1 2006/05/03 13:43:57 gsood Exp $
     . $' /
C    ===================================================================
C

        CALL GETARG(1,ARGCMD)
        IF ( ARGCMD .EQ. '-version'  .OR.  ARGCMD .EQ. '-VERSION' ) THEN
          WRITE (*,80) PGMNAM,PGMVRN,PGMVRD
   80     FORMAT('    Program:  ',A,'    Version:  ',A,'    Date:  ',A)
          STOP
        ENDIF

      RETURN
      END
