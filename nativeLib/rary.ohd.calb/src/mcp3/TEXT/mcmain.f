C MODULE MCMAIN
C-----------------------------------------------------------------------
C  MAIN ROUTINE FOR PROGRAM MCP3.
C-----------------------------------------------------------------------
	  SUBROUTINE MCP3_MAIN

      EXTERNAL FTN_STD_ERR
      INTEGER FTN_STD_ERR

      INCLUDE 'common/fp'
      INCLUDE 'common/fc'
      INCLUDE 'common/fd'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
      INCLUDE 'common/sionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mcp3/RCS/mcmain.f,v $
     . $',                                                             '
     .$Id: mcmain.f,v 1.7 2004/08/10 15:15:56 dsa Exp $
     . $' /
C    ===================================================================
C

C  Make call to "_main" to satisfy C++ linker
C      CALL MAIN()

C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"

      CALL ARGVER()

C  Set cpu timer, sys level i/o unit numbers
      CALL URTIMR (LAPSE,ITMBEG)
      CALL UPINIO ()

      ISTDERR=FTN_STD_ERR()
      ISTDIN=5
      ISTDOUT=6

      CALL MCPMN (TS,MTS,P,MP,C,MC,T,MT,D,MD)

      STOP
      END
