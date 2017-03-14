C MODULE OMAIN
C-----------------------------------------------------------------------
C
C  MAIN ROUTINE FOR PROGRAM OPT3.

	  SUBROUTINE OPT3_MAIN

      PARAMETER (MOA=500)
      DIMENSION OA(MOA)

      INCLUDE 'common/fp'
      INCLUDE 'common/fc'
      INCLUDE 'common/fd'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
      INCLUDE 'common/oldp'
      INCLUDE 'common/oldc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3/RCS/omain.f,v $
     . $',                                                             '
     .$Id: omain.f,v 1.6 2004/08/10 15:20:12 dsa Exp $
     . $' /
C    ===================================================================
C
C
C  Make call to "_main" to satisfy C++ linker
C      CALL MAIN()
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
C  Set cpu timer, sys level i/o unit numbers
      CALL URTIMR (LAPSE,ITMBEG)
      CALL UPINIO ()
C
      CALL OPTMN (TS,MTS,P,MP,C,MC,T,MT,D,MD,
     *   OA,MOA,OLDP,MOLDP,OLDC,MOLDC)
C
      CALL UPRIMC ()
C
      STOP
C
      END
