C MEMBER SUTIMR
C-----------------------------------------------------------------------
C
C DESC ROUTINE TO PRINT ELAPSED AND TOTAL CPU TIME.
C
      SUBROUTINE SUTIMR (NUNIT,ITELA,ITTOT)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sutmrx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/sutimr.f,v $
     . $',                                                             '
     .$Id: sutimr.f,v 1.1 1995/09/17 19:03:03 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,40) NUNIT
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  CHECK IF ONLY CPU TIME IS TO BE RETURNED
      IF (NUNIT.LT.0) GO TO 10
C
C  CHECK IF ONLY CPU TIME USED IS TO BE PRINTED
      IF (NUNIT.NE.LP) GO TO 20
C
C  GET CPU TIME
10    CALL URTIMR (ITELA,ITTOT)
      ITMELA=ITELA
      ITMTOT=ITTOT
C
C  STORE CPU TIME USED FOR RUN
      ITMRUN=ITMRUN+ITMELA
C
C  CHECK IF ONLY CPU TIME IS TO BE RETURNED
      IF (NUNIT.LT.0) GO TO 30
C
C  COMPUTE CPU TIME ELAPSED SINCE ROUTINE LAST CALLED AND TOTAL CPU
C  TIME USED
20    IF (ITELA.GE.0) TMELA=ITMELA/100.
      IF (ITELA.LT.0) TMELA=IABS(ITELA)/100.
      TMTOT=ITTOT/100.
C
C  PRINT ELAPSED AND TOTAL CPU TIME
      IUNIT=NUNIT
      IF (IUNIT.EQ.0) IUNIT=LP
      WRITE (IUNIT,50) TMELA,TMTOT
      CALL SULINE (IUNIT,2)
C
30    IF (ISTRCE.GT.0) WRITE (IOSDBG,60) ITMELA,ITMTOT,ITMRUN
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SUTIMR - NUNIT=',I2)
50    FORMAT ('0*** NOTE - ELAPSED CPU TIME IS ',F6.2,' SECONDS. ',
     *     'TOTAL CPU TIME IS ',F6.2,2X,'SECONDS.')
60    FORMAT (' *** EXIT SUTIMR - ITMELA=',I4,3X,'ITMTOT=',I4,3X,
     *   'ITMRUN=',I4)
C
      END
