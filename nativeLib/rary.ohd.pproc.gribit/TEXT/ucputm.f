C MEMBER UCPUTM
C-----------------------------------------------------------------------
C
      SUBROUTINE UCPUTM (NUNIT,ITELA,ITTOT)
C
C  ROUTINE TO PRINT ELAPSED AND TOTAL CPU TIME.
C
C
C  ARGUMENT LIST:
C
C     ARGUMENT   TYPE   I/O   DIM   CONTENTS
C     --------   ----   ---   ---   --------
C     NUNIT      I*4    I     1     PRINTER UNIT NUMBER
C                                     <0=DO NOT PRINT
C     ITELA      I*4    I     1     ELAPSED CPU TIME USED SINCE ROUTINE
C                                   WAS LAST CALLED.
C     ITTOT      I*4    I     1     TOTAL CPUT TIME USED
C
C  NOTES:
C    1. VARIABLES ITELA ANT ITTOT SHOULD BE INITIALIZED TO ZERO BEFORE
C       FIRST CALL TO UCPUTM.
C
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/ucputm.f,v $
     . $',                                                             '
     .$Id: ucputm.f,v 1.1 2006/05/03 13:43:59 gsood Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,40) NUNIT
         ENDIF
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
      CALL ULINE2 (NUNIT,2)
      WRITE (IUNIT,50) TMELA,TMTOT
      CALL ULINE2 (IUNIT,2)
C
30    IF (ICMTRC.GT.0) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,60) ITMELA,ITMTOT,ITMRUN
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER UCPUTM - NUNIT=',I2)
50    FORMAT ('0*** NOTE - ELAPSED CPU TIME IS ',F6.2,' SECONDS. ',
     *     'TOTAL CPU TIME IS ',F6.2,' SECONDS.')
60    FORMAT (' *** EXIT UCPUTM - ITMELA=',I5,3X,'ITMTOT=',I6,3X,
     *   'ITMRUN=',I4)
C
      END
