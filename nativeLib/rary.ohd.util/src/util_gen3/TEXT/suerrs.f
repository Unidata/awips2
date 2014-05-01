C MEMBER SUERRS
C  (from old member SUEROR)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/26/94.12:26:18 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SUERRS (IUNIT,NLINES,NUMERR)
C
C  DESC ROUTINE SUERRS CALLS SUEROR AND SULINE WHEN AN ERROR IS
C  DESC ENCOUNTERED.
C
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/suerrs.f,v $
     . $',                                                             '
     .$Id: suerrs.f,v 1.1 1995/09/17 19:02:55 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GE.3) WRITE (IOSDBG,10)
      IF (ISTRCE.GE.3) CALL SULINE (IOSDBG,1)
10    FORMAT (' *** ENTER SUERRS')
C
C  UPDATE NUMBER OF ERRORS ENCOUNTERED
      CALL SUEROR
C
      IF (IUNIT.EQ.0.OR.IOPOVP.EQ.0) GO TO 40
C
C  OVERPRINT '*** ERROR -'
      IF (ISTRCE.GE.3) WRITE (IUNIT,20)
20    FORMAT (1H )
      WRITE (IUNIT,30)
      WRITE (IUNIT,30)
30    FORMAT ('+*** ERROR')
C
C  COUNT NUMBER OF ERRORS ENCOUNTERED
40    IF (NUMERR.GE.0) NUMERR=NUMERR+1
C
C  UPDATE THE LINES PRINTED
      IF (IUNIT.GT.0.AND.NLINES.GT.0) CALL SULINE (IUNIT,NLINES)
C
      IF (ISTRCE.GE.3) WRITE (IOSDBG,50)
      IF (ISTRCE.GE.3) CALL SULINE (IOSDBG,1)
50    FORMAT (' *** EXIT SUERRS')
C
      RETURN
C
      END
