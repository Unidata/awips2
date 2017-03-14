C MEMBER SUWRNS
C  (from old member SUEROR)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/26/94.12:26:18 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SUWRNS (IUNIT,NLINES,NUMWRN)
C
C  DESC ROUTINE SUWRNS CALLS SUWARN AND SULINE WHEN AN WARNING IS
C  DESC ENCOUNTERED.
C
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/suwrns.f,v $
     . $',                                                             '
     .$Id: suwrns.f,v 1.1 1995/09/17 19:03:04 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GE.3) WRITE (IOSDBG,10)
      IF (ISTRCE.GE.3) CALL SULINE (IOSDBG,1)
10    FORMAT (' *** ENTER SUWRNS')
C
C  UPDATE NUMBER OF WARNINGS ENCOUNTERED
      CALL SUWARN
C
      IF (IUNIT.EQ.0.OR.IOPOVP.EQ.0) GO TO 40
C
C  OVERPRINT '*** WARNING -'
      IF (ISTRCE.GE.3) WRITE (IUNIT,20)
20    FORMAT (1H )
      WRITE (IUNIT,30)
      WRITE (IUNIT,30)
30    FORMAT ('+*** WARNING')
C
C  COUNT NUMBER OF WARNINGS ENCOUNTERED
40    IF (NUMWRN.GE.0) NUMWRN=NUMWRN+1
C
C  UPDATE THE LINES PRINTED
      IF (IUNIT.GT.0.AND.NLINES.GT.0) CALL SULINE (IUNIT,NLINES)
C
      IF (ISTRCE.GE.3) WRITE (IOSDBG,50)
      IF (ISTRCE.GE.3) CALL SULINE (IOSDBG,1)
50    FORMAT (' *** EXIT SUWRNS')
C
      RETURN
C
      END
