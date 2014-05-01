C MODULE USCHBF
C-----------------------------------------------------------------------
C
      SUBROUTINE USCHBF (ICHR,ISTRT,IEND,ICOL)
C
C  SEARCH FOR CHARACTER STRING FROM COLUMNS ISTRT TO IEND OF ICDBUF.
C  IF FOUND, ICOL=COLUMN NUMBER, ELSE ICOL=0
C
      CHARACTER*4 ICHR
      CHARACTER*1 XCHR
C            
      INCLUDE 'udebug'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/uschbf.f,v $
     . $',                                                             '
     .$Id: uschbf.f,v 1.2 1998/07/02 19:46:08 page Exp $
     . $' /
C    ===================================================================
C
C
      ICOL=0
C
      IF (IEND.LT.ISTRT) GO TO 30
C
      CALL UMOVEX (ICHR,1,XCHR,1,1)
C
      DO 10 I=ISTRT,IEND
         IF (IUTLDB.GT.4) WRITE (IOGDB,50) ICDBUF(I:I),XCHR
         IF (ICDBUF(I:I).EQ.XCHR) GO TO 20
10       CONTINUE
C
      GO TO 30
C
20    ICOL=I
C
30    IF (IUTLTR.GT.2) WRITE (IOGDB,40) ICHR,ISTRT,IEND,ICOL
C
      RETURN
C
40    FORMAT (' *** EXIT USCHBF : ICHR=',A4,3X,'ISTRT=',I3,3X,
     *   'IEND=',I3,3X,'ICOL=',I3)
50    FORMAT (' ICDBUF(I)=',A,3X,'XCHR=',A)
C
      END
