C MODULE USCHFL
C-----------------------------------------------------------------------
C
      SUBROUTINE USCHFL (ICHR,IFIELD,ICOL)
C
C  SEARCH FOR CHARACTER STRING IN FIELD IFIELD.
C  IF FOUND, ICOL=COLUMN NUMBER, ELSE ICOL=0
C
      CHARACTER*1 XCHR
C
      INCLUDE 'udebug'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/uschfl.f,v $
     . $',                                                             '
     .$Id: uschfl.f,v 1.2 1998/07/02 19:46:27 page Exp $
     . $' /
C    ===================================================================
C
C
      ICOL=0
C
      IF (IFIELD.GT.NFIELD) GO TO 30
C
      ISTRT=IFSTRT(IFIELD)
      ISTOP=IFSTOP(IFIELD)
C
      NPOS=0
C
      CALL UMOVEX (ICHR,1,XCHR,1,1)     
C
      DO 10 I=ISTRT,ISTOP
         NPOS=NPOS+1
         IF (IUTLDB.GT.4) WRITE (IOGDB,40) ICDBUF(I:I),XCHR
         IF (ICDBUF(I:I).EQ.XCHR) GO TO 20
10       CONTINUE
C
      GO TO 30
C
20    ICOL=NPOS
C
30    IF (IUTLDB.GT.2) WRITE (IOGDB,50) ICHR,IFIELD,ICOL
C
      RETURN
C
40    FORMAT (' ICDBUF(I:I)=',A,3X,'XCHR=',A)
50    FORMAT (' *** EXIT USCHFL : ICHR=',A4,3X,'IFIELD=',I3,3X,
     *   'ICOL=',I3)
C
      END
