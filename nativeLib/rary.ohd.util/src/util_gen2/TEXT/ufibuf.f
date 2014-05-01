C MODULE UFIBUF
C-----------------------------------------------------------------------
C
      SUBROUTINE UFIBUF (ICHR,ISTRT,IEND,ICOL)
C
C  THIS ROUTINE SEARCHES FOR CHARACTER STRING FROM COLUMNS ISTRT TO IEND
C  OF ARRAY ICDBUF.
C
C  IF FOUND, ICOL=COLUMN NUMBER, ELSE ICOL=0
C
      CHARACTER*1 ICHR
C
      INCLUDE 'ucmdbx'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ufibuf.f,v $
     . $',                                                             '
     .$Id: ufibuf.f,v 1.2 1998/07/06 13:21:43 page Exp $
     . $' /
C    ===================================================================
C
C
      ICOL=0
C
      IF (IEND.LT.ISTRT) GO TO 30
C
      DO 10 I=ISTRT,IEND
         IF (ICMDBG.GT.1) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,50) ICDBUF(I:I),ICHR
            ENDIF
         IF (ICDBUF(I:I).EQ.ICHR) GO TO 20
10       CONTINUE
      GO TO 30
C
20    ICOL=I
C
30    IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,40) ICHR,ISTRT,IEND,ICOL
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** EXIT UFIBUF : ICHR=',A1,3X,'ISTRT=',I3,3X,
     *   'IEND=',I3,3X,'ICOL=',I3)
50    FORMAT (' ICDBUF(I:I)=',A1,3X,'ICHR=',A1)
C
      END
