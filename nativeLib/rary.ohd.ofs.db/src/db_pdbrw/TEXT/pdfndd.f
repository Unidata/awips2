C MEMBER PDFNDD
C  (from old member PDPDFDAT)
C-----------------------------------------------------------------------
C
      SUBROUTINE PDFNDD (IDAY,IDATES,IDTXX)
C
C  ROUTINE TO FIND A SPECIFIC DATE IN THE FUTURE DATES ARRAY
C
      INCLUDE 'udebug'
C
      DIMENSION IDATES(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdfndd.f,v $
     . $',                                                             '
     .$Id: pdfndd.f,v 1.1 1995/09/17 18:43:50 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,40)
C
      IDTXX=0
C
      NDTS=IDATES(1)
      IF (NDTS.EQ.0) GO TO 30
C
      J=2
      DO 10 I=1,NDTS
         IF (IDATES(J).EQ.IDAY) GO TO 20
         J=J+2
10       CONTINUE
      GO TO 30
C
20    IDTXX=J
C
30    IF (IPDTR.GT.0) WRITE (IOGDB,50) IDAY,IDTXX
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER PDFNDD')
50    FORMAT (' *** EXIT PDFNDD : IDAY=',I6,3X,'IDTXX=',I4)
C
      END
