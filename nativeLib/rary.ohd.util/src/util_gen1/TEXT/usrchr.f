C MEMBER USRCHR
C (from old member UTIL1)
C-----------------------------------------------------------------------
C
      SUBROUTINE USRCHR (ICHAR,IS,IE,IX)
C
C SEARCH STRING IN IBUF(UFREEI) FROM COLUMN IS TO IE FOR ICHAR.
C  IF FOUND, IX IS COLUMN ELSE, IX IS 0
C
      INCLUDE 'ufreei'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/usrchr.f,v $
     . $',                                                             '
     .$Id: usrchr.f,v 1.1 1995/09/17 19:01:51 dws Exp $
     . $' /
C    ===================================================================
C
      IX=0
      IF (IE.LT.IS) GO TO 999
      DO 10 I=IS,IE
        IF (IBUF(I).EQ.ICHAR) GO TO 20
10    CONTINUE
C
      GO TO 999
C
20    IX=I
999   RETURN
      END
