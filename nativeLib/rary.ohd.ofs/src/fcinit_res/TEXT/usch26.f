C MEMBER USCH26
C  (from old member FCUSCH26)
C
C DESC SEARCH FOR A CHARACTER IN A STRING
C-------------------------------------------------------------------
      SUBROUTINE USCH26(ICHAR,ISTR,IS,IE,IX)
C--------------------------------------------------------------
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------
      DIMENSION ISTR(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/usch26.f,v $
     . $',                                                             '
     .$Id: usch26.f,v 1.1 1995/09/17 18:53:29 dws Exp $
     . $' /
C    ===================================================================
C
C
C SEARCH STRING ISTR FROM COLUMN IS TO IE FOR ICHAR.
C  IF FOUND, IX IS COLUMN. ELSE, IX IS 0
C
      IX=0
      IF(IE.LT.IS) GO TO 999
      DO 10 I=IS,IE
        IF(ISTR(I).EQ.ICHAR) GO TO 20
10    CONTINUE
C
      GO TO 999
C
20    IX=I
999   RETURN
      END
