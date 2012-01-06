C MEMBER RFIL51
C----------------------------------------------------------------
C
C@PROCESS LVL(77)
C
      SUBROUTINE RFIL51(STR1,IPOS,VALUE)
C
C DESC STORE A VALUE IN AN ARRAY IF NO ERRORS IN PIN51 HAVE OCCURRED
C
C.........................................
C
C  JTOSTROWSKI - HRL - MARCH 1983
C................................................................
      INCLUDE 'common/err51'
      DIMENSION STR1(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/rfil51.f,v $
     . $',                                                             '
     .$Id: rfil51.f,v 1.1 1996/03/21 14:35:19 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (NUMERR.GT.0) GO TO 9999
C
      CALL UMEMOV(VALUE,STR1(IPOS),1)
C
 9999 CONTINUE
      RETURN
      END
