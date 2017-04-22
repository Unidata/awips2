C MEMBER UNOBLK
C (from old member UTIL1)
C-----------------------------------------------------------------------
C
      SUBROUTINE UNOBLK (IBUF,I,J,K)
      DIMENSION IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/unoblk.f,v $
     . $',                                                             '
     .$Id: unoblk.f,v 1.1 1995/09/17 19:01:42 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IBLANK/2H  /
C
C        SEARCH FOR NON-BLANK
C
      DO 10 K=I,J
      IF (IBUF(K).NE.IBLANK) RETURN
   10 CONTINUE
      K=J+1
      RETURN
      END
