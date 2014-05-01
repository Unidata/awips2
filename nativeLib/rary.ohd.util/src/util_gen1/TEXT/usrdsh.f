C MEMBER USRDSH
C (from old member UTIL1)
C-----------------------------------------------------------------------
C
      SUBROUTINE USRDSH (IBUF,I,J,K)
C
C          ** VERSION 4.0
C
      DIMENSION IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/usrdsh.f,v $
     . $',                                                             '
     .$Id: usrdsh.f,v 1.1 1995/09/17 19:01:52 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IDASH/2H- /
C
C          SEARCH FOR DASH
C
      IFIND=IDASH
    5 DO 10 K=I,J
      IF (IBUF(K).EQ.IFIND) RETURN
   10 CONTINUE
      K=J+1
      RETURN
      END
