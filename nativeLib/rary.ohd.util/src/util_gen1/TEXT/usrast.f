C MEMBER USRAST
C (from old member UTIL1)
C-----------------------------------------------------------------------
C
      SUBROUTINE USRAST (IBUF,I,J,K)
C
C          ** VERSION 4.0
C
      DIMENSION IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/usrast.f,v $
     . $',                                                             '
     .$Id: usrast.f,v 1.1 1995/09/17 19:01:49 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IAST/2H* /
C
C        SEARCH FOR ASTERISK
C
      IFIND=IAST
    5 DO 10 K=I,J
      IF (IBUF(K).EQ.IFIND) RETURN
   10 CONTINUE
      K=J+1
      RETURN
      END
