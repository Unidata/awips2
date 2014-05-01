C MEMBER USRBLK
C (from old member UTIL1)
C-----------------------------------------------------------------------
C
      SUBROUTINE USRBLK (IBUF,I,J,K)
      DIMENSION IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/usrblk.f,v $
     . $',                                                             '
     .$Id: usrblk.f,v 1.1 1995/09/17 19:01:50 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IBLANK/2H  /,ICOMMA/2H, /
C
C        SEARCH FOR BLANK OR COMMA
C
      DO 10 K=I,J
      IF (IBUF(K).EQ.ICOMMA) RETURN
      IF (IBUF(K).EQ.IBLANK) RETURN
   10 CONTINUE
      K=J+1
      RETURN
      END
