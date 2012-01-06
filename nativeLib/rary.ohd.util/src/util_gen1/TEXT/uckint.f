C MEMBER UCKINT
C (from old member UTIL1)
C-----------------------------------------------------------------------
C
      SUBROUTINE UCKINT (IBUF,I,J,IERR)
      DIMENSION IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/uckint.f,v $
     . $',                                                             '
     .$Id: uckint.f,v 1.1 1995/09/17 19:01:32 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IH0,IH9/2H0 ,2H9 /
      DATA MNS/1H-/
      IERR=0
C
C        CHECK FOR VALID INTEGER
C
      L=I
      IF (IBUF(I).EQ.MNS) L=L+1
      IF (L.GT.J) IERR=1
      DO 10 K=L,J
      IF (IBUF(K).GE.IH0.AND.IBUF(K).LE.IH9) GO TO 10
      IERR=1
   10 CONTINUE
      RETURN
      END
