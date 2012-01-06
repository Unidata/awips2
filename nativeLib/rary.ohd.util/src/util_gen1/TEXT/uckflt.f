C MEMBER UCKFLT
C (from old member UTIL1)
C-----------------------------------------------------------------------
C
      SUBROUTINE UCKFLT (IBUF,I,J,KK,IERR)
      DIMENSION IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/uckflt.f,v $
     . $',                                                             '
     .$Id: uckflt.f,v 1.1 1995/09/17 19:01:31 dws Exp $
     . $' /
C    ===================================================================
C
      DATA IH0,IH9,IPNT/1H0,1H9,1H./
      DATA MNS/1H-/
      IERR=0
      KK=0
C
C        CHECK FOR VALID FLOATING POINT NUMBER
C
      IDEC=0
      L=I
      IF (IBUF(I).EQ.MNS) L=L+1
      IF (L.GT.J) IERR=1
      DO 20 K=L,J
      IF (IBUF(K).GE.IH0.AND.IBUF(K).LE.IH9) GO TO 20
      IF (IBUF(K).EQ.IPNT) GO TO 10
      IERR=1
      RETURN
   10 CONTINUE
      KK=K
      IDEC=IDEC+1
   20 CONTINUE
      IF (IDEC.GT.1) IERR=1
      RETURN
      END
