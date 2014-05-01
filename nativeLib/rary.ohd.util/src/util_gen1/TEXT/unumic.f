C MEMBER UNUMIC
C (from old member UTIL1)
C-----------------------------------------------------------------------
C
      SUBROUTINE UNUMIC (IBUF,I,J,NUM)
      INTEGER CON(10),IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/unumic.f,v $
     . $',                                                             '
     .$Id: unumic.f,v 1.1 1995/09/17 19:01:44 dws Exp $
     . $' /
C    ===================================================================
C
      DATA CON/2H0 ,2H1 ,2H2 ,2H3 ,2H4 ,2H5 ,2H6 ,2H7 ,2H8 ,2H9 /
      DATA MNS/1H-/
C
C        CONVERT FIELD TO INTEGER VALUE
C
      NUM=0
      M=I
      IF (IBUF(I).EQ.MNS) M=M+1
      IF (M.GT.J) RETURN
      DO 20 K=M,J
      DO 10 L=1,10
      IF (IBUF(K).EQ.CON(L)) GO TO 15
   10 CONTINUE
   15 NUM=10*NUM+L-1
   20 CONTINUE
      IF (M.NE.I) NUM=-NUM
      RETURN
      END
