C MEMBER UFIXED
C (from old member UTIL1)
C-----------------------------------------------------------------------
C
      SUBROUTINE UFIXED (IBUF,ANS,ISTRT,IFIN,ITYPE,ID,IERR)
C
C              VERSION 2.0 --- 4/21/77
C
      DIMENSION IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ufixed.f,v $
     . $',                                                             '
     .$Id: ufixed.f,v 1.1 1995/09/17 19:01:36 dws Exp $
     . $' /
C    ===================================================================
C
      DATA MNS/2H- /
      IERR=0
      ANS=0.0
      IF (ITYPE.LT.1) GO TO 90
      IF (ITYPE.GT.2) GO TO 90
C
C        FIND FIRST NON-BLANK CHARACTER
C
      CALL UNOBLK (IBUF,ISTRT,IFIN,IBEGIN)
      IF (IBEGIN.LE.IFIN) GO TO 10
      IF (ITYPE.EQ.1) CALL USWITC (IERR,ANS)
      RETURN
   10 CONTINUE
      GO TO (20,60), ITYPE
C
C        INTEGER DATA
C
   20 CONTINUE
      CALL UCKINT (IBUF,IBEGIN,IFIN,IERR)
      IF (IERR.EQ.1) RETURN
      CALL UNUMIC (IBUF,IBEGIN,IFIN,NUM)
      CALL USWITC (NUM,ANS)
      RETURN
C
C        REAL DATA
C
   60 CONTINUE
      CALL UCKFLT (IBUF,IBEGIN,IFIN,KK,IERR)
      IF (IERR.EQ.1.AND.KK.EQ.0) RETURN
      CALL USRBLK (IBUF,IBEGIN,IFIN,IEND)
      IEND=IEND-1
      CALL UCKFLT (IBUF,IBEGIN,IEND,KK,IERR)
      IF (IERR.EQ.1) RETURN
      IF (KK.NE.0) GO TO 75
      CALL UNUMIC (IBUF,IBEGIN,IEND,NUM)
      ANS=NUM
C
C  MAKE SURE WHOLE NUMBERS ARE EXACT
C
      IF (ID.EQ.0) RETURN
      ANS=ANS*10.0**(-ID)
      RETURN
   75 CONTINUE
      A=0.0
      B=0.0
      IF (KK.EQ.IBEGIN) GO TO 80
      CALL UNUMIC (IBUF,IBEGIN,KK-1,NUM)
      A=NUM
   80 CONTINUE
      IF (KK.EQ.IEND) GO TO 85
      CALL UNUMIC (IBUF,KK+1,IEND,NUM)
      N=IEND-KK
      B=NUM
C
C  MAKE SURE WHOLE NUMBERS COME OUT EXACT(I.E. 23.)
C
      IF (N.EQ.0) GO TO 85
      B=B*10.0**(-N)
   85 CONTINUE
      IF (A.LT.0.0) B=-B
      ANS=A+B
      IF (ANS.GT.0.0.AND.IBUF(IBEGIN).EQ.MNS) ANS=-ANS
      RETURN
   90 CONTINUE
      IERR=1
      RETURN
      END
