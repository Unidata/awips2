C MEMBER INDEXX
C  (from old member OSCEUA)
C
C=======================================================
C @PROCESS LVL(77)
      SUBROUTINE INDEXX(N, ARRIN, INDX)
C
      DIMENSION ARRIN(N), INDX(N)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/indexx.f,v $
     . $',                                                             '
     .$Id: indexx.f,v 1.1 1996/07/11 20:44:05 dws Exp $
     . $' /
C    ===================================================================
C
C
      DO 11 J = 1, N
      INDX(J) = J
   11 CONTINUE
      L = (N / 2) + 1
      IR = N
   10 CONTINUE
      IF (L .GT. 1) THEN
      L = L - 1
      INDXT = INDX(L)
      Q = ARRIN(INDXT)
      ELSE
      INDXT = INDX(IR)
      Q = ARRIN(INDXT)
      INDX(IR) = INDX(1)
      IR = IR - 1
      IF (IR .EQ. 1) THEN
      INDX(1) = INDXT
      RETURN
      END IF
      END IF
      I = L
      J = L + L
   20 IF (J .LE. IR) THEN
      IF (J .LT. IR) THEN
      IF (ARRIN(INDX(J)) .LT. ARRIN(INDX(J + 1))) J = J + 1
      END IF
      IF (Q .LT. ARRIN(INDX(J))) THEN
      INDX(I) = INDX(J)
      I = J
      J = J + J
      ELSE
      J = IR + 1
      END IF
      GOTO 20
      END IF
      INDX(I) = INDXT
      GOTO 10
      END
