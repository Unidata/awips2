C MEMBER SORT1
C  (from old member OSCEUA)
C
C================================================
C @PROCESS LVL(77)
      SUBROUTINE SORT1(N,RA)
C
C
C  SORTING SUBROUTINE ADAPTED FROM "NUMERICAL RECIPES"
C  BY W.H. PRESS ET AL., PP. 231
C
C  LIST OF VARIABLES
C     RA(.) = INTEGER ARRAY TO BE SORTED
C
      DIMENSION RA(N)
C
      INTEGER RA, RRA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/sort1.f,v $
     . $',                                                             '
     .$Id: sort1.f,v 1.1 1996/07/11 21:01:58 dws Exp $
     . $' /
C    ===================================================================
C
C
      L = (N / 2) + 1
      IR = N
   10 CONTINUE
      IF (L .GT. 1) THEN
      L = L - 1
      RRA = RA(L)
      ELSE
      RRA = RA(IR)
      RA(IR) = RA(1)
      IR = IR - 1
      IF (IR .EQ. 1) THEN
      RA(1) = RRA
      RETURN
      END IF
      END IF
      I = L
      J = L + L
   20 IF (J .LE. IR) THEN
      IF (J .LT. IR) THEN
      IF (RA(J) .LT. RA(J + 1)) J = J + 1
      END IF
      IF (RRA .LT. RA(J)) THEN
      RA(I) = RA(J)
      I = J
      J = J + J
      ELSE
      J = IR + 1
      END IF
      GOTO 20
      END IF
      RA(I) = RRA
      GOTO 10
C
C  END OF SUBROUTINE SORT1
      END
