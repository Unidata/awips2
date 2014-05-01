C MEMBER PUSH26
C  (from old member FCRPN26)
C.................................................................
C
      SUBROUTINE PUSH26(STACK,VAL,NSTACK)
C
C..................................................................
C
C  SUB. PUSH26 ADDS ONE MORE VALUE TO A STACK, MOVING EVERY OTHER ELEMEN
C  DOWN ONE POSITION IN THE STACK, AND INCREMENTING THE STACK COUNTER BY
C  ONE.
C.......................................................................
C
      DIMENSION STACK(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/push26.f,v $
     . $',                                                             '
     .$Id: push26.f,v 1.1 1995/09/17 18:52:38 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (NSTACK.EQ.0) GO TO 100
C
      DO 10 I=1,NSTACK
      L = NSTACK-I+1
      STACK(L+1) = STACK(L)
   10 CONTINUE
C
  100 NSTACK = NSTACK+1
      STACK(1) = VAL
      RETURN
      END
