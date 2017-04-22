C MEMBER XPUS26
C  (from old member FCXPUS26)
C
C DESC ADD ONE ELEMENT TO TOP OF STACK, PUSHING ALL OTHERS IN STACK
C DESC  DOWN ONE LOCATION FOR RPN PROCESSING.
C-----------------------------------------------------------------------
      SUBROUTINE XPUS26(VAL,STACK,NSTACK)
C------------------------------------------------------------------
C  SUB. XPUS26 ADDS ONE MORE VALUE TO A STACK, MOVING EVERY OTHER ELEMEN
C  DOWN ONE POSITION IN THE STACK, AND INCREMENTING THE STACK COUNTER BY
C  ONE.
C.......................................................................
C  WRITTEN BY JOE OSTROWSKI - HRL - AUGUST 1983
C.......................................................................
C
      DIMENSION STACK(1)
      LOGICAL STACK,VAL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xpus26.f,v $
     . $',                                                             '
     .$Id: xpus26.f,v 1.1 1995/09/17 19:06:46 dws Exp $
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
