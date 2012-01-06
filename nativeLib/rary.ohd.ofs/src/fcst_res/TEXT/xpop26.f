C MEMBER XPOP26
C  (from old member FCXPOP26)
C
C DESC POP TOP VALUE OF STACK FOR RPN PROCESSING
C-----------------------------------------------------------------------
      SUBROUTINE XPOP26(STACK,NSTACK)
C-----------------------------------------------------------------------
C   -- MOVE EVERY ELEMENT IN ARRAY UP ONE POSITION, THEREBY REMOVING
C      THE FIRST ELEMENT.
C....................................
C  WRITTEN BY - JOE OSTROWSKI - HRL -AUGUST 1983
C..........................................
C
C
      DIMENSION STACK(1)
      LOGICAL STACK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xpop26.f,v $
     . $',                                                             '
     .$Id: xpop26.f,v 1.1 1995/09/17 19:06:44 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (NSTACK.EQ.0) RETURN
      IF (NSTACK.EQ.1) GO TO 20
C
      DO 10 I=2,NSTACK
   10 STACK(I-1) = STACK(I)
   20 CONTINUE
      NSTACK = NSTACK-1
      RETURN
      END
