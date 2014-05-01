C MEMBER FPUS26
C  (from old member FCSPRP26)
C.................................................................
C
      SUBROUTINE FPUS26(ISTACK,IVAL,NSTACK,ONLHS,KOR,RCL,LORCL,IBUG)
C
C..................................................................
C
C  SUB. FPUS26 ADDS ONE MORE VALUE TO A STACK, MOVING EVERY OTHER ELEM
C  DOWN ONE POSITION IN THE ISTACK, AND INCREMENTING THE ISTACK COUNTER
C  ONE. ALSO SET LEFT-HAND SIDE SWITCH TO ON, PRINT '(' IF COMB. IS 'OR'
C.......................................................................
C
      INCLUDE 'common/fdbug'
      DIMENSION ISTACK(1),KOR(1),RCL(1),COMB(2)
      LOGICAL ONLHS
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/fpus26.f,v $
     . $',                                                             '
     .$Id: fpus26.f,v 1.1 1995/09/17 18:51:38 dws Exp $
     . $' /
C    ===================================================================
C
      DATA COMB/4HAND ,4HOR  /
C
      DATA PARENL/4H(   /
C
      IF (NSTACK.EQ.0) GO TO 100
C
      DO 10 I=1,NSTACK
      L = NSTACK-I+1
      ISTACK(L+1) = ISTACK(L)
   10 CONTINUE
C
  100 NSTACK = NSTACK+1
      ISTACK(1) = IVAL
C
      ONLHS = .TRUE.
C
      IF (KOR(IVAL).EQ.1) CALL FRCL26(PARENL,RCL,LORCL,1,IBUG)
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600) NSTACK,(ISTACK(M),M=1,NSTACK)
      IF (IBUG.GE.1) WRITE(IODBUG,1601) COMB(KOR(IVAL)+1)
 1600 FORMAT('  NO. IN ISTACK AFTER PUSH =',I3,' STACK IS ',20I3)
 1601 FORMAT('    TOP COMB. IS AN ',A4)
      RETURN
      END
