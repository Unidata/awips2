C MEMBER FPOP26
C  (from old member FCSPRP26)
C
C  DESC -- MOVE EVERY ELEMENT IN ARRAY UP ONE POSITION, THEREBY REMOVING
C  DESC    THE FIRST ELEMENT. (USED FOR INTEGER ISTACK)
C....................................
C
      SUBROUTINE FPOP26(ISTACK,NSTACK,IBUG)
C
C..........................................
C
      INCLUDE 'common/fdbug'
      DIMENSION ISTACK(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/fpop26.f,v $
     . $',                                                             '
     .$Id: fpop26.f,v 1.1 1995/09/17 18:51:36 dws Exp $
     . $' /
C    ===================================================================
C
C
       IF (NSTACK.EQ.0) RETURN
      IF (NSTACK.EQ.1) GO TO 20
C
       DO 10 I=2,NSTACK
   10 ISTACK(I-1) = ISTACK(I)
      NSTACK = NSTACK-1
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600) NSTACK,(ISTACK(M),M=1,NSTACK)
 1600 FORMAT('   NO. IN ISTACK AFTER POP =',I3,' STACK IS ',20I3)
C
      RETURN
C
   20 CONTINUE
      NSTACK = NSTACK-1
      ISTACK(1) = 0.01
      RETURN
      END
