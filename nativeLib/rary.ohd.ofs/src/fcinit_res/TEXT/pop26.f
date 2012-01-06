C MEMBER POP26
C  (from old member FCRPN26)
C....................................
C
      SUBROUTINE POP26(STACK,NSTACK)
C
C..........................................
C  -- MOVE EVERY ELEMENT IN ARRAY UP ONE POSITION, THEREBY REMOVING
C     THE FIRST ELEMENT.
C
      DIMENSION STACK(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/pop26.f,v $
     . $',                                                             '
     .$Id: pop26.f,v 1.1 1995/09/17 18:52:24 dws Exp $
     . $' /
C    ===================================================================
C
C
       IF (NSTACK.EQ.0) RETURN
      IF (NSTACK.EQ.1) GO TO 20
C
       DO 10 I=2,NSTACK
   10 STACK(I-1) = STACK(I)
      NSTACK = NSTACK-1
      RETURN
C
   20 CONTINUE
      NSTACK = NSTACK-1
      STACK(1) = 0.01
      RETURN
      END
