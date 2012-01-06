C MEMBER UMOVEX
C-----------------------------------------------------------------------
C
      SUBROUTINE UMOVEX (FROM,IPOS1,TO,IPOS2,NPOS)
C
C  MOVE NPOS CHARACTERS STARTING WITH FROM(IPOS1) INTO TO(IPOS2).
C
      CHARACTER*1 FROM(1),TO(1)
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/umovex.f,v $
     . $',                                                             '
     .$Id: umovex.f,v 1.1 1995/09/17 19:02:18 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (NPOS.LE.0) GO TO 20
C
      DO 10 I=1,NPOS
         TO(IPOS2+I-1)=FROM(IPOS1+I-1)
         IF (ICMDBG.GT.2)
     *      WRITE (ICMPRU,30) I,IPOS1,IPOS2,
     *         FROM(IPOS1+I-1),
     *         TO(IPOS2+I-1)
10       CONTINUE
C
20    RETURN
C
30    FORMAT (' *** IN UMOVEX : ',
     *  ' I=',I4,3X,'IPOS1=',I3,3X,'IPOS2=',I4,3X,
     *   'FROM(IPOS1+I-1)=',A,3X,
     *   'TO(IPOS2+I-1)=',A)
C
      END
