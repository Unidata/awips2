C MEMBER UMOVE
C-----------------------------------------------------------------------
C
      SUBROUTINE UMOVE (FROM,TO,NPOS)
C
C  MOVE NPOS CHARACTERS STARTING WITH FROM(1) INTO TO(1)
C
      CHARACTER*1 FROM(1),TO(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/umove.f,v $
     . $',                                                             '
     .$Id: umove.f,v 1.1 1995/09/17 19:02:17 dws Exp $
     . $' /
C    ===================================================================
C
C
      DO 10 I=1,NPOS
         TO(I)=FROM(I)
10       CONTINUE
C
      RETURN
C
      END
