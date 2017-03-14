C MEMBER UMEMO2
C-----------------------------------------------------------------------
C
      SUBROUTINE UMEMO2 (IN,IOUT,NUM)
C
C  MOVE NUM INTEGER*2 WORDS STARTING WITH IN(1) INTO OUT(1)
C
      INTEGER*2 IN(NUM),IOUT(NUM)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/umemo2.f,v $
     . $',                                                             '
     .$Id: umemo2.f,v 1.1 1995/09/17 19:02:14 dws Exp $
     . $' /
C    ===================================================================
C
C
      DO 10 I=1,NUM
         IOUT(I)=IN(I)
10       CONTINUE
C
      RETURN
C
      END
