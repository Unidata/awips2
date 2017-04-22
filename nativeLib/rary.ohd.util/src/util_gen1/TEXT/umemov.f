C MODULE UMEMOV
C-----------------------------------------------------------------------
C
      SUBROUTINE UMEMOV (IN,IOUT,NUM)
C
C  MOVE NUM CHARACTERS STARTING WITH IN(1) INTO OUT(1)
C
      CHARACTER*4 IN(NUM),IOUT(NUM)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/umemov.f,v $
     . $',                                                             '
     .$Id: umemov.f,v 1.2 1998/04/07 11:15:31 page Exp $
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
