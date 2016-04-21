C MEMBER UMEMST
C-----------------------------------------------------------------------
C
      SUBROUTINE UMEMST (IVALUE,IARRAY,NUM)
C
C  INITIALIZE VARIABLE WITH SPECIFIED VALUE
C
      DIMENSION IARRAY(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/umemst.f,v $
     . $',                                                             '
     .$Id: umemst.f,v 1.1 1995/09/17 19:02:16 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (NUM.LE.0) GO TO 20
C
      DO 10 I=1,NUM
         IARRAY(I)=IVALUE
10       CONTINUE
C
20    RETURN
C
      END
