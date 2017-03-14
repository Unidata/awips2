C MEMBER ELST26
C  (from old member FCELST26)
C
C DESC CHECK THAT ARRAY OF ELEVS ARE WITHIN ELEV VS STORAGE CURVE
C-------------------------------------------------------------------
      SUBROUTINE ELST26(VALUE,NVAL,IER)
C-------------------------------------------------------------------
C  ROUTINE TO CHECK THAT VALUES IN AN ARRAY (ARRAY OF ELEVATIONS) ALL
C  FALL WITHIN DEFINED LIMITS OF THE ELEVATION VS. STORAGE ARRAY.
C----------------------------------------------------------------------
C  JTOSTROWSKI - HRL - MARCH 1983
C----------------------------------------------------------------------
C
      INCLUDE 'common/comn26'
      DIMENSION VALUE(1),SEL(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/elst26.f,v $
     . $',                                                             '
     .$Id: elst26.f,v 1.1 1995/09/17 18:51:30 dws Exp $
     . $' /
C    ===================================================================
C
      DATA SEL/4HSTOR,4HAGE /
C
      IER = 1
C
C  SEE IF ELEVATIONS ARE WITHIN BOUNDS OF ELEV VS. STORAGE CURVE
C  IF CURVE WAS NOT DEFINED, CALL IT AN ERROR
C
      IF (NSE.GT.0) GO TO 10
C
      CALL STRN26(65,1,SEL,2)
      GO TO 9999
C
   10 CONTINUE
      DO 30 I=1,NVAL
C
           VAL = VALUE(I)
           IF (ABS(VAL+999).LT.0.0001) GO TO 30
C
      DO 20 J=2,NSE
           IF (ELSTOR(J-1).LE.VAL.AND.VAL.LE.ELSTOR(J)) GO TO 30
   20 CONTINUE
C
      CALL STER26(63,1)
      GO TO 9999
C
   30 CONTINUE
      IER = 0
C
 9999 CONTINUE
      RETURN
      END
