      SUBROUTINE INTERP55(T1,NK,TX,IT1,IT2,TINP)
C
C      THIS SUBROUTINE INTERPOLATES BETWEEN TIME STEPS
C
      INCLUDE 'common/fdbug'

      DIMENSION T1(*),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/interp55.f,v $
     . $',                                                             '
     .$Id: interp55.f,v 1.1 1999/04/23 18:08:39 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HINTE,4HRP55/
C
      CALL FPRBUG(SNAME,1,55,IBUG)

      IF(TX.GT.T1(1)) GO TO 210
      IT1=1
      IT2=2
      GO TO 230
  210 DO 220 IT2=2,NK
      IT1=IT2-1
      IF(T1(IT2).GE.TX) GO TO 230
  220 CONTINUE
      IT2=NK
      IT1=IT2-1
  230 TINP=(TX-T1(IT1))/(T1(IT2)-T1(IT1))
      RETURN
      END
