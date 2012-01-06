C MEMBER TERP21
C  (from old member FCTERP21)
C
      FUNCTION TERP21(T,NK,IT1,IT2,TIM,VAL,LVAL)
C
C      THIS SUBROUTINE INTERPOLATES BETWEEN TIME STEPS
C
C           THIS SUBROUTINE WAS WRITTEN BY:
C           JANICE LEWIS      HRL   NOVEMBER,1982     VERSION NO. 1
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
      DIMENSION TIM(1),VAL(1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/terp21.f,v $
     . $',                                                             '
     .$Id: terp21.f,v 1.1 1995/09/17 18:56:19 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HTERP,4H21  /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)
C
      IT=IT1
      IF(IT.GT.NK.OR.IT.LT.1) IT=1
C
      DO 20 IT1=IT,NK
      IF(T-TIM(IT1)) 30,30,20
   20 CONTINUE
      IT1=NK
   30 IT2=IT1-1
      IF (IT2) 40,40,50
   40 TINP=0.
      IT2=IT1
      GO TO 60
   50 TINP=(T-TIM(IT2))/(TIM(IT1)-TIM(IT2))
   60 TERP21=VAL(IT2+LVAL)+TINP*(VAL(IT1+LVAL)-VAL(IT2+LVAL))
C
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
      RETURN
      END
