C MEMBER INTP21
C  (from old member FCINTP21)
C
      SUBROUTINE INTP21(NK,TINP,IT1,IT2,T1)
C
C
C           THIS SUBROUTINE WAS WRITTEN ORIGINALLY BY:
C           DR. DANNY FREAD   HRL   APRIL 1978
C
C           THIS SUBROUTINE WAS MODIFIED TO MEET VER. NO. 5 STANDARDS
C           OF THE NWSRFS BY:
C           JANICE LEWIS      HRL   NOVEMBER,1982     VERSION NO. 1
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/M121/N,NU,NS1,JN,JJ,KIT,G,DT,TT,TIMF,F1,GZN,NYQD
C
      DIMENSION T1(1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/intp21.f,v $
     . $',                                                             '
     .$Id: intp21.f,v 1.1 1995/09/17 18:56:05 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HINTP,4H21  /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)
C
      DO 20 IT1=1,NK
      IF(TT-T1(IT1)) 30,30,20
   20 CONTINUE
   30 IT2=IT1-1
      IF (IT2) 40,40,50
   40 TINP=0.
      IT2=IT1
      GO TO 60
   50 TINP=(TT-T1(IT2))/(T1(IT1)-T1(IT2))
   60 CONTINUE
C
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
      RETURN
      END
