C MEMBER LOWQ21
C  (from old member FCLOWQ21)
C
      SUBROUTINE LOWQ21(ST1,LOST1,STM,KU,JN,NU)
     1
C
C           THIS SUBROUTINE CHECKS TO SEE IF THE INITAL CONDITIONS FALL
C           BELOW THE MINIMUM CONDITIONS SPECIFIED.
C
C           THIS SUBROUTINE WAS WRITTEN BY:
C           JANICE LEWIS      HRL   NOVEMBER,1982     VERSION NO. 1
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
      DIMENSION ST1(1),LOST1(1),STM(1),KU(1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/lowq21.f,v $
     . $',                                                             '
     .$Id: lowq21.f,v 1.2 2000/03/14 11:21:44 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HLOWQ,4H21  /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)
      IERR=0
C
      DO 100 J=1,JN
      MINX=0
C
C        FIND THE STARTING LOCATIONS OF EACH HYDROGRAPH ARRAY
C
      LOS1=LOST1(J)-1
C
C        CHECK TO SEE IF INFLOW HYDROGRAPH POINTS FELL BELOW THE
C                      SPECIFIED MINIMUM VALUE.
C
      SM=STM(J)
      IF(KU(J).EQ.2) SM=SM/1000.
C
      DO 50 L=1,NU
      S1=ST1(LOS1+L)
C
      IF(KU(J).EQ.2) S1=ABS(S1)/1000.
      IF(S1.LT.SM) MINX=MINX+1
   50 CONTINUE
C
      IF(MINX.GT.0) WRITE(IPR,60) J
   60 FORMAT(1H0//25H**WARNING**  ON RIVER NO.,I2,50H THE INFLOW HYDROGR
     *APH AT LOW FLOW CONDITIONS WENT/1H ,12X,63HBELOW THE MINIMUM VALUE
     *S SPECIFIED -- MINIMUM VALUES WERE USED.//)
      IF(MINX.GT.0) CALL WARN
  100 CONTINUE
C
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,2H**,1X,2A4,8H EXITED.)
      RETURN
      END
