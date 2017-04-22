C MODULE FCRATC21
C DESC -- THIS SUBROUTINE INTERPOLATES THE INTERNAL BOUNDARY RATING
C DESC -- CURVE TO OBTAIN THE DISCHARGE FOR A GIVEN WSEL
C                             LAST UPDATE: 03/01/94.13:09:21 BY $WC30JL
C
C
C @PROCESS LVL(77)
C
      SUBROUTINE RATC21(YY,QRT,DQRT,NRCP,RY,RX)
C
C           THIS SUBROUTINE WAS WRITTEN BY:
C           JANICE LEWIS      HRL   FEBRUARY,1992     VERSION NO. 2
C
      INCLUDE 'common/fdbug'
C
      DIMENSION RY(*),RX(*),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/ratc21.f,v $
     . $',                                                             '
     .$Id: ratc21.f,v 1.1 1996/01/16 11:29:17 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HRATC,4H21  /
C
      CALL FPRBUG(SNAME,1,21,IBUG)
      HC=RY(1)
      QRT=0.0
      DQRT=0.0
      IF(YY.LE.HC) GO TO 15
      DO 5 KK=2,NRCP
      K=KK
      IF(RY(K).LT.0.001) K=K-1
      IF(K.NE.KK) GO TO 10
      IF(RY(K).LT.0.001) GO TO 10
      IF(YY.LT.RY(K)) GO TO 10
    5 CONTINUE
   10 J1=K-1
      DQRT=(RX(K)-RX(J1))/(RY(K)-RY(J1))
      QRT=RX(J1)+DQRT*(YY-RY(J1))
   15 RETURN
      END
