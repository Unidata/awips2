C     MEMBER OPETCK
C
      SUBROUTINE OPETCK(P,MP,OA,MOA,A,MA,PARM,ILOCOA,MILOC,JB,ETHIGH,
     *ETLOW,NPARM,OPNEW)
C
C.......................................
C     THIS SUBROUTINE CHECKS TO SEE WHICH ET PARAMETER CURRENTLY IS
C     BEING OPTIMIZED AND IF THE OTHER PARAMETER ALSO HAS BEEN
C     INCLUDED FOR OPTIMIZATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL - HRL   MAY 1981   VERSION 1
C.......................................
C
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fdbug'
C
      DIMENSION P(MP),OA(MOA),A(MA),ETH(2),ETL(2),PARM(2),OPNEW(2)
      DIMENSION ILOCOA(MILOC)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/opetck.f,v $
     . $',                                                             '
     .$Id: opetck.f,v 1.3 2002/02/11 13:59:58 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      DATA ETH/4HETHI,4HGH  /
      DATA ETL/4HETLO,4HW   /
      DATA IBUG/4HOPT /
C
      IF(ITRACE.GE.1) WRITE(IODBUG,1000)
 1000 FORMAT(1H0,17H** OPETCK ENTERED)
C
C     CHECK TO SEE WHICH PARAMETER IS BEING OPTIMIZED.
C
      JBP1=JB+1
      IF(PARM(1).EQ.ETH(1).AND.PARM(2).EQ.ETH(2)) GO TO 150
C     ETLOW IS BEING OPTIMIZED.
      ETLOW=A(JB)
      IF(JB.EQ.NPARM) GO TO 121
C
C     CHECK TO SEE IF ETHIGH ALSO IS INCLUDED.
C
      DO 120 J=JBP1,NPARM
      IOPNUM=OA(ILOCOA(J)+1)
cfan      IF(IOPNUM.NE.1.OR.P(OA(ILOCOA(J))-5).NE.OPNEW(1).OR.P(OA(ILOCOA(J)
cfan     *)-4).NE.OPNEW(2)) GO TO 120
cfan
      IIII1=OA(ILOCOA(J))-5
      IIII2=OA(ILOCOA(J))-4
      IF(IOPNUM.NE.1.OR.P(IIII1).NE.OPNEW(1).OR.P(IIII2).NE.OPNEW(2)) 
     * GO TO 120
cfan
      IF(OA(ILOCOA(J)+2).NE.ETH(1).OR.OA(ILOCOA(J)+3).NE.ETH(2)) GO TO
     *120
      JK=J
      GO TO 122
  120 CONTINUE
  121 ETHIGH=1.0
      GO TO 180
  122 ETHIGH=A(JK)
      GO TO 180
C
C     ETHIGH IS BEING OPTIMIZED.
  150 ETHIGH=A(JB)
      IF(JB.EQ.NPARM) GO TO 161
C     CHECK TO SEE IF ETLOW ALSO IS INCLUDED.
C
      DO 160 J=JBP1,NPARM
      IOPNUM=OA(ILOCOA(J)+1)
cfan      IF(IOPNUM.NE.1.OR.P(OA(ILOCOA(J))-5).NE.OPNEW(1).OR.P(OA(ILOCOA(J)
cfan     *)-4).NE.OPNEW(2)) GO TO 160
cfan
      IIII3=OA(ILOCOA(J))-5
      IIII4=OA(ILOCOA(J))-4
      IF(IOPNUM.NE.1.OR.P(IIII3).NE.OPNEW(1).OR.P(IIII4).NE.OPNEW(2)) 
     * GO TO 160
cfan
      IF(OA(ILOCOA(J)+2).NE.ETL(1).OR.OA(ILOCOA(J)+3).NE.ETL(2)) GO TO
     *160
      JK=J
      GO TO 162
  160 CONTINUE
  161 ETLOW=1.0
      GO TO 180
  162 ETLOW=A(JK)
  180 CONTINUE
C
      IF(ITRACE.GE.1) WRITE(IODBUG,1002)
 1002 FORMAT(1H0,14H** EXIT OPETCK)
C
      RETURN
      END
