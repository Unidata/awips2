C MEMBER PROPT1
C  (from old member FCPROPT1)
C
      SUBROUTINE PROPT1(P,MP,C,MC,T,MT,TS,MTS,IOPT,NUMOP,IERR)
C.......................................
C     THIS SUBROUTINE CALLS THE PRINT PARAMETER AND PRINT CARRYOVER
C        SUBROUTINES FOR OPERATIONS 1-19.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MARCH 1980
C.......................................
      DIMENSION P(MP),C(MC),TS(MTS)
      INTEGER T(MT)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCOPPT/LOCT,LPM,LCO
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/propt1.f,v $
     . $',                                                             '
     .$Id: propt1.f,v 1.1 1995/09/17 18:49:53 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** PROPT1 ENTERED)
C.......................................
C     CHECK TO MAKE SURE THAT A CALL TO THE OPERATION IS INCLUDED.
      IF((NUMOP.LT.1).OR.(NUMOP.GT.19)) GO TO 80
C.......................................
C     GO TO THE SECTION FOR THE CURRENT OPERATION.
      GO TO (101,102,103,104,105,106,107,108,109,110,111,112,113,114,
     1115,116,117,118,119),NUMOP
C.......................................
C     SACRAMENTO SOIL-MOISTURE ACCOUNTING OPERATION.
  101 IF(IOPT.GT.0) CALL PRP1(P(LPM))
      IF(IOPT.LT.2) CALL PRC1(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     UNIT HYDROGRAPH OPERATION.
  102 IF(IOPT.GT.0) CALL PRP2(P(LPM))
      IF(IOPT.LT.2) CALL PRC2(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     REDUCED ORDINATE UHG OPERATION
  103 IF(IOPT.GT.0) CALL PRP3(P(LPM))
      IF(IOPT.LT.2) CALL PRC3(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     CLEAR TIME SERIES OPERATION
  104 LTS=T(LOCT+2)
      CALL PRP4(TS(LTS))
      GO TO 99
C.......................................
C     DAILY FLOW PLOT (SAC-PLOT) OPERATION.
  105 CALL PRP5(P(LPM))
      GO TO 99
C.......................................
C     MEAN DISCHARGE (MEAN-Q) OPERATION.
  106 IF(IOPT.GT.0) CALL PRP6(P(LPM))
      IF(IOPT.LT.2) CALL PRC6(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     LAG AND K OPERATION
  107 IF(IOPT.GT.0) CALL PRP7(P(LPM))
      IF(IOPT.LT.2) CALL PRC7(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     CHANNEL LOSS OPERATION
  108 IF (IOPT.GT.0) CALL PRP8(P(LPM))
      IF (IOPT.GT.1) GO TO 99
      WSAREA=P(LPM+10)
      IF (WSAREA.EQ.0.0) GO TO 99
      NEEDC=P(LPM+11)
      IF (NEEDC.EQ.1) CALL PRC8(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     MUSKINGUM ROUTING OPERATION
  109 IF(IOPT.GT.0) CALL PRP9(P(LPM))
      IF(IOPT.LT.2) CALL PRC9(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     ADD/SUBTRACT OPERATION
  110 IF(IOPT.GT.0) CALL PRP10(P(LPM))
      IF(IOPT.GT.1) GO TO 99
      NEEDC=P(LPM+10)
      IF(NEEDC.EQ.1) CALL PRC10(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     LAYERED COEFFICIENT ROUTING
  111 IF(IOPT.GT.0) CALL PRP11(P(LPM))
      IF(IOPT.LT.2) CALL PRC11(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     INSTANTANEOUS DISCHARGE PLOT (INSQPLOT) OPERATION
  112 CALL PRP12(P(LPM))
      GO TO 99
C.......................................
C     TATUM ROUTING OPERATION
  113 IF(IOPT.GT.0) CALL PRP13(P(LPM))
      IF(IOPT.LT.2) CALL PRC13(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     ADJUST SIM. Q OPERATION
  114 IF (IOPT.GT.0) CALL PRP14(P(LPM))
      IF (IOPT.LT.2) CALL PRC14(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     WEIGHT TIME SERIES
  115 CALL PRP15(P(LPM))
      GO TO 99
C.......................................
C     DAILY DISCHARGE STATISTICS OPERATION.
  116 CALL PRP16(P(LPM))
      GO TO 99
C.......................................
C     WATER YEAR PLOT OPERATION
  117 IF(IOPT.GT.0) CALL PRP17(P(LPM))
      GO TO 99
C.......................................
C     PLOT TIME SERIES OPERATION
  118 CALL PRP18(P(LPM))
      GO TO 99
C.......................................
C     SNOW MODEL OPERATION
  119 IF(IOPT.GT.0) CALL PRP19(P(LPM))
      IF(IOPT.LT.2) CALL PRC19(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     OPERATION NOT INCLUDED.
   80 IERR=1
C.......................................
   99 CONTINUE
      RETURN
      END
