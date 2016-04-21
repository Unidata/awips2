C MEMBER PROPT2
C  (from old member FCPROPT2)
C
      SUBROUTINE PROPT2(P,MP,C,MC,T,MT,TS,MTS,IOPT,NUMOP,IERR)
C                             LAST UPDATE: 02/14/94.14:40:03 BY $WC30JL
C
C.......................................
C  THIS SUBROUTINE CALLS THE PRINT PARAMETER AND PRINT CARRYOVER
C
C     SUBROUTINES FOR OPERATIONS 20-40.
C.......................................
C  SUBROUTINE INITIALLY WRITTEN BY...
C
C     ERIC ANDERSON - HRL   APRIL 1981
C.......................................
      DIMENSION P(MP),C(MC),TS(MTS)
C
      INTEGER T(MT)
C
C  COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FCOPPT/LOCT,LPM,LCO
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/propt2.f,v $
     . $',                                                             '
     .$Id: propt2.f,v 1.3 1997/12/31 18:16:37 page Exp $
     . $' /
C    ===================================================================
C
C.......................................
C  TRACE LEVEL FOR THIS SUBROUTINE=1
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** PROPT2 ENTERED)
C.......................................
C  CHECK TO MAKE SURE THAT A CALL TO THE OPERATION IS INCLUDED.
C
      IF((NUMOP.LT.20).OR.(NUMOP.GT.40)) GO TO 80
C.......................................
C  GO TO THE SECTION FOR THE CURRENT OPERATION.
C
      NUM=NUMOP-19
      GO TO (100,101,102,103,104,105,106,107,108,109,
     1       110,111,112,113,114,115,116,117,118,119,120),NUM
C.......................................
C  CHANGE TIME INTERVAL OPERATION.
C
  100 IF(IOPT.GT.0) CALL PRP20(P(LPM))
      IF(IOPT.GT.1) GO TO 99
      NEEDC=P(LPM+9)
      IF(NEEDC.EQ.1) CALL PRC20(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  DWOPER OPERATION.
C
  101 LPMM1=LPM-1
      LKD=P(LPMM1+53) + LPMM1
      LKU=P(LPMM1+54) + LPMM1
      LNB=P(LPMM1+56) + LPMM1
      LNCM=P(LPMM1+74) + LPMM1
      LNCSSS=P(LPMM1+75) + LPMM1
      LNCSS1=P(LPMM1+57) + LPMM1
      LNJUN=P(LPMM1+58) + LPMM1
      LNNYQ=P(LPMM1+59) + LPMM1
      LNQL=P(LPMM1+60) + LPMM1
      LNRCM1=P(LPMM1+61) + LPMM1
      LNRT1=P(LPMM1+62) + LPMM1
      LNT=P(LPMM1+76) + LPMM1
      LNWJ=P(LPMM1+64) + LPMM1
      LNUMLA=P(LPMM1+63) + LPMM1
      LNWJX=P(LPMM1+77) + LPMM1
      LLAD=P(LPMM1+73) + LPMM1
      LLQ=P(LPMM1+93) + LPMM1
      LNSTR=P(LPMM1+104) + LPMM1
      LNST=P(LPMM1+107) + LPMM1
      LNDIV=P(LPMM1+109) + LPMM1
      LLDIV=P(LPMM1+110) + LPMM1
      LKTYPE=P(LPMM1+108) + LPMM1
      LIWTI=P(LPMM1+125) + LCO-1
      LNQSL=P(LPMM1+143) + LPMM1

      IF(IOPT.GT.0)CALL PRP21(P(LPM),P(LKD),P(LKU),P(LNB),P(LNCM),
     1 P(LNCSSS),P(LNCSS1),P(LNJUN),P(LNNYQ),P(LNQL),P(LNRCM1),
     2 P(LNRT1),P(LNT),P(LNWJ),P(LNUMLA),P(LNWJX),P(LLAD),
     3 P(LLQ),P(LNSTR),P(LNST),P(LNDIV),P(LLDIV),P(LKTYPE),P(LNQSL))
      IF (IOPT.LT.2) CALL PRC21(P(LPM),C(LCO),P(LNB),P(LNQL),P(LNDIV),
     1 P(LNUMLA),C(LIWTI),P(LLAD))
      GO TO 99
C.......................................
C  HFS OPERATION.
C
  102 IF(IOPT.GT.0) CALL PRP22(P(LPM))
      IF (IOPT.LT.2) CALL PRC22(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  STAGE-DISCHARGE CONVERSION.
C
  103 IF(IOPT.GT.0) CALL PRP23(P(LPM))
      IF (IOPT.LT.2) CALL PRC23(P(LPM),C(LCO))
      GO TO 99
C.......................................
C   API-CONT OPERATION
C
 104  IF (IOPT.GT.0) CALL PRP24(P(LPM))
      IF (IOPT.LT.2) CALL PRC24(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  PLOT-TUL OPERATION
C
 105  CALL PRP25(P(LPM))
      GO TO 99
C.......................................
C  SINGLE RESERVOIR SIMULATION OPERATION.
C
 106  IF(IOPT.GT.0) CALL PRP26(P(LPM))
      IF(IOPT.LT.2) CALL PRC26(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  FORT WORTH TABULAR DISPLAY
C
 107  CALL PRP27(P(LPM))
      GO TO 99
C.......................................
C  CHANNEL LEAK OPERATION.
C
  108 IF(IOPT.GT.0) CALL PRP28(P(LPM))
      IF(IOPT.LT.2) CALL PRC28(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  KANSAS CITY API OPERATION
C
 109  IF(IOPT.GT.0) CALL PRP29(P(LPM))
      IF (IOPT.LT.2) CALL PRC29(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  MERGE TIME SERIES OPERATION
C
 110  CALL PRP30(P(LPM))
      GO TO 99
C.......................................
C  SNOW-43 KALMAN FILTER OPERATION
C
  111 IF (IOPT .GT. 0) CALL PRP31(P(LPM))
      IF (IOPT .LT. 2) CALL PRC31(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  FLASH FLOOD GUIDANCE OPERATION
C
  112 CALL PRP32(P(LPM))
      GO TO 99
C.......................................
C  CINCINNATI API OPERATION
C
  113 IF (IOPT .GT. 0) CALL PRP33(P(LPM))
      IF (IOPT .LT. 2) CALL PRC33(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  SALT LAKE CITY API OPERATION
C
  114 IF (IOPT .GT. 0) CALL PRP34(P(LPM))
      IF (IOPT .LT. 2) CALL PRC34(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  HARRISBURG RFC API OPERATION
C
  115 IF (IOPT .GT. 0) CALL PRP35(P(LPM))
      IF (IOPT .LT. 2) CALL PRC35(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     XINANJINANG MODEL
C
  116 IF(IOPT.GT.0) CALL PRP36(P(LPM))
      IF(IOPT.LT.2) CALL PRC36(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  MINNEAPOLIS RUNOFF TABULATION
C
  117 CALL PRP37(P(LPM))
      GO TO 99
C.......................................
C  BASEFLOW OPERATION
C
 118  IF(IOPT.GT.0)CALL PRP38(P(LPM))
      IF(IOPT.GT.1)GO TO 99
      NEEDC=P(LPM+11)
      IF(NEEDC.GT.0)CALL PRC38(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  TABLE LOOKUP -- FT. WORTH
C
 119  CALL PRP39(P(LPM))
      GO TO 99
C.......................................
C   WATER BALANCE OPERATION
C
 120  IF (IOPT.GT.0) CALL PRP40(P(LPM))
      GO TO 99
C......................................
C  OPERATION NOT INCLUDED.
C
   80 IERR=1
C.......................................
   99 CONTINUE
C
      RETURN
      END
