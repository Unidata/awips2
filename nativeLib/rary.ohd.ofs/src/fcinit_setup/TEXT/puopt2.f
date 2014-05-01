C MODULE PUOPT2
C
      SUBROUTINE PUOPT2 (P,MP,C,MC,T,MT,TS,MTS,NUMOP,IERR)
C.......................................
C  THIS ROUTINE CALLS THE PUNCH CARD ROUTINES FOR
C  OPERATIONS 20 THRU 40.
C.......................................
C  ROUTINE WRITTEN BY...
C     ERIC ANDERSON - HRL  APRIL 1981
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/puopt2.f,v $
     . $',                                                             '
     .$Id: puopt2.f,v 1.4 1998/07/02 19:34:52 page Exp $
     . $' /
C    ===================================================================
C
C.......................................
C  TRACE LEVEL=1
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** PUOPT2 ENTERED)
C.......................................
C  CHECK THAT A CALL TO THE OPERATION IS INCLUDED.
C
      IF((NUMOP.LT.20).OR.(NUMOP.GT.40)) GO TO 190
C.......................................
C  GO TO THE SECTION FOR THE CURRENT OPERATION.
C
      NUM=NUMOP-19
      GO TO  (200,201,202,203,204,205,206,207,190,209,
     1        210,211,212,213,214,215,216,217,218,219,220),NUM
C.......................................
C  CHANGE TIME INTERVAL OPERATION.
C
  200 CALL PUC20(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  DWOPER OPERATION
C
  201 LPMM1=LPM-1
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
      CALL PUC21(P(LPM),C(LCO),P(LKD),P(LKU),P(LNB),P(LNCM),
     1 P(LNCSSS),P(LNCSS1),P(LNJUN),P(LNNYQ),P(LNQL),P(LNRCM1),
     2 P(LNRT1),P(LNT),P(LNWJ),P(LNUMLA),P(LNWJX),P(LLAD),
     3 P(LLQ),P(LNSTR),P(LNST),P(LNDIV),P(LLDIV),P(LKTYPE),C(LIWTI),
     4 P(LNQSL))
      GO TO 99
C.......................................
C  HFS OPERATION.
C
  202 CALL PUC22(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  STAGE-DISCHARGE CONVERSION.
C
  203 CALL PUC23(P(LPM),C(LCO))
      GO TO 99
C.......................................
C   API-CONT OPERATION
C
  204 CALL PUC24(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  PLOT-TUL OPERATION
C
 205  CALL PUC25(P(LPM))
      GO TO 99
C.......................................
C  SINGLE RESERVOIR SIMULATION OPERATION.
C
  206 CALL PUC26(P(LPM),MC,LCO,C(LCO))
      GO TO 99
C.......................................
C  FORT WORTH TABULAR DISPLAY
C
 207  CALL PUC27(P(LPM))
      GO TO 99
C.......................................
C  KANSAS CITY API OPERATION
C
 209  CALL PUC29(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  MERGE TIME SERIES OPERATION
C
 210  CALL PUC30(P(LPM))
      GO TO 99
C.......................................
C  HYDRO-43 SNOW MODEL
C
 211  CALL PUC31(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  FLASH FLOOD GUIDANCE OPERATION
C
  212 CALL PUC32(P(LPM))
      GO TO 99
C.......................................
C  CINCINNATI API OPERATION
C
  213 CALL PUC33(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  SALT LAKE CITY API OPERATION
C
  214 CALL PUC34(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  HARRISBURG RFC API OPERATION
C
  215 CALL PUC35(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     XINANJIANG MODEL
C
  216 CALL PUC36(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  MINNEAPOLIS RUNOFF TABULATION
C
  217 CALL PUC37(P(LPM))
      GO TO 99
C.......................................
C  BASEFLOW OPERATION
C
 218  CALL PUC38(P(LPM),C(LCO))
      GO TO 99
C.......................................
C  TABLE LOOKUP -- FT. WORTH
C
 219  CALL PUC39(P(LPM))
      GO TO 99
C.......................................
C   WATER BALANCE OPERATION
C
 220  CALL PUC40(P(LPM))
      GO TO 99
C......................................
C  OPERATION NOT INCLUDED.
C
  190 IERR=1
   99 CONTINUE
      RETURN
      END
