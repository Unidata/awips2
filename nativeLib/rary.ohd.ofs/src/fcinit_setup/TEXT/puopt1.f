C MODULE PUOPT1
C
      SUBROUTINE PUOPT1 (P,MP,C,MC,T,MT,TS,MTS,NUMOP,IERR)
C.......................................
C     THIS ROUTINE CALLS THE PUNCH CARD ROUTINES FOR
C     OPERATIONS 1 THRU 19.
C.......................................
C     SUBROUTINE WRITTEN BY...
C        ERIC ANDERSON - HRL  DECEMBER 1980
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/puopt1.f,v $
     . $',                                                             '
     .$Id: puopt1.f,v 1.2 1998/07/02 19:34:25 page Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL=1
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** PUOPT1 ENTERED)
C.......................................
C     CHECK THAT A CALL TO THE OPERATION IS INCLUDED.
      IF((NUMOP.LT.1).OR.(NUMOP.GT.19)) GO TO 190
C.......................................
C     GO TO THE SECTION FOR THE CURRENT OPERATION.
      GO TO(101,102,103,104,190,106,107,108,109,110,111,112,113,114,
     1 115,190,190,118,119),NUMOP
C.......................................
C     SAC-SMA OPERATION.
  101 CALL PUC1(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     UNIT-HG OPERATION
  102 CALL PUC2(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     REDO-UHG OPERATION
  103 CALL PUC3(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     CLEAR-TS OPERATION
  104 LTS=T(LOCT+2)
      CALL PUC4(TS(LTS))
      GO TO 99
C.......................................
C     MEAN-Q OPERATION
  106 CALL PUC6(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     LAG/K ROUTING
  107 CALL PUC7(P(LPM),MC,LCO,C(LCO))
      GO TO 99
C.......................................
C      CHANLOSS OPERATION.
  108 CALL PUC8(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     MUSKINGUM ROUTING
  109 CALL PUC9(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     ADD/SUB OPERATION
  110 CALL PUC10(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     LAYERED COEFFICIENT ROUTING
  111 CALL PUC11(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     INSQPLOT OPERATION
  112 CALL PUC12(P(LPM))
      GO TO 99
C.......................................
C     TATUM ROUTING
  113 CALL PUC13(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     ADJUST-Q OPERATION
  114 CALL PUC14(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     WEIGH-TS OPERATION
  115 CALL PUC15(P(LPM))
      GO TO 99
C.......................................
C     PLOT-TS OPERATION
  118 CALL PUC18(P(LPM))
      GO TO 99
C.......................................
C     HYDRO-17 SNOW MODEL
  119 CALL PUC19(P(LPM),C(LCO))
      GO TO 99
C.......................................
C     OPERATION NOT INCLUDED.
  190 IERR=1
   99 CONTINUE
      RETURN
      END
