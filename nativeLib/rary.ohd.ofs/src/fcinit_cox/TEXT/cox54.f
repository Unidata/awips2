C MEMBER COX54
C
      SUBROUTINE COX54(PO,CO,PN,CN)
C.......................................
C     THIS IS THE CARRYOVER TRANSFER SUBROUTINE FOR THE SIMPLE
C        WATER BALANCE MODEL OPERATION.  NEW CARRYOVER (CN)
C        VALUES ARE DETERMINE FROM OLD CARRYOVER (CO) AND
C        OLD AND NEW PARAMETERS (PO AND PN RESPECTIVELY) WHENEVER
C        POSSIBLE.  IF CARRYOVER CAN NOT BE CONVERTED DEFAULT
C        VALUES INITIALLY CONTAINED IN CN ARE KEPT.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        QINGYUN DUAN - GCIP CLIMATE PROJECT  SEPTEMBER 1995  VERSION 1
C.......................................
C
      DIMENSION PO(1),CO(1),PN(1),CN(1)
      REAL DMAX,KG,ALPSM,ALPRT,KDT
      REAL DMAXO,KGO,ALPSMO,ALPRTO,KDTO
C
C***************************************************
C     COMMON BLOCKS.
C***************************************************
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox54.f,v $
     . $',                                                             '
     .$Id: cox54.f,v 1.1 1997/09/22 17:35:46 page Exp $
     . $' /
C    ===================================================================
C
C
C***************************************************
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
C***************************************************
C
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,16H** COX54 ENTERED)
C
C***************************************************
C     CHECK TO SEE IF DEBUG OUTPUT IS NEEDED FOR THIS OPERATION.
C***************************************************
C
      IBUG=0
      IF (IDBALL.GT.0) IBUG=1
      IF (NDEBUG.EQ.0) GO TO 100
      DO 10 I=1,NDEBUG
      IF (IDEBUG(I).EQ.1) GO TO 11
   10 CONTINUE
      GO TO 100
   11 IBUG=1
C***************************************************
C     GET LOCATION OF PARAMETER VALUES
C***************************************************
  100 LPM=PN(14)
      NCN=PO(17)
      NXCN=PN(18)
      IFZN=PN(22)
      LPN=PN(29)
      IF(IFZN.EQ.0) GO TO 101
      LCN=PN(30)
  101 NCO=PO(17)
      NXCO=PO(18)
      IFZO=PO(22)
      LPO=PO(29)
      IF(IFZO.EQ.0) GO TO 102
      LCO=PO(30)
C
C***************************************************
C     DEBUG OUTPUT - NEW AND OLD PARAMETERS AND CARRYOVER.
C***************************************************
C
  102 IF (IBUG.EQ.0) GO TO 110
      WRITE (IODBUG,902)
  902 FORMAT (1H0,'SWB-NILE DEBUG--NEW AND OLD PARS AND CARRYOVER')
      WRITE (IODBUG,901) (PO(I),I=1,LPO)
  901 FORMAT (1H0,15F8.3)
      WRITE (IODBUG,901) (CO(I),I=1,LCO)
      WRITE (IODBUG,901) (PN(I),I=1,LPN)
      WRITE (IODBUG,901) (CN(I),I=1,LCN)
C
C***************************************************
C     GET VALUES NEEDED FOR TRANSFER OF SOIL-MOISTURE VARIABLES.
C***************************************************
C
  110 SUCO=CO(1)
      SBCO=CO(2)
      DMAXO=PO(LPM+2)
      KGO=PO(LPM+3)
      ALPSMO=PO(LPM+4)
      ALPRTO=PO(LPM+5)
      KDTO=PO(LPM+6)
      DMRTO=DMAXO*ALPRTO
C
      DMAX=PN(LPM+2)
      KG=PN(LPM+3)
      ALPSM=PN(LPM+4)
      ALPRT=PN(LPM+5)
      KDT=PN(LPM+6)
      DMRT=DMAX*ALPRT
C
C***************************************************
C     MAKE CARRYOVER TRANSFERS OF SOIL-MOISTURE VARIABLES.
C***************************************************
C
      SU=SUCO
      IF (SU.GT.DMRT) SU=DMRT/DMRTO*SUCO
      IF (SU.LT.0.0) SU=0.0
      SB=SBCO
      IF (SB.GT.DMAX) SU=DMAX/DMAXO*SBCO
      IF (SB.LT.0.0) SB=0.0
C
C***************************************************
C     STORE NEW CARRYOVER VALUES FOR SOIL-MOISTURE VARIABLES.
C***************************************************
C
      CN(1)=SU
      CN(2)=SB
C
C***************************************************
C     FROZEN GROUND CARRYOVER TRANSFER.
C***************************************************
C
      IF(IFZN.EQ.0) GO TO 140
      IF(IFZO.EQ.0) GO TO 140
      DO 120 I=1,NXCN
  120 CN(NCN+I)=CO(NCO+I)
C
C***************************************************
C     DEBUG OUTPUT -- NEW CARRYOVERS.
C***************************************************
C
  140 IF (IBUG.EQ.0) GO TO 199
      WRITE (IODBUG,903)
  903 FORMAT (1H0,18HADJUSTED CARRYOVER)
      WRITE (IODBUG,901) (CN(I),I=1,LCN)
C.......................................
C     CARRYOVER TRANSFER RULES FOR SOIL-MOISTURE VARIABLES.
C
C     1. SU - KEEPS UPPER ZONE SOIL MOISTURE/DMRT RATIO
C             AS LONG AS CONTENTS DO NOT GO BELOW ZERO.
C     2. SB - KEEPS LOWER ZONE SOIL MOISTURE/DMAX RATIO
C             AS LONG AS CONTENTS DO NOT GO BELOW ZERO.
C.......................................
  199 RETURN
      END
