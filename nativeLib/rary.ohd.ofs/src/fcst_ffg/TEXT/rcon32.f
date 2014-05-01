C MEMBER RCON32
C  (from old member FCEX32)
C VERSION 1.10
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/08/95.14:08:22 BY $WC20SV
C
C######################################################################
C#######################################################################
C   PGM:  RCON32                VERSION 1.01
C#######################################################################
C @PROCESS LVL(77)
      SUBROUTINE RCON32(PO,CO,AIADJ,IVOPT,IFRZE,LWE,LSC,IPRINT,ITWE,
     +                  ITSC,LTA,ITA,TAVG)
C
C.......................................................................
C     THIS ROUTINE RETRIEVES THE NECESSARY DATA FOR THE 'API-CONT
C     MODEL TO RUN IN THE FFG OPERATION
C.......................................................................
C     INITIALLY WRITTEN BY -- JANICE LEWIS, HRL, NOV 1991
C              (EXTRACTED FROM ROUTINE FCEX24)
C     UPDATED FOR OPERATIONAL USE
C             TIM SWEENEY, HYDROLOGIC RESEARCH LAB        JAN 1995
c     Updated cb RSPM24 to match cb in ex24 by adding APIKS
c             Tim Sweeney,   HRL                          Nov 1995
C.......................................................................
      DIMENSION PO(*),CO(*),SNAME(2)
C.......................................................................
      COMMON/FSACPR/IPRSAC,NOFRZE
      COMMON/RSPM24/APIK,AIXW,AIXD,CW,CD,SMIX,CS,FRSX,APIX,PEX,PEN,
     -EFC,PIMPV,RIVA,RVAI,WKW,WKD,AEIK,AEIX,AEIN,ATIR,ATIX,ATIN,APIKS
      COMMON/RGPM24/BFPK,BFIK,BFIM,AICR,CG
      COMMON/FIPM24/CSOIL,CSNOW,GHC,FICR,CF,CP,CT,EFA,ITTA
      COMMON/RSCO24/API,SMI,AEI,ATI,FI,FEI,Y,PED
      COMMON/RGCO24/BFSC,BFI
      COMMON/SUMS24/SP,SR,SIMP,SRS,SRG
      COMMON/CAPIIN/API1,SMI1,BFSC1,BFI1,AEI1,ATI1,FI1,FEI1
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/rcon32.f,v $
     . $',                                                             '
     .$Id: rcon32.f,v 1.2 1995/11/01 20:24:30 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
      DATA SNAME/4HRCON,4H32  /
C.......................................................................
C
C***************************************************************
C     CHECK DEBUG AND TRACE CONTROL --  TRACE LEVEL = 1
C***************************************************************
C
      CALL FPRBUG(SNAME,1,32,IBUG)
C
C************************************************
C     GET VALUES OF CONTROL VARIABLES
C************************************************
C
      VERS   = PO(1)
      IVOPT  = PO(14)
      LPE    = 0
      LSC    = PO(16)
      IF (LSC.GT.0) ITSC=PO(LSC+3)
      LWE    = PO(17)
      IF (LWE.GT.0) ITWE=PO(LWE+3)
      LFRZE  = PO(24)
      LTA    = PO(18)
      IF (LTA.GT.0) ITTA=PO(LTA+3)
      ITA    = ITTA
      IPRINT = 0
      LRSPM  = PO(26)
      LRGPM  = PO(27)
      LPR    = PO(28)
      LSUMS  = PO(32)
      NRSPM  = 16
      LCR    = 7
      IF (LFRZE.GT.0) THEN
        ITFI = PO(LFRZE+3)
        ITFEI = PO(LFRZE+7)
        IFRZE = 1
      ELSE
        IFRZE = 0
      ENDIF
      IF (NOFRZE.EQ.1) IFRZE = 0
      AIADJ = PO(25)
C
C****************************************************
C     DEBUG OUTPUT - PRINT THE PR AND CO ARRAYS
C****************************************************
C
      IF (IBUG.EQ.0) GO TO 110
C
      WRITE(IODBUG,900) LPR,LCR
  900 FORMAT(1H0,'CONTENTS OF THE PR AND C ARRAYS FOR API-CONT',
     -5X,'NUMBER OF VALUES--PR=',I3,2X,'CO=',I3)
C
      WRITE(IODBUG,901) (PO(I),I=1,LPR)
  901 FORMAT(1H0,15F8.3)
      WRITE(IODBUG,901) (CO(I),I=1,LCR)
      WRITE(IODBUG,920)
  920 FORMAT(1H0,'CONTROL VARIABLES, POINTERS, AND TIME INTERVALS')
      WRITE(IODBUG,921) IVOPT,IPRINT,IFRZE
  921 FORMAT(1H0,12I10)
      WRITE(IODBUG,921) LSC,ITSC,LWE,ITWE,LTA,ITTA
      WRITE(IODBUG,921) LFRZE,ITFI,ITFEI
C
C********************************
C     GET PARAMETER VALUES
C     SURFACE RUNOFF
C********************************
C
  110 PXADJ = PO(LRSPM)
      APIK  = PO(LRSPM+1)
      AIXW  = PO(LRSPM+2)
      AIXD  = PO(LRSPM+3)
      CW    = PO(LRSPM+4)
      CD    = PO(LRSPM+5)
      SMIX  = PO(LRSPM+6)
      CS    = PO(LRSPM+7)
      FRSX  = PO(LRSPM+8)
      APIX  = PO(LRSPM+9)
      PEX   = PO(LRSPM+10)
      PEN   = PO(LRSPM+11)
      EFC   = PO(LRSPM+12)
      PIMPV = PO(LRSPM+13)
      RIVA  = PO(LRSPM+14)
      RVAI  = PO(LRSPM+15)
      IF(VERS.GT.1.01) THEN
        APIKS = PO(LRSPM+16)
        NRSPM = NRSPM + 1
      ENDIF

C
      IF (IVOPT.NE.0) GO TO 111
      WKW = PO(LRSPM+NRSPM)
      WKD = PO(LRSPM+NRSPM+1)
      GO TO 115
C
  111 IF (IVOPT.NE.1) GO TO 112
      AEIK = PO(LRSPM+NRSPM)
      AEIX = PO(LRSPM+NRSPM+1)
      AEIN = PO(LRSPM+NRSPM+2)
      GO TO 115
C
  112 ATIR = PO(LRSPM+NRSPM)
      ATIX = PO(LRSPM+NRSPM+1)
      ATIN = PO(LRSPM+NRSPM+2)
C
C*********************************************
C      GROUND WATER RUNOFF PARAMETERS
C*********************************************
C
  115 BFPK = PO(LRGPM)
      BFIK = PO(LRGPM+1)
      BFIM = PO(LRGPM+2)
      AICR = PO(LRGPM+3)
      CG   = PO(LRGPM+4)
C
C*********************************************
C     FROZEN GROUND PARAMETERS IF NEEDED
C*********************************************
C
      IF (IFRZE.EQ.0) GO TO 120
      FICR = PO(LFRZE+8)
      CF   = PO(LFRZE+9)
      CP   = PO(LFRZE+10)
      CSOIL= PO(LFRZE+11)
      CSNOW= PO(LFRZE+12)
      GHC  = PO(LFRZE+13)
      CT   = PO(LFRZE+14)
      EFA  = PO(LFRZE+15)
C
C*******************************
C    GET CARRYOVER VALUES
C*******************************
C
  120 API  = CO(1)
      SMI  = CO(2)
      BFSC = CO(3)
      BFI  = CO(4)
      IF (IVOPT.EQ.1) AEI  = CO(5)
      IF (IVOPT.EQ.2) ATI  = CO(5)
      IF (IVOPT.EQ.2) TAVG = CO(5)
      IF (LFRZE.EQ.0) GO TO 130
      FI  = CO(6)
      FEI = CO(7)
      IF (IFRZE.EQ.0) GO TO 130
      GO TO 140
  130 FI  = 32.0
      FEI = 0.0
C
C********************************
C    STORE INITIAL CARRYOVER
C********************************
C
  140 API1  = API
      SMI1  = SMI
      BFSC1 = BFSC
      BFI1  = BFI
      AEI1  = AEI
      ATI1  = ATI
      FI1   = FI
      FEI1  = FEI
C
C***************************************
C    INITIALIZE WATER BALANCE SUMS
C***************************************
C
      SP   = 0.0
      SR   = 0.0
      SIMP = 0.0
      SRS  = 0.0
      SRG  = 0.0
C
C**********************
C** EXIT ROUTINE  **
C**********************
C
  390 IF (ITRACE.LT.1) GO TO 399
C
      WRITE (IODBUG,905) SNAME
  905 FORMAT(1H0,'**EXIT',1X,2A4)
C
  399 RETURN
      END
