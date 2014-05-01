C MEMBER CHCO24
C  (from old member FCEX24)
C
      SUBROUTINE CHCO24(KDA,KHR,NOUTZ,NOUTDS,N,IFRZE,IPRINT,IOUT,
     1  APIX,SMIX,IBUG)
C**************************************************************
C     CHANGES API-CONT CARRYOVER BASED ON VALUES INPUT THROUGH
C       THE APICBASF AND APICCO MODS.
C**************************************************************
C     WRITTEN BY -- ERIC ANDERSON -- HRL -- OCT. 1992
C**************************************************************
C     COMMON BLOCKS
      COMMON/FCOAPI/NACV,JHAPI(10),APICO(4,10),BFX(10),FGCO(2,10)
      COMMON/RSCO24/API,SMI,AEI,ATI,FI,FEI,Y,PED
      COMMON/RGCO24/BFSC,BFI
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apicont/RCS/chco24.f,v $
     . $',                                                             '
     .$Id: chco24.f,v 1.1 1995/09/17 18:56:29 dws Exp $
     . $' /
C    ===================================================================
C
C**************************************************************
C     PRINT DEBUG INFORMATION
      IF (IBUG.GT.0) WRITE(IOUT,903) N,IPRINT,IFRZE
  903 FORMAT(1H0,'CHCO24 DEBUG VALUES',3I10)
C**************************************************************
C     STORE INITIAL VALUES
      API1=API
      SMI1=SMI
      BFSC1=BFSC
      BFI1=BFI
      FI1=FI
      FEI1=FEI
C**************************************************************
C     MAKE MOD CHANGES
      IF (APICO(1,N).GE.0.0) API=APICO(1,N)
      IF (APICO(2,N).GE.0.0) SMI=APICO(2,N)
      IF ((APICO(3,N).GE.0.0).OR.(APICO(4,N).GE.0.0)) GO TO 100
      IF (BFX(N).LT.0.0) GO TO 110
      BFSC=BFX(N)*BFSC
      GO TO 110
  100 IF (APICO(3,N).GE.0.0) BFSC=APICO(3,N)
      IF (APICO(4,N).GE.0.0) BFI=APICO(4,N)
  110 IF (IFRZE.EQ.0) GO TO 120
      IF (FGCO(1,N).GE.-450.) FI=FGCO(1,N)
      IF (FGCO(2,N).GE.0.0) FEI=FGCO(2,N)
C**************************************************************
C     CHECK UPPER LIMITS OF API AND SMI
  120 IF (API.GT.APIX) API=APIX
      IF (SMI.GT.SMIX) SMI=SMIX
C**************************************************************
C     PRINT CHANGES IF REQUESTED
      IF (IPRINT.EQ.0) RETURN
      CALL MDYH1(KDA,KHR,MO,ID,IY,IH,NOUTZ,NOUTDS,TZ)
      WRITE(IOUT,900) MO,ID,IY,IH
  900 FORMAT(1H ,'STATE VARIABLES CHANGED AS OF',I3,'/',I2,'/',I4,
     1  1X,'HOUR',I3,2X,'--',3X,'API(IN)',3X,'SMI(IN)',2X,
     2  'BFSC(IN)',3X,'BFI(IN)',2X,'FI(DEGF)',7X,'FEI')
      WRITE(IOUT,901) API1,SMI1,BFSC1,BFI1,FI1,FEI1
  901 FORMAT(1H ,38X,'INITIAL VALUES',4F10.2,F10.1,F10.2)
      WRITE(IOUT,902) API,SMI,BFSC,BFI,FI,FEI
  902 FORMAT(1H ,38X,'REVISED VALUES',4F10.2,F10.1,F10.2)
      RETURN
      END
