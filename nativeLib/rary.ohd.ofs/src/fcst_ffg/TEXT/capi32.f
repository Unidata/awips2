C MEMBER CAPI32
C  (from old member FCEX32)
C VERSION 1.10
C  ====================================================================
C  PGM:    CAPI32    VERSION 1.00
C
C @PROCESS LVL(77)
      SUBROUTINE CAPI32 (P,RS,PFRS,AI,RG,RT,PE,AESC,TMPRR,TAVG,WE,
     +                AIADJ,NXHR,KMO,KDA,KHR,IVOPT,IFRZE,LWE,LSC,IDT)
C
C**********************************************************************
C  THIS ROUTINE COMPUTES THE RUNOFF USING THE API-CONT MODEL
C
C**********************************************************************
C  INITIALLY WRITTEN BY
C     TIM SWEENEY, HYDROLOGIC RESEARCH LAB,       JAN 1995
C**********************************************************************
C  PRINCIPLE VARIABLES ...
C
C**********************************************************************
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      COMMON/RSCO24/API,SMI,AEI,ATI,FI,FEI,Y,PED
      COMMON/RGCO24/BFSC,BFI
      COMMON/CAPIIN/API1,SMI1,BFSC1,BFI1,AEI1,ATI1,FI1,FEI1
C
      DIMENSION SNAME(2),VAR1(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/capi32.f,v $
     . $',                                                             '
     .$Id: capi32.f,v 1.2 1995/11/14 20:28:00 erb Exp $
     . $' /
C    ===================================================================
C
C......................................................................
      DATA SNAME/4HCAPI,4H32  /
      DATA VAR1/4H  WK,4H AEI,4H ATI/
C......................................................................
C
C  TRACE LEVEL = 1,    DEBUG FLAG = IBUG
      VERS = 1.00
      CALL FPRBUG (SNAME,1,32,IBUG)
C
C  PRINT HEADING FOR DETAILED OUTPUT
      IPRINT = 0
      IF (IBUG.EQ.1) IPRINT = 1
      IF (IPRINT.EQ.0) GOTO 100
      IF(IPRINT.EQ.1) IOUT = IODBUG
      WRITE(IOUT,904) VAR1(IVOPT+1)
  904 FORMAT(1H0,'DAY-HR',1X,'PRECIP',1X,'SUR-RO',4X,'API',3X,A4,
     +  6X,'Y',5X,'AI',4X,'SMI',4X,'AIF',5X,'FS',5X,'FG',5X,'GI',
     +  5X,'GS',4X,'BFI',2X,'GW-RO',1X,'TOT-RO',5X,'FI',4X,'FEI')
C......................................................................
C  GET INITIAL CARRYOVER VALUES
C
  100 API  = API1
      SMI  = SMI1
      BFSC = BFSC1
      BFI  = BFI1
      AEI  = AEI1
      ATI  = ATI1
      FI   = FI1
      FEI  = FEI1
C
      CALL APIC24 (P,RS,PFRS,AI,RG,RT,PE,AESC,TMPRR,TAVG,WE,
     +             AIADJ,NXHR,KMO,KDA,KHR,IVOPT,IFRZE,LWE,LSC,
     +             IPRINT,IOUT,IDT,IBUG)
      RETURN
      END
