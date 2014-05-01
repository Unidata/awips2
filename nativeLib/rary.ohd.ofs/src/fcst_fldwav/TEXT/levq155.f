      SUBROUTINE LEVQ155(WM1,WT1,WM2,WT2,HWE,WCC,DX,BB,SNFM,SNTO,QL11,
     . SUB)
C
C      THIS SUBROUTINE COMPUTES LEVEE FLOW
C
      COMMON/LEV55/NLEV,DHLV,NPOND,DTHLV,IDTHLV

      INCLUDE 'common/fdbug'

      DIMENSION SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/levq155.f,v $
     . $',                                                             '
     .$Id: levq155.f,v 1.1 1999/04/23 18:08:40 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HLEVQ,4H155 /
C
      CALL FPRBUG(SNAME,1,55,IBUG)
C
      QL11=0.00
      HM1=WM1-HWE
      HT1=WT1-HWE
      IF(ABS(WM1-WT1).LE.DHLV) GO TO 600
      IDTHLV=1
      SUB=1.00
      DX11=0.5*(DX-BB)
      DX12=DX11+BB
      IF(WM1.LT.WT1)GO TO 400
C
C  FLOW FROM MAIN RIVER TO TRIBUTARY
      SNFM=-1.0
      SNTO=1.0
      DHA=ABS(WM1-WM2)
C
C  WATER ELEV. ABOVE LEVEE AT BOTH U/S AND D/N OF THE REACH
      IF(DHA.LE.0.0) THEN
        DX1=DX-BB
        HM=HM1
        GO TO 350
      END IF
C
C  WATER ELEV BELOW LEVEE AT EITHER U/S OR D/N OF THE REACH
C  USE AVERAGE HEAD ABOVE LEVEE
      DX1=DX*(WM1-HWE)/(WM1-WM2)
      DXS=DX1
      IF(DXS.GE.DX11) DX1=DX11
      IF(DXS.GE.DX12) DX1=DXS-BB
      HM=0.5*HM1
  350 IF(HT1.GT.0.00) THEN
        IF(HT1.GT.0.67*HM1)SUB=1.0-27.8*(HT1/HM1-0.67)**3
      END IF
      QL11=SUB*WCC*DX1*HM**1.5
      GO TO 600
  400 CONTINUE
C
C  FLOW FROM TRIBUTARY TO MAIN RIVER
      SNFM=1.0
      SNTO=-1.0
      DX1=DX
      DHA=ABS(WT1-WT2)
C
C  WATER ELEV. ABOVE LEVEE AT BOTH U/S AND D/N OF THE REACH
      IF(DHA.LE.0.0) THEN
        DX1=DX-BB
        HT=HT1
        GO TO 450
      END IF
C
C  WATER ELEV BELOW LEVEE AT EITHER U/S OR D/N OF THE REACH
C  USE AVERAGE HEAD ABOVE LEVEE
      DX1=DX*(WT1-HWE)/(WT1-WT2)
      DXS=DX1
      IF(DXS.GE.DX11) DX1=DX11
      IF(DXS.GE.DX12) DX1=DXS-BB
      HT=0.5*HT1
  450 IF(HM1.GT.0.00) THEN
        IF(HM1.GT.0.67*HT1)SUB=1.0-27.8*(HM1/HT1-0.67)**3
      END IF
      QL11=SUB*WCC*DX1*HT**1.5
  600 CONTINUE
      RETURN
      END
