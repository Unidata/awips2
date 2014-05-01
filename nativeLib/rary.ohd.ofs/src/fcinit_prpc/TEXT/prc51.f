C MEMBER FCPRC51
C***********************************************************************
C
C@PROCESS LVL(77)
C
      SUBROUTINE PRC51(PO,CO)
C
C     SUBROUTINE PRINTS THE INFORMATION STORED IN THE P ARRAY FOR THE
C     SSARRESV RESERVOIR OPERATION.
C
C***********************************************************************
C     PROGRAMMED BY KUANG HSU  OCTOBER 1994
C***********************************************************************
      REAL NORMP,NORMQ
      DIMENSION PO(*),CO(*),PR51(2)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/unit51'
      REAL*8 GTSKW(10),BWTYP(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc51.f,v $
     . $',                                                             '
     .$Id: prc51.f,v 1.3 2006/03/16 16:29:42 xfan Exp $
     . $' /
C    ===================================================================
C
C
      DATA PR51,BLANK/4HPRC5,4H1   ,4H    /
C
      DATA GTSKW/8HINSTQI1 ,8HINSTQI2 ,8HINSTQO1 ,8HINSTQO2 ,8HMEANQOUT,
     1           8HPOOL    ,8HSTORAGE ,8HOBSQO   ,8HOBSQOM  ,8HOBSH    /
      DATA BWTYP/8HFLOW    ,8HELEV    /
C
C***********************************************************************
C----------------------------
C  SET DEBUG AND TRACE LEVELS
C
      IBUG = 0
      CALL FPRBUG(PR51,1,51,IBUG)
      IF (ITRACE .GT. 0) IBUG=1
      IF (IPBUG .GT. 0) IBUG=2
C
      NRES=PO(9)
      CALL ETOM51(PO)
C dws    PO(12) was placed into an integer to replace it in the rest
C dws     of the statements to avoid compiler warnings ... 2006-01-23
 
      NUPO12 = PO(12)

C
C***********************************************************************
C     PRINT INFLOW INFORMATION
C***********************************************************************
C
C     INFLOW CARRYOVER INFORMATION
C
      LCOI = 1
      QIN = CO(LCOI)
C
      LTS = NUPO12+1
      IF(PO(LTS).NE.BLANK) GO TO 100
      WRITE(IPR,601) QIN,UNITQ
  601 FORMAT(/41X,'***** INFLOW CARRYOVER *****'/
     .       17X,'INSTANTANEOUS INFLOW = ',10X,F12.0,1X,A4)
C
C     END OF INFLOW INFORMATION
C
 100  CONTINUE
C
C***********************************************************************
C     PRINT UPERBKWR INFORMATION
C***********************************************************************
C
      IF(NRES.LE.1) GO TO 2000
      LPO=PO(11)
      ISTYP=PO(LPO)
      IRES=1
      LCO = LCOI+(NRES-IRES)*4+1
      IF(ISTYP.EQ.3) GO TO 300
C
C     UPERBKWR CARRYOVER INFORMATION
C
      QOI = CO(LCO)
      EL1 = CO(LCO+1)
      STO = CO(LCO+2)
      QL1 = CO(LCO+3)
C
      WRITE(IPR,201) QOI,UNITQ,EL1,UNITL,STO,UNITST
  201 FORMAT(//41X,'***** UPERBKWR CARRYOVER *****'/
     .       17X,'INSTANTANEOUS DISCHARGE = ',7X,F12.1,1X,A4/
     .       17X,'POOL ELEVATION = ',16X,F12.2,1X,A4/
     .       17X,'STORAGE CONTENTS = ',14X,F12.0,1X,A4)
      NTTS=PO(NUPO12)
      NTS=(NTTS-2)/NRES
      LTS=NUPO12+(NRES-IRES)*NTS*5+11
      LTSQO1 = LTS
      LTSQO2 = LTSQO1+5
      LTSQL1 = LTS+40
      LTSQL2 = LTSQL1+5
      IF(PO(LTSQL1).EQ.BLANK .AND. PO(LTSQL2).NE.BLANK) 
     & WRITE(IPR,202) QL1,UNITQ
 202  FORMAT(
     & 17X,'TRIBUTARY AND LOCAL FLOW TO DOWNSTREAM RESERVOIR = ',
     & 2X,F12.1,1X,A4)
      GO TO 1000
C
C     END OF UPERBKWR INFORMATION
C
C***********************************************************************
C     PRINT 3-VAR INFORMATION
C***********************************************************************
C
 300  CONTINUE
C
C     3-VAR CARRYOVER INFORMATION
C
      IRES = 1
      QOI = CO(LCO)
C      EL1 = CO(LCO+1)
C      STO = CO(LCO+2)
      QL1 = CO(LCO+3)
C
C      WRITE(IPR,301) QOI,UNITQ
C  301 FORMAT(//41X,'***** 3VARBKTL CARRYOVER *****'/
C     .       17X,'INSTANTANEOUS DISCHARGE = ',7X,F12.1,1X,A4)
C
      NTTS=PO(NUPO12)
      NTS=(NTTS-2)/NRES
      LTS=NUPO12+(NRES-IRES)*NTS*5+11
      LTSQL1 = LTS+40
      LTSQL2 = LTSQL1+5
      IF(PO(LTSQL1).EQ.BLANK .AND. PO(LTSQL2).NE.BLANK) 
     & WRITE(IPR,302) QL1,UNITQ
 302  FORMAT(//41X,'***** 3VARBKTL CARRYOVER *****'/
     & 17X,'TRIBUTARY AND LOCAL FLOW TO RESERVOIR = ',2X,F12.1,1X,A4)
C
C     END OF 3-VAR INFORMATION
C
C***********************************************************************
C     PRINT LWERBKWR INFORMATION
C***********************************************************************
C
 1000 CONTINUE
      LPO=PO(10)
      ISTYP=PO(LPO)
      IRES=NRES
      LCO = LCOI+(NRES-IRES)*4+1
      IF(ISTYP.NE.2) GO TO 2000
C
C     LWERBKWR CARRYOVER INFORMATION
C
      QOI = CO(LCO)
      EL1 = CO(LCO+1)
      STO = CO(LCO+2)
C      QL1 = CO(LCO+3)
C
      WRITE(IPR,402) QOI,UNITQ,EL1,UNITL,STO,UNITST
 402  FORMAT(//41X,'***** LWERBKWR CARRYOVER *****'/
     .       17X,'INSTANTANEOUS DISCHARGE = ',7X,F12.1,1X,A4/
     .       17X,'POOL ELEVATION = ',16X,F12.2,1X,A4/
     .       17X,'STORAGE CONTENTS = ',14X,F12.0,1X,A4)
      GO TO 9000
C
C     END OF LWERBKWR INFORMATION
C
C***********************************************************************
C     PRINT SAR INFORMATION
C***********************************************************************
C
C     SAR CARRYOVER INFORMATION
C
 2000 CONTINUE
      LCO = LCOI+1
      QOI = CO(LCO)
      EL1 = CO(LCO+1)
      STO = CO(LCO+2)
C      QL1 = CO(LCO+3)
C
      WRITE(IPR,2602) QOI,UNITQ,EL1,UNITL,STO,UNITST
 2602 FORMAT(/41X,'***** SAR CARRYOVER *****'/
     .       17X,'INSTANTANEOUS DISCHARGE = ',7X,F12.1,1X,A4/
     .       17X,'POOL ELEVATION = ',16X,F12.2,1X,A4/
     .       17X,'STORAGE CONTENTS = ',14X,F12.0,1X,A4)
C
C     END OF SAR INFORMATION
C
 9000 CONTINUE
C
      RETURN
      END










