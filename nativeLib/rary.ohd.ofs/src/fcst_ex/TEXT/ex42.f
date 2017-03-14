C MEMBER EX42
C  (from old member FCEX42)
C
      SUBROUTINE EX42(PO,CO,TA,RSEL,ZELV)
C.......................................
C  THIS IS THE EXECUTION SUBROUTINE FOR THE 'RSNWELEV' OPERATION
C.......................................
C  INITIALLY WRITTEN BY ERIC ANDERSON - HRL   DEC 1991
C.......................................
      DIMENSION PO(1),TA(1),RSEL(1),ZELV(1),SNAME(2),CO(1)
C  COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex42.f,v $
     . $',                                                             '
     .$Id: ex42.f,v 1.1 1995/09/17 18:57:16 dws Exp $
     . $' /
C    ===================================================================
C
C  DATA STATEMENTS
      DATA SNAME/4HEX42,4H    /
C.......................................
C  TRACE LEVEL=1,DEBUG FLAG=IBUG
      CALL FPRBUG(SNAME,1,42,IBUG)
C.......................................
C  CONTROL VARIABLES
      IDT=PO(2)
      TAELEV=PO(6)
      TALR=PO(7)
      PXTEMP=PO(8)
      IZIN=PO(12)
      IJH=(IDA-1)*24+IHR
      LJH=(LDA-1)*24+LHR
      NPD=((LJH-IJH)/IDT)+1
      IHD=(IDADAT-1)*24+IDT
      IOFF=(IJH-IHD)/IDT
      LCO=1
      LPO=20
C.......................................
C     DEBUG OUTPUT
      IF (IBUG.EQ.0) GO TO 100
      WRITE(IODBUG,900)
  900 FORMAT(1H0,'RSNWELEV DEBUG -- PO,TA,ZELV')
      WRITE(IODBUG,901) (PO(I),I=1,LPO)
  901 FORMAT(1H0,15F8.2)
      WRITE(IODBUG,902) (PO(I),I=1,LPO)
  902 FORMAT(1H0,15(4X,A4))
      I1=IOFF+1
      I2=IOFF+NPD
      WRITE(IODBUG,901) (TA(I),I=I1,I2)
      IF (IZIN.EQ.0) GO TO 100
      WRITE(IODBUG,901) (ZELV(I),I=I1,I2)
      WRITE(IODBUG,905) CO(1)
  905 FORMAT(1H0,'CARRYOVER=',F10.2)
C.......................................
C  GET FREEZING LEVEL AT START OF FIRST PERIOD IF AVAILABLE
  100 IF (IZIN.EQ.0) GO TO 120
      Z1=CO(1)
  120 IC=1
C.......................................
C  BEGIN MAIN COMPUTATIONAL L00P
      DO 200 N=1,NPD
      IF (IZIN.EQ.0) GO TO 130
C    CHECK IF FREEZING LEVEL DATA AVAILABLE
      Z2=ZELV(IOFF+N)
      IF ((IFMSNG(Z1).EQ.1).OR.(IFMSNG(Z2).EQ.1)) GO TO 130
      Z=(Z1+Z2)*0.5
      RSL=Z-(PXTEMP*(100.0/TALR))
      GO TO 150
C   USE AIR TEMPERATURE IF AVAILABLE
  130 PTA=TA(IOFF+N)
      IF (IFMSNG(PTA).EQ.1) GO TO 140
      RSL=TAELEV+((PTA-PXTEMP)*(100.0/TALR))
      GO TO 150
  140 RSL=-999.0
  150 IF (RSL.LT.-100.0) RSL=-100.0
      IF (IZIN.EQ.1) Z1=Z2
      RSEL(IOFF+N)=RSL
C.......................................
C  SAVE CARRYOVER IF REQUESTED
      IF (IZIN.EQ.0) GO TO 200
      IF (IFILLC.EQ.0) GO TO 200
      IF (NCSTOR.EQ.0) GO TO 200
      IF (IC.GT.NCSTOR) GO TO 200
      KJH=IJH+(N-1)*IDT
      KDA=((KJH-1)/24)+1
      KHR=KJH-((KDA-1)*24)
      IF ((KDA.EQ.ICDAY(IC)).AND.(KHR.EQ.ICHOUR(IC))) GO TO 170
      GO TO 200
  170 CALL FCWTCO(KDA,KHR,Z1,LCO)
      IC=IC+1
  200 CONTINUE
C  END OF COMPUTATIONAL LOOP
C.......................................
C  UPDATE CO ARRAY IF REQUESTED
      IF (IZIN.EQ.0) GO TO 290
      IF (IFILLC.EQ.0) GO TO 290
      CO(1)=Z1
C.......................................
C  DEBUG OUTPUT
  290 IF (IBUG.EQ.0) GO TO 295
      WRITE(IODBUG,903)
  903 FORMAT(1H0,'RSNWELEV OUTPUT TIME SERIES')
      WRITE(IODBUG,901) (RSEL(I),I=I1,I2)
C.......................................
  295 IF (ITRACE.GE.1) WRITE(IODBUG,904)
  904 FORMAT(1H0,'**EXIT EX42')
      RETURN
      END
