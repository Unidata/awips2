C MEMBER EX38
C  (from old member FCEX38)
C
      SUBROUTINE EX38(PO,CO,BFR,BF)
C.......................................
C     THIS IS THE EXECUTION SUBROUTINE FOR THE BASEFLOW OPERATION.
C.......................................
C     WRITTEN BY ERIC ANDERSON--HRL  MARCH 1987
C.......................................
      DIMENSION PO(1),CO(1),BFR(1),BF(1)
      DIMENSION SNAME(2),UCODE(2),CT(2)
C
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/ionum'
      INCLUDE 'common/mod138'
      INCLUDE 'common/mod102'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex38.f,v $
     . $',                                                             '
     .$Id: ex38.f,v 1.1 1995/09/17 18:57:11 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HEX38,4H    /
      DATA UCODE/3HCFS,3HCMS/
C
C.......................................
C     TRACE LEVEL=1,DEBUG FLAG=IBUG
      CALL FPRBUG(SNAME,1,38,IBUG)
C
C.......................................
C     CONTROL VARIABLES AND INITIAL VALUES.
      IDT=PO(6)
      DTQ=IDT
      P=DTQ/24.
      IOPT=PO(9)
      IC=1
      IM38=1
      IM02=1
      NCO=PO(12)
C
C.......................................
C     INITIAL DEBUG--PRINT PO AND CO.
      IF (IBUG .EQ. 0) GO TO 100
      NPO=PO(11)
      WRITE(IODBUG,900) NPO,NCO
 900  FORMAT(1H0,'BASEFLOW DEBUG--PO AND CO CONTENTS--NPO=',I3,3X,'NCO='
     $,I3)
      WRITE(IODBUG,901) (PO(I), I=1, NPO)
 901  FORMAT(1H0,15F8.2)
      WRITE(IODBUG,902) (PO(I), I=1, NPO)
 902  FORMAT(1H0,15(4X,A4))
      IF (NCO .EQ. 0) GO TO 100
      WRITE(IODBUG,901) (CO(I), I=1, NCO)
      WRITE(IODBUG,903) IOPT,NDT02,NDT38
 903  FORMAT(1H0,'MOD VALUES AND RECESSION TIME SERIES--IOPT=',I2,5X,'ND
     $T02=',I2,5X,'NDT38=',I2)
      IF(NDT02.GT.0) WRITE(IODBUG,904) (IDT02(I),VAL02(I),I=1,NDT02)
 904  FORMAT(1H0,5(I10,F10.3))
      IF (IOPT .EQ. 1) GO TO 100
      IF (NDT38 .GT. 0)WRITE(IODBUG,904) (IDT38(I),VAL38(I),I=1,NDT38)
      I1=(IDA-IDADAT)+1
      I2=(LDA-IDADAT)+1
      WRITE(IODBUG,901) (BFR(I), I=I1, I2)
C
C...............................................
C     GET INITIAL BASEFLOW COMPONENTS.
 100  CBF=PO(8)
      VBF=0.0
      IF (IOPT .EQ. 0) GO TO 190
      VBF=CO(1)-CBF
      IF(VBF.GE.0.0) GO TO 120
      VBF=0.0
      CBFX=CBF
      TBFX=CO(1)
      IF (METRIC .EQ. 1) GO TO 110
      XCFS=1.0/0.028317
      CBFX=CBFX*XCFS
      TBFX=TBFX*XCFS
 110  WRITE(IPR,905) CBFX,UCODE(METRIC+1),TBFX,UCODE(METRIC+1)
 905  FORMAT(1H0,10X,'**WARNING** CONSTANT BASEFLOW(',F8.1,1X,A3,') IS G
     $REATER THAN TOTAL BASEFLOW(',F8.1,1X,A3,').'/16X,'VARIABLE BASEFLO
     $W COMPONENT IS SET TO ZERO.')
C
C........................................
C     GET INITIAL RECESSION COEFFICIENT.
 120  IF(IOPT.NE.1) GO TO 130
      R=PO(18)
      GO TO 140
 130  R=CO(2)
C
C........................................
C     CHECK FOR VARIABLE BASEFLOW MOD CHANGE AT START OF RUN
 140  IF (NDT02 .LT. IM02) GO TO 190
      IH=(IDA-1)*24+IHR-IDT
      I1=IH-(IDT-1)/2
      I2=IH+IDT/2
      IF((IDT02(IM02).LT.I1) .OR. (IDT02(IM02).GT.I2)) GO TO 190
      VBF=VAL02(IM02)
      IM02=IM02+1
C
C........................................
C     SET INITIAL LOOP VARIABLES.

 190  KDA=IDA
      KHR=IHR
      LBF=(KDA-IDADAT)*(24/IDT)+(KHR/IDT)
      IF(IOPT .NE. 2) GO TO 200
      LBFR=(KDA-IDADAT)+1
C
C........................................
C........................................
C     BEGIN DAILY LOOP
 200  IF(IOPT .EQ. 0) GO TO 250
      IF(IOPT .EQ. 1) GO TO 220
C     GET RECESSION COEFFICIENT FOR THE DAY.
C        FIRST CHECK FOR MOD CHANGE
      IF(NDT38 .LT. IM38) GO TO 210
      I2=KDA*24
      I1=I2-23
      IF((IDT38(IM38) .LT. I1).OR.(IDT38(IM38) .GT. I2)) GO TO 210
      BFR(LBFR)=VAL38(IM38)
      IM38=IM38+1
      R=BFR(LBFR)
      GO TO 220
210   BFR(LBFR)=R
C     COMPUTE TIME INTERVAL RECESSION
 220  RDT=R**P
C
C........................................
C     BEGIN TIME INTERVAL LOOP
 250  IF(IOPT.EQ.0) GO TO 270
C     GET VARIABLE BASEFLOW--FROM MOD OR BY RECESSING PREVIOUS VALUE
      IF(NDT02.LT.IM02) GO TO 260
      IH=(KDA-1)*24+KHR
      I1=IH-(IDT-1)/2
      I2=IH+IDT/2
      IF((IDT02(IM02).LT.I1).OR.(IDT02(IM02).GT.I2)) GO TO 260
      VBF=VAL02(IM02)
      IM02=IM02+1
      GO TO 270
 260  VBF=VBF*RDT
C     COMPUTE TOTAL BASEFLOW
 270  BF(LBF)=CBF+VBF
C
C........................................
C     CHECK FOR SAVING CARRYOVER
      IF(IOPT.EQ.0) GO TO 290
      IF(IFILLC.EQ.0) GO TO 290
      IF(NCSTOR.EQ.0) GO TO 290
      IF(IC.GT.NCSTOR) GO TO 290
      IF((KDA.EQ.ICDAY(IC)).AND.(KHR.EQ.ICHOUR(IC))) GO TO 280
      GO TO 290
C     SAVE CARRYOVER
 280  CT(1)=BF(LBF)
      IF(IOPT.EQ.1) GO TO 285
      CT(2)=R
 285  CALL FCWTCO(KDA,KHR,CT,NCO)
      IC=IC+1
      IF(IBUG.EQ.0) GO TO 290
      WRITE(IODBUG,906) KDA,KHR
 906  FORMAT(1H0,'BASEFLOW CARRYOVER FOR DAY=',I6,3X,'HOUR=',I2)
      WRITE(IODBUG,901)(CT(I),I=1,NCO)
C
C........................................
C     CHECK FOR END OF RUN--IF NOT, GO BACK TO PROPER PLACE
 290  IF((KDA.EQ.LDA).AND.(KHR.EQ.LHR)) GO TO 300
      KHR=KHR+IDT
      LBF=LBF+1
      IF(KHR.LE.24) GO TO 250
      KDA=KDA+1
      KHR=IDT
      LBFR=LBFR+1
      GO TO 200
C
C........................................
C........................................
C     END OF RUN PERIOD--UPDATE CO IF REQUESTED.
 300  IF (IOPT.EQ.0) GO TO 350
      IF(IFILLC.EQ.0) GO TO 350
      CO(1)=BF(LBF)
      IF(IOPT.EQ.1) GO TO 350
      CO(2)=R
C
C........................................
C     CHECK FOR DEBUG PRINTOUT OF RESULTS.
 350  IF(IBUG.EQ.0) GO TO 390
      WRITE(IODBUG,907)
 907  FORMAT(1H0,'BASEFLOW RESULTS')
      I1=(IDA-IDADAT)*(24/IDT)+(IHR/IDT)
      I2=(LDA-IDADAT)*(24/IDT)+(LHR/IDT)
      WRITE (IODBUG,909) (BF(I),I=I1,I2)
909   FORMAT (1H0,12F10.3)
      IF (IOPT.EQ.0) GO TO 390
      WRITE(IODBUG,901)(CO(I),I=1,NCO)
      IF(IOPT.EQ.1) GO TO 390
      I1=(IDA-IDADAT)+1
      I2=(LDA-IDADAT)+1
      WRITE(IODBUG,901)(BFR(I),I=I1,I2)
C
C........................................
C     COMPLETE PROCESSING
 390  IF(ITRACE.GE.1) WRITE(IODBUG,908)
 908  FORMAT(1H0,'EXIT FCEX38')
      RETURN
      END
