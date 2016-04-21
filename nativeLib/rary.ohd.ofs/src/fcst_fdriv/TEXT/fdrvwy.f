C MEMBER FDRVWY
C  (from old member FCFDRVWY)
C
      SUBROUTINE FDRVWY(P,MP,C,MC,T,MT,D,MD,IHZERO,NUMOP,IERR)
C.......................................
C     DRIVWY CALLS EXECUTION SUBROUTINES FOR OPERATIONS THAT CAN USE
C        THE WATER YEAR SCRATCH FILE.  THESE ARE OPERATIONS NUMBER
C        16, 17, 18, AND 40.
C     Peakflow operation #47 was added by Bryce Finnerty, August, 97.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL     APRIL 1980
C.......................................
      DIMENSION P(MP),C(MC),D(MD)
      INTEGER T(MT)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     1   LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON/FCOPPT/LOCT,LPM,LCO
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fdriv/RCS/fdrvwy.f,v $
     . $',                                                             '
     .$Id: fdrvwy.f,v 1.2 1997/12/31 18:20:07 page Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** FDRVWY ENTERED)
C.......................................
C     INITIAL VALUES.
      DUMMY=0.0
      I1=1
      IZ=0
C.......................................
C     CHECK TO MAKE SURE THAT A CALL TO THE OPERATION IS INCLUDED
C        IN FDRVWY.
      IF((NUMOP.GE.16).AND.(NUMOP.LE.18)) GO TO 100
      IF (NUMOP.EQ.40) GO TO 100
      IF (NUMOP.EQ.47) GO TO 100
      GO TO 190
C.......................................
C     GO TO THE PROPER SECTION FOR THE OPERATION.
  100 IF(NUMOP.EQ.16) GO TO 116
      IF(NUMOP.EQ.17) GO TO 117
      IF(NUMOP.EQ.18) GO TO 118
      IF (NUMOP.EQ.40) GO TO 140
      IF (NUMOP.EQ.47) GO TO 470      
C.......................................
C     EXECUTE DAILY DISCHARGE STATISTICS OPERATION.
  116 IF(MAINUM.NE.3) GO TO 99
      IHR=24
      LD1=T(LOCT+3)
      LD2=T(LOCT+4)
      LD3=T(LOCT+5)
      LD4=T(LOCT+6)
      CALL EX16(P(LPM),D(LD1),D(LD2),D(LD3),D(LD4))
      GO TO 99
C.......................................
C     EXECUTE WATER YEAR PLOT OPERATION
  117 IF(MAINUM.NE.3) GO TO 99
      IDT=T(LOCT+4)
      IHR=IHZERO+IDT
      IF(IDT.EQ.0) IHR=24
      LCO=T(LOCT+3)
      LT1=LOCT+10
      LD1=T(LOCT+5)
      LD2=T(LOCT+6)
      LD3=T(LOCT+7)
      LD4=T(LOCT+8)
      LD5=LD4+101
      I=P(LPM+5)
      LD6=LD5+I
      LD7=LD6+I
      LD8=T(LOCT+9)
      CALL EX17(P(LPM),C(LCO),T(LT1),LD1,D(LD1),LD2,D(LD2),LD3,D(LD3),
     1   D(LD4),D(LD5),D(LD6),D(LD7),D(LD8),D,MD)
      GO TO 99
C.......................................
C     EXECUTE PLOT TIME SERIES OPERATION.
  118 IF ((MAINUM.EQ.2).OR.(MAINUM.EQ.4)) GO TO 99
      IDT=P(LPM+10)
      IHR=IHZERO+IDT
      LT1=LOCT+4
      NPLOTS=P(LPM+7)
      NTTS=P(LPM+8)
      LD1=T(LOCT+3)
      LD2=LD1+NPLOTS
      LD3=LD2+12
      LD4=LD3+121
      LD5=LD4+NTTS
      LD6=LD5+NTTS
      LD7=LD6+NPLOTS
      CALL EX18(P(LPM),D,T(LT1),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5),
     1D(LD6),D(LD7))
      GO TO 99
C.......................................
C     EXECUTE WATER BALANCE OPERATION
 140  IF (MAINUM.NE.3) GO TO 99
      IDT=24
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      NAREA=P(LPM+13)
      LT1=LOCT+7
      LT2=LOCT+7+NAREA
      LT3=LOCT+7+2*NAREA
      LT4=LOCT+7+3*NAREA
      CALL EX40(P(LPM),C(LCO),P,MP,C,MC,T(LT1),T(LT2),T(LT3),T(LT4),
     1  D(LD1),D(LD2),D(LD3))
      GO TO 99
C.......................................
C     EXECUTE PEAKFLOW OPERATION
 470  IF (MAINUM.NE.3) GO TO 99
      IDT=P(LPM+9)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      CALL EX47(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3))
      GO TO 99
C.......................................
C     OPERATION NOT INCLUDED.
  190 IERR=1
C.......................................
   99 CONTINUE
      RETURN
      END
