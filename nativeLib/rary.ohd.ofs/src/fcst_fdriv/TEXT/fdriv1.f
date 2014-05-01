C MEMBER FDRIV1
C  (from old member FCFDRIV1)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/20/95.11:35:36 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE FDRIV1 (P,MP,C,MC,T,MT,D,MD,IHZERO,NUMOP,IERR,iuhgd)
C.......................................
C     FDRIV1 CALLS THE EXECUTION ROUTINES FOR OPERATIONS
C        NUMBER 1 THROUGH NUMBER 15 AND 19.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MARCH 1980
C
CVK     MODIFIED 4/00 BY V. KOREN: NEW SIMULATED SNOW DEPTH SERIES ADDED
CVK     TO EX19
Cav     MODIFIED 6/03 added NEW OBSERVED SNOW DEPTH SERIES
C
CEA     MODIFIED 11/05 BY E. ANDERSON TO USE INTEGER VERSION NUMBER
C.......................................
      DIMENSION P(MP),C(MC),D(MD)
      INTEGER T(MT)
      REAL SNOWD(744)
      REAL OSNOWD(744)
cav   4/15/04 added for UHGCDATE
      character*8 OPNAME
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     1LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON/FCOPPT/LOCT,LPM,LCO
      INCLUDE 'common/fcassm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fdriv/RCS/fdriv1.f,v $
     . $',                                                             '
     .$Id: fdriv1.f,v 1.9 2006/10/03 19:33:08 hsu Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS ROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** FDRIV1 ENTERED)
C.......................................
C     INITIAL VALUES.
      DUMMY=0.0
      I1=1
      IZ=0
C.......................................
C     CHECK TO MAKE SURE THAT A CALL TO THE OPERATION IS INCLUDED
C         IN FDRIV1.
      IF ((NUMOP.GT.0).AND.(NUMOP.LT.16)) GO TO 100
      IF (NUMOP.EQ.19) GO TO 119
      GO TO 190
C.......................................
C     GO TO THE PROPER SECTION FOR THE OPERATION
  100 GO TO (101,102,103,104,105,106,107,108,109,110,111,112,113,114,
     1115),NUMOP
C.......................................
C     EXECUTE SACRAMENTO SOIL-MOISTURE ACCOUNTING OPERATION.
  101 IDT=P(LPM+1)
      IHR=IHZERO+IDT
      IF ((MAINUM.EQ.1).AND.((INASSIM.EQ.0).OR.(ASSMRUN.EQ.0)))
     1 CALL FSACTR(P(LPM),IDT)
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      LD5=T(LOCT+8)
      LD6=T(LOCT+9)
      LD7=T(LOCT+10)
      LD8=T(LOCT+11)
      LD9=T(LOCT+12)
      CALL EX1(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5),D(LD6),
     1D(LD7),D(LD8),D(LD9),D(LD10))
      GO TO 99
C.......................................
C     EXECUTE UNIT HYDROGRAPH OPERATION.
  102 IDT=P(LPM+15)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      opname=''
      write(opname,22,iostat=irwerr) (p(j),j=lpm-5,lpm-4)
22    format(2a4)
cav 4/21/04 added for uhgcdate
      CALL EX2(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),opname,iuhgd)
      GO TO 99
C.......................................
C     EXECUTE REDUCED ORDINATE UHG OPERATION.
  103 IDT=P(LPM+8)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      CALL EX3(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3))
      GO TO 99
C.......................................
C     EXECUTE CLEAR TIME SERIES OPERATION.
  104 LD1=T(LOCT+3)
      NUM=T(LOCT+4)
      CALL EX4(D(LD1),NUM)
      GO TO 99
C.......................................
C     EXECUTE DAILY FLOW PLOT (SAC-PLOT) OPERATION. -MCP ONLY
  105 IF (MAINUM.NE.3) GO TO 99
      IDT=T(LOCT+3)
      IHR=IHZERO+IDT
      IF (IDT.EQ.0)IHR=24
      LT1=LOCT+10
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LT2=LOCT+T(LOCT+8)-1
      LT3=LOCT+T(LOCT+9)-1
      LD4=T(LOCT+7)
      LD5=LD4+101
      I=P(LPM+5)
      LD6=LD5+I
      LD7=LD6+I
      CALL EX5(P(LPM),T(LT1),LD1,D(LD1),LD2,D(LD2),LD3,D(LD3),T(LT2),
     1T(LT3),D(LD4),D(LD5),D(LD6),D(LD7),P,MP,C,MC,D,MD)
      GO TO 99
C.......................................
C     EXECUTE MEAN DISCHARGE (MEAN-Q) OPERATION.
  106 IDT=P(LPM+4)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      CALL EX6(P(LPM),C(LCO),D(LD1),D(LD2))
      GO TO 99
C.......................................
C     EXECUTE LAG AND K OPERATION.
  107 ITA=P(LPM+4)
      IHR=IHZERO+ITA
      LCO=T(LOCT+3)
      ITB=P(LPM+8)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      CALL EX7(P(LPM),C(LCO),D(LD1),D(LD3))
      IF(ITB.EQ.0) GO TO 1071
      IHR=IHZERO+ITB
      CALL FADD(I1,IZ,IZ,ITA,ITB,DUMMY,D(LD3),D(LD2),D(LD4))
      GO TO 99
 1071 CALL FRPLAC(D(LD1),D(LD3),ITA)
      GO TO 99
C.......................................
C     EXECUTE CHANNEL LOSS OPERATION
  108 IDT=P(LPM+9)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      CALL EX8(P(LPM),C(LCO),D(LD1),D(LD2))
      GO TO 99
C.......................................
C     EXECUTE MUSKINGUM ROUTING OPERATION.
  109 ITA=P(LPM+9)
      IHR=IHZERO+ITA
      LCO=T(LOCT+3)
      ITB=P(LPM+13)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      CALL EX9(P(LPM),C(LCO),D(LD1),D(LD3))
      IF(ITB.EQ.0) GO TO 1091
      IHR=IHZERO+ITB
      CALL FADD(I1,IZ,IZ,ITA,ITB,DUMMY,D(LD3),D(LD2),D(LD4))
      GO TO 99
 1091 CALL FRPLAC(D(LD1),D(LD3),ITA)
      GO TO 99
C.......................................
C     EXECUTE ADD/SUBTRACT OPERATION.
  110 IDT=P(LPM+5)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+5)
      LD2=T(LOCT+4)
      LD3=T(LOCT+6)
      CALL EX10(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3))
      GO TO 99
C.......................................
C     EXECUTE LAYERED COEFFICIENT ROUTING.
  111 ITA=P(LPM+9)
      IHR=IHZERO+ITA
      LCO=T(LOCT+3)
      ITB=P(LPM+13)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      LD5=T(LOCT+8)
      CALL EX11(P(LPM),C(LCO),D(LD1),D(LD3),D(LD5))
      IF(ITB.EQ.0) GO TO 1111
      IHR=IHZERO+ITB
      CALL FADD(I1,IZ,IZ,ITA,ITB,DUMMY,D(LD3),D(LD2),D(LD4))
      GO TO 99
 1111 CALL FRPLAC(D(LD1),D(LD3),ITA)
      GO TO 99
C.......................................
C     EXECUTE INSTANTANEOUS DISCHARGE PLOT.
  112 IDT=P(LPM+9)
      IHR=IHZERO+IDT
      I=P(LPM+6)
      LD1=T(LOCT+3)
      LD2=T(LOCT+4)
      LT1=LOCT+7
      LD3=T(LOCT+6)
      LD4=LD3+101
      LD5=LD4+I
      LD6=LD5+I
      LD7=LD6+I
      LD8=LD7+I
      LRC=T(LOCT+5)
      IF (LRC.EQ.0) GO TO 1121
      CALL FGETRC(P(LRC),I)
 1121 CALL EX12(P(LPM),D(LD1),D(LD2),D,T(LT1),D(LD3),D(LD4),D(LD5),
     1D(LD6),D(LD7),D(LD8))
      GO TO 99
C.......................................
C     EXECUTE TATUM ROUTING
  113 ITA=P(LPM+9)
      IHR=IHZERO+ITA
      LCO=T(LOCT+3)
      ITB=P(LPM+13)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      LD5=T(LOCT+8)
      CALL EX13(P(LPM),C(LCO),D(LD1),D(LD3),D(LD5))
      IF(ITB.EQ.0) GO TO 1131
      IHR=IHZERO+ITB
      CALL FADD(I1,IZ,IZ,ITA,ITB,DUMMY,D(LD3),D(LD2),D(LD4))
      GO TO 99
 1131 CALL FRPLAC(D(LD1),D(LD3),ITA)
      GO TO 99
C.......................................
C     ADJUST SIM. Q OPERATION
  114 IDT=P(LPM+11)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      LD5=T(LOCT+8)
      IQ=T(LOCT+9)
      IDQ=T(LOCT+10)
      LD6=LD5+IQ
      LD7=LD6+IDQ
      LD8=LD7+IQ
      CALL EX14(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5),
     1  D(LD6),D(LD7),D(LD8))
      GO TO 99
C.......................................
C     WEIGHT TIME SERIES
  115 IDT=P(LPM+6)
      IHR=IHZERO+IDT
      LD1=T(LOCT+3)
      LT1=LOCT+4
      CALL EX15(P(LPM),D(LD1),T(LT1),D,MD)
      GO TO 99
C.......................................
C     SNOW MODEL OPERATION
CEA cav ckeck iver variable for 1.01,2.02(sim snow and temp),
CEA    and 3.01(obs snow depth)
  119 IDT=P(LPM+13)
      IHR=IHZERO+IDT
CEA   INTEGER VARIABLE (IVER) NOW USED TO CHECK THE VERSION NUMBER
      IVER=P(LPM)
      IF (IVER.EQ.1) THEN
CEA      IF(P(LPM) .EQ. 1.01) THEN
CEA   VERSION 1 OF THE PIN ROUTINE WAS USED IN FCINIT TO CONSTRUCT THE
CEA     T ARRAY WHICH INDICATES HOW SPACE IN THE D ARRAY IS USED BY
CEA     THE SNOW-17 OPERATION - NO SIMULATED OR OBSERVED DEPTH TIME SERIES.
CEA C       PREVIOUS VERSION OF THE FCINIT WAS USED IN CONSTRUCTING
CEA           THE D ARRAY.
C       PARTITION THE D ARRAY THE OLD WAY AS IN fdriv1.f,v 1.2 (1999/04/22).
C       -- LW
        LD10=T(LOCT+13)
        IF (MAINUM.NE.1) GO TO 1191
        ITPX=P(LPM+9)
        CALL FSNWTR(IDT,ITPX,D(LD10))
 1191   LCO=T(LOCT+3)
        LD1=T(LOCT+4)
        LD2=T(LOCT+5)
        LD3=T(LOCT+6)
        LD4=T(LOCT+7)
        LD5=T(LOCT+8)
        LD6=T(LOCT+9)
        LD7=T(LOCT+10)
        LD8=T(LOCT+11)
        LD9=T(LOCT+12)
        ITPX=P(LPM+9)
        NDT=IDT/ITPX
        LD11=LD10+NDT
        LD12=LD11+NDT
        LD13=LD12+NDT
C       NOTE: ARRAY SNOWD IS JUST AN IDLE ARGUMENT IN THIS CALL
C       -- LW -- ALSO OSNOWD
        CALL EX19_MOD(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD6),
     1    D(LD5),D(LD7),D(LD8),D(LD9),SNOWD(1),D(LD10),
     1    D(LD11),D(LD12),D(LD13),OSNOWD(1))
      ELSE IF (IVER.EQ.2) THEN
CEA      ELSE IF (P(LPM) .EQ. 2.01) THEN
CEA   VERSION 2 OF THE PIN ROUTINE WAS USED IN FCINIT TO CONSTRUCT THE
CEA     T ARRAY WHICH INDICATES HOW SPACE IN THE D ARRAY IS USED BY
CEA     THE SNOW-17 OPERATION - SIMULATED SNOW DEPTH CAN BE INCLUDED,
CEA     BUT NOT OBSERVED DEPTH TIME SERIES.
CVK     WORKING SPACE IN D ARRAY WAS MOVED BY ONE POSITION
        LD10=T(LOCT+14)
        IF (MAINUM.NE.1) GO TO 1192
        ITPX=P(LPM+9)
        CALL FSNWTR(IDT,ITPX,D(LD10))
 1192   LCO=T(LOCT+3)
        LD1=T(LOCT+4)
        LD2=T(LOCT+5)
        LD3=T(LOCT+6)
        LD4=T(LOCT+7)
        LD5=T(LOCT+8)
        LD6=T(LOCT+9)
        LD7=T(LOCT+10)
        LD8=T(LOCT+11)
        LD9=T(LOCT+12)
CVK     ADDED NEW POSITION FOR SNOW DEPTH TIME SERIES
        LD14=T(LOCT+13)
        ITPX=P(LPM+9)
        NDT=IDT/ITPX
        LD11=LD10+NDT
        LD12=LD11+NDT
        LD13=LD12+NDT
CVK     ADDED NEW SIMULATED ARRAY D(LD14) - OSNOWD STILL IDLE ARGUMENT
        CALL EX19(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD6),
     1    D(LD5),D(LD7),D(LD8),D(LD9),D(LD14),D(LD10),
     1    D(LD11),D(LD12),D(LD13),OSNOWD(1))
      ELSE
CEA   VERSION 3 OR 4 OF THE PIN ROUTINE WAS USED IN FCINIT TO CONSTRUCT
CEA     THE T ARRAY WHICH INDICATES HOW SPACE IN THE D ARRAY IS USED BY
CEA     THE SNOW-17 OPERATION - BOTH SIMULATED AND OBSERVED DEPTH TIME
CEA     SERIES CAN NOW BE INCLUDED.
CEA   WORKING SPACE WAS MOVED ONE MORE POSITION IN THE T ARRAY.
        LD10=T(LOCT+15)
        IF (MAINUM.NE.1) GO TO 1193
        ITPX=P(LPM+9)
        CALL FSNWTR(IDT,ITPX,D(LD10))
 1193   LCO=T(LOCT+3)
        LD1=T(LOCT+4)
        LD2=T(LOCT+5)
        LD3=T(LOCT+6)
        LD4=T(LOCT+7)
        LD5=T(LOCT+8)
        LD6=T(LOCT+9)
        LD7=T(LOCT+10)
        LD8=T(LOCT+11)
        LD9=T(LOCT+12)
        LD14=T(LOCT+13)
Cav     ADDED NEW observe snow depth ARRAY D(LD15)
        LD15=T(LOCT+14)
        ITPX=P(LPM+9)
        NDT=IDT/ITPX
        LD11=LD10+NDT
        LD12=LD11+NDT
        LD13=LD12+NDT
        CALL EX19(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD6),
     1    D(LD5),D(LD7),D(LD8),D(LD9),D(LD14),D(LD10),
     1    D(LD11),D(LD12),D(LD13),D(LD15))
      END  IF
      GO TO 99
C.......................................
C     OPERATION NOT INCLUDED.
  190 IERR=1
C.......................................
   99 CONTINUE
      RETURN
      END
