C MEMBER FDRIV2
C  (from old member FCFDRIV2)
C
C                             LAST UPDATE: 04/02/96 BY ERB
C
      SUBROUTINE FDRIV2(P,MP,C,MC,T,MT,D,MD,IHZERO,NUMOP,IERR)
C.......................................
C     DRIVE2 CALLS THE EXECUTION SUBROUTINES FOR OPERATIONS
C
C        NUMBER 20 THRU 39.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C
C        ERIC ANDERSON - HRL   APRIL 1981
C     MODIFIED BY JANICE LEWIS - HRL   FEBRUARY 1992
C        TO ELIMINATE THE CALL TO GETRC BEFORE CALLING EX21
C     Modified by Scott Townsend RTi July 2003
C         Modified code to fix a bug which kept an esp verification run
C         from completing since expected carryover values were not
C         written to the carryover file which was needed by the ESP
C         historic traces run. Removed the checks which kept the
C         calling of ex23 from happening. Pushed those checks into
C         ex23 so that ex23 can write carryover regardless of the
C         checks.
C.......................................
C     POINTERS FOR LOCATION OF PARAMETERS IN HFS P ARRAY
C
C*************** VARIABLES STORED IN PARAMETER & CARRYOVER ARRAYS ******
C...parameters and pointers to their storage location in PO & CO arrays
C
C...time interval, in hours, between observed data or forecast steps
C... and should be equal to the interval between precip & discharge data
       INTEGER PNHST
C
C
C........................................
C
      DIMENSION P(MP),C(MC),D(MD)
C
      INTEGER T(MT)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
      COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,NOW(5),
     1LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
      COMMON/FCOPPT/LOCT,LPM,LCO
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fdriv/RCS/fdriv2.f,v $
     . $',                                                             '
     .$Id: fdriv2.f,v 1.6 2003/08/12 13:06:08 hank Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** FDRIV2 ENTERED)
C
C-----------------------------------------------------------------------
C
C **LOAD POINTERS TO PARAMETER VARIABLES IN PO
C
C      POPVER= 1
C      PILBL=POPVER + 1
C      PMRNO=PILBL  +15
C      PNCC1=PMRNO  + 1
C      PNR  =PNCC1  + 1
C      PALPD=PNR    + 1
C      PXM  =PALPD  + 1
C      PALP =PXM    + 1
C      PSCM =PALP   + 6
C      PSCMCV= PSCM   + 6
C      PNORG=PSCMCV +12
C      PTSP =PNORG  + 1
C      PTSD =PTSP   + 2
C      PTSE =PTSD   + 2
C      PTSS =PTSE   + 2
C      PDTCP=PTSS   + 2
C      PDTCD=PDTCP  + 2
C      PDTCE=PDTCD  + 2
C      PDTCS=PDTCE  + 2
C      PNHST=PDTCS  + 2
       PNHST=63
C      PNHRSE= PNHST  + 1
C      PNHRSD= PNHRSE + 1
C      PAREA=PNHRSD + 1
C      PQ   =PAREA  + 1
C      PCFVMX= PQ     +12
C      PALINP= PCFVMX + 1
C      PALPAR= PALINP + 1
C      PPSTDV= PALPAR + 1
C      PPCSD=PPSTDV +20
C      PPCV =PPCSD  + 1
C      PECSD=PPCV   + 1
C      PECV =PECSD  + 1
C      PR1  =PECV   + 1
C      PR2  =PR1    + 1
C      PDTSD=PR2    + 1
C      PNDTSD= PDTSD  + 1
C      PNPA =PNDTSD + 1
C      PSP  =PNPA   + 1
C      PRP  =PSP    + 1
C      PNQ  =PRP    + 1
C      PETSW=PNQ    + 1
C      PETDAT= PETSW  + 1
C      PXDIS1= PETDAT +12
C      PSUBS=PXDIS1 + 1
C
C      PP   =PSUBS  + 1
C
C      PY    =  1
C
C-----------------------------------------------------------------------
C.......................................
C     INITIAL VALUES.
C
      DUMMY=0.0
      I1=1
      IZ=0
C.......................................
C     CHECK TO MAKE SURE THAT A CALL TO THE OPERATION IS INCLUDED
C
C         IN FDRIV2.
      IF ((NUMOP.GT.19).AND.(NUMOP.LT.40)) GO TO 100
      GO TO 190
C.......................................
C     GO TO THE PROPER SECTION FOR THE OPERATION
C
  100 NUM=NUMOP-19
      GO TO (200,201,202,203,204,205,206,207,208,209,
     1       210,211,212,213,214,215,216,217,218,219),NUM
C.......................................
C     CHANGE TIME INTERVAL OPERATION.
C
  200 IDT=P(LPM+4)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      CALL EX20(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4))
      GO TO 99
C.........................................
C     DYNAMIC ROUTING - DWOPER OPERATION.
C
  201 IDT=P(LPM+21)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
C         GET THE STARTING LOCATION OF THE RATING CURVE(S)
      LRC=T(LOCT+4)
CCC   IF(LRC.NE.0)CALL FGETRC(P(LRC),I)
C         ARRAY NB IS IN THE P ARRAY FOR THIS OPERATION
C         ** MUST ADD LPM-1 BECAUSE WHEN LOCATION WHICH WAS STORED
C         FOR NB WAS RELATIVE TO THE BEGINNING OF THE SECOND PART
C         OF THE P ARRAY FOR THIS OPERATION **
      LPNB=T(LOCT+5) + LPM-1
C         THE FOLLOWING ARE POINTERS TO LOCATIONS IN THE D ARRAY
C         FOR SEVERAL PIECES OF WORKING SPACE NEEDED BY THIS OPERATION *
      LDDTH=T(LOCT+9)
      LDFFS=T(LOCT+10)
      LDFS=T(LOCT+11)
      LDQA=T(LOCT+12)
      LDTII=T(LOCT+13)
      LDT1=T(LOCT+14)
      LDYA=T(LOCT+15)
      LDTO=T(LOCT+16)
C         THE FOLLOWING ARE POINTERS TO LOCATIONS IN THE T ARRAY
C         WHICH HOLD POINTERS TO TIME SERIES AND WORKING SPACE
C         IN THE D ARRAY
      LTXDIV=T(LOCT+6)
      LTPOLT=T(LOCT+7)
      LTITWT=T(LOCT+8)
      LTQLJ=T(LOCT+17)
      LTQLT=T(LOCT+18)
      LTQTC=T(LOCT+19)
      LTSTC=T(LOCT+20)
      LTQLST=T(LOCT+21)
      LTSTN=T(LOCT+22)
      LTST1=T(LOCT+23)
      LTITWS=T(LOCT+24)
      LTPLTS=T(LOCT+25)
      LTQL=T(LOCT+26)
      LTSTT=T(LOCT+27)
      LTQSTR=T(LOCT+28)
      LTDIV=T(LOCT+29)
C ADD T.S. FOR TIDE D/S BOUNDARY ... JML 11/97
      LTNOS=T(LOCT+32)
      LTTID=T(LOCT+33)
C ADD T.S. FOR ADJUST T.S. OPTION ... JML 11/97
      LTSTE=T(LOCT+34)
      LTIRF=T(LOCT+35)
C         EXTRACT LOCATION OF BEGINNING OF WORKING SPACE AFTER
C         ALL DWOPER TIME SERIES HAVE BEEN ALLOCATED
C         COMPUTE LENGTH OF WORKING SPACE AVAILABLE TO THIS OPERATION
      LOWORK=T(LOCT+30)
      LENWRK=MD-LOWORK+1
C
C  PRINT POINTERS IF DEBUG ON FOR DWOPER OPERATION
C
      IF(IDBALL.EQ.1)GO TO 2012
      IF(NDEBUG.LT.1)GO TO 2013
      DO 2011 I=1,NDEBUG
      IF(IDEBUG(I).EQ.21)GO TO 2012
 2011 CONTINUE
      GO TO 2013
C
 2012 WRITE(IODBUG,600)IDT,IHR,LCO,LRC,LPNB,LDDTH,LDFFS,LDFS,
     1 LDQA,LDTII,LDT1,LDYA,LDTO
 600  FORMAT(1X,7X,'IDT',7X,'IHR',7X,'LCO',7X,'LCR',6X,'LPNB',
     1 5X,'LDDTH',5X,'LDFFS',6X,'LDFS',6X,'LDQA',5X,'LDTII',
     2 6X,'LDT1',6X,'LDYA',6X,'LDTO'/1X,13I10)
      WRITE(IODBUG,601)LTXDIV,LTPOLT,LTITWT,LTQLJ,LTQLT,LTQTC,
     1 LTSTC,LTQLST,LTSTN,LTST1,LTITWS,LTPLTS,LTQL
 601  FORMAT(1X,4X,'LTXDIV',4X,'LTPOLT',4X,'LTITWT',5X,'LTQLJ',
     1 5X,'LTQLT',5X,'LTQTC',5X,'LTSTC',4X,'LTQLST',5X,'LTSTN',
     2 5X,'LTST1',4X,'LTITWS',4X,'LTPLTS',6X,'LTQL'/1X,13I10)
      WRITE(IODBUG,602)LTSTT,LTQSTR,LTDIV,LTNOS,LTTID,LTSTE,LTIRF,
     1 LOWORK,LENWRK,LOCT,LPM
 602  FORMAT(1X,5X,'LTSTT',4X,'LTQSTR',5X,'LTDIV',5X,'LTNOS',
     1 5X,'LTTID',5X,'LTSTE',5X,'LTIRF',4X,'LOWORK',4X,'LENWRK',
     2 6X,'LOCT',7X,'LPM'/1X,11I10)
      LOCTEN=T(LOCT+1)-1
      WRITE(IODBUG,603)LOCT,LOCTEN
 603  FORMAT(' A DISPLAY OF THE DWOPER PORTION OF THE T ARRAY ',
     1 'FROM LOCATION ',I6,' THROUGH ',I6,' FOLLOWS.'/'0LOCATION'/)
      DO 2014 I=LOCT,LOCTEN,10
      IEND=I+9
      IF(IEND.GT.LOCTEN)IEND=LOCTEN
      WRITE(IODBUG,604)I,(T(J),J=I,IEND)
 604  FORMAT(1X,I6,10(1X,I11))
 2014 CONTINUE
 2013 CONTINUE
C
      CALL EX21(P(LPM),P(LPM),C(LCO),D(LDDTH),D(LDFFS),D(LDFS),D(LDQA),
     1 D(LDTII),D(LDT1),D(LDYA),D,T(LTSTN),D,T(LTQLJ),D,T(LTST1),
     2 D,T(LTITWS),D,T(LTPLTS),D,T(LTQL),D,T(LTQLT),D,T(LTQTC),
     3 D,T(LTSTC),D,T(LTSTT),D(LOWORK),P(LPNB),D,T(LTDIV),
     4 D,T(LTQLST),D,T(LTQSTR),D(LDTO),D,T(LTITWT),D,T(LTPOLT),
     5 D,T(LTXDIV),D,T(LTNOS),D,T(LTTID),D,T(LTSTE),D(LTIRF),LENWRK)
      GO TO 99
C.......................................
C     HFS OPERATION
C
C202  IDT=P(LPM+53)
 202  IDT=P(LPM+PNHST-1)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      LD5=T(LOCT+8)
      LD6=T(LOCT+9)
C
      CALL EX22_OTHER(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5),
     * D(LD6))
      GO TO 99
C.......................................
C     STAGE-DISCHARGE CONVERSION.
C
CRTi July 2003
C The following code is modified to allow ex23 (stageQ) to
C run even when ESP is run and segment definition says not to
C run. Logic will be put into the operation to by pass the main
C operation code but still write carryover holding values so the
C carryover file will be complete. As it is now the carryover file
C is incomplete since the ex23 operation is skipped here. This
C causes problems in reading the ESP carryover file! The common
C NCSTOR is set to NCSTOR+100 if this is an ESP run and the segment
C definition tells the FDRIV2 not to run the operation. In EX23
C the main operation code will be skipped.
C
C NOTE: If we ever change fcst so that there can be more than 100
C       days of carryover to store (i.e. NCSTOR can exceed 100),
C       then we will have to change the 100+NCSTOR line below to
C       add some other number, like 1000000 or -1000.  Then ex23.f
C       would also have to be changed.  (comment by Hank Herr)
  203 IF (MAINUM.NE.2) GO TO 2031
      I=P(LPM+6)
      IF (I.EQ.1) THEN
         NCSTOR=100+NCSTOR
      ENDIF

 2031 IDT=P(LPM+10)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+7)
      LD4=T(LOCT+8)
      LRC=T(LOCT+6)
      CALL FGETRC(P(LRC),I)
      CALL EX23(P(LPM),C(LCO),D(LD2),D(LD1),D(LD3),D(LD4))
      GO TO 99
C.......................................
C   API-CONT OPERATION
C
 204  IDT=P(LPM+6)
      IHR=IHZERO+IDT
      IF (MAINUM.EQ.1) CALL FAPITR(P(LPM),IDT)
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
      LD10=T(LOCT+13)
      LD11=T(LOCT+14)
      LD12=T(LOCT+15)
      LD13=T(LOCT+16)
      LD14=T(LOCT+17)
      LD15=T(LOCT+18)
      LD16=T(LOCT+19)
      CALL EX24(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5),
     1  D(LD6),D(LD7),D(LD8),D(LD9),D(LD10),D(LD11),D(LD12),
     2  D(LD13),D(LD14),D(LD15),D(LD16))
      GO TO 99
C.......................................
C     PLOT-TUL OPERATION
C
 205  IDT=P(LPM+6)
      IHR=IHZERO+IDT
      LD1=T(LOCT+4)
      LT1=LOCT+5
      LRC=T(LOCT+3)
      IF (LRC.EQ.0) GO TO 2051
      CALL FGETRC(P(LRC),I)
 2051 CALL EX25(P(LPM),D,D(LD1),T(LT1))
      GO TO 99
C.......................................
C     SINGLE RESERVOIR SIMULATION OPERATION
C
 206  IDT=P(LPM+6)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      NRC=T(LOCT+5)
      LT1=LOCT+6+NRC
      CALL EX26(P(LPM),C(LCO),D,T(LT1),D(LD1))
      GO TO 99
C.......................................
C  FORTH WORTH TABULAR DISPLAY
C
 207  IDT=T(LOCT+4)
      IHR=IHZERO+IDT
      LT1=LOCT+5
      LRC=T(LOCT+3)
      IF(LRC.EQ.0)GO TO 2071
      CALL FGETRC(P(LRC),I)
 2071 CALL EX27(P(LPM),T(LT1),D)
      GO TO 99
C.......................................
C     CHANNEL LEAK OPERATION.
C
  208 IDT=P(LPM+19)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      CALL EX28(P(LPM),C(LCO),D(LD1),D(LD3),D(LD2))
      GO TO 99
C.......................................
C     KANSAS CITY API OPERATION
C
 209  IDT=P(LPM+12)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD3=T(LOCT+5)
      LD2=T(LOCT+6)
      LD4=T(LOCT+7)
      LD5=T(LOCT+8)
      CALL EX29(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5))
      GO TO 99
C.................................
C     MERGE TIME SERIES OPERATION
C
 210  IDT=P(LPM+5)
      IHR=IHZERO+IDT
      LD1=T(LOCT+3)
      LT1=LOCT+4
      CALL EX30(P(LPM),D(LD1),T(LT1),D)
      GO TO 99
C.......................................
C     SNOW-43 Kalman Filter state space formulation operation
C
 211  IDT=P(LPM+13)
      IHR=IHZERO+IDT
      LD12=T(LOCT+15)
      IF (MAINUM.NE.1) GO TO 2111
      ITPX=P(LPM+9)
      CALL FSNWTR(IDT,ITPX,D(LD12))
 2111 LCO=T(LOCT+3)
C precip data
      LD1=T(LOCT+4)
C Temperature data
      LD2=T(LOCT+5)
C Rain + Melt values
      LD3=T(LOCT+6)
C Percent snow fall data
      LD4=T(LOCT+7)
C Rain snow elevation data
      LD5=T(LOCT+9)
C Observed water equivalent
      LD6=T(LOCT+8)
C Observed Water Equivalent variance data
      LD7=T(LOCT+13)
C Simulated Water equivalent data
      LD8=T(LOCT+10)
C Simulated Water equivalent variance data
      LD9=T(LOCT+14)
C Observed areal extent of snow cover data
      LD10=T(LOCT+11)
C simulated areal snow cover data
      LD11=T(LOCT+12)
C  4 workspaces
      ITPX=P(LPM+9)
      NDT=IDT/ITPX
      LD13=LD12+NDT
      LD14=LD13+NDT
      LD15=LD14+NDT
C
      CALL EX31(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5),D(LD6),
     1  D(LD7),D(LD8),D(LD9),D(LD10),D(LD11),D(LD12),D(LD13),D(LD14),
     2  D(LD15))
      GO TO 99
C.......................................
C  FLASH FLOOD GUIDANCE OPERATION
C
  212 LP1=T(LOCT+3)
      LC1=T(LOCT+4)
      LP2=T(LOCT+5)
      LC2=T(LOCT+6)
      LD1=T(LOCT+7)
      LD2=T(LOCT+8)
      LD3=T(LOCT+9)
      LD4=T(LOCT+10)
      CALL EX32(P(LPM),P(LP1),C(LC1),P(LP2),C(LC2),D(LD1),D(LD2),
     1  D(LD3),D(LD4))
      GO TO 99
C.......................................
C     CINCINNATI API OPERATION
C
 213  IDT=P(LPM+12)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+7)
      LD3=T(LOCT+6)
      LD4=T(LOCT+5)
      LD5=T(LOCT+8)
      LD6=T(LOCT+9)
      CALL EX33(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5),D(LD6))
      GO TO 99
C.......................................
C     SALT LAKE CITY API OPERATION
C
 214  IDT=P(LPM+10)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      CALL EX34(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4))
      GO TO 99
C.......................................
C     HARRISBURG RFC API OPERATION
C
 215  IDT=P(LPM+19)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+6)
      LD3=T(LOCT+5)
      LD4=T(LOCT+7)
      LD5=T(LOCT+8)
      LD6=T(LOCT+9)
      CALL EX35(P(LPM),C(LCO),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5),D(LD6))
      GO TO 99
C.......................................
C     XINANJIANG MODEL
C
  216 IDT=P(LPM+1)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LT1=LOCT+9
      LD2=T(LOCT+5)
      LD3=T(LOCT+6)
      LD4=T(LOCT+7)
      LD5=T(LOCT+8)
      CALL EX36(P(LPM),C(LCO),D(LD1),T(LT1),D,D(LD2),D(LD3),D(LD4),
     1 D(LD5))
      GO TO 99
C.......................................
C     MINNEAPOLIS RUNOFF TABULATION
C
 217  IDT=24
      IHR=IHZERO+IDT
      LD1=T(LOCT+3)
      LD2=T(LOCT+4)
      LD3=T(LOCT+5)
      LD4=T(LOCT+6)
      LD5=T(LOCT+7)
      LD6=T(LOCT+8)
      LP1=T(LOCT+9)
      LD7=T(LOCT+10)
      LD8=T(LOCT+11)
      CALL EX37(P(LPM),D(LD1),D(LD2),D(LD3),D(LD4),D(LD5),D(LD6),
     1   P(LP1),D(LD7),D(LD8))
      GO TO 99
C.......................................
C  BASEFLOW OPERATION
C
 218  IDT=P(LPM+5)
      IHR=IHZERO+IDT
      LCO=T(LOCT+3)
      LD1=T(LOCT+4)
      LD2=T(LOCT+5)
      CALL EX38(P(LPM),C(LCO),D(LD2),D(LD1))
      GO TO 99
C.......................................
C  TABLE LOOKUP -- FT. WORTH
C
 219  IDT=P(LPM+13)
      IHR=IHZERO+IDT
      LD1=T(LOCT+3)
      LD2=T(LOCT+4)
      CALL EX39(P(LPM),D(LD1),D(LD2))
      GO TO 99
C.......................................
C     OPERATION NOT INCLUDED.
C
  190 IERR=1
C.......................................
   99 CONTINUE
C
      RETURN
      END
