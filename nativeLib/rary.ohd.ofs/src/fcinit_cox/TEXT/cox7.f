C MODULE COX7
C-----------------------------------------------------------------------
C
      SUBROUTINE COX7 (PO,CO,PN,CN)
C
C.......................................................................
C
C     THIS SUBROUTINE TRANSFERS CARRYOVER VALUES
C     FROM ONE SET OF PARAMETERS TO ANOTHER FOR
C     THE LAG/K OPERATION.
C.......................................................................
C
C     SUBROUTINE ORIGINALLY WRITTEN BY
C                           GEORGE F. SMITH  HRL - NOVEMBER, 1979.
C                UPDATED MARCH 1982 TO ALLOW METRIC AND ENGLISH UNITS
C.......................................................................
C
      INCLUDE 'common/fdbug'
C
      DIMENSION PO(1),CO(1),PN(1),CN(1)
      DIMENSION SUBN(2),CONLQO(2),CONLQN(2)
C
      CHARACTER*8 SNAME/'COX7'/
      LOGICAL FOP7,CONLGO,CONLGN,MEANQO,MEANQN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox7.f,v $
     . $',                                                             '
     .$Id: cox7.f,v 1.2 2000/03/14 11:50:23 page Exp $
     . $' /
C    ===================================================================
C
      DATA L3/4HL3  /
C
C
C  PRINT TRACE INFORMATION AND SET DEBUG PRINT FLAG
      LTRACE=1
      NOP=7
      CALL FPRBUG (SNAME,LTRACE,NOP,IB)
C
C...........................
C
C     SEE IF LAG CO NEEDS TO BE TRANSFERED.
C...........................
C
      MXCON=CN(1)
C
      IF(.NOT.FOP7(PO(19),PO(20)).OR..NOT.FOP7(PN(19),PN(20)).
     1  OR.MXCON.LT.7)GO TO 200
C
C     ........ TRANSFER LAG CARRYOVER .........
C
C     IN CASE K IS NOT ENABLED - TRANSFER CURRENT OUTFLOW
C
      CN(3)=CO(3)
C...........................
C
      MXLCOO=CO(5)
      IF(MXLCOO.LE.0)GO TO 300
C.......................................................................
C
C     SET LAG FLAGS - LAG=4 FOR OLD AND NEW PARAMETERS CONSTANT LAG
C                     LAG=3 FOR OLD CONSTANT LAG AND NEW VARIABLE LAG
C                     LAG=2 FOR OLD VARIABLE LAG AND NEW CONSTANT LAG
C                     LAG=1 FOR OLD AND NEW VARIABLE LAG
C.......................................................................
C
      NPLQO=PO(19)
      IF(NPLQO.GT.0)GO TO 10
C
      CONLGO=.TRUE.
      NPLQO=1
      CONLQO(1)=PO(20)
      CONLQO(2)=1.E20
      GO TO 20
C
   10 CONLGO=.FALSE.
C
   20 NPLQN=PN(19)
      IF(NPLQN.GT.0)GO TO 30
C
      CONLGN=.TRUE.
      NPLQN=1
      CONLQN(1)=PN(20)
      CONLQN(2)=1.E20
      GO TO 40
C
   30 CONLGN=.FALSE.
C
   40 IF(CONLGO.AND.CONLGN)LAG=4
      IF(CONLGO.AND..NOT.CONLGN)LAG=3
      IF(.NOT.CONLGO.AND.CONLGN)LAG=2
      IF(.NOT.CONLGO.AND..NOT.CONLGN)LAG=1
C.......................................................................
C
C     LAG OLD CARRYOVER VALUES BACKWARDS WITH OLD PARAMETERS
C     THEN FORWARD WITH NEW PARAMETERS
C
C     SAVE ANY NEW CARRYOVER VALUES WHICH FALL AFTER THE CARRYOVER DATE
C
C     IF NUMBER OF NEW CARRYOVER VALUES LT NUMBER OF OLD CARRYOVER
C     VALUES - DO NOT STORE MORE OLD VALUES THAN LOCATIONS FOR
C              NEW ONES
C.......................................................................
C
      LCN=6
C
      DO 100 I=1,MXLCOO
      K=5+I*2
      J=K-1
      IF(CO(K).LE.0.0)GO TO 110
      IF(IB.EQ.1)WRITE(IODBUG,605)I,K,J,CO(K),CO(J),LAG
  605 FORMAT(11X,'IN DO 100 LOOP - I,K,J,CO(K),CO(J),LAG= ',3I5,2G10.3,
     1  I3)
C
      GO TO (50,60,70,80) , LAG
C
   50 TIME=CO(K) - FSERC7(LXXXX,CO(J),NPLQO,PO(20))
     1           + FSERC7(LXXXX,CO(J),NPLQN,PN(20))
      GO TO 90
C
   60 TIME=CO(K) - FSERC7(LXXXX,CO(J),NPLQO,PO(20))
     1           + FSERC7(LXXXX,CO(J),NPLQN,CONLQN)
      GO TO 90
C
   70 TIME=CO(K) - FSERC7(LXXXX,CO(J),NPLQO,CONLQO)
     1           + FSERC7(LXXXX,CO(J),NPLQN,PN(20))
      GO TO 90
C
   80 TIME=CO(K) - FSERC7(LXXXX,CO(J),NPLQO,CONLQO)
     1           + FSERC7(LXXXX,CO(J),NPLQN,CONLQN)
C
   90 IF(IB.EQ.1)WRITE(IODBUG,606)TIME
  606 FORMAT(11X,'TIME= ',G10.3)
      IF(TIME.LE.0.0)GO TO 100
      IF(LCN+1.GT.MXCON)GO TO 110
      CN(LCN)=CO(J)
      CN(LCN+1)=TIME
      LCN=LCN+2
C
  100 CONTINUE
C.......................................................................
C
C     LAG CURRENT INFLOW BACKWARDS WITH OLD PARAMETERS
C     AND FORWARD WITH NEW PARAMETERS
C
C     IF TIME FALLS AFTER CARRYOVER DATE, MOVE OTHER
C     LAG CARRYOVER VALUES OVER, AND STORE CURRENT INFLOW
C     LAGGED BACK AND FORWARD IN FIRST NEW CARRYOVER SLOTS
C.......................................................................
C
  110 IF(IB.EQ.1)WRITE(IODBUG,607)LAG,CN(2)
  607 FORMAT(1H0,10X,'AFTER DO 100 LOOP - LAG,CN(2)= ',I5,1X,G10.3)
      GO TO (150,160,170,180) , LAG
C
  150 TIME= 0.0  - FSERC7(LXXXX,CN(2),NPLQO,PO(20))
     1           + FSERC7(LXXXX,CN(2),NPLQN,PN(20))
      GO TO 190
C
  160 TIME= 0.0  - FSERC7(LXXXX,CN(2),NPLQO,PO(20))
     1           + FSERC7(LXXXX,CN(2),NPLQN,CONLQN)
      GO TO 190
C
  170 TIME= 0.0  - FSERC7(LXXXX,CN(2),NPLQO,CONLQO)
     1           + FSERC7(LXXXX,CN(2),NPLQN,PN(20))
      GO TO 190
C
  180 TIME= 0.0  - FSERC7(LXXXX,CN(2),NPLQO,CONLQO)
     1           + FSERC7(LXXXX,CN(2),NPLQN,CONLQN)
C
  190 IF(IB.EQ.1)WRITE(IODBUG,608)TIME
  608 FORMAT(1H0,10X,'TIME= ',G10.3)
      IF(TIME.LE.0.0)GO TO 195
C
      LCN=LCN-1
      IF(LCN.LT.6)GO TO 220
      DO 210 I=6,LCN
      L=LCN-I+6
      IF(L+2.GT.MXCON)GO TO 220
      CN(L+2)=CN(L)
  210 CONTINUE
C
  220 CN(6)=CN(2)
      CN(7)=TIME
C
C     ........ ZERO THE REST OF LAG CARRYOVER NOT FILLED ABOVE .........
C
      IF(LCN+2.GE.MXCON)GO TO 195
      LCNP3=LCN+3
      NVAL=MXCON-LCNP3+1
      Z=0.
      IF(IB.EQ.1)WRITE(IODBUG,609)LCNP3,NVAL
  609 FORMAT(1H0,10X,'ABOUT TO ZERO REMAINDER OF NEW C ARRAY - ',
     1  'LCNP3,NVAL= ',2I5)
      CALL UMEMST (Z,CN(LCNP3),NVAL)
C
  195 IF(IB.EQ.1)WRITE(IODBUG,600)LCN,LAG
  600 FORMAT(1H0,10X,6HLCN = ,I5,8H, LAG = ,I5)
C.......................................................................
C
C      SEE IF K CO NEEDS TO BE TRANSFERED
C..........................
C
  200 IBKO=PO(18)
      IBKO1=IBKO+1
      IBKN=PN(18)
      IBKN1=IBKN+1
C
      IF(.NOT.FOP7(PO(IBKO),PO(IBKO1)).OR.
     1   .NOT.FOP7(PN(IBKN),PN(IBKN1)))GO TO 300
C
C     ......... K ENABLED - SEE IF OLD AND/OR NEW INFLOW ARE MEANQ .....
C
      DTYPIN=PO(4)
      CALL FDCODE(DTYPIN,STDUNT,INDIMS,MSNG,NUPDT,TSCALE,NEXTRA,IER)
      MEANQO=.FALSE.
      IF(INDIMS.EQ.L3)MEANQO=.TRUE.
      DTYPIN=PN(4)
      CALL FDCODE(DTYPIN,STDUNT,INDIMS,MSNG,NUPDT,TSCALE,NEXTRA,IER)
      MEANQN=.FALSE.
      IF(INDIMS.EQ.L3)MEANQN=.TRUE.
C
      IF(IB.EQ.1)WRITE(IODBUG,604)MEANQO,MEANQN
  604 FORMAT(1H0,10X,'MEANQO = ',L4,', MEANQN = ',L4)
C
C     ......... TRANSFER CURRENT INSTANTANEOUS OUTFLOW AND
C        CURRENT STORAGE CARRYOVER VALUES ......................
C
      CN(3)=CO(3)
      CN(4)=CO(4)
C
C    ....... GO TO SEPARATE CODING FOR EACH COMBINATION OF MEAN AND
C            INSTANTANEOUS INFLOWS ....................................
C
      IF(MEANQO.OR.MEANQN)GO TO 250
C
C     ..... BOTH OLD AND NEW INFLOW ARE INSTANTANEOUS HERE ........
C
C     ....... TRANSFER OLD CURRENT INFLOW TO NEW CURRENT INFLOW ......
C
      CN(2)=CO(2)
C
      GO TO 300
C
  250 IF(.NOT.MEANQO.OR.MEANQN)GO TO 300
C
C     ...... OLD INFLOW IS MEANQ AND NEW INFLOW IS INSTANTANEOUS .......
C
C     ...... ASSUME NEW CURRENT INFLOW EQUAL TO NEW CURRENT OUTFLOW ...
C
      CN(2)=CN(3)
C
C     IF NEW INFLOW IS MEANQ DON'T HAVE TO TRANSFER ANY
C     VALUE INTO NEW CURRENT INFLOW AS THIS IS NOT USED IN
C     ANY COMPUTATIONS
C
C.......................................................................
C
C     PRINT DEBUG INFORMATION
C.......................................................................
C
  300 IF(IB.NE.1)RETURN
C
      NPO=PO(16)
      NCO=CO(1)
      NPN=PN(16)
      NCN=CN(1)
C
      WRITE(IODBUG,601)
  601 FORMAT(1H0,10X,18HOLD P AND C ARRAYS)
C
      CALL FPRPC7(NPO,PO,NCO,CO)
C
      WRITE(IODBUG,602)
  602 FORMAT(1H0,10X,18HNEW P AND C ARRAYS)
C
      CALL FPRPC7(NPN,PN,NCN,CN)
C
      WRITE(IODBUG,603)
  603 FORMAT(1H0,10X,15H** LEAVING COX7)
C
      RETURN
      END
