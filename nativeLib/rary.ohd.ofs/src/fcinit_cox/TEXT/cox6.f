C MEMBER COX6
C  (from old member FCCOX6)
C
      SUBROUTINE COX6(PO,CO,PN,CN,WO,LW)
C.......................................................................
C     THIS IS THE CARRYOVER TRANSFER SUBROUTINE FOR THE
C     MEAN DISCHARGE OPERATION.  NEW CARRYOVER (CN) VALUES ARE
C     DETERMINED FROM OLD CARRYOVER (CO) AND OLD AND NEW
C     PARAMETERS (PO AND PN).  IF THE TIME INTERVAL OF THE MEAN OR
C     INSTANTANEOUS DISCHARGE TIME SERIES HAS CHANGED, THE LENGTH
C     OF THE CARRYOVER ARRAY MAY CHANGE.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL - HRL - NOVEMBER 1979   VERSION 1
C.......................................................................
      DIMENSION PO(1), CO(1), PN(1), CN(1), SNAME(2), WO(1)
C
C     COMMON BLOCKS.
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox6.f,v $
     . $',                                                             '
     .$Id: cox6.f,v 1.1 1995/09/17 18:47:28 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HCOX6,4H    /
C.......................................................................
C     CHECK TRACE LEVEL AND DEBUG OUTPUT OPTION
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      JTRACE=1
      IOPNUM=6
      CALL FPRBUG(SNAME,JTRACE,IOPNUM,IBUG)
      IPO=PO(11)
      IDTQ=PO(5)
      IDTQN=PN(5)
      IDTQB=PO(9)
      IDTQBN=PN(9)
      NCOMAX=IDTQB/IDTQ
      NCONEW=IDTQBN/IDTQN
      IF(IBUG.EQ.0) GO TO 100
C     DEBUG OUTPUT - OLD AND NEW P AND C ARRAYS.
      WRITE(IODBUG,900)
  900 FORMAT(1H0,40HOLD AND NEW PARAMETERS AND OLD CARRYOVER)
      WRITE(IODBUG,902) (PO(I),I=1,IPO)
  902 FORMAT(1H0,15F8.3)
      WRITE(IODBUG,902) (PN(I),I=1,IPO)
      WRITE(IODBUG,902) (CO(I),I=1,NCOMAX)
  100 CONTINUE
      NCOADJ=IDTQB/IDTQN
      ICHECK=0
C
      IF(LW.GE.NCOADJ) GO TO 102
      WRITE(IPR,904)NCOADJ,LW
  904 FORMAT(1H0,19X,52H**WARNING** MORE CARRYOVER WORKING SPACE IS NEED
     1ED (,I4,21H) THAN IS AVAILABLE (,I4,2H)./
     2  23X,39HNO CARRYOVER VALUES CAN BE TRANSFERRED.)
      CALL WARN
      GO TO 308
  102 CONTINUE
C.......................................................................
C     CHECK TO SEE IF IDTQ WAS CHANGED
      IF(IDTQ.NE.IDTQN) GO TO 111
      DO 101 I=1,NCOMAX
      WO(I)=CO(I)
  101 CONTINUE
      GO TO 110
  111 IF((IDTQ/IDTQN)*IDTQN.EQ.IDTQ) GO TO 540
      IF((IDTQN/IDTQ)*IDTQ.EQ.IDTQN) GO TO 520
      WRITE(IPR,910) IDTQ,IDTQN
  910 FORMAT(1H0,10X,28H**WARNING** NEITHER THE OLD(,I2,
     121H HOURS) NOR THE NEW (,I2,43H HOURS) DISCHARGE TIME SERIES TIME
     2INTERVAL/23X,33HIS AN EVEN MULTIPLE OF THE OTHER./23X,39HNO CARRYO
     3VER VALUES CAN BE TRANSFERRED.)
      CALL WARN
      NCONEW=NCOMAX
      GO TO 308
C.......................................................................
C     COMPUTE ADJUSTED Q'S FOR IDTQN GT IDTQ
C
  520 CONTINUE
      ICHECK=1
      DO 522 I=1,NCOADJ
      WO(I)=0.0
  522 CONTINUE
      IRATIO=IDTQN/IDTQ
      K=1
      DO 524 J=1,NCOMAX,IRATIO
      WO(K)=CO(J)
      K=K+1
  524 CONTINUE
      GO TO 110
C.......................................................................
C     COMPUTE ADJUSTED Q'S FOR IDTQN LT IDTQ
C
  540 CONTINUE
      ICHECK=2
      DO 544 I=1,NCOADJ
      WO(I)=0.0
  544 CONTINUE
      NRATIO=IDTQ/IDTQN
      J=NRATIO-1
      K=1
      DO 550 M=1,NCOMAX
      N=M+1
      WO(K)=CO(M)
      IF(M.EQ.NCOMAX) GO TO 570
      DO 560 I=1,J
      K=K+I
      VAL=CO(N)
      IF(IFMSNG(VAL).EQ.0) GO TO 546
      WO(K)=-999.
      GO TO 560
  546 WO(K)=((CO(N)-CO(M))/NRATIO*I)+CO(M)
  560 CONTINUE
  561 CONTINUE
      K=K+1
  550 CONTINUE
  570 CONTINUE
      DO 580 I=1,J
      K=K+I
      WO(K)=-999.
  580 CONTINUE
  110 CONTINUE
C.......................................................................
C     CHECK FOR NO. OF VALUES TO BE STORED
C
      IF(IDTQBN.EQ.IDTQB) GO TO 700
      IF(IDTQBN.LT.IDTQB) GO TO 650
C
C.......................................................................
C     STORE CARRYOVER FOR IDTQBN GT IDTQB
C
      DO 601 I=1,NCONEW
      CN(I)=-999.
  601 CONTINUE
      GO TO 400
C.......................................................................
C     STORE CARRYOVER FOR IDTQBN LT IDTQB
C
  650 CONTINUE
      DO 660 I=1,NCOMAX
      J=NCOMAX-I+1
      C=CO(J)
      IF(IFMSNG(C).EQ.0) GO TO 665
  660 CONTINUE
  665 CONTINUE
      NLOC=J-IDTQBN/IDTQ
      IF(ICHECK.NE.0) GO TO 810
      LOCADJ=NLOC
      GO TO 850
  810 IF(ICHECK.NE.1) GO TO 820
      LOCADJ=NLOC/IRATIO
      GO TO 850
  820 LOCADJ=NLOC*NRATIO
  850 CONTINUE
      DO 870 I=1,NCONEW
      K=LOCADJ+I
      CN(I)=WO(K)
  870 CONTINUE
      GO TO 400
C.......................................................................
C     STORE CARRYOVER FOR IDTQBN EQ IDTQB
C
  700 CONTINUE
      DO 710 I=1,NCONEW
      CN(I)=WO(I)
  710 CONTINUE
  400 CONTINUE
      IF(IBUG.EQ.0) GO TO 600
C.......................................................................
C     DEBUG OUTPUT - ADJUSTED DISCHARGES AND NEW CARRYOVER VALUES
      IF(IDTQN.EQ.IDTQ) GO TO 310
      WRITE(IODBUG,920)
  920 FORMAT(1H0,25HADJUSTED DISCHARGE VALUES)
      WRITE(IODBUG,902) (WO(I),I=1,NCOADJ)
  308 IF(IBUG.EQ.0) GO TO 600
  310 WRITE(IODBUG,922)
  922 FORMAT(1H0,20HNEW CARRYOVER VALUES)
      WRITE(IODBUG,902) (CN(I),I=1,NCONEW)
C.......................................................................
C     CARRYOVER TRANSFER RULES
C
C     1. CARRYOVER VALUES ARE INST. DISCHARGE VALUES.
C
C     2. IF IDTQN.NE.IDTQ :
C
C        A. IF IDTQN IS A MULTIPLE OF IDTQ, DISCHARGE VALUES ARE
C           ADJUSTED BY SAVING ONLY THE VALUES WHICH OCCUR AT THE
C           END OF THE NEW TIME INTERVALS.
C        B. IF IDTQ IS A MULTIPLE OF IDTQN, DISCHARGE VALUES ARE
C           ADJUSTED BY LINEARLY INTERPOLATING BETWEEN EXISTING
C           VALUES.
C        C. IF NEITHER IDTQN NOR IDTQ IS A MULTIPLE OF THE OTHER,
C           CARRYOVER IS NOT TRANSFERRED.
C
C     3. THE NUMBER OF NEW CARRYOVER VALUES IS IDTQBN/IDTQN.
C
C        A. IF IDTQBN.GT.IDTQB, ALL CARRYOVER VALUES EQUAL -999.
C        B. IF IDTQBN.LT.IDTQB, THE LAST IDTQBN/IDTQ POSITIVE VALUES
C           ARE SAVED AS CARRYOVER.
C        C. IF IDTQBN.EQ.IDTQB, ALL THE ADJUSTED VALUES BECOME THE NEW
C           CARRYOVER VALUES.
C
C.......................................................................
  600 RETURN
      END
