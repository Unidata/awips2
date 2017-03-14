C MEMBER XINQ26
C  (from old member FCXINQ26)
C
C DESC DETERMINE INFLOW VALUES FOR TIME PERIOD
C-------------------------------------------------------------------
      SUBROUTINE XINQ26(PO,CO,W,D,IDPT,LOCWS,LOCOWS)
C-------------------------------------------------------------------
C  ROUTINE TO ASSIGN THE INFLOW VALUES FOR THE CURRENT TIME PERIOD IN
C  EXECUTION TIME PERIOD LOOP. VALUES ASSIGNED ARE QI1, QI2, AND QIM.
C  THESE ARE THE INST. INFLOW AT THE BEGINNING OF THE PERIOD, THE INST.
C  INFLOW AT THE END OF THE PERIOD, AND THE PERIOD MEAN INFLOW, RESPEC-
C  TIVELY.  THE VALUES ARE PULLED FROM DIFFERENT LOCATIONS DEPENDENT
C  ON THE COMBINATION OF 'RAINEVAP' AND 'BACKFLOW' USED FOR THE RUN.
C----------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - OCTOBER 1983
C----------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/exg26'
      INCLUDE 'common/xre26'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
C
      DIMENSION PO(1),CO(1),W(1),D(1),IDPT(1),LOCWS(1),LOCOWS(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xinq26.f,v $
     . $',                                                             '
     .$Id: xinq26.f,v 1.4 2005/02/22 13:52:30 hsu Exp $
     . $' /
C    ===================================================================
C
C
C  DEBUG TRACE IF REQUESTED
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XINQ26 ***')
C
C  SET POINTERS FIRST
C
      LTSQI = IDPT(1) + IDOFST*NTIM24
      LTSQIM = IDPT(2) + IDOFST*NTIM24
      LBFQI = LOCOWS(1)
      LBFQIM = LBFQI + NDD*NTIM24
C
C-----------------------------------------------------------------------
C  CHECK ON COMBINATION OF 'RAINEVAP' AND 'BACKFLOW'
C
      IF (DORAIN) GO TO 2000
      IF (DOBACK) GO TO 1000
C
C  HERE WE ARE NOT USING EITHER 'RAINEVAP' OR 'BACKFLOW', SO INFLOW
C  VALUES ARE JUST PULLED FROM THE TIME-SERIES WITH THE MEAN VALUE
C  CONVERTED FROM CMSD TO TIMD.
C
      QI1 = D(LTSQI+NS2-2)
      QI2 = D(LTSQI+NS2-1)
      QIM = D(LTSQIM+NS2-1) * NTIM24
C
C  THAT'S ALL FOR THIS SITUATION
C
      GO TO 5000
C
C---------------------------------------------------------------------
C  HERE WE HAVE ADJUSTED INFLOWS COMPUTED BY 'BACKFLOW' ONLY. WE PULL
C  VALUES FROM THE WORK SPACE FOR ADJUSTED INFLOWS.
C
 1000 CONTINUE
C
      QI1 = W(LBFQI+NS2-2)
      QI2 = W(LBFQI+NS2-1)
      QIM = W(LBFQIM+NS2-1)
C
C  THAT'S IT FOR THIS SITUATION.
C
      GO TO 5000
C
C------------------------------------------------------------------
C  HERE WE HAVE 'RAINEVAP' USED. CHECK TO SEE IF 'BACKFLOW' IS ALSO
C  USED.
C
 2000 CONTINUE
C
      IF (DOBACK) GO TO 3000
C
C 'RAINEVAP' IS USED ALONE. TO COMPUTE WE ADD THE ADD'L Q TO THE TIME
C  SERIES VALUES. (MEAN TS VALUE MUST BE CONVERTED FIRST.)
C
      QI1 = D(LTSQI+NS2-2) + READLQ
      QI2 = D(LTSQI+NS2-1) + READLQ
      QIM = D(LTSQIM+NS2-1)*NTIM24 + READLQ
C
C  THAT'S IT FOR THSI SITUATION.
C
      GO TO 5000
C
C-------------------------------------------------------------
C  HERE 'RAINEVAP' AND 'BACKFLOW' ARE BOTH USED.
C  FIRST CHECK TO SEE IF THIS IS AN ADJUST RUN.
C
 3000 CONTINUE
      IF (ADJRUN) GO TO 4000
      GO TO 3500
C
C  THIS IS A SIMULATED RUN. SEE WHICH PASS THROUGH THE SIMULATED WE'RE
C  AT.
C
      IF (NUMSIM .EQ. 2) GO TO 3500
C
C  FIRST SIMULATED PASS. ADD ADD'L FLOW TO TIME SERIES VALUES.
C
      QI1 = D(LTSQI+NS2-2) + READLQ
      QI2 = D(LTSQI+NS2-1) + READLQ
      QIM = D(LTSQIM+NS2-1)*NTIM24 + READLQ
C
C  THAT'S IT FOR THIS SITUATION.
C
      GO TO 5000
C
C-------------------------------------------------------
C  SECOND SIMULATED PASS. ADD ADD'L FLOW TO BASE FLOWS HELD IN WORK
C  SPACE FOR 'BACKFLOW'.
C
 3500 CONTINUE
C
C      QI1 = W(LBFQI+NS2-2) + READLQ
C      QI2 = W(LBFQI+NS2-1) + READLQ
C      QIM = W(LBFQIM+NS2-1) * NTIM24 + READLQ
      REX= READLQ
      IF(NS2.LE.LOBSA) REX= 0.0
      QI1 = W(LBFQI+NS2-2) + REX
      QI2 = W(LBFQI+NS2-1) + REX
      QIM = W(LBFQIM+NS2-1) + REX
C
C  THAT'S IT FOR THIS SITUATION.
C
      GO TO 5000
C
C--------------------------------------------------------------
C  HERE WE HAVE AN ADJUST RUN. PULL INFLOW VALUES FROM WORK SPACE FOR
C  'BACKFLOW'.
C
 4000 CONTINUE
C
C      QI1 = W(LBFQI+NS2-2)
C      QI2 = W(LBFQI+NS2-1)
C      QIM = W(LBFQIM+NS2-1) * NTIM24
      REX= READLQ
      IF(NS2.LE.LOBSA) REX= 0.0
      QI1 = W(LBFQI+NS2-2) + REX
      QI2 = W(LBFQI+NS2-1) + REX
      QIM = W(LBFQIM+NS2-1) + REX
C
C---------------------------------------------------------------
C  ONE LAST CHECK MUST BE MADE.
C  IF WE'RE AT THE FIRST PERIOD, QI1 IS THE CARRYOVER VALUE AND THE
C  MEAN PERIOD FLOW MUST BE COMPUTED.
C
 5000 CONTINUE
      IF (NS2 .NE. 1) GO TO 9000
C
      QI1 = CO(1)
      QIM = (QI1 + QI2)/2.0
C
C----------------------------------
C  ALL DONE.
C
 9000 CONTINUE
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XINQ26 ***')
      RETURN
      END
