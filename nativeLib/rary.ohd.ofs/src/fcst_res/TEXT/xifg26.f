C MODULE XIFG26
C
C DESC DETERMINE RESULT OF 'IF' EXPRESSION.
C--------------------------------------------------------------------
      SUBROUTINE XIFG26(PO,LOCIFG,W,LOCWS,LOCOWS,TRUEIF)
C----------------------------------------------------------------------
C  SUBROUTINE TO EVALUATE THE OUTCOME OF AN IF GROUP'S COMPARISONS.
C
C  COMPARISONS ARE OF THREE TYPES:
C     1) A,B,RELOP TYPE (E.G. IF(A.GT.B) )
C     2) LOGICAL TYPE, ON/OFF SITUATION
C     3) COMBINATION, TWO EXPRESSIONS COMBINED WITH EITHER AN 'OR' OR
C        AN 'AND'.
C--------------------------------------------------------------------
C  WRITTEN BY -JOE OSTROWSKI - AUGUST 1983
C--------------------------------------------------------------------
C
      INCLUDE 'common/exg26'
      INCLUDE 'common/resv26'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sumi26'
      INCLUDE 'common/srcg26'
      INCLUDE 'common/flas26'
      INCLUDE 'common/genq26'
C
      DIMENSION PO(1),W(1),LOCWS(1),LOCOWS(1),STACK(20)
C
      LOGICAL TRUEIF,STACK,RZLT,RCHK26,ICHK26
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xifg26.f,v $
     . $',                                                             '
     .$Id: xifg26.f,v 1.4 2000/03/13 21:07:07 page Exp $
     . $' /
C    ===================================================================
C
C
C  DO TRACE
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XIFG26 ***')
C
C  INITIALIZE OUTPUT AND COUNTER VARIABLES
C    THE FIRST VALUE IN THE IF GROUP IS THE NO. OF WORDS IN THE GROUP.
C    WE PROCESS THE GROUP UNTIL THE LAST WORD.
C
      PRDCHG=0.001*MINODT
      TRUEIF = .FALSE.
      NWIF = PO(LOCIFG)
      LASTIF = LOCIFG + NWIF
      NSTACK = 0
C
C--------------------------------------------------------------------
C  INCREMENT THE IF WORD BY ONE. IF WE'RE PAST THE END, JUST SET THE
C  TEST RESULT.
C
C  IF NOT THE END, DETERMINE TYPE OF COMPARISON TO BE MADE AND SEND
C  CONTROL TO THE PROPER SUBSECTION.
C
   10 CONTINUE
      LOCIFG = LOCIFG + 1
      IF (LOCIFG.GT.LASTIF) GO TO 9000
C
      IVAL = PO(LOCIFG)
      IF (IVAL.LT.500) GO TO 1000
      IF (IVAL.GT.550) GO TO 500
C
C-----------------------------------
C  THE NEXT CODE INDICATES AN A,B,RELOP TYPE COMPARISON.
C  SET LOCATION OF CMPV IN PO ARRAY. RELATIVE POINTER IS HELD AS NEG. NO
C  GET VALUE OF 'B' IN RELATION AND 'RELOP'
C
      ILCMP = - PO(LOCIFG+1)
      LOCMP = IOFCMP + ILCMP
      CALL XGCV26(PO,LOCMP,W,LOCOWS,UVALUE,IVALU)
      RELOP = PO(LOCIFG+2)
      LOCIFG = LOCIFG + 2
      ITYCHK = 0
C
C-----------------------------------
C  THERE ARE SEVEN TYPES OF COMPARISONS. SEND CONTROL TO CORRECT ONE.
C
      IVAL = IVAL-500
      GO TO (110,120,130,140,150,160,170), IVAL
C
C-----------------------------
C  INSTANTANEOUS DISCHARGE
C
  110 CONTINUE
      SVAL = QO2
      IF (NSCHEX.EQ.0) SVAL = QO1
      GO TO 200
C
C------------------------------
C  INSTANTANEOUS INFLOW
C
  120 CONTINUE
      SVAL = QI2
      GO TO 200
C
C------------------------------
C  POOL ELEVATION
C
  130 CONTINUE
      SVAL = ELEV2
      IF (NSCHEX.EQ.0) SVAL = ELEV1
      GO TO 200
C
C------------------------------
C  STORAGE CONTENTS
C
  140 CONTINUE
      SVAL = S2
      IF (NSCHEX.EQ.0) SVAL = S1
      GO TO 200
C
C-----------------------------
C  JULIAN DAY OF YEAR
C
  150 CONTINUE
      ITYCHK = 1
      LVAL = JULDAY
      GO TO 200
C
C-----------------------------
C  MEAN DISCHARGE
C
  160 CONTINUE
      SVAL = QOM
      IF (NSCHEX.NE.0) GO TO 200
      LOCQOM = LOCWS(4)
      SVAL = W(LOCQOM)
      GO TO 200
C
C------------------------------
C  MEAN INFLOW
C
  170 CONTINUE
      SVAL = QIM
      GO TO 200
C
C------------------------------------------------------------
C  GET COMPARISON RESULT
C
  200 CONTINUE
      IF (ITYCHK.EQ.0) RZLT = RCHK26(SVAL,UVALUE,RELOP)
      IF (ITYCHK.EQ.1) RZLT = ICHK26(LVAL,IVALU,RELOP)
C
C  PUSH RESULT ON TO TOP OF STACK
C
      CALL XPUS26(RZLT,STACK,NSTACK)
      GO TO 10
C
C--------------------------------------------------------------------
C  TYPE 2 COMPARISON (LOGICAL TYPE VARIABLE). JUST SET RESULT AND PUSH
C  ON TO STACK.
C
  500 CONTINUE
      IVAL = IVAL - 550
      GO TO (510,520,530,540,550,560,570,580,590,600) , IVAL
C
C-------------------------
C  'FLOOD' VARIABLE
C
  510 CONTINUE
      RZLT = FLOOD
      GO TO 700
C
C-------------------------
C  'SURCHARGE' VARIABLE
C
  520 CONTINUE
      RZLT = SURCHG
      GO TO 700
C
C------------------------
C  'FORECAST' VARIABLE
C
  530 CONTINUE
      RZLT = FCST
      GO TO 700
C
C------------------------
C  'GOFLASH' VARIABLE
C
  540 CONTINUE
      RZLT = GOFLSH
      GO TO 700
C
C------------------------
C  'NFLOOD' VARIABLE
C
  550 CONTINUE
      RZLT = .NOT.FLOOD
      GO TO 700
C
C-------------------------
C  'NSURCHARGE' VARIABLE
C
  560 CONTINUE
      RZLT = .NOT.SURCHG
      GO TO 700
C
C-------------------------
C  'NFORECAST' VARIABLE
C
  570 CONTINUE
      RZLT = .NOT.FCST
      GO TO 700
C
C-------------------------
C  'NGOFLASH' VARIABLE
C
  580 CONTINUE
      RZLT = .NOT.GOFLSH
      GO TO 700
C
C---------------------------
C  'RISING' POOL VARIABLE
C
  590 CONTINUE
      RZLT = .FALSE.
      CHANGE = ELEV2 - ELEV1
      IF (NSCHEX.NE.0) GO TO 595
C
cc      LOCEL1 = LOCWS(4) + 4
      LOCEL1 = LOCWS(4) + 3
      ELT1 = W(LOCEL1)
      CHANGE = ELEV1 - ELT1
C
  595 CONTINUE
      IF (CHANGE.GT.PRDCHG) RZLT = .TRUE.
      GO TO 700
C
C----------------------------
C  'FALLING' POOL VARIABLE
C
  600 CONTINUE
      RZLT = .FALSE.
      CHANGE = ELEV2 - ELEV1
      IF (NSCHEX.NE.0) GO TO 605
C
cc      LOCEL1 = LOCWS(4) + 4
      LOCEL1 = LOCWS(4) + 3
      ELT1 = W(LOCEL1)
      CHANGE = ELEV1 - ELT1
C
  605 CONTINUE
cc      IF (CHANGE.LT.0.00) RZLT = .FALSE.
      IF (CHANGE.LT.-PRDCHG) RZLT = .TRUE.
      GO TO 700
C
C------------------------------------------------------------
C  PUSH COMPARISON RESULT ON TO TOP OF STACK
C
  700 CONTINUE
      CALL XPUS26(RZLT,STACK,NSTACK)
      GO TO 10
C
C---------------------------------------------------------------
C  TYPE 3 COMPARISON. A COMBINATOR HAS BEEN FOUND.
C
C  WE NEED TO GET THE TOP TWO ELEMENTS OF THE STACK, AND EVALUATE
C  THE EXPRESSION, AND PUSH THIS RESULT ON TO  THE TOP OF THE STACK.
C
 1000 CONTINUE
      IF (IVAL.GT.140) GO TO 1100
C
C  COMBINATOR IS 'OR'
C
      RZLT = STACK(1) .OR. STACK(2)
      GO TO 1150
C
C  COMBINATOR IS 'AND'
C
 1100 CONTINUE
      RZLT = STACK(1) .AND. STACK(2)
C
C  TAKE TOP TWO ELEMENTS OF STACK AND PUSH RESULT ON TOP.
C
 1150 CONTINUE
      CALL XPOP26(STACK,NSTACK)
      CALL XPOP26(STACK,NSTACK)
      CALL XPUS26(RZLT,STACK,NSTACK)
C
      GO TO 10
C
C---------------------------------------------------------------------
C  WE'VE REACHED THE END OF THE IF GROUP. THE FINAL RESULT IS THE
C  VALUE AT THE TOP OF THE STACK.
C
 9000 CONTINUE
      TRUEIF = STACK(1)
C
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XIFG26 ***')
C
      RETURN
      END
