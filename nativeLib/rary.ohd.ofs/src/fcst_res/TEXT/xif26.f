C MEMBER XIF26
C  (from old member FCXIF26)
C
C DESC PROCESS 'IF' GROUP (INCLUDING ALL EMBEDDED 'IF''S).
C--------------------------------------------------------------------
      SUBROUTINE XIF26(PO,IPTR,W,D,IDPT,LOCWS,LOCOWS)
C--------------------------------------------------------------------
C  SUBROUTINE TO PROCESS AN IF GROUP AND ANY OTHER IMBEDDED IF'S
C
C  NO CHECKS NEED TO BE MADE ON VALIDITY OF RCL CODES AS THEY
C  (HOPEFULLY) HAVE BEEN MADE IN THE PIN ROUTINE.
C--------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - HRL - JULY 1983
C--------------------------------------------------------------------
C
      INCLUDE 'common/resv26'
      INCLUDE 'common/exg26'
      INCLUDE 'common/fdbug'
C
      DIMENSION PO(1),W(1),D(1),IDPT(1),LOCWS(1),LOCOWS(1)
      LOGICAL TRUEIF
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/xif26.f,v $
     . $',                                                             '
     .$Id: xif26.f,v 1.1 1995/09/17 19:06:38 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IBUG.GE.1) WRITE(IODBUG,1600)
 1600 FORMAT('   *** ENTER XIF26 ***')
C
      NSTACK = 0
C
C  ENTRY INTO THIS SUBROUTINE INDICATES THE DISCOVERY OF AN 'IF'
C  STATEMENT. GO DIRECTLY TO THAT PROCESSING SECTION.
C
      GO TO 100
C
C--------------------------------------------------------------------
C  WE MUST FIRST CONVERT THE RCL STATEMENT CODES INTO A MORE MANAGEABLE
C  RANGE OF NUMBERS FOR THE COMPUTED GOTO. THE ORIGINAL CODE NUMBERS AND
C  THE CONVERTED NUMBERS ARE:
C        DO -  5   2
C        IF - 10   1
C      ELSE - 12   3
C    ELSEIF - 13   4
C     ENDIF - 19   6
C
C  NO GOTO NUMBER OF FIVE SHOULD BE FOUND, SO JUST EXIT IF THAT IS FOUND
C  OF COURSE, THE PIN ROUTINE WILL SEE TO THAT.
C
   50 CONTINUE
      ICODE = PO(IPTR)
      MOD1 = MOD(ICODE,10)
      LOCK = MOD(MOD1,5) + MOD1/5 + 1
C
      GO TO (100,200,300,101,9000,600) , LOCK
C
C---------------------------------------------------------------------
C  'IF' STATEMENT PROCESSING.
C     1) INCREMENT STACK,
C     2) CHECK OUTCOME OF IF GROUP COMPARISON TEST
C
  100 CONTINUE
      NSTACK = NSTACK + 1
C
C---------------------------------------------------------------------
C  'ELSEIF' STATMENT PROCESSING IS THE SAME AS THE 'IF' PROCESSING
C   EXCEPT THE STACK IS NOT INCREMENTED.
C
  101 CONTINUE
C
      IPTR = IPTR + 1
      LOCIFG = -PO(IPTR) + IFOFF
      CALL XIFG26(PO,LOCIFG,W,LOCWS,LOCOWS,TRUEIF)
C
C  IF THE COMPARISON IS TRUE, REPOSITION THE POINTER TWO SPACES TO THE
C  'TRUE' ACTION CODE.
C
C  IF THE COMPARISON IS FALSE, THE LOCATION OF THE NEXT CODE IS THE
C  ABSOLUTE VALUE OF THE NEXT PO LOCATION PLUS THE RCL OFFSET.
C
      IPTR = IPTR + 2
      IF (.NOT.TRUEIF) IPTR = -PO(IPTR-1) + IOFRCL
C
C  GO PROCESS THE NEXT CODE.
C
      GO TO 50
C
C----------------------------------------------------------------------
C  'DO' STATEMENT PROCESSING. EXECUTE THE S/U AND SET THE POINTER
C   FOR THE NEXT CODE. IF THE VALUE AT THAT LOCATION IS NEGATIVE, GO TO
C   THAT LOCATION. THE CODE AT THAT LOCATION SHOULD BE AN ENDIF.
C
  200 CONTINUE
      IPTR = IPTR + 1
      CALL XDO26(PO(IPTR),PO,W,D,IDPT,LOCWS,LOCOWS)
      IPTR = IPTR + 1
      IF (PO(IPTR).LT.0.0) IPTR = -PO(IPTR) + IOFRCL
      GO TO 50
C
C--------------------------------------------------------------------
C  'ELSE' STATEMENT FOUND. THE ONLY CODE THAT CAN FOLLOW IS THE 'DO'
C   JUST SET THE POINTER TO THE NEXT LOCATION AND PROCESS THE 'DO'.
C
  300 CONTINUE
      IPTR = IPTR + 1
      GO TO 50
C
C--------------------------------------------------------------------
C  'ENDIF' FOUND. DECREMENT THE STACK, AND IF NO MORE OPEN 'IF' GROUPS,
C   JUST EXIT. OTHERWISE, RESET THE POINTER AS IF THIS FOLLOWED A 'DO'.
C
  600 CONTINUE
      NSTACK = NSTACK - 1
      IF (NSTACK.EQ.0) GO TO 9000
C
      IPTR = IPTR + 1
      IF(PO(IPTR).LT.0.0) IPTR = -PO(IPTR) + IOFRCL
      GO TO 50
C
C----------------------------------
C  FINISHED WITH THIS 'IF' GROUP.
C
 9000 CONTINUE
      IF (IBUG.GE.1) WRITE(IODBUG,1699)
 1699 FORMAT('    *** EXIT XIF26 ***')
      RETURN
      END
