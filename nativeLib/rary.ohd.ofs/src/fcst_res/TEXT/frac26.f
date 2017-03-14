C MEMBER FRAC26
C  (from old member FCFRAC26)
C***********************************************************************
      FUNCTION FRAC26(DIFQI,DIFQOK,QI1,QO1,QOK1,QSFRAC,S1,SFILL,IBUG)
C***********************************************************************
C FUNCTION FRAC26 COMPUTES THE FRACTION OF THE TIME PERIOD IN SUBROUTINE
C OVER26 THAT IS REQUIRED BEFORE ROUTING BEGINS (RISING POOL) OR ROUTING
C ENDS (FALLING POOL).  THE QUADRATIC EQUATION IS USED TO SOLVE THE
C EQUATION.  THE EQUATION IS IN THE FORM (A*X**2+B*X+C=0.).  X IS THE
C DESIRED FRACTION OF THE TIME PERIOD.
C***********************************************************************
C THIS FUNCTION WAS ORIGINALLY PROGRAMMED BY
C     WILLIAM E. FOX -- CONSULTING HYDROLOGIST
C     DECEMBER, 1981
C***********************************************************************
C FUNCTION FRAC26 IS IN
C***********************************************************************
C VARIABLES IN THE ARGUMENT LIST ARE AS FOLLOWS:
C     DIFQI -- INFLOW AT END MINUS INFLOW AT BEGINNING OF TIME PERIOD.
C     DIFQOK - NON-SPILLWAY DISCHARGE AT END MINUS NON-SPILLWAY
C       DISCHARGE AT BEGINNING OF TIME PERIOD.
C     QI1 -- INFLOW AT BEGINNING OF TIME PERIOD.
C     QO1 -- TOTAL DISCHARGE AT BEGINNING OF TIME PERIOD.
C     QOK1 -- NON-SPILLWAY DISCHARGE AT BEGINNING OF TIME PERIOD.
C     QSFRAC -- SPILLWAY DISCHARGE AT TIME POOL IS FILLED TO (OR FALLS
C       TO) LEVEL OF PASSING INFLOW OR UNCONTROLLED SPILLWAY CREST.
C     S1 -- POOL STORAGE AT BEGINNING OF TIME PERIOD.   UNITS MUST BE
C       MEAN DISCHARGE FOR TIME PERIOD.
C     SFILL -- STORAGE AT POOL LEVEL FOR PASSING INFLOW FOR GATED
C       SPILLWAY OR STORAGE AT CREST ELEVATION FOR UNCONTROLLED SPILL-
C       WAY.  SAME UNITS AS S1.
C     IBUG -- NO TRACE OR DEBUG (IBUG=0), TRACE ONLY (IBUG=1),TRACE AND
C       DEBUG (IBUG=2).
C
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/ERRDAT/IOERR,NWARN,NERRS
      COMMON/WHERE/ISEG(2),IOPNUM,OPNAME(2)
C ADDITION FOR MAINTENANCE 290
      COMMON/NWRN26/NWFRAC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/frac26.f,v $
     . $',                                                             '
     .$Id: frac26.f,v 1.1 1995/09/17 19:05:44 dws Exp $
     . $' /
C    ===================================================================
C
      IF(IBUG-1)50,10,20
   10 WRITE(IODBUG,30)
      GO TO 50
   20 WRITE(IODBUG,30)
   30 FORMAT(1H0,10X,17H** FRAC26 ENTERED)
      WRITE(IODBUG,40)DIFQI,DIFQOK,QI1,QO1,QOK1,QSFRAC,S1,SFILL,IBUG
   40 FORMAT(1H0,47H DIFQI,DIFQOK,QI1,QO1,QOK1,QSFRAC,S1,SFILL,IBUG/1X,
     $8F12.3,I6)
C***********************************************************************
C COMPUTE A, B, ANC C.
C***********************************************************************
   50 A=DIFQI-DIFQOK
      B=2.*QI1-QO1-QOK1-QSFRAC
      C=-2.*(SFILL-S1)
C***********************************************************************
C CHECK IF A IS OR IS NOT EQUAL TO 0.
C***********************************************************************
      IF(A.NE.0.) GO TO 60
C***********************************************************************
C A IS EQUQL TO 0.  SOLVE THE EQUATION: BX+C=0.
C***********************************************************************
      X=-C/B
      GO TO 70
C***********************************************************************
C WHEN A IS NOT EQUAL TO 0., SOLVE THE QUADRATIC EQUATION:
C X=(-B-(B*B-4*A*C)**0.5)/(2.*A) AND SEE IF THE ROOT IS BETWEEN 0.0 AND
C 1.0.  (B*B-4*A*C) MUST FIRST BE CHECKED FOR A NEGATIVE VALUE.
C***********************************************************************
   60 Y=B*B-4*A*C
      IF(Y.LT.0.) GO TO 80
      X=(-B-(B*B-4*A*C)**0.5)/(2.*A)
      IF(X.GE.0.0.AND.X.LE.1.0) GO TO 110
C***********************************************************************
C SINCE X IS NOT BETWEEN 0. AND 1.0, SOLVE THE QUADRATIC EQUATION:
C X=(-B+(B*B-4*A*C)**0.5)/(2.*A)
C***********************************************************************
      X=(-B+(B*B-4*A*C)**0.5)/(2.*A)
   70 IF(X.GE.0.0.AND.X.LE.1.0) GO TO 110
80    CONTINUE
      IF (X.LE.0.0) X=0.
      IF (X.GT.0.0) X=1.0
C EJV MODIFY FOR MAINTENANCE 290
C ONLY PRINT FOLLOWING WARNING ONCE PER SEGMENT (VARIABLE NWFRAC
C KEEPS TRACK OF NUMBER OF WARNINGS).  ALSO DONT PRINT THE WARNING
C IF C=0 AND DIFQ1 LE 0 WHICH MEANS THAT POOL IS NOT HI ENOUGH
C FOR SPILLWAY AND INFLOW IS DROPPING SO FRACTION SHOULD BE 0 FOR
C NO SPILLWAY THIS TIME PERIOD
      IF (ABS(C).LT.0.00001 .AND. DIFQI.LE.0.0) GO TO 110
      NWFRAC=NWFRAC+1
      IF (NWFRAC.GT.1) GO TO 110
      WRITE(IPR,90) X
90    FORMAT(1H0,10X,'**WARNING** IN FRAC26-COMPUTATION OF FRACTION',
     $ 'OF TIME DAM IS IN SPILWAY MODE WASNT IN RANGE 0-1 FOR'
     $ /10X,'GIVEN TIME PERIOD.  SO FRACTION IS SET EQUAL TO',
     $ F4.1,'...ITS ALL OK, NO NEED TO WORRY BABY')
      CALL WARN
C END EJV MODIFY
  110 FRAC26=X
      IF(IBUG-1)160,140,120
  120 WRITE(IODBUG,130)X
  130 FORMAT(1H0,27H FRACTION OF TIME PERIOD IS,F12.3)
  140 WRITE(IODBUG,150)
  150 FORMAT(1H0,10X,17H** LEAVING FRAC26)
  160 RETURN
      END
