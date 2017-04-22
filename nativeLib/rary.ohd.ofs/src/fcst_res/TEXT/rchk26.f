C MEMBER RCHK26
C  (from old member FCRCHK26)
C
C DESC FUNCTION TO EVALUATE A.GT.B TYPE REAL RELATION
C--------------------------------------------------------------------
      LOGICAL FUNCTION RCHK26(SLEFT,SRIGHT,RELOP)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/rchk26.f,v $
     . $',                                                             '
     .$Id: rchk26.f,v 1.1 1995/09/17 19:06:07 dws Exp $
     . $' /
C    ===================================================================
C
C--------------------------------------------------------------------
C  FUNCTION FOR EVALUATING AN (A.GT.B) TYPE COMPARISON FOR EXECUTING
C  RCL. (THE EQUIVALENT FUNCTION FOR INTEGER VALUES IS ICHK26).
C
C  RELATIONAL OPERATORS ARE INDICATED BY THE CODES:
C
C    GT - 164.0
C    LE - 165.0
C    GE - 166.0
C    LT - 167.0
C    NE - 168.0
C    EQ - 169.0
C
C--------------------------------------------------------------------
C  WRITTEN BY - JOE OSTROWSKI - AUGUST 1983
C---------------------------------------------------------------------
C
      RCHK26 = .FALSE.
      ILOC = RELOP - 163
      GO TO (10,20,30,40,50,60), ILOC
C
C  .GT.
C
   10 CONTINUE
      IF (SLEFT.GT.SRIGHT) RCHK26 = .TRUE.
      GO TO 90
C
C  .LE.
C
   20 CONTINUE
      IF (SLEFT.LE.SRIGHT) RCHK26 = .TRUE.
      GO TO 90
C
C  .GE.
C
   30 CONTINUE
      IF (SLEFT.GE.SRIGHT) RCHK26 = .TRUE.
      GO TO 90
C
C  .LT.
C
   40 CONTINUE
      IF (SLEFT.LT.SRIGHT) RCHK26 = .TRUE.
      GO TO 90
C
C  .NE.
C
   50 CONTINUE
      IF (SLEFT.NE.SRIGHT) RCHK26 = .TRUE.
      GO TO 90
C
C  .EQ.
C
   60 CONTINUE
      IF (SLEFT.EQ.SRIGHT) RCHK26 = .TRUE.
      GO TO 90
C
   90 CONTINUE
      RETURN
      END
