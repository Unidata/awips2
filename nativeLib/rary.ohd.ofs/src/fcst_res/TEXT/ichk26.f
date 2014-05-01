C MEMBER ICHK26
C  (from old member FCICHK26)
C
C DESC FUNCTION FOR EVALUATING A.GT.B TYPE INTEGER RELATION
C--------------------------------------------------------------------
      LOGICAL FUNCTION ICHK26(ILEFT,IRIGHT,RELOP)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/ichk26.f,v $
     . $',                                                             '
     .$Id: ichk26.f,v 1.1 1995/09/17 19:05:46 dws Exp $
     . $' /
C    ===================================================================
C
C--------------------------------------------------------------------
C  FUNCTION FOR EVALUATING AN (A.GT.B) TYPE COMPARISON FOR EXECUTING
C  RCL. (THE EQUIVALENT FUNCTION FOR REAL VALUES IS RCHK26).
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
      ICHK26 = .FALSE.
      ILOC = RELOP - 163
      GO TO (10,20,30,40,50,60), ILOC
C
C  .GT.
C
   10 CONTINUE
      IF (ILEFT.GT.IRIGHT) ICHK26 = .TRUE.
      GO TO 90
C
C  .LE.
C
   20 CONTINUE
      IF (ILEFT.LE.IRIGHT) ICHK26 = .TRUE.
      GO TO 90
C
C  .GE.
C
   30 CONTINUE
      IF (ILEFT.GE.IRIGHT) ICHK26 = .TRUE.
      GO TO 90
C
C  .LT.
C
   40 CONTINUE
      IF (ILEFT.LT.IRIGHT) ICHK26 = .TRUE.
      GO TO 90
C
C  .NE.
C
   50 CONTINUE
      IF (ILEFT.NE.IRIGHT) ICHK26 = .TRUE.
      GO TO 90
C
C  .EQ.
C
   60 CONTINUE
      IF (ILEFT.EQ.IRIGHT) ICHK26 = .TRUE.
      GO TO 90
C
   90 CONTINUE
      RETURN
      END
