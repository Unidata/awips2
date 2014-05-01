C MEMBER MATH26
C  (from old member FCMATH26)
C
C DESC PERFORM MATHEMATICAL CALCULATIONS ON TWO VALUES
C--------------------------------------------------------------------
      SUBROUTINE MATH26(VALIO,IOP,FACTOR)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_res/RCS/math26.f,v $
     . $',                                                             '
     .$Id: math26.f,v 1.1 1995/09/17 19:05:48 dws Exp $
     . $' /
C    ===================================================================
C
C--------------------------------------------------------------------
C  SUBROUTINE TO PERFORM MATHEMATICAL CALCULATIONS ON TWO VALUES.
C  THE CALCS CAN BE EITHER ADDITION, SUBTRACTION, MULTIPLICATION, OR
C  DIVISION.
C--------------------------------------------------------------------
C
      GO TO (1,2,3,4), IOP
C
C  ADDITION
C----------
C
    1 CONTINUE
      VALIO = VALIO + FACTOR
      GO TO 90
C
C  SUBTRACTION
C-------------
C
    2 CONTINUE
      VALIO = VALIO - FACTOR
      GO TO 90
C
C  MULTIPLICATION
C----------------
C
    3 CONTINUE
      VALIO = VALIO * FACTOR
      GO TO 90
C
C  DIVISION
C----------
C
    4 CONTINUE
      IF (FACTOR.LT.0.000001) FACTOR = 0.01
      VALIO = VALIO / FACTOR
      GO TO 90
C
   90 CONTINUE
      RETURN
      END
