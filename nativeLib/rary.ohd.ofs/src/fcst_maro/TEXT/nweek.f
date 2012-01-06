C MEMBER NWEEK
C  (from old member PPGNWEEK)
C
      FUNCTION NWEEK(MO, NDATE, NYR)
C
C.....THIS FUNCTION COMPUTES A WEEK NUMBER (1-52) FOR THE YEAR, USING
C.....THE MONTH, DATE, AND YEAR.
C
C.....THE ARGUMENT LIST IS AS FOLLOWS:
C
C.....MO     - THE NUMBER OF THE MONTH.
C.....NDATE  - THE DATE.
C.....NYR    - THE YEAR.
C
C.....NOTE:  THE YEAR THAT IS PASSED TO THIS FUNCTION FROM SUBROUTINE
C.....       GSETTM IS A 4 DIGIT YEAR. HOWEVER, THE ARGUMENT MAY BE
C.....       EITHER 2 OR 4 DIGITS. THE YEAR IS USED TO DETERMINE IF
C.....       THE YEAR IS A LEAP YEAR OR NOT.
C
C
      DIMENSION IDAYS(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/nweek.f,v $
     . $',                                                             '
     .$Id: nweek.f,v 1.1 1995/09/17 19:02:54 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA IDAYS /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
C
      KP = (NYR/4)*4
      IF(KP .EQ. NYR) IDAYS(2) = 29
      KP = (NYR/100)*100
      IF(KP .EQ. NYR) IDAYS(2) = 28
      KP = (NYR/400)*400
      IF(KP .EQ. NYR) IDAYS(2) = 29
C
      ISUM = 0
      KP = MO - 1
      IF(KP .EQ. 0) GOTO 200
C
      DO 100 NP = 1, KP
      ISUM = ISUM + IDAYS(NP)
  100 CONTINUE
C
  200 ISUM = ISUM + NDATE
      NWEEK = (ISUM/7) + 1
C
      RETURN
      END
