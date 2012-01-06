C MEMBER GDTTST
C  (from old member PPGDTTST)
C
      SUBROUTINE GDTTST(IMO, I, J, IDATE, K, L, IYEAR, M, ISTATC)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_maro/RCS/gdttst.f,v $
     . $',                                                             '
     .$Id: gdttst.f,v 1.1 1995/09/17 19:01:29 dws Exp $
     . $' /
C    ===================================================================
C
C
C.....THIS SUBROUTINE IS CALLED TO CHECK THE VALIDITY OF THE DATE-TIME
C.....GROUPS ENTERED INTO THE MARO FUNCTION. IT CHECKS IF THE MONTH,
C.....DATE, AND YEAR ARE VALID NUMBERS, I.E., WITHIN PERMISSIBLE RANGES.
C
C.....THE ARGUMENT LIST IS:
C
C.....IMO    - THE MONTH TO BE TESTED.
C.....I      - THE LOWER LIMIT OF THE MONTH NUMBER.
C.....J      - THE UPPER LIMIT OF THE MONTH NUMBER.
C.....IDATE  - THE DATE TO BE TESTED.
C.....K      - THE LOWER LIMIT OF THE DATE.
C.....L      - THE UPPER LIMIT OF THE DATE.
C.....IYEAR  - THE YEAR TO BE TESTED.
C.....M      - THE LOWER LIMIT OF THE YEAR (THERE IS CURRENTLY NO
C....          UPPER LIMIT).
C.....ISTATC - A STATUS CODE.
C.....         = 0  NORMAL RETURN. ALL DATES TESTED ARE VALID.
C.....         = 1  ABNORMAL RETURN. ONE OR MORE DATES ARE OUT OF THE
C.....              VALID LIMITS.
C
C.....ORIGINALLY WRITTEN BY:
C
C.....JERRY M. NUNN     WGRFC FT. WORTH, TEXAS     FEBRUARY 12, 1988
C
      ISTATC = 0
C
C.....CHECK THE MONTH.
C
      IF((IMO .LT. I) .OR. (IMO .GT. J)) GOTO 200
C
C.....CHECK THE DATE.
C
      IF(IMO .NE. 2) GOTO 100
      JP = (IYEAR/4)*4
      KP = (IYEAR/100)*100
      NP = (IYEAR/400)*400
      IF(JP .EQ. IYEAR) L = 29
      IF(KP .EQ. IYEAR) L = 28
      IF(NP .EQ. IYEAR) L = 29
  100 IF((IDATE .LT. K) .OR. (IDATE .GT. L)) GOTO 200
C
C.....CHECK THE YEAR.
C
      IF(IYEAR .LT. M) GOTO 200
      RETURN
C
  200 ISTATC = 1
      RETURN
      END
