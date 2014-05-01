C MODULE UFRLFX
C-----------------------------------------------------------------------
C
C  ROUTINE TO CONVERT A CHARACTER STRING TO A REAL VALUE.
C
C
      SUBROUTINE UFRLFX (ANS,ISTRT,IBEG,IEND,ID,ISTAT)
C
C  INPUT VARIABLES:
C     ISTRT  - STARTING LOCATION OF CHARACTER STRING IN ICDBUF.
C     IBEG   - LOCATION OF FIRST CHARACTER TO BE CONVERTED
C     IEND   - LOCATION OF LAST  CHARACTER TO BE CONVERTED
C     ID     - NEGATIVE EXPONENT OF ANSWER IF NO DECIMAL POSITIONS
C              (ANS*10**(-ID))
C
C  OUTPUT VARIABLES:
C     ANS    - NUMERIC VALUE
C     ISTAT  - STATUS CODE
C              1 = INVALID CHARACTER FOUND
C
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/ufrlfx.f,v $
     . $',                                                             '
     .$Id: ufrlfx.f,v 1.2 1998/07/02 19:45:07 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IUTLTR.GT.1) WRITE (IOGDB,20)
C
      ISTAT=0
C
C  CONVERT FROM CHARACTER TO NUMERIC
      NCHAR=IEND-IBEG+1
      NDEC=0
      IPRERR=1
      CALL UFA2F (ICDBUF(ISTRT:ISTRT),IBEG,NCHAR,NDEC,ANS,IPRERR,LP,
     *   IERR)
      IF (IERR.EQ.0) GO TO 10
C
      ISTAT=1
C
      IF (IUTLDB.GT.0) THEN
         IF (IBEG.EQ.IEND) WRITE (IOGDB,30) IBEG
         IF (IBEG.NE.IEND) WRITE (IOGDB,40) IBEG,IEND
         ENDIF
C
10    IF (IUTLTR.GT.1) WRITE (IOGDB,50)
C
      RETURN
C
20    FORMAT (' *** ENTER UFRLFX')
30    FORMAT (' IN UFRLFX - REAL VALUE EXPECTED IN COLUMN ',I2)
40    FORMAT (' IN UFRLFX - REAL VALUE EXPECTED IN COLUMNS ',I2,'-',I2)
50    FORMAT (' *** EXIT UFRLFX')
C
      END
