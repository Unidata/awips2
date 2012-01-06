C MODULE UFINFX
C-----------------------------------------------------------------------
C
C  ROUTINE TO CONVERT A CHARACTER STRING TO A INTEGER VALUE.
C
      SUBROUTINE UFINFX (IANS,ISTRT,IBEG,IEND,ISTAT)
C
C  INPUT VARIABLES:
C     ISTRT  - STARTING LOCATION OF CHARACTER STRING IN ICDBUF
C     IBEG   - LOCATION OF FIRST CHARACTER TO BE CONVERTED
C     IEND   - LOCATION OF LAST  CHARACTER TO BE CONVERTED
C
C  OUTPUT VARIABLES:
C     IANS   - NUMERIC VALUE
C     ISTAT  - STATUS CODE
C              1 = INVALID CHARACTER FOUND
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/ufinfx.f,v $
     . $',                                                             '
     .$Id: ufinfx.f,v 1.2 1998/07/02 19:41:56 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IUTLTR.GT.1) WRITE (IOGDB,20)
C
      ISTAT=0
C
C  CONVERT FROM CHARACTER TO INTEGER
      NCHAR=IEND-IBEG+1
      IPRERR=1
      CALL UFA2I (ICDBUF(ISTRT:ISTRT),IBEG,NCHAR,IANS,IPRERR,LP,IERR)
      IF (IERR.EQ.0) GO TO 10
C
      ISTAT=1
C
      IF (IUTLDB.GT.1) THEN
         IF (IBEG.EQ.IEND) WRITE (IOGDB,30) IBEG
         IF (IBEG.NE.IEND) WRITE (IOGDB,40) IBEG,IEND
         ENDIF
C
10    IF (IUTLTR.GT.1) WRITE (IOGDB,50)
C
      RETURN
C
20    FORMAT (' *** ENTER UFINFX')
30    FORMAT (' IN UFINFX - INTEGER VALUE EXPECTED IN COLUMN ',I2)
40    FORMAT (' IN UFINFX - INTEGER VALUE EXPECTED IN COLUMNS ',I2,
     *   '-',I2)
50    FORMAT (' *** EXIT UFINFX')
C
      END
