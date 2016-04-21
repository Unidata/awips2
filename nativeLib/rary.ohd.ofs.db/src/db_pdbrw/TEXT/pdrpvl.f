C MODULE PDRPVL
C-----------------------------------------------------------------------
C
      SUBROUTINE PDRPVL (NVPOB,LTIME,VAL,NTIME,MIN,VAL2,ISTAT)
C
C  THIS ROUTINE REPLACES A TIME AND VALUE IN AN RRS RECORD.
C  IT CHOOSES THE TIME AND VALUE BASED ON MINUTES. THE CLOSEST
C  MINUTE TO THE HOUR IS USED. IF THE MINUTES ARE EQUIDISTANT, THE
C  THE MINUTE AFTER THE HOUR IS USED.
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ------    ----  ---   ---   -----------
C       NVPOB      I     I     1    NUMBER OF VALUES PER OBSERVATION
C       LTIME      I    I/O    1    TIME OF OBSERVATION
C       VAL        R    I/O    1    VALUE
C       NTIME      I     I     1    JULIAN HOUR OF NEW OB
C       MIN        I     I     1    MINUTE OF NEW OB
C       VAL2       R     I     1    NEW VALUE
C       ISTAT      I     O     1    STATUS INDICATOR:
C                                    0=OK
C                                    1=DATA NOT CHANGED
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      COMMON /PDBREV/ IREVSN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdrpvl.f,v $
     . $',                                                             '
     .$Id: pdrpvl.f,v 1.2 2002/02/11 19:53:48 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
      IF (IPDTR.GT.1) WRITE (IOGDB,*) 'ENTER PDRPVL'
C
      IF (IPDDB.GT.1) WRITE (IOGDB,*) 'NVPOB=',NVPOB,
     *   ' LTIME=',LTIME,' VAL=',VAL,' NTIME=',NTIME,
     *   ' MIN=',MIN,' VAL2=',VAL2,' IREVSN=',IREVSN
C
      IF (NVPOB.EQ.2) GO TO 40
      IF (NVPOB.EQ.3) GO TO 30
C
      WRITE (LP,20) NVPOB
20    FORMAT (' **ERROR** IN PDRPVL - VALUE OF NVPOB (',I2,
     *   ') IS INVALID.')
      GO TO 60
C
C  PERIOD AVERAGE DATA - DO NOT CHECK MINUTES - JUST REPLACE VALUE
30    VAL=VAL2
      GO TO 60
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  INSTANTANEOUS DATA - CHECK IF OLD OR NEW MINUTES CLOSER TO HOUR
C
C  GET MINUTES FROM OLD TIME
40    LMIN=MOD(LTIME,100)
C
C  SET DIFFERENCE FOR OLD
      LDIF=LMIN
      IF (LDIF.GT.30) LDIF=60-LMIN
C
C  SET DIFFERENCE FOR NEW
      NDIF=MIN
      IF (NDIF.GT.30) NDIF=60-MIN
C
C  IF OLD MINUTES CLOSER TO HOUR THAN NEW THEN NEVER CHANGE
      IF (LDIF.LT.NDIF) GO TO 55
C
C  IF OLD MINUTES FARTHER FROM HOUR THAN NEW THEN ALWAYS CHANGE
      IF (LDIF.GT.NDIF) GO TO 50
C
C  MINUTES SAME - CHANGE ONLY IF REVISION SWITCH IS ON
      IF (IREVSN.NE.1) GO TO 55
C
C  PUT IN NEW TIME AND VALUE
50    VAL=VAL2
      LTIME=NTIME*100+MIN
      GO TO 60
C
C  DATA NOT CHANGED
55    ISTAT=1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    IF (IPDTR.GT.1) WRITE (IOGDB,*) 'EXIT PDRPVL : ISTAT=',ISTAT
C
      RETURN
C
      END
