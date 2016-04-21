C MODULE EWRIT
C-----------------------------------------------------------------------
C
      SUBROUTINE EWRIT(X,LARY,JREC,NXWD)
C
C   THIS ROUTINE WRITES THE ESP ARRAYS TO THE ESP PARAMETER FILE.
C
C   THIS ROUTINE WAS WRITTEN BY GERALD N DAY.
C
      CHARACTER*8 OLDOPN
C
      DIMENSION X(*)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/esprec'
      INCLUDE 'common/espfle'
      INCLUDE 'common/eunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/espinit/RCS/ewrit.f,v $
     . $',                                                             '
     .$Id: ewrit.f,v 1.4 2001/06/13 13:05:16 dws Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=0
      CALL FSTWHR ('EWRIT   ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE(IODBUG,*) 'ENTER EWRIT'
C
      IF (LRECL.LE.0) THEN
         WRITE (IPR,10) LRECL
10    FORMAT ('0**ERROR** LRECL (',I3,
     *   ') IS LESS THAN OR EQUAL TO ZERO.')
         CALL ERROR
         GO TO 50
         ENDIF
C
      LOCX=0
      LENGTH=LARY
C
20    KOUNT=LENGTH
      LEFT=LRECL-NXWD+1
      IF (LEFT.LT.LENGTH) KOUNT=LEFT
      DO 30 I=1,KOUNT
         ESPDAT(NXWD)=X(LOCX+I)
         NXWD=NXWD+1
30       CONTINUE
      IF (NXWD.LE.LRECL) GO TO 50
      CALL UWRITT (KEPARM,JREC,ESPDAT,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (IPR,40) JREC,KEPARM
40    FORMAT ('0**ERROR** READING RECORD ',I4,' FROM UNIT ',I3,'.')
         CALL ERROR
         GO TO 50
         ENDIF
      NXWD=1
      JREC=JREC+1
      LENGTH=LENGTH-KOUNT
      IF (LENGTH.EQ.0) GO TO 50
      LOCX=LOCX+KOUNT
      GO TO 20
C
50    CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
