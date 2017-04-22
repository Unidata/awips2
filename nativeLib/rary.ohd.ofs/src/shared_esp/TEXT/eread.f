C MODULE EREAD
C-----------------------------------------------------------------------
C
      SUBROUTINE EREAD (X,LARY,JREC,NXWD)
C
C   THIS ROUTINE FILLS THE ESP ARRAYS FROM THE ESP PARAMETER FILE.
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/eread.f,v $
     . $',                                                             '
     .$Id: eread.f,v 1.3 2001/06/13 11:28:37 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=0
      CALL FSTWHR ('EREAD   ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (ITRACE.GE.1) WRITE(IODBUG,*) 'ENTER EREAD'
C
      IF (LRECL.LE.0) THEN
         WRITE (IPR,10) LRECL
10    FORMAT ('0**ERROR** LRECL (',I3,
     *   ') IS LESS THAN OR EQUAL TO ZERO.')
         CALL ERROR
         GO TO 60
         ENDIF
C
      LENGTH=LARY
      KOUNT=0
      LOCX=0
C
      IF (NXWD.GT.LRECL) GO TO 40
C
20    KOUNT=LENGTH
      LEFT=LRECL-NXWD+1
      IF (LEFT.LT.LENGTH) KOUNT=LEFT
      DO 30 I=1,KOUNT
         X(LOCX+I)=ESPDAT(NXWD)
         NXWD=NXWD+1
30       CONTINUE
C
      LENGTH=LENGTH-KOUNT
      IF (LENGTH.EQ.0) GO TO 60
C
40    NXWD=1
      LOCX=LOCX+KOUNT
      JREC=JREC+1
      CALL UREADT (KEPARM,JREC,ESPDAT,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (IPR,50) JREC,KEPARM
50    FORMAT ('0**ERROR** READING RECORD ',I4,' FROM UNIT ',I3,'.')
         CALL ERROR
         GO TO 60
         ENDIF
      GO TO 20
C
60    CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
      END
