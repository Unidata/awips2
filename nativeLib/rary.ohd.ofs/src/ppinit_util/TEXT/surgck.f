C MODULE SURGCK
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK IF STATION NAME FALLS IN SPECIFIED RANGE.
C
      SUBROUTINE SURGCK (SRNGE1,SRNGE2,LCOLN1,LCOLN2,STAID,LSTAID,
     *   IMATCH,ISTAT)
C
      CHARACTER*8 STAID,SRNGE1,SRNGE2
      CHARACTER*8 CHKST,CHKR1,CHKR2
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/surgck.f,v $
     . $',                                                             '
     .$Id: surgck.f,v 1.3 2000/03/14 12:55:14 page Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      LTRACE=ISTRC('UTIL')
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SURGCK'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UTIL')
C
      ISTAT=0
C      
      IMATCH=0
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'SRNGE1=',SRNGE1,' SRNGE2=',SRNGE2
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,*) 'LCOLN1=',LCOLN1,' LCOLN2=',LCOLN2
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,*) 'STAID=',STAID
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK IF SELECTED RANGE SPECIFIED
      IF (LCOLN1.EQ.0.AND.LCOLN2.EQ.0) THEN
         IF (STAID.LT.SRNGE1.OR.STAID.GT.SRNGE2) GO TO 50
         GO TO 40
         ENDIF
C
C  CHECK IF MATCHING PATTERNS AFTER ':'
      IF (LCOLN1.EQ.1.OR.LCOLN2.EQ.1) THEN
         NSTC=LENSTR(STAID)
         NSR1C=LENSTR(SRNGE1)
         IF (NSTC.LT.NSR1C) GO TO 50
         ISR1B=1
         ISR1E=LENSTR(SRNGE1)
         NSR1C=ISR1E-ISR1B+1
         ISTE=LENSTR(STAID)
         ISTB=ISTE-NSR1C+1
         IF (STAID(ISTB:ISTE).EQ.SRNGE1(ISR1B:ISR1E)) GO TO 40
         IF (LCOLN2.GT.0) THEN
            NSR2C=LENSTR(SRNGE2)
            IF (NSTC.LT.NSR2C) GO TO 50
            ISR2B=1
            ISR2E=LENSTR(SRNGE2)
            NSR2C=ISR2E-ISR2B+1
            CHKST=' '
            CHKR1=' '
            CHKR2=' '
            CALL SUBSTR (STAID,ISTB,NSTC,CHKST,1)
            CALL SUBSTR (SRNGE1,ISR1B,NSR1C,CHKR1,1)
            CALL SUBSTR (SRNGE2,ISR2B,NSR2C,CHKR2,1)
            IF (LDEBUG.GT.0) THEN
               CALL SULINE (IOSDBG,1)
               WRITE (IOSDBG,*) 'CHKST=',CHKST,' CHKR1=',CHKR1,
     *            ' CHKR2=',CHKR2
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (CHKST.LT.CHKR1.OR.CHKST.GT.CHKR2) GO TO 50
            GO TO 40
            ENDIF
         GO TO 50
         ENDIF
C
C  SELECTED RANGE SPECIFIED
      NCHAR=LSTAID
      IF (LCOLN1.GT.0) NCHAR=LCOLN1-1
      CHKST=' '
      CALL SUBSTR (STAID,1,NCHAR,CHKST,1)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'CHKST=',CHKST
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (SRNGE2.EQ.' ') THEN
         IF (CHKST.LT.SRNGE1.OR.CHKST.GT.SRNGE1) GO TO 50
         ELSE
            IF (CHKST.LT.SRNGE1.OR.CHKST.GT.SRNGE2) GO TO 50
         ENDIF
C
C  MATCH FOUND
40    IMATCH=1
C
50    IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SURGCK : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
      END
