C MODULE SFUPWR
C-----------------------------------------------------------------------
C
C  ROUTINE TO DEFINE GENERAL USER 1/D**POWER PARAMETERS
C
      SUBROUTINE SFUPWR (PRNOTE,INULL,NUMFLD,NFLD,REAL,ITYPE,NUMERR,
     *   NUMWRN,XMNPWR,XMXPWR,XDPOWR,DPOWER,IOFSET,ISTAT)
C
      CHARACTER*(*) PRNOTE
C
      DIMENSION DPOWER(*)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfupwr.f,v $
     . $',                                                             '
     .$Id: sfupwr.f,v 1.2 1998/04/07 15:14:29 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,140)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DEFN')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,180) NUMFLD,NUMERR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      NFIELD=NUMFLD-IOFSET
      GO TO (10,50,90),NFIELD
      WRITE (LP,150) NFIELD
      ISTAT=1
      GO TO 130
C
C  DEFAULT FOR PROCESSING PCPN DATA
10    IF (INULL.EQ.1) THEN
         DPOWER(1)=XDPOWR
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,160) NUMFLD,XDPOWR
            CALL SULINE (LP,2)
            ENDIF
         GO TO 30
         ENDIF
      IF (ITYPE.EQ.2) THEN
         WRITE (LP,170) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 130
         ENDIF
30    DPOWER(1)=REAL
      IF (DPOWER(1).GE.XMNPWR.AND.DPOWER(1).LE.XMXPWR) GO TO 40
         WRITE (LP,190) DPOWER(1),XMNPWR,XMXPWR
         CALL SUWRNS (LP,2,NUMWRN)
         DPOWER(1)=XDPOWR
         WRITE (LP,250) XDPOWR
         CALL SULINE (LP,1)
40    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,200) DPOWER(1)
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 130
C
C  DEFAULT FOR PROCESSING TEMP DATA
50    IF (INULL.EQ.1) THEN
         DPOWER(2)=XDPOWR
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,160) NUMFLD,XDPOWR
            CALL SULINE (LP,2)
            ENDIF
         GO TO 70
         ENDIF
      IF (ITYPE.EQ.2) THEN
         WRITE (LP,170) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 130
         ENDIF
70    DPOWER(2)=REAL
      IF (DPOWER(2).GE.XMNPWR.AND.DPOWER(2).LE.XMXPWR) GO TO 80
         WRITE (LP,210) DPOWER(2),XMNPWR,XMXPWR
         CALL SUWRNS (LP,2,NUMWRN)
         DPOWER(2)=XDPOWR
         WRITE (LP,250) XDPOWR
80    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,220) DPOWER(2)
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 130
C
C  DEFAULT FOR PROCESSING PE DATA
90    IF (INULL.EQ.1) THEN
         DPOWER(3)=XDPOWR
         IF (PRNOTE.EQ.'YES') THEN
            WRITE (LP,160) NUMFLD,XDPOWR
            CALL SULINE (LP,2)
            ENDIF
         GO TO 110
         ENDIF
      IF (ITYPE.EQ.2) THEN
         WRITE (LP,170) NUMFLD,NFLD
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 130
         ENDIF
110   DPOWER(3)=REAL
      IF (DPOWER(3).GE.XMNPWR.AND.DPOWER(3).LE.XMXPWR) GO TO 120
         WRITE (LP,230) DPOWER(3),XMNPWR,XMXPWR
         CALL SUWRNS (LP,2,NUMWRN)
         DPOWER(3)=XDPOWR
         WRITE (LP,250) XDPOWR
120   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,240) DPOWER(3)
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 130      
C
130   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,260)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
140   FORMAT (' *** ENTER SFUPWR')
150   FORMAT ('0*** ERROR - IN SFUPWR - ',I2,' IS AN INVALID FIELD ',
     *   'NUMBER.')
160   FORMAT ('0*** NOTE - NO VALUE FOUND FOR REQUIRED FIELD ',I2,
     *   '. DEFAULT VALUE (',F8.2,') WILL BE USED.')
170   FORMAT ('0*** ERROR - NON-CHARACTER DATA EXPECTED IN INPUT ',
     *   'FIELD ',I2,' (CARD FIELD ',I2,').')
180   FORMAT (' NUMFLD=',I2,3X,'NUMERR=',I2)
190   FORMAT ('0*** WARNING - DEFAULT POWER IN 1/D**POWER FOR PCPN ',
     *   'DATA (',F7.2,') DOES NOT FALL BETWEEN ',F7.2,' AND ',F7.2,'.')
200   FORMAT (' DEFAULT POWER IN 1/D**POWER FOR PCPN DATA SET TO ',F7.2)
210   FORMAT ('0*** WARNING - DEFAULT POWER IN 1/D**POWER FOR TEMP ',
     *   'DATA (',F7.2,') DOES NOT FALL BETWEEN ',F7.2,' AND ',F7.2,'.')
220   FORMAT (' DEFAULT POWER IN 1/D**POWER FOR TEMP DATA SET TO ',F7.2)
230   FORMAT ('0*** WARNING - DEFAULT POWER IN 1/D**POWER FOR PE   ',
     *   'DATA (',F7.2,') DOES NOT FALL BETWEEN ',F7.2,' AND ',F7.2,'.')
240   FORMAT (' DEFAULT POWER IN 1/D**POWER FOR PE DATA SET TO ',F7.2)
250   FORMAT (T16,'THE DEFAULT VALUE (',F7.2,') WILL BE USED.')
260   FORMAT (' *** EXIT SFUPWR')
C
      END
