C MODULE SPNTWK
C-----------------------------------------------------------------------
C
C   ROUTINE TO PRINT GENERAL NETWORK PARAMETERS.
C
      SUBROUTINE SPNTWK (IVNTWK,INWDTE,NNWFLG,INWFLG,UNUSED,ISTAT)
C
      CHARACTER*3 CODE
C
      DIMENSION INWDTE(*),INWFLG(*),UNUSED(*)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/spntwk.f,v $
     . $',                                                             '
     .$Id: spntwk.f,v 1.2 1998/04/07 18:00:14 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,60)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('NTWK')
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(15).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      WRITE (LP,70)
      CALL SULINE (LP,2)
      WRITE (LP,80)
      CALL SULINE (LP,2)
      WRITE (LP,*) ' '
      CALL SULINE (LP,1)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.EQ.0) GO TO 10
         WRITE (IOSDBG,*) 'IVNTWK=',IVNTWK
         CALL SULINE (IOSDBG,2)
C
C  PRINT DATE PARAMETER ARRAY LAST UPDATED
10    WRITE (LP,90) (INWDTE(I),I=1,4)
      CALL SULINE (LP,2)
C
C  PRINT NETWORK INDICATORS
      IF (LDEBUG.EQ.0) GO TO 20
         WRITE (LP,100) (INWFLG(I),I=1,NNWFLG)
         CALL SULINE (LP,3)
20    WRITE (LP,120)
      CALL SULINE (LP,2)
      WRITE (LP,130)
      CALL SULINE (LP,1)
C
      DO 40 I=1,NNWFLG
         IF (ISNWPG(LP).EQ.0) GO TO 30
            WRITE (LP,120)
            CALL SULINE (LP,2)
            WRITE (LP,130)
            CALL SULINE (LP,1)
30       CODE='NO'
         IF (INWFLG(I).EQ.1) CODE='YES'
         IF (I.EQ.1) THEN
            WRITE (LP,140) I,CODE
            CALL SULINE (LP,4)
            ENDIF
         IF (I.EQ.2) THEN
            WRITE (LP,150) I,CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.3) THEN
            WRITE (LP,160) I,CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.4) THEN
            WRITE (LP,170) I,CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.5) THEN
            WRITE (LP,180) I,CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.6) THEN
            WRITE (LP,190) I,CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.7) THEN
            WRITE (LP,200) I,CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.8) THEN
            WRITE (LP,210) I,CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.9) THEN
            WRITE (LP,220) I,CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.10) THEN
            WRITE (LP,230) I,CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.11) THEN
            WRITE (LP,240) I,CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.12) THEN
            WRITE (LP,250) I,'OP24',CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.13) THEN
            WRITE (LP,250) I,'OPVR',CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.14) THEN
            WRITE (LP,250) I,'OT24',CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.15) THEN
            WRITE (LP,250) I,'OE24',CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.16) THEN
            WRITE (LP,250) I,'ORRS',CODE
            CALL SULINE (LP,2)
            ENDIF
         IF (I.EQ.17) THEN
            WRITE (LP,260) I,CODE
            CALL SULINE (LP,2)
            ENDIF
40       CONTINUE
C
C  PRINT UNUSED POSITIONS
      IF (LDEBUG.EQ.0) GO TO 50
         NUNSED=2
         WRITE (IOSDBG,*) 'NUNSED=',NUNSED
         CALL SULINE (IOSDBG,2)
C
50    WRITE (LP,70)
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,270)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER SPNTWK')
70    FORMAT ('0',132('-'))
80    FORMAT ('0*--> NTWK PARAMETERS ',
     *   '(NETWORK COMPUTATION INDICATORS)')
90    FORMAT ('0DATE LAST UPDATED = ',I2.2,'/',I2.2,'/',I4.4,'.',I4.4)
100   FORMAT (' INWFLG(1,NNWFLG)=',10(I5,2X) / T24,10(I5,2X))
120   FORMAT ('0',T10,'INDICATOR',T75,'COMPUTE?')
130   FORMAT (T10,9('-'),T75,8('-'))
140   FORMAT ('0',T6,I2,2X,
     *   'UPDATE 5 CLOSEST 24-HR  PCPN STATIONS PER QUADRANT AND' /
     *   T17,'CHECK IDENTIFIERS OF STATIONS WITH SIGNIFICANCE ' /
     *   T17,'   WEIGHTS',
     *   T75,A)
150   FORMAT ('0',T6,I2,2X,
     *   'UPDATE 3 CLOSEST <24-HR PCPN STATIONS PER QUADRANT',
     *   T75,A)
160   FORMAT ('0',T6,I2,2X,
     *   'UPDATE 3 CLOSEST MXMN   TEMP STATIONS PER QUADRANT',
     *   T75,A)
170   FORMAT ('0',T6,I2,2X,
     *   'UPDATE 3 CLOSEST INST   TEMP STATIONS PER QUADRANT',
     *   T75,A)
180   FORMAT ('0',T6,I2,2X,
     *   'UPDATE 2 CLOSEST FMM    TEMP STATIONS PER QUADRANT',
     *   T75,A)
190   FORMAT ('0',T6,I2,2X,
     *   'UPDATE MAP  1/D**2 OR 1/D**POWER TIME DISTRIBUTION WEIGHTS',
     *   T75,A)
200   FORMAT ('0',T6,I2,2X,
     *   'UPDATE MAP  GRID POINT, THIESSEN OR 1/D**2 STATION WEIGHTS',
     *   T75,A)
210   FORMAT ('0',T6,I2,2X,
     *   'UPDATE MAT  GRID POINT OR 1/D**POWER WEIGHTS',
     *   T75,A)
220   FORMAT ('0',T6,I2,2X,
     *   'UPDATE MAP  AREA PARAMETERS DUE TO BASIN BOUNDARY CHANGE',
     *   T75,A)
230   FORMAT ('0',T6,I2,2X,
     *   'UPDATE MAT  AREA PARAMETERS DUE TO BASIN BOUNDARY CHANGE',
     *   T75,A)
240   FORMAT ('0',T6,I2,2X,
     *   'UPDATE MAPE 1/D**POWER WEIGHTS',
     *   T75,A)
250   FORMAT ('0',T6,I2,2X,
     *   'UPDATE ',A,' ALPHABETICAL ORDER',
     *   T75,A)
260   FORMAT ('0',T6,I2,2X,
     *   'UPDATE GP24 AND OG24 ALPHABETICAL ORDER',
     *   T75,A)
270   FORMAT (' *** EXIT SPNTWK')
C
      END
