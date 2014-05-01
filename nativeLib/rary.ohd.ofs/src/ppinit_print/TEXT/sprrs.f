C MODULE SPRRS
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT STATION RRS PARAMETERS.
C
      SUBROUTINE SPRRS (IPRNT,IVRRS,STAID,NUMBER,DESCRP,STATE,NTYPE,
     *   NMISS,NDIST,RRSTYP,URMISS,IRTIME,NVLPOB,MNODAY,NUMOBS,
     *   ITSREC,INTERP,EXTRAP,FLOMIN,FRACT,UNUSED,ISTAT)
C
      CHARACTER*4 POPT
C
      DIMENSION UNUSED(1)
C
      INCLUDE 'scommon/dimsta'
      INCLUDE 'scommon/dimrrs'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/sprrs.f,v $
     . $',                                                             '
     .$Id: sprrs.f,v 1.2 1998/04/07 18:01:35 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('RRS ')
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      IF (IPRNT.EQ.0.AND.LDEBUG.EQ.0) GO TO 10
         IF (IPRNT.EQ.1) THEN
            WRITE (LP,100) STAID
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.2) THEN
            WRITE (LP,120) DESCRP
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.3) THEN
            WRITE (LP,110) NUMBER
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.0) THEN
            WRITE (LP,90)
            CALL SULINE (LP,2)
            ENDIF
         GO TO 20
10    WRITE (LP,90)
      CALL SULINE (LP,2)
      WRITE (LP,140)
      CALL SULINE (LP,1)
      GO TO 30
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
20    IF (LDEBUG.GT.0) THEN
         WRITE (LP,150) IVRRS
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT RRS STATION IDENTIFIER AND DESCRIPTIVE INFORMATION
      IF (IPRNT.EQ.1) THEN
         IF (NUMBER.GT.0) THEN
            WRITE (LP,160) STAID,DESCRP,STATE,NUMBER
            CALL SULINE (LP,2)
            ELSE
               WRITE (LP,170) STAID,DESCRP,STATE
               CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  PRINT DATA TYPE, MISSING DATA INDICATOR, DATA TIME INTERVAL,
C  NUMBER OF VALUES PER OBSERVATION, MINIMUM NUMBER OF DAYS OF DATA
C  TO BE RETAINED IN PPDB, TYPICAL NUMBER OF OBSERVATIONS HELD IN
C  PPDB, INTERPOLATION OPTION AND EXTRAPOLATION RECESSION CONSTANT
30    IF (ISLEFT(6).GT.0) CALL SUPAGE
      IF (NMISS.EQ.0) THEN
          WRITE (LP,190)
         CALL SULINE (LP,4)
         ELSE
            WRITE (LP,180)
            CALL SULINE (LP,4)
         ENDIF
      N=0
      INDNTS=0
      DO 40 I=1,NTYPE
         IF (IRTIME(I).LT.0) INDNTS=1
         IF (URMISS(I).NE.'SAME') THEN
            N=N+1
            POPT='LIN'
            IF (INTERP(N).EQ.1) POPT='RET'
            WRITE (LP,200) I,RRSTYP(I),IRTIME(I),MNODAY(I),NUMOBS(I),
     *         NVLPOB(I),'NO',URMISS(I),POPT,EXTRAP(N)
            CALL SULINE (LP,1)
            GO TO 40
            ENDIF
         WRITE (LP,220) I,RRSTYP(I),IRTIME(I),MNODAY(I),NUMOBS(I),
     *      NVLPOB(I),'YES'
         CALL SULINE (LP,1)
40       CONTINUE
      IF (INDNTS.EQ.1) THEN
         WRITE (LP,210)
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT RECORD NUMBER OF TIME SERIES HEADER
      DO 50 I=1,NTYPE
         IF (LDEBUG.GT.0) THEN
            IF (I.EQ.1) THEN
               WRITE (LP,240)
               CALL SULINE (LP,2)
               ENDIF
            WRITE (LP,250) RRSTYP(I),ITSREC(I)
            CALL SULINE (LP,1)
            ENDIF
         IF (IRTIME(I).GT.0.AND.ITSREC(I).EQ.0) THEN
            WRITE (LP,260) RRSTYP(I),STAID
            CALL SUWRNS (LP,2,-1)
            ENDIF
50       CONTINUE
C
      IF (NDIST.EQ.0) GO TO 70
C
C  PRINT MINIMUM FLOW AND DISTRIBUTION FACTORS
      WRITE (LP,140)
      CALL SULINE (LP,2)
      WRITE (LP,230)
      CALL SULINE (LP,2)
      WRITE (LP,270)
      CALL SULINE (LP,3)
      DO 60 I=1,NTYPE
         IF (FLOMIN(I).LE.0.) GO TO 60
            WRITE (LP,280) RRSTYP(I),FLOMIN(I),(FRACT(N,I),N=1,12)
            CALL SULINE (LP,1)
            WRITE (LP,290) (FRACT(N,I),N=13,24)
            CALL SULINE (LP,1)
60       CONTINUE
C
C  PRINT NUMBER OF UNUSED POSITIONS
70    NUNUSD=1
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,300) NUNUSD
         CALL SULINE (LP,2)
         ENDIF
C
      WRITE (LP,130)
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,310)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER SPRRS')
90    FORMAT ('0*-->  RRS  PARAMETERS')
100   FORMAT ('0',59('-'),' ID=',A,2X,59('-'))
110   FORMAT ('0',59('-'),' NUMBER=',I4,2X,59('-'))
120   FORMAT ('0',52('-'),' DESC=',A,1X,52('-'))
130   FORMAT ('0',132('-'))
140   FORMAT (' ')
150   FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
160   FORMAT ('0*--> RRS  PARAMETERS :  ',
     *   'IDENTIFIER = ',A,5X,
     *   'DESCRIPTION = ',A,5X,
     *   'STATE = ',A2,5X,
     *   'NUMBER = ',I4)
170   FORMAT ('0*--> RRS  PARAMETERS :  ',
     *   'IDENTIFIER = ',A,5X,
     *   'DESCRIPTION = ',A,5X,
     *   'STATE = ',A2,5X,
     *   'NUMBER = ** NONE **')
180   FORMAT ('0DATA TYPE CODES:',
     *   22X,30X,'NUM VAL',4X,
     *      'MISSING',4X,
     *      'OUTPUT' /
     *   22X,'TYPE',4X,
     *      'TIME INT',4X,
     *      'MIN DAYS',4X,
     *      'TYPICAL OBS',4X,
     *      'PER OBS',4X,
     *      'ALLOWED',4X,
     *      'TYPE  ',4X,
     *      'INTERP',4X,
     *      'EXTRAP' /
     *   22X,4('-'),4X,
     *      8('-'),4X,
     *      8('-'),4X,
     *      11('-'),4X,
     *      7('-'),4X,
     *      7('-'),4X,
     *      6('-'),4X,
     *      6('-'),4X,
     *      6('-'))
190   FORMAT ('0DATA TYPE CODES:',
     *   22X,30X,'NUM VAL',4X,
     *      'MISSING' /
     *   22X,'TYPE',4X,
     *      'TIME INT',4X,
     *      'MIN DAYS',4X,
     *      'TYPICAL OBS',4X,
     *      'PER OBS',4X,'
     *      ALLOWED' /
     *   22X,4('-'),4X,
     *      8('-'),4X,
     *      8('-'),4X,
     *      11('-'),4X,
     *      7('-'),4X,
     *      7('-'))
200   FORMAT (17X,I3,2X,A4,6X,I3,9X,I3,10X,I4,10X,I2,9X,A4,6X,A4,
     *   6X,A4,6X,F5.3)
210   FORMAT ('0NOTE : NEGATIVE TIME INTERVAL INDICATES DATA ',
     *   'TYPE HAS NO TIME SERIES IN PROCESSED DATA BASE.')
220   FORMAT (17X,I3,2X,A4,6X,I3,9X,I3,10X,I4,10X,I2,9X,A4)
230   FORMAT ('0FLOW DISTRIBUTION FACTORS FOR MEAN DISHARGE DATA ',
     *   'TYPES')
240   FORMAT ('0RECORD NUMBER OF TIME SERIES HEADERS IN PROCESSED ',
     *   'DATA BASE')
250   FORMAT (T10,A4,5X,I5)
260   FORMAT ('0*** WARNING - IN SPRRS - TIME SERIES RECORD NUMBER ',
     *   'IS ZERO FOR DATA TYPE ',A4,' FOR STATION ',A,'.')
270   FORMAT ('0',T5,'DATA',6X,'    ' /
     *   T5,'TYPE',3X,
     *      'MIN FLOW',3X,
     *      'HOURS',3X,
     *      'DISTRIBUTION FACTORS' /
     *   T5,4('-'),
     *      3X,8('-'),3X,
     *      5('-'),3X,
     *      20('-'))
280   FORMAT ('0',T5,A4,3X,F8.1,3X,' 1-12',3X,12(F5.4,3X))
290   FORMAT (T23,'13-24',3X,12(F5.4,3X))
300   FORMAT('0NUMBER OF UNUSED POSITIONS = ',I2)
310   FORMAT('*** EXIT SPRRS')
C
      END
