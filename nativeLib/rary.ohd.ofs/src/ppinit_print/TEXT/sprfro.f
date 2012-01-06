C MEMBER SPRFRO
C-----------------------------------------------------------------------
C
C DESC PRINT RAINFALL-RUNOFF RELATION PARAMETERS
C
      SUBROUTINE SPRFRO (IVRFRO,RFROID,NURFRO,RFROPM,
     *   UNUSED,ISTAT)
C
      DIMENSION UNUSED(1)
      INCLUDE 'scommon/dimrfro'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/sprfro.f,v $
     . $',                                                             '
     .$Id: sprfro.f,v 1.1 1995/09/17 19:14:21 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,60)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HRFRO)
C
      ISTAT=0
      NUMERR=0
      NUMWRN=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      WRITE (LP,80) RFROID
      CALL SULINE (LP,2)
      WRITE (LP,100)
      CALL SULINE (LP,2)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
      IF (LDEBUG.EQ.0) GO TO 10
         WRITE (LP,110) IVRFRO
         CALL SULINE (LP,2)
C
C  PRINT RAINFALL-RUNOFF RELATION IDENTIFIER AND NUMBER
10    WRITE (LP,120) RFROID,NURFRO
      CALL SULINE (LP,2)
C
C  PRINT NUMBER OF UNUSED POSITIONS
      NUNUSD=2
      IF (LDEBUG.EQ.0) GO TO 20
         WRITE (LP,150) NUNUSD
         CALL SULINE (LP,2)
C
C  PRINT RAINFALL-RUNOFF RELATION PARAMETERS
20    WRITE (LP,160) RFROPM
      CALL SULINE (LP,3)
C
30    WRITE (LP,70)
      CALL SULINE (LP,1)
      WRITE (LP,90)
      CALL SULINE (LP,1)
C
40    IF (LDEBUG.GT.0.AND.ISTAT.EQ.0) WRITE (IOSDBG,180) RFROID
      IF (LDEBUG.GT.0.AND.ISTAT.EQ.0) CALL SULINE (IOSDBG,2)
C
50    IF (ISTRCE.GT.0) WRITE (IOSDBG,190) ISTAT
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER SPRFRO')
70    FORMAT (1H )
80    FORMAT (1H0,60(1H-),' ID=',2A4,1X,59(1H-))
90    FORMAT (1H ,132(1H-))
100   FORMAT (1H )
110   FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
120   FORMAT ('0*--> RFRO PARAMETERS :   IDENTIFIER = ',2A4,5X,
     *   'NUMBER = ',I2)
150   FORMAT ('0NUMBER OF UNUSED POSITIONS = ',I2)
160   FORMAT ('0RAINFALL-RUNOFF RELATION PARAMETERS : ',
     *   6X,'A',6X,'WN',6X,'WX',6X,'BI',6X,'SK',6X,'SM',
     *   6X,'E1',6X,'E2',6X,'CP',5X,'POW' /
     *   T41,10(F7.2,1X))
180   FORMAT ('0*** NOTE - RFRO PARAMETERS FOR IDENTIFIER ',2A4,
     *   ' SUCCESSFULLY PRINTED.')
190   FORMAT (' *** EXIT SPRFRO - STATUS CODE=',I2)
C
      END
