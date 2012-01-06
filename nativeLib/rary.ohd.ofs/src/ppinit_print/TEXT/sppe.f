C MODULE SPPE
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT STATION PE PARAMETERS.
C
      SUBROUTINE SPPE (IPRNT,UNITS,IVPE,
     *   STAID,NUMBER,DESCRP,STATE,STALAT,
     *   ANEMHT,PFACT,ITYRAD,PECOR,PEB3,PECOEF,PESUM,NPESUM,JPESUM,
     *   UNUSED,ISTAT)
C
      CHARACTER*4 UNITS
      CHARACTER*4 TUNITS,DEGMIN,UCODE
C      
      DIMENSION UNUSED(1)
      DIMENSION MINDEG(2)
C
      INCLUDE 'scommon/dimsta'
      INCLUDE 'scommon/dimpe'      
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/sppe.f,v $
     . $',                                                             '
     .$Id: sppe.f,v 1.2 1998/04/07 18:01:21 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,70)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('PE  ')
C
      ISTAT=0
C
C  DECODE UNITS
      CALL SUDCDU (UNITS,TUNITS,DEGMIN,IERR)
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      IF (IPRNT.EQ.0.AND.LDEBUG.EQ.0) GO TO 10
         IF (IPRNT.EQ.1) THEN
            WRITE (LP,90) STAID
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.2) THEN
            WRITE (LP,110) DESCRP
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.3) THEN
            WRITE (LP,100) STATE
            CALL SULINE (LP,2)
            ENDIF
         IF (IPRNT.EQ.0) THEN
            WRITE (LP,80)
            CALL SULINE (LP,2)
            ENDIF
         WRITE (LP,130)
         CALL SULINE (LP,2)
         GO TO 20
10    WRITE (LP,80)
      CALL SULINE (LP,2)
      GO TO 40
C
C  PRINT PARAMETER ARRAY VERSION NUMBER
20    IF (LDEBUG.GT.0) THEN
         WRITE (LP,150) IVPE
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT PE STATION IDENTIFIER AND DESCRIPTIVE INFORMATION
      IF (LDEBUG.GT.0) GO TO 30
      IF (IPRNT.EQ.0) GO TO 40
30    IF (NUMBER.GT.0) THEN
         WRITE (LP,160) STAID,DESCRP,STATE,NUMBER
         CALL SULINE (LP,2)
         ENDIF
      IF (NUMBER.EQ.0) THEN
         WRITE (LP,170) STAID,DESCRP,STATE
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT LATITUDE, ANEMOMETER HEIGHT AND CORRECTION FACTOR
40    VAL=ANEMHT
      UCODE='M'
      IF (TUNITS.EQ.'ENGL') THEN
         UCODE='FT'
         CALL UDUCNV ('M   ','FT  ',1,1,ANEMHT,VAL,IERR)
         ENDIF
      IF (DEGMIN.EQ.'YES') THEN
         CALL SUDMDD ('DM  ',1,STALAT,MINDEG,IERR)
         WRITE (LP,190) MINDEG,VAL,UCODE
         CALL SULINE (LP,2)
         GO TO 50
         ENDIF
      WRITE (LP,180) STALAT,VAL,UCODE,PECOR
      CALL SULINE (LP,2)
C
C  PRINT PRIMARY TYPE OF RADIATION DATA TO BE USED AND B3 PARAMETER
50    IF (ITYRAD.LT.1.OR.ITYRAD.GT.3) THEN
         WRITE (LP,200) ITYRAD
         CALL SUERRS (LP,2,-1)
         GO TO 60
         ENDIF
      IF (ITYRAD.EQ.1) THEN
         WRITE (LP,210) 'SKY',PEB3
         CALL SULINE (LP,2)
         ENDIF
      IF (ITYRAD.EQ.2) THEN
         WRITE (LP,210) 'SUN',PEB3
         CALL SULINE (LP,2)
         ENDIF
      IF (ITYRAD.EQ.3) THEN
         WRITE (LP,210) 'RAD',PEB3
         CALL SULINE (LP,2)
         ENDIF
C
60    IF (LDEBUG.GT.0) THEN
C     PRINT P FACTOR AND FOURIER SERIES COEFFICIENTS
         WRITE (LP,220) PFACT,
     *      (J-1,PECOEF(J),J=1,4),
     *      (J,PECOEF(J+4),J=1,2)
         CALL SULINE (LP,2)
C     PRINT PE SUM FOR LAST 12 MONTHS, NUMBER OF VALUES IN PESUM FOR
C     EACH MONTH AND JULIAN DATE OF LAST DAY INCLUDED IN PESUM
         WRITE (LP,230)
         CALL SULINE (LP,2)
         WRITE (LP,240) PESUM
         CALL SULINE (LP,2)
         WRITE (LP,250) NPESUM
         CALL SULINE (LP,2)
         WRITE (LP,260) JPESUM
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT NUMBER OF UNUSED POSITIONS
      NUNUSD=2
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,270) NUNUSD
         CALL SULINE (LP,2)
         ENDIF
C
      WRITE (LP,120)
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,280)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER SPPE')
80    FORMAT ('0*-->  PE   PARAMETERS')
90    FORMAT ('0',59('-'),' ID=',A,2X,59('-'))
100   FORMAT ('0',59('-'),' NUMBER=',I4,2X,59('-'))
110   FORMAT ('0',52('-'),' DESC=',A,1X,51('-'))
120   FORMAT ('0',132('-'))
130   FORMAT (' ')
150   FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
160   FORMAT ('0*--> PE   PARAMETERS :  ',
     *   'IDENTIFIER = ',A,5X,
     *   'DESCRIPTION = ',A,5X,
     *   'STATE = ',A2,5X,
     *   'NUMBER = ',I4)
170   FORMAT ('0*--> PE   PARAMETERS :  ',
     *   'IDENTIFIER = ',A,5X,
     *   'DESCRIPTION = ',A,5X,
     *   'STATE = ',A2,5X,
     *   'NUMBER = ** NONE **')
180   FORMAT ('0LATITUDE = ',F5.2,5X,
     *   'ANEMOMETER HEIGHT = ',F5.1,1X,A4,
     *   'CORRECTION FACTOR = ',F5.2)
190   FORMAT ('0LATITUDE = ',I2,'-',I2,5X,
     *   'ANEMOMETER HEIGHT = ',F5.1,1X,A4,
     *   'CORRECTION FACTOR = ',F5.2)
200   FORMAT ('0*** ERROR - IN SPPE - INVALID VALUE OF ITYRAD : ',I3)
210   FORMAT ('0PRIMARY TYPE OF RADIATION DATA = ',A,5X,
     *   'B3 PARAMETER = ',F5.2)
220   FORMAT ('0P FACTOR = ',F6.3,5X,'FOURIER COEFFICIENTS:  ',
     *   4('A',I1,'=',F7.2,2X),2('B',I1,'=',F6.2,2X))
230   FORMAT ('0PE CONSISTENCY ANALYSIS STATISTICS:',8X,
     *   'JAN    FEB    MAR    APR    MAY    JUN    JUL',
     *   '    AUG    SEP    OCT    NOV    DEC')
240   FORMAT ('0',16X,'PE SUM FOR LAST 12 MONTHS',2X,12(F5.2,2X))
250   FORMAT ('0NUMBER OF VALUES IN PESUM FOR EACH MONTH',3X,
     *   12(I3,4X))
260   FORMAT ('0JULIAN DATE OF LAST DAY INCLUDED IN PESUM = ',I3)
270   FORMAT('0NUMBER OF UNUSED POSITIONS = ',I2)
280   FORMAT(' *** EXIT SPPE')
C
      END
