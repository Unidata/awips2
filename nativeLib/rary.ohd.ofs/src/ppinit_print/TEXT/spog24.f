C MEMBER SPOG24
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 09/15/94.07:34:04 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC PRINT GRID-POINT STATION ALPHABETICAL ORDER
C
      SUBROUTINE SPOG24 (IVOG24,UNUSED,ISORT,IPNTRS,NUMSTA,
     *   LARRAY,ARRAY,ISTAT)
C
      INTEGER*2 IPNTRS(1)
      REAL*4 XOLD/4HOLD /
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(1)
C
      INCLUDE 'scommon/dimstan'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/spog24.f,v $
     . $',                                                             '
     .$Id: spog24.f,v 1.1 1995/09/17 19:14:17 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,70)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('OG24')
C
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      WRITE (LP,80)
      CALL SULINE (LP,2)
      WRITE (LP,90)
      CALL SULINE (LP,2)
      WRITE (LP,100)
      CALL SULINE (LP,2)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER AND NUMBER OF STATIONS
      IF (LDEBUG.EQ.0) GO TO 10
         WRITE (LP,110) IVOG24
         CALL SULINE (LP,2)
         WRITE (LP,120) NUMSTA
         CALL SULINE (LP,2)
C
10    IF (NUMSTA.GT.0) GO TO 20
         WRITE (LP,120) NUMSTA
         CALL SULINE (LP,2)
         GO TO 50
C
C  PRINT HOW LIST WAS SORTED
20    IF (ISORT.EQ.1) WRITE (LP,130)
      IF (ISORT.EQ.2) WRITE (LP,140)
      CALL SULINE (LP,2)
C
C  PRINT RECORD NUMBER OF GENL PARAMETERS, GRID-POINT ADDRESS, STATION
C  NAME AND STATION DESCRIPTION
      IF (ISLEFT(5).GT.0) CALL SUPAGE
      WRITE (LP,150)
      CALL SULINE (LP,3)
      IPRERR=1
      RDISP=XOLD
      DO 40 I=1,NUMSTA
         CALL UREPET (' ',STAID,8)
         IPOS=I*2-1
         IPTR=IPNTRS(IPOS)
         IF (IPTR.LE.0) THEN
            WRITE (LP,85) IPTR,IPOS
            CALL SUWRNS (LP,2,-1)
            GO TO 40
            ENDIF
      INCLUDE 'scommon/callsrstan'
         IF (IERR.GT.0) THEN
            ISTAT=1
            GO TO 40
            ENDIF
         IF (ISNWPG(LP).EQ.0) GO TO 35
            WRITE (LP,90)
            CALL SULINE (LP,2)
            WRITE (LP,100)
            CALL SULINE (LP,2)
            WRITE (LP,150)
            CALL SULINE (LP,3)
35       NGPA=IPNTRS(I*2)
         WRITE (LP,160) I,IPTR,NGPA,STATE,STAID,DESCRP
         CALL SULINE (LP,1)
40       CONTINUE
      WRITE (LP,170)
      CALL SULINE (LP,2)
      WRITE (LP,175)
      CALL SULINE (LP,2)
C
C  PRINT NUMBER OF UNUSED POSITIONS
50    IF (LDEBUG.EQ.0) GO TO 60
         NUNUSD=2
         WRITE (LP,180) NUNUSD
         CALL SULINE (LP,2)
C
60    WRITE (LP,80)
      CALL SULINE (LP,2)
      WRITE (LP,100)
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,190)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER SPOG24')
80    FORMAT ('0',132('-'))
85    FORMAT ('0*** WARNING - IN SPOG24 - STAN RECORD NUMBER (',I5,
     *   ') FOUND AT POSITION ',I5,
     *   ' OF OG24 PARAMETERS IS LESS THAN ZERO.')
90    FORMAT ('0*--> OG24 PARAMTERS ',
     *   '(GRID-POINT STATION ALPHABETICAL ORDER)')
100   FORMAT (' ')
110   FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
120   FORMAT ('0NUMBER OF GRID-POINT STATIONS = ',I4)
130   FORMAT ('0LIST ORDERED BY STATE AND STATION IDENTIFIER')
140   FORMAT ('0LIST ORDERED BY STATE AND STATION DESCRIPTION')
150   FORMAT ('0',8X,'PNTR ',3X,'GPA ',3X,'STATE',3X,'IDENTIFIER',
     *   3X,'DESCRIPTION' /
     *   9X,5('-'),3X,4('-'),3X,5('-'),3X,10('-'),3X,20('-'))
160   FORMAT (3X,I4,2X,I5,3X,I4,5X,A2,4X,2A4,5X,5A4)
170   FORMAT ('0',8X,'PNTR = RECORD NUMBER OF THE GENL PARAMETER ',
     *   'ARRAY IN THE PREPROCESSOR PARAMETRIC DATA BASE.')
175   FORMAT ('0',8X,'GPA  = GRID-POINT ADDRESS STORED IN THE GENL ',
     *   'PARAMETER ARRAY.')
180   FORMAT ('0NUMBER OF UNUSED POSITIONS = ',I2)
190   FORMAT (' *** EXIT SPOG24')
C
      END
