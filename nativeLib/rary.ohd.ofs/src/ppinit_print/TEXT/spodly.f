C MEMBER SPODLY
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 09/15/94.13:57:37 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC PRINT DAILY DATA STATION ALPHABETICAL ORDER
C
      SUBROUTINE SPODLY (TYPE,IVTYPE,UNUSED,ISORT,IPNTRS,NUMSTA,
     *   LARRAY,ARRAY,ISTAT)
C
      CHARACTER*4 TYPE,XTYPE
C
      INTEGER*2 IPNTRS(*)
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(*)
C
C  STAN PARAMETER ARRAYS
      CHARACTER*4 STAID(2),DESCRP(5),STATE
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/spodly.f,v $
     . $',                                                             '
     .$Id: spodly.f,v 1.2 1996/05/07 11:35:09 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(TYPE)
C
      ISTAT=0
      NUMERR=0
C
C  CHECK FOR VALID TYPE
      IF (TYPE.EQ.'OP24'.OR.
     *    TYPE.EQ.'OPVR'.OR.
     *    TYPE.EQ.'OT24'.OR.
     *    TYPE.EQ.'OE24') THEN
         ELSE
            WRITE (LP,140) TYPE
            CALL SUERRS (LP,2,NUMERR)
            ISTAT=1
            GO TO 90
         ENDIF
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      WRITE (LP,110)
      CALL SULINE (LP,2)
      WRITE (LP,120) TYPE
      CALL SULINE (LP,2)
      WRITE (LP,130)
      CALL SULINE (LP,2)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER AND NUMBER OF STATIONS
      IF (LDEBUG.GT.0) THEN
         WRITE (LP,150) IVODLY
         CALL SULINE (LP,2)
         WRITE (LP,160) TYPE,NUMSTA
         CALL SULINE (LP,2)
         ENDIF
C
C  PRINT HOW LIST WAS SORTED
      IF (ISORT.EQ.1) THEN
         WRITE (LP,170)
         CALL SULINE (LP,2)
         ENDIF
      IF (ISORT.EQ.2) THEN
         WRITE (LP,180)
         CALL SULINE (LP,2)
         ENDIF
C
C  CHECK NUMBER OF STATIONS
      IF (NUMSTA.EQ.0) THEN
         WRITE (LP,160) TYPE,NUMSTA
         CALL SULINE (LP,2)
         GO TO 70
         ENDIF
C
C  PRINT PCPN PREPROCESSOR DATA BASE POINTER, STATION NAME AND
C  DESCRIPTION
      WRITE (LP,190)
      CALL SULINE (LP,3)
      XTYPE='????'
      IF (TYPE.EQ.'OP24') XTYPE='PP24'
      IF (TYPE.EQ.'OPVR') XTYPE='PPVR'
      IF (TYPE.EQ.'OT24') XTYPE='TM24'
      IF (TYPE.EQ.'OE24') XTYPE='EA24'
      DO 60 I=1,NUMSTA
         DO 40 ND=1,5
            DESCRP(ND)='????'
40          CONTINUE
         STATE='????'
         IPPDB=IPNTRS(I)
         ITM=I
         CALL SUGTID (ITM,XTYPE,IPPDB,STAID,DESCRP,STATE,IESTCH,
     *      LARRAY,ARRAY,IERR)
         IF (IERR.NE.0) THEN
            ISTAT=1
            GO TO 60
            ENDIF
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,120) TYPE
            CALL SULINE (LP,2)
            WRITE (LP,130)
            CALL SULINE (LP,2)
            WRITE (LP,190)
            CALL SULINE (LP,3)
            ENDIF
         WRITE (LP,200) I,IPPDB,STATE,STAID,DESCRP
         CALL SULINE(LP,1)
60       CONTINUE
      WRITE (LP,210)
      CALL SULINE (LP,2)
C
C  PRINT NUMBER OF UNUSED POSITIONS
70    IF (LDEBUG.GT.0) THEN
         NUNUSD=2
         WRITE (LP,220) NUNUSD
         CALL SULINE (LP,2)
         ENDIF
C
      WRITE (LP,110)
      CALL SULINE (LP,2)
C
90    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,230)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT (' *** ENTER SPODLY')
110   FORMAT ('0',132('-'))
120   FORMAT ('0*--> ',A4,' PARAMETERS ',
     *   '(STATION ALPHABETICAL ORDER)')
130   FORMAT (' ')
140   FORMAT ('0*** ERROR IN SPODLY - INVALID TYPE CODE : ',A4)
150   FORMAT ('0PARAMETER ARRAY VERSION NUMBER = ',I2)
160   FORMAT ('0NUMBER OF ',A4,' STATIONS = ',I4)
170   FORMAT ('0LIST ORDERED BY STATE AND STATION IDENTIFIER')
180   FORMAT ('0LIST ORDERED BY STATE AND STATION DESCRIPTION')
190   FORMAT ('0',8X,'PNTR ',3X,'STATE',3X,'IDENTIFIER',3X,'DESCRIPTION'
     *   / 9X,5('-'),3X,5('-'),3X,10('-'),3X,20('-'))
200   FORMAT (3X,I4,2X,I5,3X,A4,4X,2A4,5X,5A4)
210   FORMAT ('0',8X,'PNTR = LOCATION OF THE POINTERS ',
     *   'IN THE POINTER ARRAY RETURNED FROM THE PREPROCESSOR ',
     *   'DATA BASE ROUTINE RPPDLY.')
220   FORMAT ('0NUMBER OF UNUSED POSITIONS = ',I2)
230   FORMAT (' *** EXIT SPODLY')
C
      END
