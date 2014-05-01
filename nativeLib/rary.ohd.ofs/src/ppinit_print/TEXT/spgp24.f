C MODULE SPGP24
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT GP24 PARAMETERS
C
      SUBROUTINE SPGP24 (IVGP24,NGP24,IGP24,NSGP24,ISGP24,UNUSED,ISTAT)
C
      REAL NONE(2)/4H**NO,4HNE**/
C
      INTEGER*2 IGP24(1),ISGP24(1)
C
      DIMENSION UNUSED(1)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_print/RCS/spgp24.f,v $
     . $',                                                             '
     .$Id: spgp24.f,v 1.2 2001/06/13 14:03:22 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SPGP24'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PRINT DEBUG LEVEL
      LDEBUG=ISBUG('GP24')
      ISTAT=0
C
C  CHECK NUMBER OF LINES LEFT ON PAGE
      IF (ISLEFT(5).GT.0) CALL SUPAGE
C
C  PRINT HEADING
      WRITE (LP,110)
      CALL SULINE (LP,2)
      WRITE (LP,120)
      CALL SULINE (LP,2)
      WRITE (LP,130)
      CALL SULINE (LP,2)
C
C  PRINT PARAMETER ARRAY VERSION NUMBER,
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,140) IVGP24,NGP24,NSGP24
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PRINT STATION GRID-POINT ADDRESS
      NPER=5
      NTIME=NGP24/NPER
      IF (MOD(NGP24,NPER).NE.0) NTIME=NTIME+1
      IF (NTIME.EQ.0) NTIME=1
      NUM1=1
      NUM2=NGP24
      NCHK=NTIME
      IF (NCHK.GT.NPSMLN) NCHK=10
      IF (ISLEFT(2+NCHK).GT.0) CALL SUPAGE
      WRITE (LP,180)
      CALL SULINE (LP,2)
      DO 30 I=1,NTIME
         WRITE (LP,200) (J,IGP24(J*2-1),J=NUM1,NUM2,NTIME)
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,180)
            CALL SULINE (LP,2)
            ENDIF
         NUM1=NUM1+1
30       CONTINUE
      NCHK=NTIME
      IF (NCHK.GT.NPSMLN) NCHK=10
      IF (ISLEFT(2+NCHK).GT.0) CALL SUPAGE
      WRITE (LP,190)
      CALL SULINE (LP,2)
      NUM1=1
      DO 40 I=1,NTIME
         WRITE (LP,200) (J,IGP24(J*2),J=NUM1,NUM2,NTIME)
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,190)
            CALL SULINE (LP,2)
            ENDIF
         NUM1=NUM1+1
40       CONTINUE
C
C  PRINT PP24 DATA POINTERS FOR STATIONS THAT SHARE SAME GRID-POINT
C  ADDRESS
      IF (NSGP24.EQ.0) THEN
         WRITE (LP,210) NONE
         CALL SULINE (LP,2)
         GO TO 80
         ENDIF
      NPER=5
      NTIME=NSGP24/NPER
      IF (MOD(NSGP24,NPER).NE.0) NTIME=NTIME+1
      IF (NTIME.EQ.0) NTIME=1
      NUM1=1
      NUM2=NSGP24
      NCHK=NTIME
      IF (NCHK.GT.NPSMLN) NCHK=10
      IF (ISLEFT(2+NCHK).GT.0) CALL SUPAGE
      WRITE (LP,220)
      CALL SULINE (LP,2)
      DO 70 I=1,NTIME
         WRITE (LP,200) (J,ISGP24(J),J=NUM1,NUM2,NTIME)
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,220)
            CALL SULINE (LP,2)
            ENDIF
         NUM1=NUM1+1
70       CONTINUE
C
C  PRINT NUMBER OF UNUSED POSITIONS
80    IF (LDEBUG.EQ.0) GO TO 90
         NUNUSD=2
         WRITE (IOSDBG,150) NUNUSD
         CALL SULINE (IOSDBG,1)
C
90    WRITE (LP,110)
      CALL SULINE (LP,2)
      WRITE (LP,130)
      CALL SULINE (LP,2)
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SPGP24'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT ('0',132('-'))
120   FORMAT ('0*-->  GP24 PARAMETERS')
130   FORMAT (' ')
140   FORMAT (' IVGP24=',I2,3X,'NGP24=',I4,3X,'NSGP24=',I4)
150   FORMAT (' NUMBER OF UNUSED POSITIONS = ',I2)
180   FORMAT ('0',T5,'STATION GRID-POINT ADDRESS')
190   FORMAT ('0',T5,'NUMBER OF STATIONS AT GRID-POINT ADDRESS AND ',
     *   'POINTER TO SHARED GRID-POINT ADDRESSES')
200   FORMAT (T10,5('(',I5,')',2X,I5,10X))
210   FORMAT ('0',T5,'PP24 DATA POINTERS FOR STATIONS THAT SHARE ',
     *   'THE SAME GRID-POINT ADDRESS = ',2A4)
220   FORMAT ('0',T5,'PP24 DATA POINTERS FOR STATIONS THAT SHARE ',
     *   'THE SAME GRID-POINT ADDRESS')
C
      END
