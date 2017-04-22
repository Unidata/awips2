C MEMBER SUGTBX
C-----------------------------------------------------------------------
C
C DESC ROUTINE TO FILL GRID BLOCK USAGE COMMON BLOCK
C
      SUBROUTINE SUGTBX (LARRAY,ARRAY,ISTAT)
C
C
      REAL*8 BLNK8/8H        /
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(2)
      INCLUDE 'scommon/dimgbox'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sgboxx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sugtbx.f,v $
     . $',                                                             '
     .$Id: sugtbx.f,v 1.1 1995/09/17 19:15:30 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,70)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HGBOX)
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,80) IBXFIL
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      ISTAT=0
      NGBOX=0
C
      DO 10 I=1,99
         IGBOX(I)=0
         GBOXLT(I)=0.0
         GBOXLN(I)=0.0
         IGBOXM(I)=0
10       CONTINUE
C
C  READ GBOX PARAMETERS
      IPTR=0
      IPRERR=0
20    CALL SUBSTR (BLNK8,1,8,GBOXID,1)
      INCLUDE 'scommon/callsrgbox'
      IF (IERR.EQ.2.AND.NGBOX.EQ.0) GO TO 50
      IF (IERR.EQ.6) GO TO 50
      IF (IERR.GT.0)
     *   CALL SRPPST (GBOXID,4HGBOX,IPTR,LARRAY,NFILL,IPTRNX,IERR)
      IRGBOX=IERR
      IF (IRGBOX.GT.0) GO TO 30
      IF (LDEBUG.LE.1) GO TO 30
      INCLUDE 'scommon/callspgbox'
30    IF (IRGBOX.GT.0) GO TO 35
         IGBOX(NUGBOX)=1
         GBOXLT(NUGBOX)=BOXLOC(1)
         GBOXLN(NUGBOX)=BOXLOC(2)
         IGBOXM(NUGBOX)=IBXMDR
         NGBOX=NGBOX+1
35    IF (IPTRNX.EQ.0) GO TO 40
         IPTR=IPTRNX
         GO TO 20
C
C  SET INDICATOR THAT COMMON BLOCK FILLED
40    IBXFIL=1
C
50    IF (LDEBUG.EQ.0) GO TO 60
         WRITE (IOSDBG,90) IBXFIL,NGBOX
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,100)
         CALL SULINE (IOSDBG,1)
         DO 55 I=1,99
            WRITE (IOSDBG,105) I,IGBOX(I),GBOXLT(I),GBOXLN(I),IGBOXM(I)
            CALL SULINE (IOSDBG,1)
55          CONTINUE
C
60    IF (ISTRCE.GT.0) WRITE (IOSDBG,110)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER SUGTBX')
80    FORMAT (' IBXFIL=',I2)
90    FORMAT (' IBXFIL=',I2,3X,'NGBOX=',I2)
100   FORMAT (5X,'IGBOX',3X,'GBOXLT',4X,'GBOXLN',3X,'IGBOXM')
105   FORMAT (2X,I2,1X,I5,3X,F7.2,3X,F7.2,3X,I4)
110   FORMAT (' *** EXIT SUGTBX')
C
      END
