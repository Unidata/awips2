C MEMBER SUDCDU
C-----------------------------------------------------------------------
C
C DESC DECODE UNITS AND DEGMIN OPTION FOR PRINTING PARAMETERS
C
      SUBROUTINE SUDCDU (XUNITS,UNITS,DEGMIN,ISTAT)
C
      REAL YES/4HYES /,NO/4HNO  /
      REAL ENGL/4HENGL/,METR/4HMETR/
      REAL BLNK/4H    /
C
      DIMENSION MINDEG(4)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sudcdu.f,v $
     . $',                                                             '
     .$Id: sudcdu.f,v 1.1 1995/09/17 19:15:21 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,200)
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HSYS )
C
      ISTAT=0
      NUMERR=0
C
      IF (LDEBUG.EQ.0) GO TO 10
         WRITE (IOSDBG,210) XUNITS
         CALL SULINE (IOSDBG,1)
C
C  DECODE UNITS
10    UNITS=BLNK
      CALL UINDEX (XUNITS,4,2HEN,2,ICOL)
      IF (ICOL.GT.0) UNITS=ENGL
      CALL UINDEX (XUNITS,4,2HME,2,ICOL)
      IF (ICOL.GT.0) UNITS=METR
      IF (UNITS.EQ.ENGL.OR.UNITS.EQ.METR) GO TO 20
         WRITE (LP,240) XUNITS
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         UNITS=ENGL
20    DEGMIN=NO
      CALL UINDEX (XUNITS,4,2HDM,2,ICOL)
      IF (ICOL.GT.0) DEGMIN=YES
      IF (LDEBUG.GT.0) WRITE (IOSDBG,250) UNITS,DEGMIN
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      IF (ISTRCE.GT.0) WRITE (IOSDBG,530) ISTAT
      IF (ISTRCE.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
200   FORMAT (' *** ENTER SUDCDU')
210   FORMAT (' XUNITS=',A4)
240   FORMAT ('0*** ERROR - IN SUDCDU - REQUESTED DATA UNITS CANNOT ',
     *   'BE DECODED FROM VALUE PASSED : ',A4,'. ENGL UNITS ASSUMMED.')
250   FORMAT (' UNITS=',A4,3X,'DEGMIN=',A4)
530   FORMAT (' *** EXIT SUDCDU - STATUS CODE=',I2)
C
      END
