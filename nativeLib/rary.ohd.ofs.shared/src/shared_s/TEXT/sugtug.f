C MODULE SUGTUG
C----------------------------------------------------------------------
C
C  ROUTINE TO READ USER GENERAL PARAMETERS AND FILL COMMON BLOCK.
C
      SUBROUTINE SUGTUG (LARRAY,ARRAY,ISTAT)
C
      CHARACTER*4 UNITS
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sugnlx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sugtug.f,v $
     . $',                                                             '
     .$Id: sugtug.f,v 1.2 1998/04/07 11:46:22 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,10)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UTIL')
C
      ISTAT=0
C
C  READ USER GENERAL PARAMETERS
      IPRERR=1
      INCLUDE 'scommon/callsrugnl'
      IF (IERR.GT.0) ISTAT=IERR
C
      IF (ISTAT.EQ.0) THEN
         IUGFIL=1
         IF (LDEBUG.GT.0) THEN
            UNITS='ENGL'
            INCLUDE 'scommon/callspugnl'
           ENDIF
        ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,20) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER SUGTUG')
20    FORMAT (' *** EXIT SUGTUG : ISTAT=',I2)
C
      END
