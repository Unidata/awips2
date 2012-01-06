C MODULE SUGTDS
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET STATION DESCRIPTION FROM THE PREPROCESSOR
C  PARAMETRIC DATA BASE
C
      SUBROUTINE SUGTDS (STAID,DESCRP,STATE,STACOR,STAELV,LARRAY,ARRAY,
     *   TYPMSG,ISTAT)
C
      CHARACTER*(*) TYPMSG
      CHARACTER*4 RDISP
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'scommon/dimstan'
      DIMENSION UNUSED(10)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sugtds.f,v $
     . $',                                                             '
     .$Id: sugtds.f,v 1.3 1998/04/09 12:23:53 page Exp $
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
      LDEBUG=ISBUG('SYS ')
C
      ISTAT=0
C
      DESCRP=' '
C
C  READ PARAMETER RECORD
      IPTR=0
      IPRERR=0
      IF (LDEBUG.GT.0) IPRERR=1
      RDISP='OLD'
      INCLUDE 'scommon/callsrstan'
      IF (IERR.GT.0) THEN
         IF (TYPMSG.EQ.'NONE') THEN
            ELSE
               IF (TYPMSG.EQ.'NOTE') THEN
                  WRITE (LP,20) TYPMSG(1:LENSTR(TYPMSG)),STAID
                  CALL SULINE (LP,2)
                  ENDIF
               IF (TYPMSG.EQ.'ERROR') THEN
                  WRITE (LP,20) TYPMSG(1:LENSTR(TYPMSG)),STAID,IERR
                  CALL SUERRS (LP,2,-1)
                  ENDIF
               IF (TYPMSG.EQ.'WARNING') THEN
                  WRITE (LP,20) TYPMSG(1:LENSTR(TYPMSG)),STAID,IERR
                  CALL SUWRNS (LP,2,-1)
                  ENDIF
            ENDIF
         ISTAT=1
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *     ' STAID=',STAID,
     *     ' DESCRP=',DESCRP,
     *     ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,30) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER SUGTDS')
20    FORMAT ('0*** ',A,' - IN SUGTDS - CANNOT READ STAN PARAMETERS ',
     *   'FOR STATION ',A,'. STATION MAY NOT BE DEFINED. ' :
     *   '(SRSTAN STATUS CODE=',I2,').')
30    FORMAT (' *** EXIT SUGTDS - ISTAT=',I2)
C
      END
